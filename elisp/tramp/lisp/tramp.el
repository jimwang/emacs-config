;;; -*- mode: Emacs-Lisp; coding: iso-2022-7bit; -*-
;;; tramp.el --- Transparent Remote Access, Multiple Protocol

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Kai Gro,A_(Bjohann <kai.grossjohann@gmx.net>
;;         Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides remote file editing, similar to ange-ftp.
;; The difference is that ange-ftp uses FTP to transfer files between
;; the local and the remote host, whereas tramp.el uses a combination
;; of rsh and rcp or other work-alike programs, such as ssh/scp.
;;
;; For more detailed instructions, please see the info file.
;;
;; Notes:
;; -----
;;
;; This package only works for Emacs 21.1 and higher, and for XEmacs 21.4
;; and higher.  For XEmacs 21, you need the package `fsf-compat' for
;; the `with-timeout' macro.)
;;
;; This version might not work with pre-Emacs 21 VC unless VC is
;; loaded before tramp.el.  Could you please test this and tell me about
;; the result?  Thanks.
;;
;; Also see the todo list at the bottom of this file.
;;
;; The current version of Tramp can be retrieved from the following URL:
;;            http://ftp.gnu.org/gnu/tramp/
;;
;; There's a mailing list for this, as well.  Its name is:
;;            tramp-devel@gnu.org
;; You can use the Web to subscribe, under the following URL:
;;            http://lists.gnu.org/mailman/listinfo/tramp-devel
;;
;; For the adventurous, the current development sources are available
;; via CVS.  You can find instructions about this at the following URL:
;;            http://savannah.gnu.org/projects/tramp/
;; Click on "CVS" in the navigation bar near the top.
;;
;; Don't forget to put on your asbestos longjohns, first!

;;; Code:

;; The Tramp version number and bug report address, as prepared by configure.
(require 'trampver)

(if (featurep 'xemacs)
    (require 'timer-funcs)
  (require 'timer))

(require 'format-spec)                  ;from Gnus 5.8, also in tar ball
;; As long as password.el is not part of (X)Emacs, it shouldn't
;; be mandatory
(if (featurep 'xemacs)
    (load "password" 'noerror)
  (require 'password nil 'noerror))     ;from No Gnus, also in tar ball

;; The explicit check is not necessary in Emacs, which provides the
;; feature even if implemented in C, but it appears to be necessary
;; in XEmacs.
(unless (and (fboundp 'base64-encode-region)
	     (fboundp 'base64-decode-region))
  (require 'base64))                       ;for the mimencode methods
(require 'shell)
(require 'advice)

(require 'tramp-cache)

(autoload 'tramp-uuencode-region "tramp-uu"
  "Implementation of `uuencode' in Lisp.")

(unless (fboundp 'uudecode-decode-region)
  (autoload 'uudecode-decode-region "uudecode"))

;; Load foreign methods.  Because they do require Tramp internally, this
;; must be done with the `eval-after-load' trick.

(eval-after-load "tramp"
  (if (featurep 'xemacs)
      '(require 'tramp-efs)
    '(require 'tramp-ftp)))

;; tramp-smb uses "smbclient" from Samba.
;; Not available under Cygwin and Windows, because they don't offer
;; "smbclient".  And even not necessary there, because Emacs supports
;; UNC file names like "//host/share/localname".
(unless (memq system-type '(cygwin windows-nt))
  (eval-after-load "tramp"
    '(require 'tramp-smb)))

;; tramp-util offers integration into other (X)Emacs packages like
;; compile.el, gud.el etc.
(unless (memq system-type '(cygwin windows-nt))
  (eval-after-load "tramp"
    '(require 'tramp-util)))

(eval-when-compile
  (require 'cl)
  (require 'custom))

;; Avoid byte-compiler warnings if the byte-compiler supports this.
;; Currently, XEmacs supports this.
(eval-when-compile
  (when (featurep 'xemacs)
    (byte-compiler-options (warnings (- unused-vars)))))

;; `last-coding-system-used' in unknown in XEmacs.
(eval-when-compile
  (unless (boundp 'last-coding-system-used)
    (defvar last-coding-system-used nil)))

;; `directory-sep-char' is an obsolete variable in Emacs.  But it is
;; used in XEmacs, so we set it here and there.  The following is needed
;; to pacify Emacs byte-compiler.
(eval-when-compile
  (unless (boundp 'byte-compile-not-obsolete-var)
    (defvar byte-compile-not-obsolete-var nil))
  (setq byte-compile-not-obsolete-var 'directory-sep-char))

;;; User Customizable Internal Variables:

(defgroup tramp nil
  "Edit remote files with a combination of rsh and rcp or similar programs."
  :group 'files
  :version "22.1")

(defcustom tramp-verbose 3
  "*Verbosity level for tramp.
Any level x includes messages for all levels 1 .. x-1.  The levels are

 0  silent (no tramp messages at all)
 1  errors
 2  warnings
 3  connection to remote hosts (default level)
 4  activities
 5  internal
 6  caching
 7  connection properties
 9  sent and received strings
10  traces (huge)."
  :group 'tramp
  :type 'integer)

(defcustom tramp-debug-buffer nil
  "*Whether to send all commands and responses to a debug buffer."
  :group 'tramp
  :type 'boolean)

;; Emacs case
(eval-and-compile
  (when (boundp 'backup-directory-alist)
    (defcustom tramp-backup-directory-alist nil
      "Alist of filename patterns and backup directory names.
Each element looks like (REGEXP . DIRECTORY), with the same meaning like
in `backup-directory-alist'.  If a Tramp file is backed up, and DIRECTORY
is a local file name, the backup directory is prepended with Tramp file
name prefix \(method, user, host\) of file.

\(setq tramp-backup-directory-alist backup-directory-alist\)

gives the same backup policy for Tramp files on their hosts like the
policy for local files."
      :group 'tramp
      :type '(repeat (cons (regexp :tag "Regexp matching filename")
			   (directory :tag "Backup directory name"))))))

;; XEmacs case.  We cannot check for `bkup-backup-directory-info', because
;; the package "backup-dir" might not be loaded yet.
(eval-and-compile
  (when (featurep 'xemacs)
    (defcustom tramp-bkup-backup-directory-info nil
      "*Alist of (FILE-REGEXP BACKUP-DIR OPTIONS ...))
It has the same meaning like `bkup-backup-directory-info' from package
`backup-dir'.  If a Tramp file is backed up, and BACKUP-DIR is a local
file name, the backup directory is prepended with Tramp file name prefix
\(method, user, host\) of file.

\(setq tramp-bkup-backup-directory-info bkup-backup-directory-info\)

gives the same backup policy for Tramp files on their hosts like the
policy for local files."
      :type '(repeat
	      (list (regexp :tag "File regexp")
		    (string :tag "Backup Dir")
		    (set :inline t
			 (const ok-create)
			 (const full-path)
			 (const prepend-name)
			 (const search-upward))))
      :group 'tramp)))

(defcustom tramp-auto-save-directory nil
  "*Put auto-save files in this directory, if set.
The idea is to use a local directory so that auto-saving is faster."
  :group 'tramp
  :type '(choice (const nil)
                 string))

(defcustom tramp-encoding-shell
  (if (memq system-type '(windows-nt))
      (getenv "COMSPEC")
    "/bin/sh")
  "*Use this program for encoding and decoding commands on the local host.
This shell is used to execute the encoding and decoding command on the
local host, so if you want to use `~' in those commands, you should
choose a shell here which groks tilde expansion.  `/bin/sh' normally
does not understand tilde expansion.

For encoding and deocding, commands like the following are executed:

    /bin/sh -c COMMAND < INPUT > OUTPUT

This variable can be used to change the \"/bin/sh\" part.  See the
variable `tramp-encoding-command-switch' for the \"-c\" part.  Also, see the
variable `tramp-encoding-reads-stdin' to specify whether the commands read
standard input or a file.

Note that this variable is not used for remote commands.  There are
mechanisms in tramp.el which automatically determine the right shell to
use for the remote host."
  :group 'tramp
  :type '(file :must-match t))

(defcustom tramp-encoding-command-switch
  (if (string-match "cmd\\.exe" tramp-encoding-shell)
      "/c"
    "-c")
  "*Use this switch together with `tramp-encoding-shell' for local commands.
See the variable `tramp-encoding-shell' for more information."
  :group 'tramp
  :type 'string)

(defcustom tramp-encoding-reads-stdin t
  "*If non-nil, encoding commands read from standard input.
If nil, the filename is the last argument.

Note that the commands always must write to standard output."
  :group 'tramp
  :type 'boolean)

;; CCC I have changed all occurrences of comint-quote-filename with
;; tramp-shell-quote-argument, except in tramp-handle-expand-many-files.
;; There, comint-quote-filename was removed altogether.  If it turns
;; out to be necessary there, something will need to be done.
;;-(defcustom tramp-file-name-quote-list
;;-  '(?] ?[ ?\| ?& ?< ?> ?\( ?\) ?\; ?\  ?\* ?\? ?\! ?\" ?\' ?\` ?# ?\@ ?\+ )
;;-  "*Protect these characters from the remote shell.
;;-Any character in this list is quoted (preceded with a backslash)
;;-because it means something special to the shell.  This takes effect
;;-when sending file and directory names to the remote shell.
;;-
;;-See `comint-file-name-quote-list' for details."
;;-  :group 'tramp
;;-  :type '(repeat character))

(defcustom tramp-methods
  '( ("rcp"   (tramp-login-program        "rsh")
              (tramp-copy-program         "rcp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("scp"   (tramp-login-program        "ssh")
              (tramp-copy-program         "scp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-e" "none")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("scp1"  (tramp-login-program        "ssh")
              (tramp-copy-program         "scp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-1" "-e" "none")))
              (tramp-copy-args            ("-1"))
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("scp2"  (tramp-login-program        "ssh")
              (tramp-copy-program         "scp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-2" "-e" "none")))
              (tramp-copy-args            ("-2"))
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("scp1_old"
              (tramp-login-program        "ssh1")
              (tramp-copy-program         "scp1")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-e" "none")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("scp2_old"
              (tramp-login-program        "ssh2")
              (tramp-copy-program         "scp2")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-e" "none")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("sftp"  (tramp-login-program        "ssh")
              (tramp-copy-program         "sftp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-e" "none")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("rsync" (tramp-login-program        "ssh")
              (tramp-copy-program         "rsync")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-e" "none")))
              (tramp-copy-args            ("-e" "ssh"))
              (tramp-copy-keep-date-arg   "-t")
	      (tramp-password-end-of-line nil))
     ("remcp" (tramp-login-program        "remsh")
              (tramp-copy-program         "rcp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("rsh"   (tramp-login-program        "rsh")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("ssh"   (tramp-login-program        "ssh")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-e" "none")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("ssh1"  (tramp-login-program        "ssh")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-1" "-e" "none")))
              (tramp-copy-args            ("-1"))
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("ssh2"  (tramp-login-program        "ssh")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-2" "-e" "none")))
              (tramp-copy-args            ("-2"))
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("ssh1_old"
              (tramp-login-program        "ssh1")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-e" "none")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("ssh2_old"
              (tramp-login-program        "ssh2")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-e" "none")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("remsh" (tramp-login-program        "remsh")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("telnet"
              (tramp-login-program        "telnet")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("su"    (tramp-login-program        "su")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("-") ("%u")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("sudo"  (tramp-login-program        "sudo")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("-u" "%u")
					   ("-s" "-p" "Password:")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("scpx"  (tramp-login-program        "ssh")
              (tramp-copy-program         "scp")
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-e" "none" "-t" "-t" "/bin/sh")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     ("sshx"  (tramp-login-program        "ssh")
              (tramp-copy-program         nil)
              (tramp-remote-sh            "/bin/sh")
              (tramp-login-args           (("%h") ("-l" "%u") ("-p" "%p")
					   ("-e" "none" "-t" "-t" "/bin/sh")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("krlogin"
	      (tramp-login-program        "krlogin")
	      (tramp-copy-program         nil)
	      (tramp-remote-sh            "/bin/sh")
	      (tramp-login-args           (("%h") ("-l" "%u") ("-x")))
	      (tramp-copy-args            nil)
	      (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line nil))
     ("plink" (tramp-login-program        "plink")
	      (tramp-copy-program         nil)
	      (tramp-remote-sh            "/bin/sh")
	      (tramp-login-args           (("%h") ("-l" "%u") ("-P" "%p")
					   ("-ssh")))
					;optionally add "-v"
	      (tramp-copy-args            nil)
	      (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line "xy")) ;see docstring for "xy"
     ("plink1"
	      (tramp-login-program        "plink")
	      (tramp-copy-program         nil)
	      (tramp-remote-sh            "/bin/sh")
	      (tramp-login-args           (("%h") ("-l" "%u") ("-P" "%p")
					   ("-1" "-ssh")))
					;optionally add "-v"
	      (tramp-copy-args            nil)
	      (tramp-copy-keep-date-arg   nil)
	      (tramp-password-end-of-line "xy")) ;see docstring for "xy"
     ("pscp"  (tramp-login-program        "plink")
	      (tramp-copy-program         "pscp")
	      (tramp-remote-sh            "/bin/sh")
	      (tramp-login-args           (("%h") ("-l" "%u") ("-P" "%p")
					   ("-ssh")))
	      (tramp-copy-args            ("-scp"))
	      (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line "xy")) ;see docstring for "xy"
     ("psftp" (tramp-login-program        "plink")
	      (tramp-copy-program         "pscp")
	      (tramp-remote-sh            "/bin/sh")
	      (tramp-login-args           ("%h") ("-l" "%u") ("-P" "%p")
					  ("-ssh"))
	      (tramp-copy-args            ("-psftp"))
	      (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line "xy")) ;see docstring for "xy"
     ("fcp"   (tramp-login-program        "fsh")
              (tramp-copy-program         "fcp")
              (tramp-remote-sh            "/bin/sh -i")
              (tramp-login-args           (("%h") ("-l" "%u") ("sh" "-i")))
              (tramp-copy-args            nil)
              (tramp-copy-keep-date-arg   "-p")
	      (tramp-password-end-of-line nil))
     )
  "*Alist of methods for remote files.
This is a list of entries of the form (NAME PARAM1 PARAM2 ...).
Each NAME stands for a remote access method.  Each PARAM is a
pair of the form (KEY VALUE).  The following KEYs are defined:
  * `tramp-remote-sh'
    This specifies the Bourne shell to use on the remote host.  This
    MUST be a Bourne-like shell.  It is normally not necessary to set
    this to any value other than \"/bin/sh\": tramp wants to use a shell
    which groks tilde expansion, but it can search for it.  Also note
    that \"/bin/sh\" exists on all Unixen, this might not be true for
    the value that you decide to use.  You Have Been Warned.
  * `tramp-login-program'
    This specifies the name of the program to use for logging in to the
    remote host.  This may be the name of rsh or a workalike program,
    or the name of telnet or a workalike, or the name of su or a workalike.
  * `tramp-login-args'
    This specifies the list of arguments to pass to the above
    mentioned program.  Please note that this is a list of list of arguments,
    that is, normally you don't want to put \"-a -b\" or \"-f foo\"
    here.  Instead, you want a list (\"-a\" \"-b\"), or (\"-f\" \"foo\").
    There are some patterns: \"%h\" in this list is replaced by the host
    name, \"%u\" is replaced by the user name, \"%p\" is replaced by the
    port number, and \"%%\" can be used to obtain a literal percent character.
    If a list containing \"%h\", \"%u\" or \"%p\" is unchanged during
    expansion (i.e. no host or no user specified), this list is not used as
    argument.  By this, arguments like (\"-l\" \"%u\") are optional.
  * `tramp-copy-program'
    This specifies the name of the program to use for remotely copying
    the file; this might be the absolute filename of rcp or the name of
    a workalike program.
  * `tramp-copy-args'
    This specifies the list of parameters to pass to the above mentioned
    program, the hints for `tramp-login-args' also apply here.
  * `tramp-copy-keep-date-arg'
    This specifies the parameter to use for the copying program when the
    timestamp of the original file should be kept.  For `rcp', use `-p', for
    `rsync', use `-t'.
  * `tramp-password-end-of-line'
    This specifies the string to use for terminating the line after
    submitting the password.  If this method parameter is nil, then the
    value of the normal variable `tramp-default-password-end-of-line'
    is used.  This parameter is necessary because the \"plink\" program
    requires any two characters after sending the password.  These do
    not have to be newline or carriage return characters.  Other login
    programs are happy with just one character, the newline character.
    We use \"xy\" as the value for methods using \"plink\".

What does all this mean?  Well, you should specify `tramp-login-program'
for all methods; this program is used to log in to the remote site.  Then,
there are two ways to actually transfer the files between the local and the
remote side.  One way is using an additional rcp-like program.  If you want
to do this, set `tramp-copy-program' in the method.

Another possibility for file transfer is inline transfer, i.e. the
file is passed through the same buffer used by `tramp-login-program'.  In
this case, the file contents need to be protected since the
`tramp-login-program' might use escape codes or the connection might not
be eight-bit clean.  Therefore, file contents are encoded for transit.
See the variables `tramp-local-coding-commands' and
`tramp-remote-coding-commands' for details.

So, to summarize: if the method is an out-of-band method, then you
must specify `tramp-copy-program' and `tramp-copy-args'.  If it is an
inline method, then these two parameters should be nil.

Notes:

When using `su' or `sudo' the phrase `open connection to a remote host'
sounds strange, but it is used nevertheless, for consistency.
No connection is opened to a remote host, but `su' or `sudo' is
started on the local host.  You are not allowed to specify a remote
host other than `localhost' or the name of the local host."
  :group 'tramp
  :type '(repeat
          (cons string
                (set (list (const tramp-connection-function) function)
                     (list (const tramp-login-program)
			   (choice (const nil) string))
                     (list (const tramp-copy-program)
			   (choice (const nil) string))
                     (list (const tramp-remote-sh)
			   (choice (const nil) string))
                     (list (const tramp-login-args)
			   (list (list (repeat string))))
                     (list (const tramp-copy-args) (repeat string))
                     (list (const tramp-copy-keep-date-arg)
			   (choice (const nil) string))
                     (list (const tramp-encoding-command)
			   (choice (const nil) string))
                     (list (const tramp-decoding-command)
			   (choice (const nil) string))
                     (list (const tramp-encoding-function)
			   (choice (const nil) function))
                     (list (const tramp-decoding-function)
			   (choice (const nil) function))
		     (list (const tramp-password-end-of-line)
			   (choice (const nil) string))))))

(defcustom tramp-default-method
  (if (and (fboundp 'executable-find)
	   (executable-find "plink"))
      "plink"
    "ssh")
  "*Default method to use for transferring files.
See `tramp-methods' for possibilities.
Also see `tramp-default-method-alist'."
  :group 'tramp
  :type 'string)

(defcustom tramp-default-method-alist
  '(("\\`localhost\\'" "\\`root\\'" "su"))
  "*Default method to use for specific host/user pairs.
This is an alist of items (HOST USER METHOD).  The first matching item
specifies the method to use for a file name which does not specify a
method.  HOST and USER are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-method' takes effect.

If the file name does not specify the user, lookup is done using the
empty string for the user name.

See `tramp-methods' for a list of possibilities for METHOD."
  :group 'tramp
  :type '(repeat (list (regexp :tag "Host regexp")
		       (regexp :tag "User regexp")
		       (string :tag "Method"))))

(defcustom tramp-default-user
  nil
  "*Default method to use for transferring files.
It is nil by default; otherwise settings in configuration files like
\"~/.ssh/config\" would be overwritten.  Also see `tramp-default-user-alist'."
  :group 'tramp
  :type 'string)

(defcustom tramp-default-user-alist
  `(("\\`su\\(do\\)?\\'" nil "root")
    ("\\`r\\(em\\)?\\(cp\\|sh\\)\\|telnet\\'" nil ,(user-login-name)))
  "*Default user to use for specific method/host pairs.
This is an alist of items (METHOD HOST USER).  The first matching item
specifies the user to use for a file name which does not specify a
user.  METHOD and USER are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-user' takes effect.

If the file name does not specify the method, lookup is done using the
empty string for the method name."
  :group 'tramp
  :type '(repeat (list (regexp :tag "Method regexp")
		       (regexp :tag "Host regexp")
		       (string :tag "User"))))

(defcustom tramp-default-host
  (system-name)
  "*Default host to use for transferring files.
Useful for su and sudo methods mostly."
  :group 'tramp
  :type 'string)

(defcustom tramp-default-proxies-alist nil
  "*Route to be followed for specific host/user pairs.
This is an alist of items (HOST USER PROXY).  The first matching item
specifies the proxy to be passed for a file name located on a remote target
matching USER@HOST.  HOST and USER are regular expressions or nil, which is
interpreted as a regular expression which always matches.  PROXY must be
a Tramp filename without a localname part.  Method and user name on PROXY
are optional, which is interpreted with the default values."
  :group 'tramp
  :type '(repeat (list (regexp :tag "Host regexp")
		       (regexp :tag "User regexp")
		       (string :tag "Proxy remote name"))))

;; Default values for non-Unices seeked
(defconst tramp-completion-function-alist-rsh
  (unless (memq system-type '(windows-nt))
    '((tramp-parse-rhosts "/etc/hosts.equiv")
      (tramp-parse-rhosts "~/.rhosts")))
  "Default list of (FUNCTION FILE) pairs to be examined for rsh methods.")

;; Default values for non-Unices seeked
(defconst tramp-completion-function-alist-ssh
  (unless (memq system-type '(windows-nt))
    '((tramp-parse-rhosts      "/etc/hosts.equiv")
      (tramp-parse-rhosts      "/etc/shosts.equiv")
      (tramp-parse-shosts      "/etc/ssh_known_hosts")
      (tramp-parse-sconfig     "/etc/ssh_config")
      (tramp-parse-shostkeys   "/etc/ssh2/hostkeys")
      (tramp-parse-sknownhosts "/etc/ssh2/knownhosts")
      (tramp-parse-rhosts      "~/.rhosts")
      (tramp-parse-rhosts      "~/.shosts")
      (tramp-parse-shosts      "~/.ssh/known_hosts")
      (tramp-parse-sconfig     "~/.ssh/config")
      (tramp-parse-shostkeys   "~/.ssh2/hostkeys")
      (tramp-parse-sknownhosts "~/.ssh2/knownhosts")))
  "Default list of (FUNCTION FILE) pairs to be examined for ssh methods.")

;; Default values for non-Unices seeked
(defconst tramp-completion-function-alist-telnet
  (unless (memq system-type '(windows-nt))
    '((tramp-parse-hosts "/etc/hosts")))
  "Default list of (FUNCTION FILE) pairs to be examined for telnet methods.")

;; Default values for non-Unices seeked
(defconst tramp-completion-function-alist-su
  (unless (memq system-type '(windows-nt))
    '((tramp-parse-passwd "/etc/passwd")))
  "Default list of (FUNCTION FILE) pairs to be examined for su methods.")

(defvar tramp-completion-function-alist nil
  "*Alist of methods for remote files.
This is a list of entries of the form (NAME PAIR1 PAIR2 ...).
Each NAME stands for a remote access method.  Each PAIR is of the form
\(FUNCTION FILE).  FUNCTION is responsible to extract user names and host
names from FILE for completion.  The following predefined FUNCTIONs exists:

 * `tramp-parse-rhosts'      for \"~/.rhosts\" like files,
 * `tramp-parse-shosts'      for \"~/.ssh/known_hosts\" like files,
 * `tramp-parse-sconfig'     for \"~/.ssh/config\" like files,
 * `tramp-parse-shostkeys'   for \"~/.ssh2/hostkeys/*\" like files,
 * `tramp-parse-sknownhosts' for \"~/.ssh2/knownhosts/*\" like files,
 * `tramp-parse-hosts'       for \"/etc/hosts\" like files,
 * `tramp-parse-passwd'      for \"/etc/passwd\" like files.
 * `tramp-parse-netrc'       for \"~/.netrc\" like files.

FUNCTION can also be a customer defined function.  For more details see
the info pages.")

(eval-after-load "tramp"
  '(progn
     (tramp-set-completion-function
      "rcp" tramp-completion-function-alist-rsh)
     (tramp-set-completion-function
      "scp" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "scp1" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "scp2" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "scp1_old" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "scp2_old" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "rsync" tramp-completion-function-alist-rsh)
     (tramp-set-completion-function
      "remcp" tramp-completion-function-alist-rsh)
     (tramp-set-completion-function
      "rsh" tramp-completion-function-alist-rsh)
     (tramp-set-completion-function
      "ssh" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "ssh1" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "ssh2" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "ssh1_old" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "ssh2_old" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "remsh" tramp-completion-function-alist-rsh)
     (tramp-set-completion-function
      "telnet" tramp-completion-function-alist-telnet)
     (tramp-set-completion-function
      "su" tramp-completion-function-alist-su)
     (tramp-set-completion-function
      "sudo" tramp-completion-function-alist-su)
     (tramp-set-completion-function
      "scpx" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "sshx" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "krlogin" tramp-completion-function-alist-rsh)
     (tramp-set-completion-function
      "plink" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "plink1" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "pscp" tramp-completion-function-alist-ssh)
     (tramp-set-completion-function
      "fcp" tramp-completion-function-alist-ssh)))

(defcustom tramp-rsh-end-of-line "\n"
  "*String used for end of line in rsh connections.
I don't think this ever needs to be changed, so please tell me about it
if you need to change this.
Also see the method parameter `tramp-password-end-of-line' and the normal
variable `tramp-default-password-end-of-line'."
  :group 'tramp
  :type 'string)

(defcustom tramp-default-password-end-of-line
  tramp-rsh-end-of-line
  "*String used for end of line after sending a password.
This variable provides the default value for the method parameter
`tramp-password-end-of-line', see `tramp-methods' for more details.

It seems that people using plink under Windows need to send
\"\\r\\n\" (carriage-return, then newline) after a password, but just
\"\\n\" after all other lines.  This variable can be used for the
password, see `tramp-rsh-end-of-line' for the other cases.

The default value is to use the same value as `tramp-rsh-end-of-line'."
  :group 'tramp
  :type 'string)

(defcustom tramp-remote-path
  '("/bin" "/usr/bin" "/usr/sbin" "/usr/local/bin" "/usr/ccs/bin"
    "/local/bin" "/local/freeware/bin" "/local/gnu/bin"
    "/usr/freeware/bin" "/usr/pkg/bin" "/usr/xpg4/bin" "/usr/contrib/bin")
  "*List of directories to search for executables on remote host.
For every remote host, this variable will be set buffer local,
keeping the list of existing directories on that host.

You can use `~' in this list, but when searching for a shell which groks
tilde expansion, all directory names starting with `~' will be ignored."
  :group 'tramp
  :type '(repeat string))

(defcustom tramp-terminal-type "dumb"
  "*Value of TERM environment variable for logging in to remote host.
Because Tramp wants to parse the output of the remote shell, it is easily
confused by ANSI color escape sequences and suchlike.  Often, shell init
files conditionalize this setup based on the TERM environment variable."
  :group 'tramp
  :type 'string)

(defcustom tramp-remote-process-environment
  `("HISTFILE=$HOME/.tramp_history" "HISTSIZE=1" "LC_TIME=C"
    ,(concat "TERM=" tramp-terminal-type)
    "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=")
  "*List of environment variables to be set on the remote host.

Each element should be a string of the form ENVVARNAME=VALUE.  An
entry ENVVARNAME= diables the corresponding environment variable,
which might have been set in the init files like ~/.profile.

Special handling is applied to the PATH environment, which should
not be set here. Instead of, it should be set via `tramp-remote-path'."
  :group 'tramp
  :type '(repeat string))

(defcustom tramp-login-prompt-regexp
  ".*ogin\\( .*\\)?: *"
  "*Regexp matching login-like prompts.
The regexp should match at end of buffer.

Sometimes the prompt is reported to look like \"login as:\"."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-shell-prompt-pattern
  "^[^#$%>\n]*[#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*"
  "Regexp to match prompts from remote shell.
Normally, Tramp expects you to configure `shell-prompt-pattern'
correctly, but sometimes it happens that you are connecting to a
remote host which sends a different kind of shell prompt.  Therefore,
Tramp recognizes things matched by `shell-prompt-pattern' as prompt,
and also things matched by this variable.  The default value of this
variable is similar to the default value of `shell-prompt-pattern',
which should work well in many cases."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-password-prompt-regexp
  "^.*\\([pP]assword\\|passphrase\\).*:\^@? *"
  "*Regexp matching password-like prompts.
The regexp should match at end of buffer.

The `sudo' program appears to insert a `^@' character into the prompt."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-wrong-passwd-regexp
  (concat "^.*"
	  ;; These strings should be on the last line
	  (regexp-opt '("Permission denied."
			"Login incorrect"
			"Login Incorrect"
			"Connection refused"
			"Connection closed"
			"Sorry, try again."
			"Name or service not known"
			"Host key verification failed."
			"Tramp connection closed") t)
	  ".*"
	  "\\|"
	  "^.*\\("
	  ;; Here comes a list of regexes, separated by \\|
	  "Received signal [0-9]+"
	  "\\).*")
  "*Regexp matching a `login failed' message.
The regexp should match at end of buffer."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-yesno-prompt-regexp
  (concat
   (regexp-opt '("Are you sure you want to continue connecting (yes/no)?") t)
   "\\s-*")
  "Regular expression matching all yes/no queries which need to be confirmed.
The confirmation should be done with yes or no.
The regexp should match at end of buffer.
See also `tramp-yn-prompt-regexp'."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-yn-prompt-regexp
  (concat (regexp-opt '("Store key in cache? (y/n)") t)
	  "\\s-*")
  "Regular expression matching all y/n queries which need to be confirmed.
The confirmation should be done with y or n.
The regexp should match at end of buffer.
See also `tramp-yesno-prompt-regexp'."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-terminal-prompt-regexp
  (concat "\\("
	  "TERM = (.*)"
	  "\\|"
	  "Terminal type\\? \\[.*\\]"
	  "\\)\\s-*")
  "Regular expression matching all terminal setting prompts.
The regexp should match at end of buffer.
The answer will be provided by `tramp-action-terminal', which see."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-operation-not-permitted-regexp
  (concat "\\(" "preserving times.*" "\\|" "set mode" "\\)" ":\\s-*"
	  (regexp-opt '("Operation not permitted") t))
  "Regular expression matching keep-date problems in (s)cp operations.
Copying has been performed successfully already, so this message can
be ignored safely."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-process-alive-regexp
  ""
  "Regular expression indicating a process has finished.
In fact this expression is empty by intention, it will be used only to
check regularly the status of the associated process.
The answer will be provided by `tramp-action-process-alive',
`tramp-action-out-of-band', which see."
  :group 'tramp
  :type 'regexp)

(defcustom tramp-temp-name-prefix "tramp."
  "*Prefix to use for temporary files.
If this is a relative file name (such as \"tramp.\"), it is considered
relative to the directory name returned by the function
`tramp-temporary-file-directory' (which see).  It may also be an
absolute file name; don't forget to include a prefix for the filename
part, though."
  :group 'tramp
  :type 'string)

(defcustom tramp-discard-garbage nil
  "*If non-nil, try to discard garbage sent by remote shell.
Some shells send such garbage upon connection setup."
  :group 'tramp
  :type 'boolean)

(defcustom tramp-sh-extra-args '(("/bash\\'" . "-norc -noprofile"))
  "*Alist specifying extra arguments to pass to the remote shell.
Entries are (REGEXP . ARGS) where REGEXP is a regular expression
matching the shell file name and ARGS is a string specifying the
arguments.

This variable is only used when Tramp needs to start up another shell
for tilde expansion.  The extra arguments should typically prevent the
shell from reading its init file."
  :group 'tramp
  ;; This might be the wrong way to test whether the widget type
  ;; `alist' is available.  Who knows the right way to test it?
  :type (if (get 'alist 'widget-type)
	    '(alist :key-type string :value-type string)
	  '(repeat (cons string string))))

;; XEmacs is distributed with few Lisp packages.  Further packages are
;; installed using EFS.  If we use a unified filename format, then
;; Tramp is required in addition to EFS.  (But why can't Tramp just
;; disable EFS when Tramp is loaded?  Then XEmacs can ship with EFS
;; just like before.)  Another reason for using a separate filename
;; syntax on XEmacs is that EFS hooks into XEmacs in many places, but
;; Tramp only knows how to deal with `file-name-handler-alist', not
;; the other places.

;; Currently, we have the choice between 'ftp, 'sep, and 'url.
;;;###autoload
(defcustom tramp-syntax
  (if (featurep 'xemacs) 'sep 'ftp)
  "Tramp filename syntax to be used.

It can have the following values:

  'ftp -- Ange-FTP respective EFS like syntax (GNU Emacs default)
  'sep -- Syntax as defined for XEmacs (not available yet for GNU Emacs)
  'url -- URL-like syntax."
  :group 'tramp
  :type (if (featurep 'xemacs)
	    '(choice (const :tag "EFS"    ftp)
		     (const :tag "XEmacs" sep)
		     (const :tag "URL"    url))
	  '(choice (const :tag "Ange-FTP" ftp)
		   (const :tag "URL"      url))))

(defconst tramp-prefix-format
  (cond ((equal tramp-syntax 'ftp) "/")
	((equal tramp-syntax 'sep) "/[")
	((equal tramp-syntax 'url) "/")
	(t (error "Wrong `tramp-syntax' defined")))
  "*String matching the very beginning of tramp file names.
Used in `tramp-make-tramp-file-name'.")

(defconst tramp-prefix-regexp
  (concat "^" (regexp-quote tramp-prefix-format))
  "*Regexp matching the very beginning of tramp file names.
Should always start with \"^\". Derived from `tramp-prefix-format'.")

(defconst tramp-method-regexp
  "[a-zA-Z_0-9-]+"
  "*Regexp matching methods identifiers.")

(defconst tramp-postfix-method-format
  (cond ((equal tramp-syntax 'ftp) ":")
	((equal tramp-syntax 'sep) "/")
	((equal tramp-syntax 'url) "://")
	(t (error "Wrong `tramp-syntax' defined")))
  "*String matching delimeter between method and user or host names.
Used in `tramp-make-tramp-file-name'.")

(defconst tramp-postfix-method-regexp
  (regexp-quote tramp-postfix-method-format)
  "*Regexp matching delimeter between method and user or host names.
Derived from `tramp-postfix-method-format'.")

(defconst tramp-user-regexp
  "[^:/ \t]+"
  "*Regexp matching user names.")

(defconst tramp-postfix-user-format
  "@"
  "*String matching delimeter between user and host names.
Used in `tramp-make-tramp-file-name'.")

(defconst tramp-postfix-user-regexp
  (regexp-quote tramp-postfix-user-format)
  "*Regexp matching delimeter between user and host names.
Derived from `tramp-postfix-user-format'.")

(defconst tramp-host-regexp
  "[a-zA-Z0-9_.-]+"
  "*Regexp matching host names.")

(defconst tramp-prefix-port-format
  (cond ((equal tramp-syntax 'ftp) "#")
	((equal tramp-syntax 'sep) "#")
	((equal tramp-syntax 'url) ":")
	(t (error "Wrong `tramp-syntax' defined")))
  "*String matching delimeter between host names and port numbers.")

(defconst tramp-prefix-port-regexp
  (regexp-quote tramp-prefix-port-format)
  "*Regexp matching delimeter between host names and port numbers.
Derived from `tramp-prefix-port-format'.")

(defconst tramp-port-regexp
  "[0-9]+"
  "*Regexp matching port numbers.")

(defconst tramp-host-with-port-regexp
  (concat "\\(" tramp-host-regexp "\\)"
	        tramp-prefix-port-regexp
	  "\\(" tramp-port-regexp "\\)")
  "*Regexp matching host names with port numbers.")

(defconst tramp-postfix-host-format
  (cond ((equal tramp-syntax 'ftp) ":")
	((equal tramp-syntax 'sep) "]")
	((equal tramp-syntax 'url) "")
	(t (error "Wrong `tramp-syntax' defined")))
  "*String matching delimeter between host names and localnames.
Used in `tramp-make-tramp-file-name'.")

(defconst tramp-postfix-host-regexp
  (regexp-quote tramp-postfix-host-format)
  "*Regexp matching delimeter between host names and localnames.
Derived from `tramp-postfix-host-format'.")

(defconst tramp-localname-regexp
  ".*$"
  "*Regexp matching localnames.")

;; File name format.

(defconst tramp-file-name-structure
  (list
   (concat
    tramp-prefix-regexp
    "\\(" "\\(" tramp-method-regexp "\\)" tramp-postfix-method-regexp "\\)?"
    "\\(" "\\(" tramp-user-regexp "\\)"   tramp-postfix-user-regexp   "\\)?"
    "\\(" tramp-host-regexp
          "\\(" tramp-prefix-port-regexp  tramp-port-regexp "\\)?" "\\)?"
    tramp-postfix-host-regexp
    "\\(" tramp-localname-regexp "\\)")
   2 4 5 7)

  "*List of five elements (REGEXP METHOD USER HOST FILE), detailing \
the tramp file name structure.

The first element REGEXP is a regular expression matching a tramp file
name.  The regex should contain parentheses around the method name,
the user name, the host name, and the file name parts.

The second element METHOD is a number, saying which pair of
parentheses matches the method name.  The third element USER is
similar, but for the user name.  The fourth element HOST is similar,
but for the host name.  The fifth element FILE is for the file name.
These numbers are passed directly to `match-string', which see.  That
means the opening parentheses are counted to identify the pair.

See also `tramp-file-name-regexp'.")

;;;###autoload
(defconst tramp-file-name-regexp-unified
  "\\`/[^/:]+:"
  "Value for `tramp-file-name-regexp' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and
Tramp.  See `tramp-file-name-structure' for more explanations.")

;;;###autoload
(defconst tramp-file-name-regexp-separate
  "\\`/\\[.*\\]"
  "Value for `tramp-file-name-regexp' for separate remoting.
XEmacs uses a separate filename syntax for Tramp and EFS.
See `tramp-file-name-structure' for more explanations.")

;;;###autoload
(defconst tramp-file-name-regexp-url
  "\\`/[^/:]+://"
  "Value for `tramp-file-name-regexp' for separate remoting.
XEmacs uses a separate filename syntax for Tramp and EFS.
See `tramp-file-name-structure' for more explanations.")

;;;###autoload
(defconst tramp-file-name-regexp
  (cond ((equal tramp-syntax 'ftp) tramp-file-name-regexp-unified)
	((equal tramp-syntax 'sep) tramp-file-name-regexp-separate)
	((equal tramp-syntax 'url) tramp-file-name-regexp-url)
	(t (error "Wrong `tramp-syntax' defined")))
  "*Regular expression matching file names handled by tramp.
This regexp should match tramp file names but no other file names.
\(When tramp.el is loaded, this regular expression is prepended to
`file-name-handler-alist', and that is searched sequentially.  Thus,
if the tramp entry appears rather early in the `file-name-handler-alist'
and is a bit too general, then some files might be considered tramp
files which are not really tramp files.

Please note that the entry in `file-name-handler-alist' is made when
this file (tramp.el) is loaded.  This means that this variable must be set
before loading tramp.el.  Alternatively, `file-name-handler-alist' can be
updated after changing this variable.

Also see `tramp-file-name-structure'.")

;;;###autoload
(defconst tramp-completion-file-name-regexp-unified
  "^/$\\|^/[^/:][^/]*$"
  "Value for `tramp-completion-file-name-regexp' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and
Tramp.  See `tramp-file-name-structure' for more explanations.")

;;;###autoload
(defconst tramp-completion-file-name-regexp-separate
  "^/\\([[][^]]*\\)?$"
  "Value for `tramp-completion-file-name-regexp' for separate remoting.
XEmacs uses a separate filename syntax for Tramp and EFS.
See `tramp-file-name-structure' for more explanations.")

;;;###autoload
(defconst tramp-completion-file-name-regexp-url
  "^/$\\|^/[^/:]+\\(:\\(/\\(/[^/]*\\)?\\)?\\)?$"
  "Value for `tramp-completion-file-name-regexp' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and
Tramp.  See `tramp-file-name-structure' for more explanations.")

;;;###autoload
(defconst tramp-completion-file-name-regexp
  (cond ((equal tramp-syntax 'ftp) tramp-completion-file-name-regexp-unified)
	((equal tramp-syntax 'sep) tramp-completion-file-name-regexp-separate)
	((equal tramp-syntax 'url) tramp-completion-file-name-regexp-url)
	(t (error "Wrong `tramp-syntax' defined")))
  "*Regular expression matching file names handled by tramp completion.
This regexp should match partial tramp file names only.

Please note that the entry in `file-name-handler-alist' is made when
this file (tramp.el) is loaded.  This means that this variable must be set
before loading tramp.el.  Alternatively, `file-name-handler-alist' can be
updated after changing this variable.

Also see `tramp-file-name-structure'.")

(defcustom tramp-actions-before-shell
  '((tramp-login-prompt-regexp tramp-action-login)
    (tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (shell-prompt-pattern tramp-action-succeed)
    (tramp-shell-prompt-pattern tramp-action-succeed)
    (tramp-yesno-prompt-regexp tramp-action-yesno)
    (tramp-yn-prompt-regexp tramp-action-yn)
    (tramp-terminal-prompt-regexp tramp-action-terminal)
    (tramp-process-alive-regexp tramp-action-process-alive))
  "List of pattern/action pairs.
Whenever a pattern matches, the corresponding action is performed.
Each item looks like (PATTERN ACTION).

The PATTERN should be a symbol, a variable.  The value of this
variable gives the regular expression to search for.  Note that the
regexp must match at the end of the buffer, \"\\'\" is implicitly
appended to it.

The ACTION should also be a symbol, but a function.  When the
corresponding PATTERN matches, the ACTION function is called."
  :group 'tramp
  :type '(repeat (list variable function)))

(defcustom tramp-actions-copy-out-of-band
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-action-out-of-band))
  "List of pattern/action pairs.
This list is used for copying/renaming with out-of-band methods.
See `tramp-actions-before-shell' for more info."
  :group 'tramp
  :type '(repeat (list variable function)))

(defcustom tramp-initial-commands
  '("unset HISTORY"
    "unset correct"
    "unset autocorrect")
  "List of commands to send to the first remote shell that we see.
These commands will be sent to any shell, and thus they should be
designed to work in such circumstances.  Also, restrict the commands
to the bare necessity for getting the remote shell into a state
where it is possible to execute the Bourne-ish shell.

At the moment, the command to execute the Bourne-ish shell uses strange
quoting which `tcsh' tries to correct, so we send the command \"unset
autocorrect\" to the remote host."
  :group 'tramp
  :type '(repeat string))

;; Chunked sending kludge.  We set this to 500 for black-listed constellations
;; known to have a bug in `process-send-string'; some ssh connections appear
;; to drop bytes when data is sent too quickly.  There is also a connection
;; buffer local variable, which is computed depending on remote host properties
;; when `tramp-chunksize' is zero or nil.
(defcustom tramp-chunksize
  (when (and (not (featurep 'xemacs))
	     (memq system-type '(hpux)))
    500)
;; Parentheses in docstring starting at beginning of line are escaped.
;; Fontification is messed up when
;; `open-paren-in-column-0-is-defun-start' set to t.
  "*If non-nil, chunksize for sending input to local process.
It is necessary only on systems which have a buggy `process-send-string'
implementation.  The necessity, whether this variable must be set, can be
checked via the following code:

  (with-temp-buffer
    (let* ((user \"xxx\") (host \"yyy\")
           (init 0) (step 50)
           (sent init) (received init))
      (while (= sent received)
        (setq sent (+ sent step))
        (erase-buffer)
        (let ((proc (start-process (buffer-name) (current-buffer)
                                   \"ssh\" \"-l\" user host \"wc\" \"-c\")))
          (when (memq (process-status proc) '(run open))
            (process-send-string proc (make-string sent ?\\ ))
            (process-send-eof proc)
            (process-send-eof proc))
          (while (not (progn (goto-char (point-min))
                             (re-search-forward \"\\\\w+\" (point-max) t)))
            (accept-process-output proc 1))
          (when (memq (process-status proc) '(run open))
            (setq received (string-to-number (match-string 0)))
            (delete-process proc)
            (message \"Bytes sent: %s\\tBytes received: %s\" sent received)
            (sit-for 0))))
      (if (> sent (+ init step))
          (message \"You should set `tramp-chunksize' to a maximum of %s\"
                   (- sent step))
        (message \"Test does not work\")
        (display-buffer (current-buffer))
        (sit-for 30))))

In the Emacs normally running Tramp, evaluate the above code
\(replace \"xxx\" and \"yyy\" by the remote user and host name,
respectively).  You can do this, for example, by pasting it into
the `*scratch*' buffer and then hitting C-j with the cursor after the
last closing parenthesis.  Note that it works only if you have configured
\"ssh\" to run without password query, see ssh-agent(1).

You will see the number of bytes sent successfully to the remote host.
If that number exceeds 1000, you can stop the execution by hitting
C-g, because your Emacs is likely clean.

When it is necessary to set `tramp-chunksize', you might consider to
use an out-of-the-band method (like \"scp\") instead of an internal one
\(like \"ssh\"), because setting `tramp-chunksize' to non-nil decreases
performance.

If your Emacs is buggy, the code stops and gives you an indication
about the value `tramp-chunksize' should be set.  Maybe you could just
experiment a bit, e.g. changing the values of `init' and `step'
in the third line of the code.

Please raise a bug report via \"M-x tramp-bug\" if your system needs
this variable to be set as well."
  :group 'tramp
  :type '(choice (const nil) integer))

;; Logging in to a remote host normally requires obtaining a pty.  But
;; Emacs on MacOS X has process-connection-type set to nil by default,
;; so on those systems Tramp doesn't obtain a pty.  Here, we allow
;; for an override of the system default.
(defcustom tramp-process-connection-type t
  "Overrides `process-connection-type' for connections from Tramp.
Tramp binds process-connection-type to the value given here before
opening a connection to a remote host."
  :group 'tramp
  :type '(choice (const nil) (const t) (const pty)))

;;; Internal Variables:

(defvar tramp-buffer-file-attributes nil
  "Holds the `ls -ild' output for the current buffer.
This variable is local to each buffer.  It is not used if the remote
machine groks Perl.  If it is used, it's used as an emulation for
the visited file modtime.")
(make-variable-buffer-local 'tramp-buffer-file-attributes)

(defvar tramp-md5-function
  (cond ((and (require 'md5) (fboundp 'md5)) 'md5)
	((fboundp 'md5-encode)
	 (lambda (x) (base64-encode-string
		      (funcall (symbol-function 'md5-encode) x))))
	(t (error "Couldn't find an `md5' function")))
  "Function to call for running the MD5 algorithm.")

(defvar tramp-end-of-output
  (concat "///"
	  (funcall tramp-md5-function
		   (concat
		    (prin1-to-string process-environment)
		    (current-time-string))))
  "String used to recognize end of output.")

(defvar tramp-connection-function nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-remote-sh nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-login-program nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-login-args nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-copy-program nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-copy-args nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-copy-keep-date-arg nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-encoding-command nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-decoding-command nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-encoding-function nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-decoding-function nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-password-end-of-line nil
  "This internal variable holds a parameter for `tramp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `tramp-methods' (which see).")

(defvar tramp-current-method nil
  "Connection method for this *tramp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar tramp-current-user nil
  "Remote login name for this *tramp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar tramp-current-host nil
  "Remote host for this *tramp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar tramp-current-hop-method nil
  "Connection method for the actual hop in this *tramp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar tramp-current-hop-user nil
  "Remote login name for the actual hop in this *tramp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar tramp-current-hop-host nil
  "Remote host for the actual hop in this *tramp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defconst tramp-uudecode
  "(echo begin 600 /tmp/tramp.$$; tail +2) | uudecode
cat /tmp/tramp.$$
rm -f /tmp/tramp.$$"
  "Shell function to implement `uudecode' to standard output.
Many systems support `uudecode -o /dev/stdout' or `uudecode -o -'
for this or `uudecode -p', but some systems don't, and for them
we have this shell function.")

;; Perl script to implement `file-attributes' in a Lisp `read'able
;; output.  If you are hacking on this, note that you get *no* output
;; unless this spits out a complete line, including the '\n' at the
;; end.
;; The device number is returned as "-1", because there will be a virtual
;; device number set in `tramp-handle-file-attributes'
(defconst tramp-perl-file-attributes
  "%s -e '
@stat = lstat($ARGV[0]);
if (($stat[2] & 0170000) == 0120000)
{
    $type = readlink($ARGV[0]);
    $type = \"\\\"$type\\\"\";
}
elsif (($stat[2] & 0170000) == 040000)
{
    $type = \"t\";
}
else
{
    $type = \"nil\"
};
$uid = ($ARGV[1] eq \"integer\") ? $stat[4] : \"\\\"\" . getpwuid($stat[4]) . \"\\\"\";
$gid = ($ARGV[1] eq \"integer\") ? $stat[5] : \"\\\"\" . getgrgid($stat[5]) . \"\\\"\";
printf(
    \"(%%s %%u %%s %%s (%%u %%u) (%%u %%u) (%%u %%u) %%u %%u t (%%u . %%u) -1)\\n\",
    $type,
    $stat[3],
    $uid,
    $gid,
    $stat[8] >> 16 & 0xffff,
    $stat[8] & 0xffff,
    $stat[9] >> 16 & 0xffff,
    $stat[9] & 0xffff,
    $stat[10] >> 16 & 0xffff,
    $stat[10] & 0xffff,
    $stat[7],
    $stat[2],
    $stat[1] >> 16 & 0xffff,
    $stat[1] & 0xffff
);' \"$1\" \"$2\" \"$3\" 2>/dev/null"
  "Perl script to produce output suitable for use with `file-attributes'
on the remote file system.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.")

(defconst tramp-perl-directory-files-and-attributes
  "%s -e '
chdir($ARGV[0]) or printf(\"\\\"Cannot change to $ARGV[0]: $''!''\\\"\\n\"), exit();
opendir(DIR,\".\") or printf(\"\\\"Cannot open directory $ARGV[0]: $''!''\\\"\\n\"), exit();
@list = readdir(DIR);
closedir(DIR);
$n = scalar(@list);
printf(\"(\\n\");
for($i = 0; $i < $n; $i++)
{
    $filename = $list[$i];
    @stat = lstat($filename);
    if (($stat[2] & 0170000) == 0120000)
    {
        $type = readlink($filename);
        $type = \"\\\"$type\\\"\";
    }
    elsif (($stat[2] & 0170000) == 040000)
    {
        $type = \"t\";
    }
    else
    {
        $type = \"nil\"
    };
    $uid = ($ARGV[1] eq \"integer\") ? $stat[4] : \"\\\"\" . getpwuid($stat[4]) . \"\\\"\";
    $gid = ($ARGV[1] eq \"integer\") ? $stat[5] : \"\\\"\" . getgrgid($stat[5]) . \"\\\"\";
    printf(
        \"(\\\"%%s\\\" %%s %%u %%s %%s (%%u %%u) (%%u %%u) (%%u %%u) %%u %%u t (%%u . %%u) (%%u %%u))\\n\",
        $filename,
        $type,
        $stat[3],
        $uid,
        $gid,
        $stat[8] >> 16 & 0xffff,
        $stat[8] & 0xffff,
        $stat[9] >> 16 & 0xffff,
        $stat[9] & 0xffff,
        $stat[10] >> 16 & 0xffff,
        $stat[10] & 0xffff,
        $stat[7],
        $stat[2],
        $stat[1] >> 16 & 0xffff,
        $stat[1] & 0xffff,
        $stat[0] >> 16 & 0xffff,
        $stat[0] & 0xffff);
}
printf(\")\\n\");' \"$1\" \"$2\" \"$3\" 2>/dev/null"
  "Perl script implementing `directory-files-attributes' as Lisp `read'able
output.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.")

;; ;; These two use uu encoding.
;; (defvar tramp-perl-encode "%s -e'\
;; print qq(begin 644 xxx\n);
;; my $s = q();
;; my $res = q();
;; while (read(STDIN, $s, 45)) {
;;     print pack(q(u), $s);
;; }
;; print qq(`\n);
;; print qq(end\n);
;; '"
;;   "Perl program to use for encoding a file.
;; Escape sequence %s is replaced with name of Perl binary.")

;; (defvar tramp-perl-decode "%s -ne '
;; print unpack q(u), $_;
;; '"
;;   "Perl program to use for decoding a file.
;; Escape sequence %s is replaced with name of Perl binary.")

;; These two use base64 encoding.
(defconst tramp-perl-encode-with-module
  "%s -MMIME::Base64 -0777 -ne 'print encode_base64($_)' 2>/dev/null"
  "Perl program to use for encoding a file.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.
This implementation requires the MIME::Base64 Perl module to be installed
on the remote host.")

(defconst tramp-perl-decode-with-module
  "%s -MMIME::Base64 -0777 -ne 'print decode_base64($_)' 2>/dev/null"
  "Perl program to use for decoding a file.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.
This implementation requires the MIME::Base64 Perl module to be installed
on the remote host.")

(defconst tramp-perl-encode
  "%s -e '
# This script is contributed by Juanma Barranquero <lektu@terra.es>.
# Copyright (C) 2002 Free Software Foundation, Inc.
use strict;

my %%trans = do {
    my $i = 0;
    map {(substr(unpack(q(B8), chr $i++), 2, 6), $_)}
      split //, q(ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/);
};

binmode(\\*STDIN);

# We read in chunks of 54 bytes, to generate output lines
# of 72 chars (plus end of line)
$/ = \\54;

while (my $data = <STDIN>) {
    my $pad = q();

    # Only for the last chunk, and only if did not fill the last three-byte packet
    if (eof) {
        my $mod = length($data) %% 3;
        $pad = q(=) x (3 - $mod) if $mod;
    }

    # Not the fastest method, but it is simple: unpack to binary string, split
    # by groups of 6 bits and convert back from binary to byte; then map into
    # the translation table
    print
      join q(),
        map($trans{$_},
            (substr(unpack(q(B*), $data) . q(00000), 0, 432) =~ /....../g)),
              $pad,
                qq(\\n);
}' 2>/dev/null"
  "Perl program to use for encoding a file.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.")

(defconst tramp-perl-decode
  "%s -e '
# This script is contributed by Juanma Barranquero <lektu@terra.es>.
# Copyright (C) 2002 Free Software Foundation, Inc.
use strict;

my %%trans = do {
    my $i = 0;
    map {($_, substr(unpack(q(B8), chr $i++), 2, 6))}
      split //, q(ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/)
};

my %%bytes = map {(unpack(q(B8), chr $_), chr $_)} 0 .. 255;

binmode(\\*STDOUT);

# We are going to accumulate into $pending to accept any line length
# (we do not check they are <= 76 chars as the RFC says)
my $pending = q();

while (my $data = <STDIN>) {
    chomp $data;

    # If we find one or two =, we have reached the end and
    # any following data is to be discarded
    my $finished = $data =~ s/(==?).*/$1/;
    $pending .= $data;

    my $len = length($pending);
    my $chunk = substr($pending, 0, $len & ~3);
    $pending = substr($pending, $len & ~3 + 1);

    # Easy method: translate from chars to (pregenerated) six-bit packets, join,
    # split in 8-bit chunks and convert back to char.
    print join q(),
      map $bytes{$_},
        ((join q(), map {$trans{$_} || q()} split //, $chunk) =~ /......../g);

    last if $finished;
}' 2>/dev/null"
  "Perl program to use for decoding a file.
Escape sequence %s is replaced with name of Perl binary.
This string is passed to `format', so percent characters need to be doubled.")

; These values conform to `file-attributes' from XEmacs 21.2.
; GNU Emacs and other tools not checked.
(defconst tramp-file-mode-type-map '((0  . "-")  ; Normal file (SVID-v2 and XPG2)
				     (1  . "p")  ; fifo
				     (2  . "c")  ; character device
				     (3  . "m")  ; multiplexed character device (v7)
				     (4  . "d")  ; directory
				     (5  . "?")  ; Named special file (XENIX)
				     (6  . "b")  ; block device
				     (7  . "?")  ; multiplexed block device (v7)
				     (8  . "-")  ; regular file
				     (9  . "n")  ; network special file (HP-UX)
				     (10 . "l")  ; symlink
				     (11 . "?")  ; ACL shadow inode (Solaris, not userspace)
				     (12 . "s")  ; socket
				     (13 . "D")  ; door special (Solaris)
				     (14 . "w")) ; whiteout (BSD)
  "A list of file types returned from the `stat' system call.
This is used to map a mode number to a permission string.")

(defvar tramp-last-cmd-time nil
  "Internal Tramp variable recording the time when the last cmd was sent.
This variable is buffer-local in every buffer.")
(make-variable-buffer-local 'tramp-last-cmd-time)

;; New handlers should be added here.  The following operations can be
;; handled using the normal primitives: file-name-as-directory,
;; file-name-directory, file-name-nondirectory,
;; file-name-sans-versions, get-file-buffer.
(defconst tramp-file-name-handler-alist
  '(
    (load . tramp-handle-load)
    (make-symbolic-link . tramp-handle-make-symbolic-link)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    (file-truename . tramp-handle-file-truename)
    (file-exists-p . tramp-handle-file-exists-p)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-executable-p . tramp-handle-file-executable-p)
    (file-readable-p . tramp-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-writable-p . tramp-handle-file-writable-p)
    (file-ownership-preserved-p . tramp-handle-file-ownership-preserved-p)
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-attributes . tramp-handle-file-attributes)
    (file-modes . tramp-handle-file-modes)
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes . tramp-handle-directory-files-and-attributes)
    (file-name-all-completions . tramp-handle-file-name-all-completions)
    (file-name-completion . tramp-handle-file-name-completion)
    (add-name-to-file . tramp-handle-add-name-to-file)
    (copy-file . tramp-handle-copy-file)
    (rename-file . tramp-handle-rename-file)
    (set-file-modes . tramp-handle-set-file-modes)
    (make-directory . tramp-handle-make-directory)
    (delete-directory . tramp-handle-delete-directory)
    (delete-file . tramp-handle-delete-file)
    (directory-file-name . tramp-handle-directory-file-name)
    ;; `start-process' and `call-process' are not official yet.
    (start-process . tramp-handle-start-process)
    (call-process . tramp-handle-call-process)
    ;; Shouldn't be necessary any longer once `call-process' has a
    ;; file name handler.
    (process-file . tramp-handle-process-file)
    (insert-directory . tramp-handle-insert-directory)
    (expand-file-name . tramp-handle-expand-file-name)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (file-local-copy . tramp-handle-file-local-copy)
    (file-remote-p . tramp-handle-file-remote-p)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (write-region . tramp-handle-write-region)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (unhandled-file-name-directory . tramp-handle-unhandled-file-name-directory)
    (dired-compress-file . tramp-handle-dired-compress-file)
    (dired-recursive-delete-directory
     . tramp-handle-dired-recursive-delete-directory)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime))
  "Alist of handler functions.
Operations not mentioned here will be handled by the normal Emacs functions.")

;; Handlers for partial tramp file names. For GNU Emacs just
;; `file-name-all-completions' is needed. The other ones are necessary
;; for XEmacs.
(defconst tramp-completion-file-name-handler-alist
  '(
    (file-name-directory . tramp-completion-handle-file-name-directory)
    (file-name-nondirectory . tramp-completion-handle-file-name-nondirectory)
    (file-exists-p . tramp-completion-handle-file-exists-p)
    (file-name-all-completions . tramp-completion-handle-file-name-all-completions)
    (file-name-completion . tramp-completion-handle-file-name-completion)
    (expand-file-name . tramp-completion-handle-expand-file-name))
  "Alist of completion handler functions.
Used for file names matching `tramp-file-name-regexp'. Operations not
mentioned here will be handled by `tramp-file-name-handler-alist' or the
normal Emacs functions.")

;; Handlers for foreign methods, like FTP or SMB, shall be plugged here.
(defvar tramp-foreign-file-name-handler-alist
  ;; (identity . tramp-sh-file-name-handler) should always be the last
  ;; entry, since `identity' always matches.
  '((identity . tramp-sh-file-name-handler))
  "Alist of elements (FUNCTION . HANDLER) for foreign methods handled specially.
If (FUNCTION FILENAME) returns non-nil, then all I/O on that file is done by
calling HANDLER.")

;;; Internal functions which must come first.

(defsubst tramp-debug-message (fmt-string &rest args)
  "Append message to debug buffer.
Message is formatted with FMT-STRING as control string and the remaining
ARGS to actually emit the message (if applicable).

This function expects to be called from the tramp buffer only!"
  (when (get-buffer
	 (tramp-buffer-name
	  tramp-current-method tramp-current-user tramp-current-host))
    (with-current-buffer
	(tramp-get-debug-buffer
	 tramp-current-method tramp-current-user tramp-current-host)
      (goto-char (point-max))
      (unless (bolp)
	(insert "\n"))
      ;; Timestamp
      (insert (format-time-string "%T "))
      ;; Calling function
      (let ((btn 1) btf fn)
	(while (not fn)
	  (setq btf (nth 1 (backtrace-frame btn)))
	  (if (not btf)
	      (setq fn "")
	    (when (symbolp btf)
	      (setq fn (symbol-name btf))
	      (unless (and (string-match "^tramp" fn)
			   (not (string-match
				 "^tramp\\(-debug\\)?\\(-message\\|-error\\|-warning\\|-trace\\)\\(-for-buffer\\)?$"
				 fn)))
		(setq fn nil)))
	    (incf btn)))
	(insert (format "%s " fn)))
      ;; The message
      (insert (apply 'format fmt-string args)))))

(defsubst tramp-message (level fmt-string &rest args)
  "Emit a message depending on verbosity level.
First arg LEVEL says to be quiet if `tramp-verbose' is less than LEVEL.  The
message is emitted only if `tramp-verbose' is greater than or equal to LEVEL.
Calls functions `message' and `tramp-debug-message' with FMT-STRING as control
string and the remaining ARGS to actually emit the message (if applicable).

This function expects to be called from the tramp buffer only!"
  (when (<= level tramp-verbose)
    (apply
     'message
     (concat
      (cond
       ((= level 0) "")
       ((= level 1) "Error: ")
       ((= level 2) "Warning: ")
       (t           "Tramp: "))
      fmt-string)
     args)
    (when tramp-debug-buffer
      (apply 'tramp-debug-message
	     (concat (format "(%d) # " level) fmt-string) args))))

(defsubst tramp-message-for-buffer
  (method user host level fmt-string &rest args)
  "Like `tramp-message' but temporarily switches to the tramp buffer.
First three args METHOD, USER, and HOST identify the tramp buffer to use,
remaining args passed to `tramp-message'."
  (when (get-buffer (tramp-buffer-name method user host))
    (with-current-buffer (tramp-get-buffer method user host)
      (apply 'tramp-message level fmt-string args))))

(defsubst tramp-error
  (method user host signal fmt-string &rest args)
  "Emit an error.
First three args METHOD, USER, and HOST identify the tramp buffer
to use, SIGNAL is the signal identifier to be raised, remaining
args passed to `tramp-message'.  Finally, signal SIGNAL is raised."
  (tramp-message-for-buffer
   method user host 1
   (error-message-string
    (list signal (get signal 'error-message) (apply 'format fmt-string args))))
  (signal signal (list (apply 'format fmt-string args))))

(defsubst tramp-trace (fmt-string &rest args)
  "Emit a trace message.
The trace message should usually be input or output of the tramp process.
Calls function `tramp-debug-message' with FMT-STRING as control string and the
remaining ARGS to actually emit the message (if applicable).

This function expects to be called from the tramp buffer only!"
  (when (and (<= 10 tramp-verbose) tramp-debug-buffer)
    (apply 'tramp-debug-message (concat "$ " fmt-string) args)))

(defsubst tramp-line-end-position nil
  "Return point at end of line.
Calls `line-end-position' or `point-at-eol' if defined, else
own implementation."
  (cond
   ((fboundp 'line-end-position) (funcall (symbol-function 'line-end-position)))
   ((fboundp 'point-at-eol) 	 (funcall (symbol-function 'point-at-eol)))
   (t (save-excursion (end-of-line) (point)))))

(defmacro with-parsed-tramp-file-name (filename var &rest body)
  "Parse a Tramp filename and make components available in the body.

First arg FILENAME is evaluated and dissected into its components.
Second arg VAR is a symbol.  It is used as a variable name to hold
the filename structure.  It is also used as a prefix for the variables
holding the components.  For example, if VAR is the symbol `foo', then
`foo' will be bound to the whole structure, `foo-method' will be bound to
the method component, and so on for `foo-user', `foo-host', `foo-localname'.

Remaining args are Lisp expressions to be evaluated (inside an implicit
`progn').

If VAR is nil, then we bind `v' to the structure and `method', `user',
`host', `localname' to the components."
  `(let* ((,(or var 'v) (tramp-dissect-file-name ,filename))
	  (,(if var (intern (concat (symbol-name var) "-method")) 'method)
	   (tramp-file-name-method ,(or var 'v)))
	  (,(if var (intern (concat (symbol-name var) "-user")) 'user)
	   (tramp-file-name-user ,(or var 'v)))
	  (,(if var (intern (concat (symbol-name var) "-host")) 'host)
	   (tramp-file-name-host ,(or var 'v)))
	  (,(if var (intern (concat (symbol-name var) "-localname")) 'localname)
	   (tramp-file-name-localname ,(or var 'v))))
     ,@body))

(put 'with-parsed-tramp-file-name 'lisp-indent-function 2)
;; To be activated for debugging containing this macro.
;; It works only when VAR is nil.  Otherwise, it can be deactivated by
;; (put 'with-parsed-tramp-file-name 'edebug-form-spec 0)
;; I'm too stupid to write a precise SPEC for it.
(put 'with-parsed-tramp-file-name 'edebug-form-spec t)

(defmacro tramp-let-maybe (variable value &rest body)
  "Let-bind VARIABLE to VALUE in BODY, but only if VARIABLE is not obsolete.
BODY is executed whether or not the variable is obsolete.
The intent is to protect against `obsolete variable' warnings."
  `(if (get ',variable 'byte-obsolete-variable)
       (progn ,@body)
     (let ((,variable ,value))
       ,@body)))
(put 'tramp-let-maybe 'lisp-indent-function 2)
(put 'tramp-let-maybe 'edebug-form-spec t)

;;; Config Manipulation Functions:

(defun tramp-set-completion-function (method function-list)
  "Sets the list of completion functions for METHOD.
FUNCTION-LIST is a list of entries of the form (FUNCTION FILE).
The FUNCTION is intended to parse FILE according its syntax.
It might be a predefined FUNCTION, or a user defined FUNCTION.
Predefined FUNCTIONs are `tramp-parse-rhosts', `tramp-parse-shosts',
`tramp-parse-sconfig',`tramp-parse-hosts', `tramp-parse-passwd',
and `tramp-parse-netrc'.

Example:

    (tramp-set-completion-function
     \"ssh\"
     '((tramp-parse-sconfig \"/etc/ssh_config\")
       (tramp-parse-sconfig \"~/.ssh/config\")))"

  (let ((r function-list)
	(v function-list))
    (setq tramp-completion-function-alist
	  (delete (assoc method tramp-completion-function-alist)
		  tramp-completion-function-alist))

    (while v
      ;; Remove double entries
      (when (member (car v) (cdr v))
	(setcdr v (delete (car v) (cdr v))))
      ;; Check for function and file
      (unless (and (functionp (nth 0 (car v)))
		   (file-exists-p (nth 1 (car v))))
	(setq r (delete (car v) r)))
      (setq v (cdr v)))

    (when r
      (add-to-list 'tramp-completion-function-alist
		   (cons method r)))))

(defun tramp-get-completion-function (method)
  "Returns list of completion functions for METHOD.
For definition of that list see `tramp-set-completion-function'."
  (cdr (assoc method tramp-completion-function-alist)))

;;; File Name Handler Functions:

(defun tramp-handle-make-symbolic-link
  (filename linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for tramp files.
If LINKNAME is a non-Tramp file, it is used verbatim as the target of
the symlink.  If LINKNAME is a Tramp file, only the localname component is
used as the target of the symlink.

If LINKNAME is a Tramp file and the localname component is relative, then
it is expanded first, before the localname component is taken.  Note that
this can give surprising results if the user/host for the source and
target of the symlink differ."
  (with-parsed-tramp-file-name linkname l
    (let ((ln (tramp-get-remote-ln l-method l-user l-host))
	  (cwd (file-name-directory l-localname)))
      (unless ln
	(tramp-error
	 l-method l-user l-host 'file-error
	 "Making a symbolic link. ln(1) does not exist on the remote host."))

      ;; Do the 'confirm if exists' thing.
      (when (file-exists-p linkname)
	;; What to do?
	(if (or (null ok-if-already-exists) ; not allowed to exist
		(and (numberp ok-if-already-exists)
		     (not (yes-or-no-p
			   (format
			    "File %s already exists; make it a link anyway? "
			    l-localname)))))
	    (tramp-error
	     l-method l-user l-host 'file-already-exists
	     "File %s already exists" l-localname)
	  (delete-file linkname)))

      ;; If FILENAME is a Tramp name, use just the localname component.
      (when (tramp-tramp-file-p filename)
	(setq filename (tramp-file-name-localname
			(tramp-dissect-file-name
			 (expand-file-name filename)))))

      ;; Right, they are on the same host, regardless of user, method, etc.
      ;; We now make the link on the remote machine. This will occur as the user
      ;; that FILENAME belongs to.
      (zerop
       (tramp-send-command-and-check
	l-method l-user l-host
	(format "cd %s && %s -sf %s %s"
		cwd ln
		filename
		l-localname)
	t)))))


(defun tramp-handle-load (file &optional noerror nomessage nosuffix must-suffix)
  "Like `load' for tramp files."
  (with-parsed-tramp-file-name (expand-file-name file) nil
    (unless nosuffix
      (cond ((file-exists-p (concat file ".elc"))
	     (setq file (concat file ".elc")))
	    ((file-exists-p (concat file ".el"))
	     (setq file (concat file ".el")))))
    (when must-suffix
      ;; The first condition is always true for absolute file names.
      ;; Included for safety's sake.
      (unless (or (file-name-directory file)
		  (string-match "\\.elc?\\'" file))
	(tramp-error
	 method user host 'file-error
	 "File `%s' does not include a `.el' or `.elc' suffix" file)))
    (unless noerror
      (when (not (file-exists-p file))
	(tramp-error
	 method user host 'file-error
	 "Cannot load nonexistent file `%s'" file)))
    (if (not (file-exists-p file))
	nil
      (unless nomessage
	(tramp-message-for-buffer method user host 0 "Loading %s..." file))
      (let ((local-copy (file-local-copy file)))
	;; MUST-SUFFIX doesn't exist on XEmacs, so let it default to nil.
	(load local-copy noerror t t)
	(delete-file local-copy))
      (unless nomessage
	(tramp-message-for-buffer method user host 0 "Loading %s...done" file))
      t)))

;; Localname manipulation functions that grok TRAMP localnames...
(defun tramp-handle-file-name-directory (file)
  "Like `file-name-directory' but aware of TRAMP files."
  ;; everything except the last filename thing is the directory
  (with-parsed-tramp-file-name file nil
    ;; For the following condition, two possibilities should be tried:
    ;; (1) (string= localname "")
    ;; (2) (or (string= localname "") (string= localname "/"))
    ;; The second variant fails when completing a "/" directory on
    ;; the remote host, that is a filename which looks like
    ;; "/user@host:/".  But maybe wildcards fail with the first variant.
    ;; We should do some investigation.
    (if (string= localname "")
	;; For a filename like "/[foo]", we return "/".  The `else'
	;; case would return "/[foo]" unchanged.  But if we do that,
	;; then `file-expand-wildcards' ceases to work.  It's not
	;; quite clear to me what's the intuition that tells that this
	;; behavior is the right behavior, but oh, well.
	"/"
      ;; run the command on the localname portion only
      ;; CCC: This should take into account the remote machine type, no?
      ;;  --daniel <daniel@danann.net>
      (tramp-make-tramp-file-name
       method user host
       ;; This will not recurse...
       (or (file-name-directory localname) "")))))

(defun tramp-handle-file-name-nondirectory (file)
  "Like `file-name-nondirectory' but aware of TRAMP files."
  (with-parsed-tramp-file-name file nil
    (file-name-nondirectory localname)))

(defun tramp-handle-file-truename (filename &optional counter prev-dirs)
  "Like `file-truename' for tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-cache-data method user host localname "file-truename"
      (let* ((steps        (tramp-split-string localname "/"))
	     (localnamedir (tramp-let-maybe directory-sep-char ?/ ;for XEmacs
			     (file-name-as-directory localname)))
	     (is-dir (string= localname localnamedir))
	     (thisstep nil)
	     (numchase 0)
	     ;; Don't make the following value larger than necessary.
	     ;; People expect an error message in a timely fashion when
	     ;; something is wrong; otherwise they might think that Emacs
	     ;; is hung.  Of course, correctness has to come first.
	     (numchase-limit 20)
	     (result nil)			;result steps in reverse order
	     symlink-target)
	(tramp-message-for-buffer
	 method user host
	 4 "Finding true name for `%s'" filename)
	(while (and steps (< numchase numchase-limit))
	  (setq thisstep (pop steps))
	  (tramp-message-for-buffer
	   method user host
	   5 "Check %s"
	   (mapconcat 'identity
		      (append '("") (reverse result) (list thisstep))
		      "/"))
	  (setq symlink-target
		(nth 0 (file-attributes
			(tramp-make-tramp-file-name
			 method user host
			 (mapconcat 'identity
				    (append '("")
					    (reverse result)
					    (list thisstep))
				    "/")))))
	  (cond ((string= "." thisstep)
		 (tramp-message-for-buffer
		  method user host
		  5 "Ignoring step `.'"))
		((string= ".." thisstep)
		 (tramp-message-for-buffer
		  method user host
		  5 "Processing step `..'")
		 (pop result))
		((stringp symlink-target)
		 ;; It's a symlink, follow it.
		 (tramp-message-for-buffer
		  method user host
		  5 "Follow symlink to %s" symlink-target)
		 (setq numchase (1+ numchase))
		 (when (file-name-absolute-p symlink-target)
		   (setq result nil))
		 ;; If the symlink was absolute, we'll get a string like
		 ;; "/user@host:/some/target"; extract the
		 ;; "/some/target" part from it.
		 (when (tramp-tramp-file-p symlink-target)
		   (unless (tramp-equal-remote filename symlink-target)
		     (tramp-error
		      method user host 'file-error
		      "Symlink target `%s' on wrong host" symlink-target))
		   (setq symlink-target localname))
		 (setq steps
		       (append (tramp-split-string symlink-target "/")
			       steps)))
		(t
		 ;; It's a file.
		 (setq result (cons thisstep result)))))
	(when (>= numchase numchase-limit)
	  (tramp-error
	   method user host 'file-error
	   "Maximum number (%d) of symlinks exceeded" numchase-limit))
	(setq result (reverse result))
	;; Combine list to form string.
	(setq result
	      (if result
		  (mapconcat 'identity (cons "" result) "/")
		"/"))
	(when (and is-dir (or (string= "" result)
			      (not (string= (substring result -1) "/"))))
	  (setq result (concat result "/")))
	(tramp-message-for-buffer
	 method user host
	 4 "True name of `%s' is `%s'" filename result)
	(tramp-make-tramp-file-name method user host result)))))

;; Basic functions.

(defun tramp-handle-file-exists-p (filename)
  "Like `file-exists-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-cache-data method user host localname "file-exists-p"
      (save-excursion
	(zerop (tramp-send-command-and-check
		method user host
		(format
		 "%s %s"
		 (tramp-get-file-exists-command method user host)
		 (tramp-shell-quote-argument localname))))))))

;; Devices must distinguish physical file systems.  The device numbers
;; provided by "lstat" aren't unique, because we operate on different hosts.
;; So we use virtual device numbers, generated by Tramp.  Both Ange-FTP and
;; EFS use device number "-1".  In order to be different, we use device number
;; (-1 x), whereby "x" is unique for a given (method user host).
(defvar tramp-devices nil
  "Keeps virtual device numbers.")

;; CCC: This should check for an error condition and signal failure
;;      when something goes wrong.
;; Daniel Pittman <daniel@danann.net>
(defun tramp-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for tramp files."
  (unless id-format (setq id-format 'integer))
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-cache-data
	method user host localname (format "file-attributes-%s" id-format)
      (when (file-exists-p filename)
	;; file exists, find out stuff
	(save-excursion
	  (tramp-convert-file-attributes
	   method user host
	   (if (tramp-get-remote-stat method user host)
	       (tramp-handle-file-attributes-with-stat
		method user host localname id-format)
	     (if (tramp-get-remote-perl method user host)
		 (tramp-handle-file-attributes-with-perl
		  method user host localname id-format)
	       (tramp-handle-file-attributes-with-ls
		method user host localname id-format)))))))))

(defun tramp-handle-file-attributes-with-ls
  (method user host localname &optional id-format)
  "Implement `file-attributes' for tramp files using the ls(1) command."
  (let (symlinkp dirp
		 res-inode res-filemodes res-numlinks
		 res-uid res-gid res-size res-symlink-target)
    (tramp-message-for-buffer
     method user host
     5 "file attributes with ls: %s"
     (tramp-make-tramp-file-name method user host localname))
    (tramp-send-command
     method user host
     (format "%s %s %s"
	     (tramp-get-ls-command method user host)
	     (if (eq id-format 'integer) "-ildn" "-ild")
	     (tramp-shell-quote-argument localname)))
    ;; parse `ls -l' output ...
    ;; ... inode
    (setq res-inode
	  (condition-case err
	      (read (current-buffer))
	    (invalid-read-syntax
	     (when (and (equal (cadr err)
			       "Integer constant overflow in reader")
			(string-match
			 "^[0-9]+\\([0-9][0-9][0-9][0-9][0-9]\\)\\'"
			 (caddr err)))
	       (let* ((big (read (substring (caddr err) 0
					    (match-beginning 1))))
		      (small (read (match-string 1 (caddr err))))
		      (twiddle (/ small 65536)))
		 (cons (+ big twiddle)
		       (- small (* twiddle 65536))))))))
    ;; ... file mode flags
    (setq res-filemodes (symbol-name (read (current-buffer))))
    ;; ... number links
    (setq res-numlinks (read (current-buffer)))
    ;; ... uid and gid
    (setq res-uid (read (current-buffer)))
    (setq res-gid (read (current-buffer)))
    (when (eq id-format 'integer)
      (unless (numberp res-uid) (setq res-uid -1))
      (unless (numberp res-gid) (setq res-gid -1)))
    ;; ... size
    (setq res-size (read (current-buffer)))
    ;; From the file modes, figure out other stuff.
    (setq symlinkp (eq ?l (aref res-filemodes 0)))
    (setq dirp (eq ?d (aref res-filemodes 0)))
    ;; if symlink, find out file name pointed to
    (when symlinkp
      (search-forward "-> ")
      (setq res-symlink-target
	    (buffer-substring (point) (tramp-line-end-position))))
    ;; return data gathered
    (list
     ;; 0. t for directory, string (name linked to) for symbolic
     ;; link, or nil.
     (or dirp res-symlink-target)
     ;; 1. Number of links to file.
     res-numlinks
     ;; 2. File uid.
     res-uid
     ;; 3. File gid.
     res-gid
     ;; 4. Last access time, as a list of two integers. First
     ;; integer has high-order 16 bits of time, second has low 16
     ;; bits.
     ;; 5. Last modification time, likewise.
     ;; 6. Last status change time, likewise.
     '(0 0) '(0 0) '(0 0)		;CCC how to find out?
     ;; 7. Size in bytes (-1, if number is out of range).
     res-size
     ;; 8. File modes, as a string of ten letters or dashes as in ls -l.
     res-filemodes
     ;; 9. t iff file's gid would change if file were deleted and
     ;; recreated.  Will be set in `tramp-convert-file-attributes'
     t
     ;; 10. inode number.
     res-inode
     ;; 11. Device number.  Will be replaced by a virtual device number.
     -1
     )))

(defun tramp-handle-file-attributes-with-perl
  (method user host localname &optional id-format)
  "Implement `file-attributes' for tramp files using a Perl script."
  (tramp-message-for-buffer
   method user host
   5 "file attributes with perl: %s"
   (tramp-make-tramp-file-name method user host localname))
  (tramp-maybe-send-script
   method user host tramp-perl-file-attributes "tramp_perl_file_attributes")
  (tramp-send-command-and-read
   method user host
   (format "tramp_perl_file_attributes %s %s"
	   (tramp-shell-quote-argument localname) id-format)))

(defun tramp-handle-file-attributes-with-stat
  (method user host localname &optional id-format)
  "Implement `file-attributes' for tramp files using stat(1) command."
  (tramp-message-for-buffer
   method user host
   5 "file attributes with stat: %s"
   (tramp-make-tramp-file-name method user host localname))
  (tramp-send-command-and-read
   method user host
   (format
    "%s -c '((\"%%N\") %%h %s %s %%X.0 %%Y.0 %%Z.0 %%s \"%%A\" t %%i -1)' %s"
    (tramp-get-remote-stat method user host)
    (if (eq id-format 'integer) "%u" "%U")
    (if (eq id-format 'integer) "%g" "%G")
    (tramp-shell-quote-argument localname))))

(defun tramp-handle-set-visited-file-modtime (&optional time-list)
  "Like `set-visited-file-modtime' for tramp files."
  (unless (buffer-file-name)
    (error "Can't set-visited-file-modtime: buffer `%s' not visiting a file"
	   (buffer-name)))
  (if time-list
      (tramp-run-real-handler 'set-visited-file-modtime (list time-list))
    (let ((f (buffer-file-name))
	  coding-system-used)
      (with-parsed-tramp-file-name f nil
	(let* ((attr (file-attributes f))
	       ;; '(-1 65535) means file doesn't exists yet.
	       (modtime (or (nth 5 attr) '(-1 65535))))
	  (when (boundp 'last-coding-system-used)
	    (setq coding-system-used (symbol-value 'last-coding-system-used)))
	  ;; We use '(0 0) as a don't-know value.  See also
	  ;; `tramp-handle-file-attributes-with-ls'.
	  (if (not (equal modtime '(0 0)))
	      (tramp-run-real-handler 'set-visited-file-modtime (list modtime))
	    (save-excursion
	      (tramp-send-command
	       method user host
	       (format "%s -ild %s"
		       (tramp-get-ls-command method user host)
		       (tramp-shell-quote-argument localname)))
	      (setq attr (buffer-substring (point)
					   (progn (end-of-line) (point)))))
	    (setq tramp-buffer-file-attributes attr))
	  (when (boundp 'last-coding-system-used)
	    (set 'last-coding-system-used coding-system-used))
	  nil)))))

;; CCC continue here

;; This function makes the same assumption as
;; `tramp-handle-set-visited-file-modtime'.
(defun tramp-handle-verify-visited-file-modtime (buf)
  "Like `verify-visited-file-modtime' for tramp files.
At the time `verify-visited-file-modtime' calls this function, we
already know that the buffer is visiting a file and that
`visited-file-modtime' does not return 0.  Do not call this
function directly, unless those two cases are already taken care
of."
  (with-current-buffer buf
    ;; There is no file visiting the buffer, or the buffer has no
    ;; recorded last modification time.
    (if (or (not (buffer-file-name))
	    (eq (visited-file-modtime) 0))
	t
      (let ((f (buffer-file-name)))
	(with-parsed-tramp-file-name f nil
	  (let* ((attr (file-attributes f))
		 (modtime (nth 5 attr))
		 (mt (visited-file-modtime)))

 	    (cond
	     ;; file exists, and has a known modtime.
	     ((and attr (not (equal modtime '(0 0))))
	      (< (abs (tramp-time-diff
		       modtime
		       ;; For compatibility, deal with both the old
		       ;; (HIGH . LOW) and the new (HIGH LOW)
		       ;; return values of `visited-file-modtime'.
		       (if (atom (cdr mt))
			   (list (car mt) (cdr mt))
			 mt)))
		 2))
	     ;; modtime has the don't know value.
	     (attr
	      (save-excursion
		(tramp-send-command
		 method user host
		 (format "%s -ild %s"
			 (tramp-get-ls-command method user host)
			 (tramp-shell-quote-argument localname)))
		(setq attr (buffer-substring
			    (point) (progn (end-of-line) (point)))))
	      (equal tramp-buffer-file-attributes attr))
	     ;; If file does not exist, say it is not modified
	     ;; if and only if that agrees with the buffer's record.
	     (t (equal mt '(-1 65535))))))))))

(defun tramp-handle-set-file-modes (filename mode)
  "Like `set-file-modes' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (tramp-cache-flush-file method user host localname)
    (save-excursion
      (unless (zerop (tramp-send-command-and-check
		      method user host
		      (format "chmod %s %s"
			      (tramp-decimal-to-octal mode)
			      (tramp-shell-quote-argument localname))))
	(tramp-error
	 method user host 'file-error
	 ;; FIXME: extract the proper text from chmod's stderr.
	 "Error while changing file's mode %s" filename)))))

;; Simple functions using the `test' command.

(defun tramp-handle-file-executable-p (filename)
  "Like `file-executable-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-cache-data method user host localname "file-executable-p"
      (zerop (tramp-run-test "-x" filename)))))

(defun tramp-handle-file-readable-p (filename)
  "Like `file-readable-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-cache-data method user host localname "file-readable-p"
      (zerop (tramp-run-test "-r" filename)))))

;; When the remote shell is started, it looks for a shell which groks
;; tilde expansion.  Here, we assume that all shells which grok tilde
;; expansion will also provide a `test' command which groks `-nt' (for
;; newer than).  If this breaks, tell me about it and I'll try to do
;; something smarter about it.
(defun tramp-handle-file-newer-than-file-p (file1 file2)
  "Like `file-newer-than-file-p' for tramp files."
  (cond ((not (file-exists-p file1))
         nil)
        ((not (file-exists-p file2))
         t)
        ;; We are sure both files exist at this point.
        (t
         (save-excursion
	   ;; We try to get the mtime of both files.  If they are not
	   ;; equal to the "dont-know" value, then we subtract the times
	   ;; and obtain the result.
	   (let ((fa1 (file-attributes file1))
		 (fa2 (file-attributes file2)))
	     (if (and (not (equal (nth 5 fa1) '(0 0)))
		      (not (equal (nth 5 fa2) '(0 0))))
		 (> 0 (tramp-time-diff (nth 5 fa2) (nth 5 fa1)))
	       ;; If one of them is the dont-know value, then we can
	       ;; still try to run a shell command on the remote host.
	       ;; However, this only works if both files are Tramp
	       ;; files and both have the same method, same user, same
	       ;; host.
	       (unless (tramp-equal-remote file1 file2)
		 (with-parsed-tramp-file-name
		     (if (tramp-tramp-file-p file1) file1 file2) nil
		   (tramp-error
		    method user host 'file-error
		    "Files %s and %s must have same method, user, host"
		    file1 file2)))
	       (with-parsed-tramp-file-name file1 nil
		 (zerop (tramp-run-test2
			 (tramp-get-test-nt-command method user host)
			 file1 file2)))))))))

;; Functions implemented using the basic functions above.

(defun tramp-handle-file-modes (filename)
  "Like `file-modes' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (file-exists-p filename)
      (tramp-mode-string-to-int
       (nth 8 (file-attributes filename))))))

(defun tramp-handle-file-directory-p (filename)
  "Like `file-directory-p' for tramp files."
  ;; Care must be taken that this function returns `t' for symlinks
  ;; pointing to directories.  Surely the most obvious implementation
  ;; would be `test -d', but that returns false for such symlinks.
  ;; CCC: Stefan Monnier says that `test -d' follows symlinks.  And
  ;; I now think he's right.  So we could be using `test -d', couldn't
  ;; we?
  ;;
  ;; Alternatives: `cd %s', `test -d %s'
  (with-parsed-tramp-file-name filename nil
    (with-cache-data method user host localname "file-directory-p"
      (zerop (tramp-run-test "-d" filename)))))

(defun tramp-handle-file-regular-p (filename)
  "Like `file-regular-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (and (file-exists-p filename)
	 (eq ?- (aref (nth 8 (file-attributes filename)) 0)))))

(defun tramp-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (let ((x (car (file-attributes filename))))
      (when (stringp x)
	;; When Tramp is running on VMS, then `file-name-absolute-p'
	;; might do weird things.
	(if (file-name-absolute-p x)
	    (tramp-make-tramp-file-name method user host x)
	  x)))))

(defun tramp-handle-file-writable-p (filename)
  "Like `file-writable-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-cache-data method user host localname "file-writable-p"
      (if (file-exists-p filename)
	  ;; Existing files must be writable.
	  (zerop (tramp-run-test "-w" filename))
	;; If file doesn't exist, check if directory is writable.
	(and (zerop (tramp-run-test
		     "-d" (file-name-directory filename)))
	     (zerop (tramp-run-test
		     "-w" (file-name-directory filename))))))))

(defun tramp-handle-file-ownership-preserved-p (filename)
  "Like `file-ownership-preserved-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-cache-data method user host localname "file-ownership-preserved-p"
      (let ((attributes (file-attributes filename)))
	;; Return t if the file doesn't exist, since it's true that no
	;; information would be lost by an (attempted) delete and create.
	(or (null attributes)
	    (= (nth 2 attributes)
	       (tramp-get-remote-uid method user host 'integer)))))))

;; Other file name ops.

(defun tramp-handle-directory-file-name (directory)
  "Like `directory-file-name' for tramp files."
  ;; If localname component of filename is "/", leave it unchanged.
  ;; Otherwise, remove any trailing slash from localname component.
  ;; Method, host, etc, are unchanged.  Does it make sense to try
  ;; to avoid parsing the filename?
  (with-parsed-tramp-file-name directory nil
    (if (and (not (zerop (length localname)))
	     (eq (aref localname (1- (length localname))) ?/)
	     (not (string= localname "/")))
	(substring directory 0 -1)
      directory)))

;; Directory listings.

(defun tramp-handle-directory-files
  (directory &optional full match nosort files-only)
  "Like `directory-files' for tramp files."
  ;; FILES-ONLY is valid for XEmacs only.
  (when (file-directory-p directory)
    (setq directory (expand-file-name directory))
    (let ((temp (nreverse (file-name-all-completions "" directory)))
	  result item)

      (while temp
	(setq item (directory-file-name (pop temp)))
	(when (and (or (null match) (string-match match item))
		   (or (null files-only)
		       ;; files only
		       (and (equal files-only t) (file-regular-p item))
		       ;; directories only
		       (file-directory-p item)))
	  (push (if full
		    (concat (file-name-as-directory directory) item)
		  item)
		result)))
      result)))

(defun tramp-handle-directory-files-and-attributes
  (directory &optional full match nosort id-format)
  "Like `directory-files-and-attributes' for tramp files."
  (unless id-format (setq id-format 'integer))
  (when (file-directory-p directory)
    (setq directory (expand-file-name directory))
    (let* ((temp
	    (copy-tree
	     (with-parsed-tramp-file-name directory nil
	       (with-cache-data
		   method user host localname
		   (format "directory-files-and-attributes-%s" id-format)
		 (save-excursion
		   (mapcar
		    '(lambda (x)
		       (cons (car x)
			     (tramp-convert-file-attributes
			      method user host (cdr x))))
		    (if (tramp-get-remote-stat method user host)
			(tramp-handle-directory-files-and-attributes-with-stat
			 method user host localname id-format)
		      (if (tramp-get-remote-perl method user host)
			  (tramp-handle-directory-files-and-attributes-with-perl
			   method user host localname id-format)))))))))
	   result item)

      (while temp
	(setq item (pop temp))
	(when (or (null match) (string-match match (car item)))
	  (when full
	    (setcar item
		    (concat (file-name-as-directory directory) (car item))))
	  (push item result)))

      (if nosort
	  result
	(sort result (lambda (x y) (string< (car x) (car y))))))))

(defun tramp-handle-directory-files-and-attributes-with-perl
  (method user host localname &optional id-format)
  "Implement `directory-files-and-attributes' for tramp files using a Perl script."
  (tramp-message-for-buffer
   method user host
   5 "directory-files-and-attributes with perl: %s"
   (tramp-make-tramp-file-name method user host localname))
  (tramp-maybe-send-script
   method user host
   tramp-perl-directory-files-and-attributes
   "tramp_perl_directory_files_and_attributes")
  (let ((object
	 (tramp-send-command-and-read
	  method user host
	  (format "tramp_perl_directory_files_and_attributes %s %s"
		  (tramp-shell-quote-argument localname) id-format))))
    (when (stringp object) (tramp-error method user host 'file-error object))
    object))

(defun tramp-handle-directory-files-and-attributes-with-stat
  (method user host localname &optional id-format)
  "Implement `directory-files-and-attributes' for tramp files using stat(1) command."
  (tramp-message-for-buffer
   method user host
   5 "directory-files-and-attributes with stat: %s"
   (tramp-make-tramp-file-name method user host localname))
  (tramp-send-command-and-read
   method user host
   (format
    (concat
     "cd %s; echo \"(\"; (%s -a | xargs "
     "%s -c '(\"%%n\" (\"%%N\") %%h %s %s %%X.0 %%Y.0 %%Z.0 %%s \"%%A\" t %%i -1)'); "
     "echo \")\"")
    (tramp-shell-quote-argument localname)
    (tramp-get-ls-command method user host)
    (tramp-get-remote-stat method user host)
    (if (eq id-format 'integer) "%u" "%U")
    (if (eq id-format 'integer) "%g" "%G"))))

;; This function should return "foo/" for directories and "bar" for
;; files.
(defun tramp-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for tramp files."
  (unless (save-match-data (string-match "/" filename))
    (with-parsed-tramp-file-name directory nil
      (all-completions
       filename
       (mapcar
	'list
	(with-cache-data method user host localname "file-name-all-completions"
	  (let (result)
	    (save-excursion
	      (tramp-barf-unless-okay
	       method user host
	       (format "cd %s" (tramp-shell-quote-argument localname))
	       "tramp-handle-file-name-all-completions: Couldn't `cd %s'"
	       (tramp-shell-quote-argument localname))

	      ;; Get a list of directories and files, including reliably
	      ;; tagging the directories with a trailing '/'.  Because I
	      ;; rock.  --daniel@danann.net
	      (tramp-send-command
	       method user host
	       (format (concat "%s -a 2>/dev/null | while read f; do "
			       "if %s -d \"$f\" 2>/dev/null; "
			       "then echo \"$f/\"; else echo \"$f\"; fi; done")
		       (tramp-get-ls-command method user host)
		       (tramp-get-test-command method user host)))

	      ;; Now grab the output.
	      (goto-char (point-max))
	      (while (zerop (forward-line -1))
		(push (buffer-substring (point)	(tramp-line-end-position))
		      result))

	      (tramp-send-command method user host "cd")
	      result))))))))

;; The following isn't needed for Emacs 20 but for 19.34?
(defun tramp-handle-file-name-completion (filename directory)
  "Like `file-name-completion' for tramp files."
  (unless (tramp-tramp-file-p directory)
    (error
     "tramp-handle-file-name-completion invoked on non-tramp directory `%s'"
     directory))
  (try-completion
   filename
   (mapcar (lambda (x) (cons x nil))
	   (file-name-all-completions filename directory))))

;; cp, mv and ln

(defun tramp-handle-add-name-to-file
  (filename newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for tramp files."
  (unless (tramp-equal-remote filename newname)
    (with-parsed-tramp-file-name
	(if (tramp-tramp-file-p filename) filename newname) nil
      (tramp-error
       method user host 'file-error
       "add-name-to-file: %s"
       "only implemented for same method, same user, same host")))
  (with-parsed-tramp-file-name filename v1
    (with-parsed-tramp-file-name newname v2
      (let ((ln (when v1 (tramp-get-remote-ln v1-method v1-user v1-host))))
	(when (and (not ok-if-already-exists)
		   (file-exists-p newname)
		   (not (numberp ok-if-already-exists))
		   (y-or-n-p
		    (format
		     "File %s already exists; make it a new name anyway? "
		     newname)))
	  (tramp-error
	   v2-method v2-user v2-host 'file-error
	   "add-name-to-file: file %s already exists" newname))
	(tramp-cache-flush-file v2-method v2-user v2-host v2-localname)
	(tramp-barf-unless-okay
	 v1-method v1-user v1-host
	 (format "%s %s %s" ln (tramp-shell-quote-argument v1-localname)
		 (tramp-shell-quote-argument v2-localname))
	 "error with add-name-to-file, see buffer `%s' for details"
	 (buffer-name))))))

(defun tramp-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date)
  "Like `copy-file' for tramp files."
  ;; Check if both files are local -- invoke normal copy-file.
  ;; Otherwise, use tramp from local system.
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  ;; At least one file a tramp file?
  (if (or (tramp-tramp-file-p filename)
          (tramp-tramp-file-p newname))
      (tramp-do-copy-or-rename-file
       'copy filename newname ok-if-already-exists keep-date)
    (tramp-run-real-handler
     'copy-file (list filename newname ok-if-already-exists keep-date))))

(defun tramp-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for tramp files."
  ;; Check if both files are local -- invoke normal rename-file.
  ;; Otherwise, use tramp from local system.
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  ;; At least one file a tramp file?
  (if (or (tramp-tramp-file-p filename)
          (tramp-tramp-file-p newname))
      (tramp-do-copy-or-rename-file
       'rename filename newname ok-if-already-exists t)
    (tramp-run-real-handler
     'rename-file (list filename newname ok-if-already-exists))))

(defun tramp-do-copy-or-rename-file
  (op filename newname &optional ok-if-already-exists keep-date)
  "Copy or rename a remote file.
OP must be `copy' or `rename' and indicates the operation to perform.
FILENAME specifies the file to copy or rename, NEWNAME is the name of
the new file (for copy) or the new name of the file (for rename).
OK-IF-ALREADY-EXISTS means don't barf if NEWNAME exists already.
KEEP-DATE means to make sure that NEWNAME has the same timestamp
as FILENAME.

This function is invoked by `tramp-handle-copy-file' and
`tramp-handle-rename-file'.  It is an error if OP is neither of `copy'
and `rename'.  FILENAME and NEWNAME must be absolute file names."
  (unless (memq op '(copy rename))
    (error "Unknown operation `%s', must be `copy' or `rename'" op))
  (unless ok-if-already-exists
    (when (file-exists-p newname)
      (with-parsed-tramp-file-name newname nil
	(tramp-error method user host 'file-already-exists newname))))
  (let ((t1 (tramp-tramp-file-p filename))
	(t2 (tramp-tramp-file-p newname))
	v1-method v1-user v1-host v1-localname
	v2-method v2-user v2-host v2-localname)

    ;; Check which ones of source and target are Tramp files.
    ;; We cannot invoke `with-parsed-tramp-file-name';
    ;; it fails if the file isn't a Tramp file name.
    (if t1
	(with-parsed-tramp-file-name filename l
	  (setq v1-method l-method
		v1-user l-user
		v1-host l-host
		v1-localname l-localname))
      (setq v1-localname filename))
    (if t2
	(with-parsed-tramp-file-name newname l
	  (tramp-cache-flush-file l-method l-user l-host l-localname)
	  (setq v2-method l-method
		v2-user l-user
		v2-host l-host
		v2-localname l-localname))
      (setq v2-localname newname))

    (cond
     ;; Both are Tramp files.
     ((and t1 t2)
      (cond
       ;; Shortcut: if method, host, user are the same for both
       ;; files, we invoke `cp' or `mv' on the remote host
       ;; directly.
       ((tramp-equal-remote filename newname)
	(tramp-do-copy-or-rename-file-directly
	 op v1-method v1-user v1-host v1-localname v2-localname keep-date))
       ;; If both source and target are Tramp files,
       ;; both are using the same copy-program, then we
       ;; can invoke rcp directly.  Note that
       ;; default-directory should point to a local
       ;; directory if we want to invoke rcp.
       ((and (equal v1-method v2-method)
	     (tramp-method-out-of-band-p v1-method v1-user v1-host)
	     (not (string-match tramp-host-with-port-regexp v1-host))
	     (not (string-match tramp-host-with-port-regexp v2-host)))
	(tramp-do-copy-or-rename-file-out-of-band
	 op filename newname keep-date))
       ;; No shortcut was possible.  So we copy the
       ;; file first.  If the operation was `rename', we go
       ;; back and delete the original file (if the copy was
       ;; successful).  The approach is simple-minded: we
       ;; create a new buffer, insert the contents of the
       ;; source file into it, then write out the buffer to
       ;; the target file.  The advantage is that it doesn't
       ;; matter which filename handlers are used for the
       ;; source and target file.
       (t
	(tramp-do-copy-or-rename-file-via-buffer
	 op filename newname keep-date))))

     ;; One file is a Tramp file, the other one is local.
     ((or t1 t2)
      ;; If the Tramp file has an out-of-band method, the corresponding
      ;; copy-program can be invoked.
      (if (or (tramp-method-out-of-band-p v1-method v1-user v1-host)
	      (tramp-method-out-of-band-p v2-method v2-user v2-host))
	  (tramp-do-copy-or-rename-file-out-of-band
	   op filename newname keep-date)
	;; Use the generic method via a Tramp buffer.
	(tramp-do-copy-or-rename-file-via-buffer
	 op filename newname keep-date)))

     (t
      ;; One of them must be a Tramp file.
      (error "Tramp implementation says this cannot happen")))))

(defun tramp-do-copy-or-rename-file-via-buffer (op filename newname keep-date)
  "Use an Emacs buffer to copy or rename a file.
First arg OP is either `copy' or `rename' and indicates the operation.
FILENAME is the source file, NEWNAME the target file.
KEEP-DATE is non-nil if NEWNAME should have the same timestamp as FILENAME."
  (let ((tmpbuf (generate-new-buffer " *tramp tmp*"))
	(modtime (nth 5 (file-attributes filename))))
    (when (and keep-date (or (null modtime) (equal modtime '(0 0))))
      (tramp-message
       2 "Cannot preserve file time stamp with inline copying across machines"))
    (save-excursion
      (unwind-protect
	  (with-current-buffer tmpbuf
	    (erase-buffer)
	    (insert-file-contents-literally filename)
	    ;; We don't want the target file to be compressed, so we
	    ;; let-bind `jka-compr-inhibit' to t.
	    (let ((coding-system-for-write 'binary)
		  (jka-compr-inhibit t))
	      (write-region (point-min) (point-max) newname)))
	(kill-buffer tmpbuf))
      ;; KEEP-DATE handling.
      (when keep-date
	(when (and (not (null modtime))
		   (not (equal modtime '(0 0))))
	  (tramp-touch newname modtime)))
      ;; Set the mode.
      (set-file-modes newname (file-modes filename)))
    ;; If the operation was `rename', delete the original file.
    (unless (eq op 'copy)
      (delete-file filename))))

(defun tramp-do-copy-or-rename-file-directly
  (op method user host localname1 localname2 keep-date)
  "Invokes `cp' or `mv' on the remote system.
OP must be one of `copy' or `rename', indicating `cp' or `mv',
respectively.  METHOD, USER, and HOST specify the connection.
LOCALNAME1 and LOCALNAME2 specify the two arguments of `cp' or `mv'.
If KEEP-DATE is non-nil, preserve the time stamp when copying."
  ;; CCC: What happens to the timestamp when renaming?
  (let ((cmd (cond ((and (eq op 'copy) keep-date) "cp -f -p")
                   ((eq op 'copy) "cp -f")
                   ((eq op 'rename) "mv -f")
                   (t (tramp-error
		       method user host 'file-error
                       "Unknown operation `%s', must be `copy' or `rename'"
                       op)))))
    (save-excursion
      (tramp-send-command
       method user host
       (format "%s %s %s"
               cmd
               (tramp-shell-quote-argument localname1)
               (tramp-shell-quote-argument localname2)))
      (goto-char (point-min))
      (unless
	  (or
	   (and (eq op 'copy) keep-date
		;; Mask cp -f error.
		(re-search-forward tramp-operation-not-permitted-regexp nil t))
	   (zerop (tramp-send-command-and-check method user host nil)))
	(pop-to-buffer (current-buffer))
	(tramp-error
	 method user host 'file-error
	 "Copying directly failed, see buffer `%s' for details."
	 (buffer-name))))
    ;; Set the mode.
    ;; CCC: Maybe `chmod --reference=localname1 localname2' could be used
    ;;      where available?
    (unless (or (eq op 'rename) keep-date)
      (set-file-modes
       (tramp-make-tramp-file-name method user host localname2)
       (file-modes
	(tramp-make-tramp-file-name method user host localname1))))))

(defun tramp-do-copy-or-rename-file-out-of-band (op filename newname keep-date)
  "Invoke rcp program to copy.
One of FILENAME and NEWNAME must be a Tramp name, the other must
be a local filename.  The method used must be an out-of-band method."
  (let ((t1 (tramp-tramp-file-p filename))
	(t2 (tramp-tramp-file-p newname))
	v1-method v1-user v1-host v1-localname
	v2-method v2-user v2-host v2-localname
	method user host
	copy-program copy-args copy-keep-date-arg
	source target trampbuf)

    ;; Check which ones of source and target are Tramp files.
    ;; We cannot invoke `with-parsed-tramp-file-name';
    ;; it fails if the file isn't a Tramp file name.
    (if t1
	(with-parsed-tramp-file-name filename l
	  (setq v1-method l-method
		v1-user l-user
		v1-host l-host
		v1-localname l-localname
		method (tramp-find-method l-method l-user l-host)
		user   (tramp-find-user   l-method l-user l-host)
		host   (tramp-find-host   l-method l-user l-host)
		copy-program (tramp-get-method-parameter
			      method user host 'tramp-copy-program)
		copy-args (tramp-get-method-parameter
			   method user host 'tramp-copy-args)
		copy-keep-date-arg (tramp-get-method-parameter
				    method user host 'tramp-copy-keep-date-arg)))
      (setq v1-localname filename))

    (if t2
	(with-parsed-tramp-file-name newname l
	  (setq v2-method l-method
		v2-user l-user
		v2-host l-host
		v2-localname l-localname
		method (tramp-find-method l-method l-user l-host)
		user   (tramp-find-user   l-method l-user l-host)
		host   (tramp-find-host   l-method l-user l-host)
		copy-program (tramp-get-method-parameter
			      method user host 'tramp-copy-program)
		copy-args (tramp-get-method-parameter
			   method user host 'tramp-copy-args)
		copy-keep-date-arg (tramp-get-method-parameter
				    method user host 'tramp-copy-keep-date-arg)))
      (setq v2-localname newname))

    ;; The following should be changed.  We need a more general
    ;; mechanism to parse extra host args.
    (if (not t1)
	(setq source v1-localname)
      (when (string-match tramp-host-with-port-regexp v1-host)
	(setq copy-args (cons "-P" (cons (match-string 2 v1-host) copy-args)))
	(setq v1-host (match-string 1 v1-host)))
      (setq source
	     (tramp-make-copy-program-file-name
	      v1-user v1-host
	      (tramp-shell-quote-argument v1-localname))))

    (if (not t2)
	(setq target v2-localname)
      (when (string-match tramp-host-with-port-regexp v2-host)
	(setq copy-args (cons "-P" (cons (match-string 2 v2-host) copy-args)))
	(setq v2-host (match-string 1 v2-host)))
      (setq target
	     (tramp-make-copy-program-file-name
	      v2-user v2-host
	      (tramp-shell-quote-argument v2-localname))))

    ;; Handle KEEP-DATE argument.
    (when (and keep-date copy-keep-date-arg)
      (setq copy-args (cons copy-keep-date-arg copy-args)))

    (setq copy-args (append copy-args (list source target))
	  trampbuf (generate-new-buffer (tramp-buffer-name method user host)))

    ;; Use an asynchronous process.  By this, password can be handled.
    (save-excursion

      ;; Check for program.
      (when (and (fboundp 'executable-find)
		 (not (executable-find copy-program)))
	(error "Cannot find copy program: %s" copy-program))

      (set-buffer trampbuf)
      (setq tramp-current-method (tramp-find-method method user host)
	    tramp-current-user   (tramp-find-user   method user host)
	    tramp-current-host   (tramp-find-host   method user host))
      (setq tramp-current-hop-method tramp-current-method
	    tramp-current-hop-user   tramp-current-user
	    tramp-current-hop-host   tramp-current-host)
      (tramp-message 0 "Transferring %s to %s..." filename newname)
      (tramp-message
       5 "Sending command `%s'"
       (mapconcat 'identity (cons copy-program copy-args) " "))

      ;; Use rcp-like program for file transfer.
      (let ((p (apply 'start-process (buffer-name trampbuf) trampbuf
		      copy-program copy-args)))
	(tramp-set-process-query-on-exit-flag p nil)
	(tramp-process-actions
	 p method user host tramp-actions-copy-out-of-band))
      (kill-buffer trampbuf)
      (tramp-message 0 "Transferring %s to %s...done" filename newname)

      ;; Handle KEEP-DATE argument.
      (when (and keep-date (not copy-keep-date-arg))
	(set-file-times newname (nth 5 (file-attributes filename))))

      ;; Set the mode.
      (unless (and keep-date copy-keep-date-arg)
	(set-file-modes newname (file-modes filename))))

    ;; If the operation was `rename', delete the original file.
    (unless (eq op 'copy)
      (delete-file filename))))

;; mkdir
(defun tramp-handle-make-directory (dir &optional parents)
  "Like `make-directory' for tramp files."
  (setq dir (expand-file-name dir))
  (with-parsed-tramp-file-name dir nil
    (save-excursion
      (tramp-barf-unless-okay
       method user host
       (format " %s %s"
	       (if parents "mkdir -p" "mkdir")
	       (tramp-shell-quote-argument localname))
       "Couldn't make directory %s" dir))))

;; CCC error checking?
(defun tramp-handle-delete-directory (directory)
  "Like `delete-directory' for tramp files."
  (setq directory (expand-file-name directory))
  (with-parsed-tramp-file-name directory nil
    (tramp-cache-flush-directory method user host localname)
    (save-excursion
      (tramp-send-command
       method user host
       (format "rmdir %s ; echo ok"
	       (tramp-shell-quote-argument localname))))))

(defun tramp-handle-delete-file (filename)
  "Like `delete-file' for tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (tramp-cache-flush-file method user host localname)
    (save-excursion
      (unless (zerop (tramp-send-command-and-check
		      method user host
		      (format "rm -f %s"
			      (tramp-shell-quote-argument localname))))
	(tramp-error
	 method user host 'file-error "Couldn't delete %s" filename)))))

;; Dired.

;; CCC: This does not seem to be enough. Something dies when
;;      we try and delete two directories under TRAMP :/
(defun tramp-handle-dired-recursive-delete-directory (filename)
  "Recursively delete the directory given.
This is like `dired-recursive-delete-directory' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (tramp-cache-flush-directory method user host filename)
    ;; Run a shell command 'rm -r <localname>'
    ;; Code shamelessly stolen for the dired implementation and, um, hacked :)
    (unless (file-exists-p filename)
      (tramp-error
       method user host 'file-error "No such directory: %s" filename))
    ;; Which is better, -r or -R? (-r works for me <daniel@danann.net>)
    (tramp-send-command
     method user host
     (format "rm -r %s" (tramp-shell-quote-argument localname))
     ;; Don't read the output, do it explicitely.
     nil t)
    ;; Wait for the remote system to return to us...
    ;; This might take a while, allow it plenty of time.
    (tramp-wait-for-output (tramp-get-connection-process method user host) 120)
    ;; Make sure that it worked...
    (and (file-exists-p filename)
	 (tramp-error
	  method user host 'file-error
	  "Failed to recursively delete %s" filename))))

(defun tramp-handle-dired-compress-file (file &rest ok-flag)
  "Like `dired-compress-file' for tramp files."
  ;; OK-FLAG is valid for XEmacs only, but not implemented.
  ;; Code stolen mainly from dired-aux.el.
  (with-parsed-tramp-file-name file nil
    (tramp-cache-flush-file method user host localname)
    (save-excursion
      (let ((suffixes
	     (if (not (featurep 'xemacs))
		 ;; Emacs case
		 (symbol-value 'dired-compress-file-suffixes)
	       ;; XEmacs has `dired-compression-method-alist', which is
	       ;; transformed into `dired-compress-file-suffixes' structure.
	       (mapcar
		'(lambda (x)
		   (list (concat (regexp-quote (nth 1 x)) "\\'")
			 nil
			 (mapconcat 'identity (nth 3 x) " ")))
		(symbol-value 'dired-compression-method-alist))))
	    suffix)
	;; See if any suffix rule matches this file name.
	(while suffixes
	  (let (case-fold-search)
	    (if (string-match (car (car suffixes)) localname)
		(setq suffix (car suffixes) suffixes nil))
	    (setq suffixes (cdr suffixes))))

	(cond ((file-symlink-p file)
	       nil)
	      ((and suffix (nth 2 suffix))
	       ;; We found an uncompression rule.
	       (tramp-message-for-buffer
		method user host 0 "Uncompressing %s..." file)
	       (when (zerop (tramp-send-command-and-check
			     method user host
			     (concat (nth 2 suffix) " " localname)))
		 (tramp-message-for-buffer
		  method user host 0 "Uncompressing %s...done" file)
		 ;; `dired-remove-file' is not defined in XEmacs
		 (funcall (symbol-function 'dired-remove-file) file)
		 (string-match (car suffix) file)
		 (concat (substring file 0 (match-beginning 0)))))
	      (t
	       ;; We don't recognize the file as compressed, so compress it.
	       ;; Try gzip.
	       (tramp-message-for-buffer
		method user host 0 "Compressing %s..." file)
	       (when (zerop (tramp-send-command-and-check
			     method user host
			     (concat "gzip -f " localname)))
		 (tramp-message-for-buffer
		  method user host 0 "Compressing %s...done" file)
		 ;; `dired-remove-file' is not defined in XEmacs
		 (funcall (symbol-function 'dired-remove-file) file)
		 (cond ((file-exists-p (concat file ".gz"))
			(concat file ".gz"))
		       ((file-exists-p (concat file ".z"))
			(concat file ".z"))
		       (t nil)))))))))

;; Pacify byte-compiler.  The function is needed on XEmacs only.  I'm
;; not sure at all that this is the right way to do it, but let's hope
;; it works for now, and wait for a guru to point out the Right Way to
;; achieve this.
;;(eval-when-compile
;;  (unless (fboundp 'dired-insert-set-properties)
;;    (fset 'dired-insert-set-properties 'ignore)))
;; Gerd suggests this:
(eval-when-compile (require 'dired))
;; Note that dired is required at run-time, too, when it is needed.
;; It is only needed on XEmacs for the function
;; `dired-insert-set-properties'.

(defun tramp-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (tramp-cache-flush-file method user host localname)
    (if (and (boundp 'ls-lisp-use-insert-directory-program)
	     (not (symbol-value 'ls-lisp-use-insert-directory-program)))
	(tramp-run-real-handler
	 'insert-directory (list filename switches wildcard full-directory-p))
      ;; For the moment, we assume that the remote "ls" program does not
      ;; grok "--dired".  In the future, we should detect this on
      ;; connection setup.
      (when (string-match "^--dired\\s-+" switches)
	(setq switches (replace-match "" nil t switches)))
      (tramp-message-for-buffer
       method user host
       4 "Inserting directory `ls %s %s', wildcard %s, fulldir %s"
       switches filename (if wildcard "yes" "no")
       (if full-directory-p "yes" "no"))
      (when wildcard
        (setq wildcard (file-name-nondirectory localname))
        (setq localname (file-name-directory localname)))
      (when (listp switches)
        (setq switches (mapconcat 'identity switches " ")))
      (unless full-directory-p
        (setq switches (concat "-d " switches)))
      (when wildcard
        (setq switches (concat switches " " wildcard)))
      (save-excursion
        ;; If `full-directory-p', we just say `ls -l FILENAME'.
        ;; Else we chdir to the parent directory, then say `ls -ld BASENAME'.
        (if full-directory-p
            (tramp-send-command
	     method user host
             (format "%s %s %s"
                     (tramp-get-ls-command method user host)
                     switches
                     (if wildcard
                         localname
                       (tramp-shell-quote-argument (concat localname ".")))))
          (tramp-barf-unless-okay
	   method user host
           (format "cd %s" (tramp-shell-quote-argument
                            (file-name-directory localname)))
           "Couldn't `cd %s'"
           (tramp-shell-quote-argument (file-name-directory localname)))
          (tramp-send-command
	   method user host
           (format "%s %s %s"
                   (tramp-get-ls-command method user host)
                   switches
                   (if wildcard
                       localname
		     (if (zerop (length (file-name-nondirectory localname)))
			 ""
		       (tramp-shell-quote-argument
			(file-name-nondirectory localname))))))))
      ;; We cannot use `insert-buffer-substring' because the tramp buffer
      ;; changes its contents before insertion due to calling
      ;; `expand-file' and alike.
      (insert
       (with-current-buffer (tramp-get-buffer method user host)
	 (buffer-string)))
      (save-excursion
	(tramp-send-command method user host "cd")))))

;; CCC is this the right thing to do?
(defun tramp-handle-unhandled-file-name-directory (filename)
  "Like `unhandled-file-name-directory' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (expand-file-name (tramp-make-tramp-file-name method user host "~/"))))

;; Canonicalization of file names.

(defun tramp-drop-volume-letter (name)
  "Cut off unnecessary drive letter from file NAME.
The function `tramp-handle-expand-file-name' calls `expand-file-name'
locally on a remote file name.  When the local system is a W32 system
but the remote system is Unix, this introduces a superfluous drive
letter into the file name.  This function removes it.

Doesn't do anything if the NAME does not start with a drive letter."
  (if (and (> (length name) 1)
           (char-equal (aref name 1) ?:)
           (let ((c1 (aref name 0)))
             (or (and (>= c1 ?A) (<= c1 ?Z))
                 (and (>= c1 ?a) (<= c1 ?z)))))
      (substring name 2)
    name))

(defun tramp-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for tramp files.
If the localname part of the given filename starts with \"/../\" then
the result will be a local, non-Tramp, filename."
  ;; If DIR is not given, use DEFAULT-DIRECTORY or "/".
  (setq dir (or dir default-directory "/"))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (concat (file-name-as-directory dir) name)))
  ;; If NAME is not a tramp file, run the real handler
  (if (not (tramp-tramp-file-p name))
      (tramp-run-real-handler 'expand-file-name
                              (list name nil))
    ;; Dissect NAME.
    (with-parsed-tramp-file-name name nil
      (unless (file-name-absolute-p localname)
	(setq localname (concat "~/" localname)))
      (save-excursion
	;; Tilde expansion if necessary.  This needs a shell which
	;; groks tilde expansion!  The function `tramp-find-shell' is
	;; supposed to find such a shell on the remote host.  Please
	;; tell me about it when this doesn't work on your system.
	(when (string-match "\\`\\(~[^/]*\\)\\(.*\\)\\'" localname)
	  (let ((uname (match-string 1 localname))
		(fname (match-string 2 localname)))
	    ;; We cannot simply apply "~/", because under sudo "~/" is
	    ;; expanded to the local user home directory but to the
	    ;; root home directory.  On the other hand, using always
	    ;; the default user name for tilde expansion is not
	    ;; appropriate either, because ssh and companions might
	    ;; use a user name from the config file.
	    (when (and (string-equal uname "~")
		       (string-match "\\`su\\(do\\)?\\'"
				     (tramp-find-method method user host)))
	      (setq uname (concat uname (tramp-find-user method user host))))
	    (tramp-send-command
	     method user host (format "cd %s; pwd" uname))
	    (goto-char (point-min))
	    (setq uname (buffer-substring (point) (tramp-line-end-position)))
	    (setq localname (concat uname fname))))
	;; There might be a double slash, for example when "~/"
	;; expands to "/". Remove this.
	(while (string-match "//" localname)
	  (setq localname (replace-match "/" t t localname)))
	;; No tilde characters in file name, do normal
	;; expand-file-name (this does "/./" and "/../").  We bind
	;; `directory-sep-char' here for XEmacs on Windows, which
	;; would otherwise use backslash.  `default-directory' is
	;; bound, because on Windows there would be problems with UNC
	;; shares or Cygwin mounts.
	(tramp-let-maybe directory-sep-char ?/
	  (let ((default-directory (tramp-temporary-file-directory)))
	    (tramp-make-tramp-file-name
	     (tramp-find-method method user host)
	     (tramp-find-user   method user host)
	     (tramp-find-host   method user host)
	     (tramp-drop-volume-letter
	      (tramp-run-real-handler 'expand-file-name
				      (list localname))))))))))

(defun tramp-handle-substitute-in-file-name (filename)
  "Like `substitute-in-file-name' for tramp files.
If the URL Tramp syntax is chosen, \"//\" as method delimeter and \"/~\" at
beginning of local filename are not substituted."
  (if (equal tramp-syntax 'url)
      (with-parsed-tramp-file-name filename nil
	;; We need to check localname only.  The other parts cannot contain
	;; "//" or "/~".
	(if (and (> (length localname) 1)
		 (or (string-match "//" localname)
		     (string-match "/~" localname 1)))
	    (tramp-run-real-handler 'substitute-in-file-name (list filename))
	  (tramp-make-tramp-file-name
	   (when method (substitute-in-file-name method))
	   (when user (substitute-in-file-name user))
	   (when host (substitute-in-file-name host))
	   (when localname (substitute-in-file-name localname)))))
    (tramp-run-real-handler 'substitute-in-file-name (list filename))))

;; In XEmacs, electricity is implemented via a key map (see minibuf.el).
;; Must be disabled.
(when (and (equal tramp-syntax 'url)
	   (boundp 'read-file-name-map)
	   (keymapp (symbol-value 'read-file-name-map)))
  (define-key (symbol-value 'read-file-name-map) "/" nil)
  (define-key (symbol-value 'read-file-name-map) "~" nil))


;; Remote commands.

;; We use BUFFER also as connection buffer during setup. Because of
;; this, its original contents must be saved, and restored once
;; connection has been setup.
(defun tramp-handle-start-process (name buffer program &rest args)
  "Like `start-process' for Tramp files."
  (with-parsed-tramp-file-name default-directory nil
    (unwind-protect
	(save-excursion
	  ;; Set the new process properties.
	  (tramp-set-connection-property
	   "process-name" name method user host)
	  (tramp-set-connection-property
	   "process-buffer" (get-buffer-create buffer) method user host)
	  ;; Activate narrowing in order to save BUFFER contents.
	  (with-current-buffer (tramp-get-connection-buffer method user host)
	    (narrow-to-region (point-max) (point-max)))
	  ;; Goto working directory
	  (tramp-send-command
	   method user host
	   (format "cd %s" (tramp-shell-quote-argument localname)))
	  ;; Send the command.
	  (tramp-send-command
	   method user host
	   (format "%s; exit"
		   (mapconcat 'tramp-shell-quote-argument
			      (cons program args) " "))
	   nil t) ; nooutput
	  ;; Return process.
	  (get-process (tramp-get-connection-process method user host)))
      ;; Save exit.
      (with-current-buffer (tramp-get-connection-buffer method user host)
	(widen))
      (tramp-set-connection-property "process-name" nil method user host)
      (tramp-set-connection-property "process-buffer" nil method user host))))

(defun tramp-handle-call-process
  (program &optional infile destination display &rest args)
  "Like `call-process' for Tramp files."
  ;; The implementation is not complete yet.
  (when (and (numberp destination) (zerop destination))
    (error "Implementation does not handle immediate return"))

  (with-parsed-tramp-file-name default-directory nil
    (let ((temp-name-prefix (tramp-make-tramp-temp-file method user host))
	  command input stderr outbuf ret)
      ;; Compute command.
      (setq command (mapconcat 'tramp-shell-quote-argument
			       (cons program args) " "))
      ;; Determine input.
      (when (stringp infile)
	(setq infile (expand-file-name infile))
	(if (tramp-equal-remote default-directory infile)
	    ;; INFILE is on the same remote host.
	    (setq input (with-parsed-tramp-file-name infile nil localname))
	  ;; INFILE must be copied to remote host.
	  (setq input (concat temp-name-prefix ".in"))
	  (copy-file
	   infile
	   (tramp-make-tramp-file-name method user host input)
	   t)))
      (when input (setq command (format "%s <%s" command input)))

      ;; Determine output.
      (cond
       ;; Just a buffer
       ((bufferp destination)
	(setq outbuf destination))
       ;; A buffer name
       ((stringp destination)
	(setq outbuf (get-buffer-create destination)))
       ;; (REAL-DESTINATION ERROR-DESTINATION)
       ((consp destination)
	;; output
	(cond
	 ((bufferp (car destination))
	  (setq outbuf (car destination)))
	 ((stringp (car destination))
	  (setq outbuf (get-buffer-create (car destination)))))
	;; stderr
	(cond
	 ((stringp (cadr destination))
	  (setcar (cdr destination) (expand-file-name (cadr destination)))
	  (if (tramp-equal-remote default-directory (cadr destination))
	      ;; stderr is on the same remote host.
	      (setq stderr (with-parsed-tramp-file-name
			       (cadr destination) nil localname))
	    ;; stderr must be copied to remote host.  The temporary
	    ;; file must be deleted after execution.
	    (setq stderr (concat temp-name-prefix ".err"))))
	 ;; stderr to be discarded
	 ((null (cadr destination))
	  (setq stderr "/dev/null"))))
       ;; 't
       (destination
	(setq outbuf (current-buffer))))
      (when stderr (setq command (format "%s 2>%s" command stderr)))

      ;; If we have a temporary file, it must be removed after operation.
      (when (and input (string-match temp-name-prefix input))
	(setq command (format "%s; rm %s" command input)))
      ;; Goto working directory.
      (save-excursion
	(tramp-send-command
	 method user host
	 (format "cd %s" (tramp-shell-quote-argument localname))))
      ;; Send the command.  It might not return in time, so we protect it.
      (condition-case nil
	  (unwind-protect
	      (save-excursion (tramp-send-command method user host command))
	    ;; We should show the output anyway.
	    (when outbuf
	      (with-current-buffer outbuf
		(insert-buffer-substring
		 (tramp-get-connection-buffer method user host)))
	      (when display (display-buffer outbuf))))
	;; When the user did interrupt, we should do it also.
	(kill-buffer (tramp-get-connection-buffer method user host))
	(setq ret 1))
      (unless ret
	;; Check return code.
	(setq ret (tramp-send-command-and-check method user host nil))
	;; Provide error file.
	(when (and stderr (string-match temp-name-prefix stderr))
	  (rename-file (tramp-make-tramp-file-name method user host stderr)
		       (cadr destination) t)))
      ;; Return exit status.
      ret)))

(defun tramp-handle-process-file (program &optional infile buffer display &rest args)
  "Like `process-file' for Tramp files."
  (apply 'call-process program infile buffer display args))

;; File Editing.

(defsubst tramp-make-temp-file ()
  (funcall (if (fboundp 'make-temp-file) 'make-temp-file 'make-temp-name)
	   (expand-file-name tramp-temp-name-prefix
			     (tramp-temporary-file-directory))))

(defsubst tramp-make-tramp-temp-file (method user host)
  (format
   "/tmp/%s%s"
   tramp-temp-name-prefix
   (process-id
    (get-buffer-process (tramp-get-connection-buffer method user host)))))

(defvar tramp-handle-file-local-copy-hook nil
  "Normal hook to be run at the end of `tramp-handle-file-local-copy'.")

(defun tramp-handle-file-local-copy (filename)
  "Like `file-local-copy' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (let ((tramp-buf (tramp-get-buffer method user host))
	  ;; We used to bind the following as late as possible.
	  ;; loc-enc and loc-dec were bound directly before the if
	  ;; statement that checks them.  But the functions
	  ;; tramp-get-* might invoke the "are you awake" check in
	  ;; tramp-maybe-open-connection, which is an unfortunate time
	  ;; since we rely on the buffer contents at that spot.
	  (rem-enc (tramp-get-remote-encoding method user host))
	  (rem-dec (tramp-get-remote-decoding method user host))
	  (loc-enc (tramp-get-local-encoding method user host))
	  (loc-dec (tramp-get-local-decoding method user host))
	  tmpfil)
      (unless (file-exists-p filename)
	(tramp-error
	 method user host 'file-error
	 "Cannot make local copy of non-existing file `%s'"
	 filename))
      (setq tmpfil (tramp-make-temp-file))

      (cond ((tramp-method-out-of-band-p method user host)
	     ;; `copy-file' handles out-of-band methods
	     (copy-file filename tmpfil t t))

	    ((and rem-enc rem-dec)
	     ;; Use inline encoding for file transfer.
	     (save-excursion
	       ;; Following line for setting tramp-current-method,
	       ;; tramp-current-user, tramp-current-host.
	       (set-buffer tramp-buf)
	       (tramp-message 5 "Encoding remote file %s..." filename)
	       (tramp-barf-unless-okay
		method user host
		(concat rem-enc " < " (tramp-shell-quote-argument localname))
		"Encoding remote file failed, see buffer `%s' for details"
		tramp-buf)

	       (tramp-message 5 "Decoding remote file %s..." filename)

	       ;; Here is where loc-enc and loc-dec used to be let-bound.
	       (if (and (symbolp loc-dec) (fboundp loc-dec))
		   ;; If local decoding is a function, we call it.
		   (let ((tmpbuf (generate-new-buffer " *tramp tmp*")))
		     (unwind-protect
			 (with-current-buffer tmpbuf
			   (erase-buffer)
			   (insert-buffer-substring tramp-buf)
			   (tramp-message-for-buffer
			    method user host
			    5 "Decoding remote file %s with function %s..."
			    filename loc-dec)
			   (let ((coding-system-for-write 'binary))
			     (funcall loc-dec (point-min) (point-max))
			     (write-region (point-min) (point-max) tmpfil)))
		       (kill-buffer tmpbuf)))
		 ;; If tramp-decoding-function is not defined for this
		 ;; method, we invoke tramp-decoding-command instead.
		 (let ((tmpfil2 (tramp-make-temp-file)))
		   (write-region (point-min) (point-max) tmpfil2)
		   (tramp-message
		    5 "Decoding remote file %s with command %s..."
		    filename loc-dec)
		   (tramp-call-local-coding-command
		    loc-dec tmpfil2 tmpfil)
		   (delete-file tmpfil2)))
	       (tramp-message-for-buffer
		method user host
		5 "Decoding remote file %s...done" filename)
	       ;; Set proper permissions.
	       (set-file-modes tmpfil (file-modes filename))))

	    (t (tramp-error
		method user host 'file-error
		"Wrong method specification for `%s'" method)))
      (run-hooks 'tramp-handle-file-local-copy-hook)
      tmpfil)))

(defun tramp-handle-file-remote-p (filename)
  "Like `file-remote-p' for tramp files."
  (when (tramp-tramp-file-p filename)
    (with-parsed-tramp-file-name filename nil
      (tramp-make-tramp-file-name
       (tramp-find-method method user host)
       (tramp-find-user   method user host)
       (tramp-find-host   method user host)
       ""))))

(defun tramp-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for tramp files."
  (barf-if-buffer-read-only)
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (if (not (file-exists-p filename))
	(progn
	  (when visit
	    (setq buffer-file-name filename)
	    (set-visited-file-modtime)
	    (set-buffer-modified-p nil))
	  (tramp-error
	   method user host 'file-error
	   "File %s not found on remote host" filename)
	  (list (expand-file-name filename) 0))
      ;; `insert-file-contents-literally' takes care to avoid calling
      ;; jka-compr.  By let-binding inhibit-file-name-operation, we
      ;; propagate that care to the file-local-copy operation.
      (let ((local-copy
	     (let ((inhibit-file-name-operation
		    (when (eq inhibit-file-name-operation
			      'insert-file-contents)
		      'file-local-copy)))
	       (file-local-copy filename)))
	    coding-system-used result)
	(when visit
	  (setq buffer-file-name filename)
	  (set-visited-file-modtime)
	  (set-buffer-modified-p nil))
	(tramp-message-for-buffer
	 method user host
	 4 "Inserting local temp file `%s'..." local-copy)
	(setq result (insert-file-contents local-copy nil beg end replace))
	;; Now `last-coding-system-used' has right value.  Remember it.
	(when (boundp 'last-coding-system-used)
	  (setq coding-system-used (symbol-value 'last-coding-system-used)))
	(tramp-message-for-buffer
	 method user host
	 4 "Inserting local temp file `%s'...done" local-copy)
	(delete-file local-copy)
	(when (boundp 'last-coding-system-used)
	  (set 'last-coding-system-used coding-system-used))
	(list (expand-file-name filename)
	      (second result))))))


(defun tramp-handle-find-backup-file-name (filename)
  "Like `find-backup-file-name' for tramp files."
  (with-parsed-tramp-file-name filename nil
    ;; We set both variables. It doesn't matter whether it is
    ;; Emacs or XEmacs
    (let ((backup-directory-alist
	   ;; Emacs case
	   (when (boundp 'backup-directory-alist)
	     (if (boundp 'tramp-backup-directory-alist)
		 (mapcar
		  '(lambda (x)
		     (cons
		      (car x)
		      (if (and (stringp (cdr x))
			       (file-name-absolute-p (cdr x))
			       (not (tramp-file-name-p (cdr x))))
			  (tramp-make-tramp-file-name method user host (cdr x))
			(cdr x))))
		  (symbol-value 'tramp-backup-directory-alist))
	       (symbol-value 'backup-directory-alist))))

	  (bkup-backup-directory-info
	   ;; XEmacs case
	   (when (boundp 'bkup-backup-directory-info)
	     (if (boundp 'tramp-bkup-backup-directory-info)
		 (mapcar
		  '(lambda (x)
		     (nconc
		      (list (car x))
		      (list
		       (if (and (stringp (car (cdr x)))
				(file-name-absolute-p (car (cdr x)))
				(not (tramp-file-name-p (car (cdr x)))))
			   (tramp-make-tramp-file-name
			    method user host (car (cdr x)))
			 (car (cdr x))))
		      (cdr (cdr x))))
		  (symbol-value 'tramp-bkup-backup-directory-info))
	       (symbol-value 'bkup-backup-directory-info)))))

      (tramp-run-real-handler 'find-backup-file-name (list filename)))))
(defun tramp-handle-make-auto-save-file-name ()
  "Like `make-auto-save-file-name' for tramp files.
Returns a file name in `tramp-auto-save-directory' for autosaving this file."
  (let ((buffer-file-name
	 (tramp-subst-strs-in-string
	  '(("_" . "|")
	    ("/" . "_a")
	    (":" . "_b")
	    ("|" . "__")
	    ("[" . "_l")
	    ("]" . "_r"))
	  (buffer-file-name))))
    (when tramp-auto-save-directory
      (setq buffer-file-name
	    (expand-file-name buffer-file-name tramp-auto-save-directory))
      (unless (file-exists-p tramp-auto-save-directory)
	(make-directory tramp-auto-save-directory t)))
    ;; Run plain `make-auto-save-file-name'.  There might be an advice when
    ;; it is not a magic file name operation (since Emacs 22).
    ;; We must deactivate it temporarily.
    (if (not (ad-is-active 'make-auto-save-file-name))
	(tramp-run-real-handler
	 'make-auto-save-file-name nil)
      ;; else
      (ad-deactivate 'make-auto-save-file-name)
      (prog1
       (tramp-run-real-handler
	'make-auto-save-file-name nil)
       (ad-activate 'make-auto-save-file-name)))))


(defvar tramp-handle-write-region-hook nil
  "Normal hook to be run at the end of `tramp-handle-write-region'.")

;; CCC grok APPEND, LOCKNAME
(defun tramp-handle-write-region
  (start end filename &optional append visit lockname confirm)
  "Like `write-region' for tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (unless (null append)
      (tramp-error
       method user host 'file-error
       "Cannot append to file using Tramp (`%s')" filename))
    ;; Following part commented out because we don't know what to do about
    ;; file locking, and it does not appear to be a problem to ignore it.
    ;; Ange-ftp ignores it, too.
    ;;  (when (and lockname (stringp lockname))
    ;;    (setq lockname (expand-file-name lockname)))
    ;;  (unless (or (eq lockname nil)
    ;;              (string= lockname filename))
    ;;    (error
    ;;     "tramp-handle-write-region: LOCKNAME must be nil or equal FILENAME"))
    ;; XEmacs takes a coding system as the seventh argument, not `confirm'
    (when (and (not (featurep 'xemacs)) confirm (file-exists-p filename))
      (unless (y-or-n-p (format "File %s exists; overwrite anyway? " filename))
	(tramp-error method user host 'file-error "File not overwritten")))
    (let ((rem-enc (tramp-get-remote-encoding method user host))
	  (rem-dec (tramp-get-remote-decoding method user host))
	  (loc-enc (tramp-get-local-encoding method user host))
	  (loc-dec (tramp-get-local-decoding method user host))
	  (modes (save-excursion (file-modes filename)))
	  ;; We use this to save the value of `last-coding-system-used'
	  ;; after writing the tmp file.  At the end of the function,
	  ;; we set `last-coding-system-used' to this saved value.
	  ;; This way, any intermediary coding systems used while
	  ;; talking to the remote shell or suchlike won't hose this
	  ;; variable.  This approach was snarfed from ange-ftp.el.
	  coding-system-used
	  ;; Write region into a tmp file.  This isn't really needed if we
	  ;; use an encoding function, but currently we use it always
	  ;; because this makes the logic simpler.
	  (tmpfil (tramp-make-temp-file)))
      ;; We say `no-message' here because we don't want the visited file
      ;; modtime data to be clobbered from the temp file.  We call
      ;; `set-visited-file-modtime' ourselves later on.
      (tramp-run-real-handler
       'write-region
       (if confirm ; don't pass this arg unless defined for backward compat.
	   (list start end tmpfil append 'no-message lockname confirm)
	 (list start end tmpfil append 'no-message lockname)))
      ;; Now, `last-coding-system-used' has the right value.  Remember it.
      (when (boundp 'last-coding-system-used)
	(setq coding-system-used (symbol-value 'last-coding-system-used)))
      ;; The permissions of the temporary file should be set.  If
      ;; filename does not exist (eq modes nil) it has been renamed to
      ;; the backup file.  This case `save-buffer' handles
      ;; permissions.
      (when modes (set-file-modes tmpfil modes))
      ;; This is a bit lengthy due to the different methods possible for
      ;; file transfer.  First, we check whether the method uses an rcp
      ;; program.  If so, we call it.  Otherwise, both encoding and
      ;; decoding command must be specified.  However, if the method
      ;; _also_ specifies an encoding function, then that is used for
      ;; encoding the contents of the tmp file.
      (cond ((tramp-method-out-of-band-p method user host)
	     ;; `copy-file' handles out-of-band methods
	     (copy-file tmpfil filename t t))

	    ((and rem-enc rem-dec)
	     ;; Use inline file transfer
	     (let ((tmpbuf (generate-new-buffer " *tramp tmp*")))
	       (save-excursion
		 ;; Encode tmpfil into tmpbuf
		 (tramp-message-for-buffer
		  method user host
		  5 "Encoding region...")
		 (unwind-protect
		     (with-current-buffer tmpbuf
		       (erase-buffer)
		       ;; Use encoding function or command.
		       (if (and (symbolp loc-enc) (fboundp loc-enc))
			   (progn
			     (tramp-message-for-buffer
			      method user host
			      5 "Encoding region using function `%s'..."
			      (symbol-name loc-enc))
			     (insert-file-contents-literally tmpfil)
			     ;; CCC.  The following `let' is a workaround for
			     ;; the base64.el that comes with pgnus-0.84.  If
			     ;; both of the following conditions are
			     ;; satisfied, it tries to write to a local file
			     ;; in default-directory, but at this point,
			     ;; default-directory is remote.
			     ;; (CALL-PROCESS-REGION can't write to remote
			     ;; files, it seems.)  The file in question is a
			     ;; tmp file anyway.
			     (let ((default-directory
				     (tramp-temporary-file-directory)))
			       (funcall loc-enc (point-min) (point-max))))

			 (tramp-message-for-buffer
			  method user host
			  5 "Encoding region using command `%s'..." loc-enc)
			 (unless (equal 0 (tramp-call-local-coding-command
					   loc-enc tmpfil t))
			   (tramp-error
			    method user host 'file-error
			    (concat "Cannot write to `%s', local encoding"
				    " command `%s' failed")
			    filename loc-enc)))

		       ;; Send tmpbuf into remote decoding command which
		       ;; writes to remote file.  Because this happens on the
		       ;; remote host, we cannot use the function.
		       (goto-char (point-max))
		       (unless (bolp) (newline))
		       (tramp-message-for-buffer
			method user host
			5 "Decoding region into remote file %s..." filename)
		       (tramp-send-command
			method user host
			(format
			 "%s >%s <<'EOF'\n%sEOF"
			 rem-dec
			 (tramp-shell-quote-argument localname)
			 (buffer-string)))
		       (tramp-barf-unless-okay
			method user host nil
			(concat "Couldn't write region to `%s',"
				" decode using `%s' failed")
			filename rem-dec)
		       ;; When `file-precious-flag' is set, the region is
		       ;; written to a temporary file.  Check that the
		       ;; checksum is equal to that from the local tmpfil.
		       (when file-precious-flag
			 (set-buffer tmpbuf)
			 (erase-buffer)
			 (let ((default-directory
				 (tramp-temporary-file-directory)))
			   (and
			    ;; cksum runs locally
			    (zerop (call-process "cksum" tmpfil t))
			    ;; cksum runs remotely
			    (zerop
			     (tramp-send-command-and-check
			      method user host
			      (format
			       "cksum <%s"
			       (tramp-shell-quote-argument localname))))
			    ;; ... they are different
			    (not
			     (string-equal
			      (buffer-string)
			      (with-current-buffer tmpbuf (buffer-string))))
			    (tramp-error
			     method user host 'file-error
			     (concat "Couldn't write region to `%s',"
				     " decode using `%s' failed")
			     filename rem-dec))))
		       (tramp-message-for-buffer
			method user host
			5 "Decoding region into remote file %s...done" filename)
		       (tramp-cache-flush-file method user host localname))
		   (kill-buffer tmpbuf)))))
	    (t
	     (tramp-error
	      method user host 'file-error
	      (concat "Method `%s' should specify both encoding and "
		      "decoding command or an rcp program")
	      method)))
      (delete-file tmpfil)
      (when (or (eq visit t) (stringp visit))
	(set-visited-file-modtime
	 ;; We must pass modtime explicitely, because filename can be different
	 ;; from (buffer-file-name), f.e. if `file-precious-flag' is set.
	 (nth 5 (file-attributes filename))))
      ;; Make `last-coding-system-used' have the right value.
      (when (boundp 'last-coding-system-used)
	(set 'last-coding-system-used coding-system-used))
      (when (or (eq visit t) (null visit) (stringp visit))
	(tramp-message-for-buffer method user host 0 "Wrote %s" filename))
      (run-hooks 'tramp-handle-write-region-hook))))

;; Call down to the real handler.
;; Because EFS does not play nicely with TRAMP (both systems match a
;; TRAMP file name) it is needed to disable efs as well as tramp for the
;; operation.
;;
;; Other than that, this is the canon file-handler code that the doco
;; says should be used here. Which is nice.
;;
;; Under XEmacs current, EFS also hooks in as
;; efs-sifn-handler-function to handle any filename with environment
;; variables. This has two implications:
;; 1) That EFS may not be completely dead (yet) for TRAMP filenames
;; 2) That TRAMP might want to do the same thing.
;; Details as they come in.
;;
;; Daniel Pittman <daniel@danann.net>

;; (defun tramp-run-real-handler (operation args)
;;   "Invoke normal file name handler for OPERATION.
;; This inhibits EFS and Ange-FTP, too, because they conflict with tramp.
;; First arg specifies the OPERATION, remaining ARGS are passed to the
;; OPERATION."
;;   (let ((inhibit-file-name-handlers
;;          (list 'tramp-file-name-handler
;; 	       'efs-file-handler-function
;;                'ange-ftp-hook-function
;;                (and (eq inhibit-file-name-operation operation)
;;                     inhibit-file-name-handlers)))
;;         (inhibit-file-name-operation operation))
;;     (apply operation args)))

(defun tramp-run-real-handler (operation args)
  "Invoke normal file name handler for OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (let* ((inhibit-file-name-handlers
	  `(tramp-file-name-handler
	    tramp-completion-file-name-handler
	    cygwin-mount-name-hook-function
	    cygwin-mount-map-drive-hook-function
	    .
	    ,(and (eq inhibit-file-name-operation operation)
		  inhibit-file-name-handlers)))
	 (inhibit-file-name-operation operation))
    (apply operation args)))

;; This function is used from `tramp-completion-file-name-handler' functions
;; only, if `tramp-completion-mode' is true. But this cannot be checked here
;; because the check is based on a full filename, not available for all
;; basic I/O operations.
(defun tramp-completion-run-real-handler (operation args)
  "Invoke `tramp-file-name-handler' for OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (let* ((inhibit-file-name-handlers
	  `(tramp-completion-file-name-handler
	    cygwin-mount-name-hook-function
	    cygwin-mount-map-drive-hook-function
	    .
	    ,(and (eq inhibit-file-name-operation operation)
		  inhibit-file-name-handlers)))
	 (inhibit-file-name-operation operation))
    (apply operation args)))

;; We handle here all file primitives.  Most of them have the file
;; name as first parameter; nevertheless we check for them explicitly
;; in order to be signalled if a new primitive appears.  This
;; scenario is needed because there isn't a way to decide by
;; syntactical means whether a foreign method must be called.  It would
;; ease the life if `file-name-handler-alist' would support a decision
;; function as well but regexp only.
(defun tramp-file-name-for-operation (operation &rest args)
  "Return file name related to OPERATION file primitive.
ARGS are the arguments OPERATION has been called with."
  (cond
   ; FILE resp DIRECTORY
   ((member operation
	    (list 'access-file 'byte-compiler-base-file-name 'delete-directory
		  'delete-file 'diff-latest-backup-file 'directory-file-name
		  'directory-files 'directory-files-and-attributes
		  'dired-compress-file 'dired-uncache
		  'file-accessible-directory-p 'file-attributes
		  'file-directory-p 'file-executable-p 'file-exists-p
		  'file-local-copy 'file-remote-p 'file-modes
		  'file-name-as-directory 'file-name-directory
		  'file-name-nondirectory 'file-name-sans-versions
		  'file-ownership-preserved-p 'file-readable-p
		  'file-regular-p 'file-symlink-p 'file-truename
		  'file-writable-p 'find-backup-file-name 'find-file-noselect
		  'get-file-buffer 'insert-directory 'insert-file-contents
		  'load 'make-directory 'make-directory-internal
		  'set-file-modes 'substitute-in-file-name
		  'unhandled-file-name-directory 'vc-registered
		  ; XEmacs only
		  'abbreviate-file-name 'create-file-buffer
		  'dired-file-modtime 'dired-make-compressed-filename
		  'dired-recursive-delete-directory 'dired-set-file-modtime
		  'dired-shell-unhandle-file-name 'dired-uucode-file
		  'insert-file-contents-literally 'recover-file
		  'vm-imap-check-mail 'vm-pop-check-mail 'vm-spool-check-mail))
    (if (file-name-absolute-p (nth 0 args))
	(nth 0 args)
      (expand-file-name (nth 0 args))))
   ; FILE DIRECTORY resp FILE1 FILE2
   ((member operation
	    (list 'add-name-to-file 'copy-file 'expand-file-name
		  'file-name-all-completions 'file-name-completion
		  'file-newer-than-file-p 'make-symbolic-link 'rename-file
		  ; XEmacs only
		  'dired-make-relative-symlink
		  'vm-imap-move-mail 'vm-pop-move-mail 'vm-spool-move-mail))
    (save-match-data
      (cond
       ((string-match tramp-file-name-regexp (nth 0 args)) (nth 0 args))
       ((string-match tramp-file-name-regexp (nth 1 args)) (nth 1 args))
       (t (buffer-file-name (current-buffer))))))
   ; START END FILE
   ((eq operation 'write-region)
    (nth 2 args))
   ; BUF
   ((member operation
	    (list 'set-visited-file-modtime 'verify-visited-file-modtime
                  ; Emacs 22 only
		  'make-auto-save-file-name
	          ; XEmacs only
		  'backup-buffer))
    (buffer-file-name
     (if (bufferp (nth 0 args)) (nth 0 args) (current-buffer))))
   ; COMMAND
   ((member operation
	    (list 'dired-call-process
                  ; Emacs only
		  'shell-command
                  ; Emacs 22 only
                  'process-file
	          ; XEmacs only
		  'dired-print-file 'dired-shell-call-process
		  ; nowhere yet (but let's hope)
		  'start-process 'call-process))
    default-directory)
   ; unknown file primitive
   (t (error "unknown file I/O primitive: %s" operation))))

(defun tramp-find-foreign-file-name-handler (filename)
  "Return foreign file name handler if exists."
  (when (and (stringp filename) (tramp-tramp-file-p filename))
    (let (elt
	  res
	  (handler-alist tramp-foreign-file-name-handler-alist))
      (while handler-alist
	(setq elt (car handler-alist)
	      handler-alist (cdr handler-alist))
	(when (funcall (car elt) filename)
	  (setq handler-alist nil)
	  (setq res (cdr elt))))
      res)))

;; Main function.
;;;###autoload
(defun tramp-file-name-handler (operation &rest args)
  "Invoke Tramp file name handler.
Falls back to normal file name handler if no tramp file name handler exists."
  (save-match-data
    (let* ((filename (apply 'tramp-file-name-for-operation operation args))
	   (foreign (tramp-find-foreign-file-name-handler filename)))
      (cond
       (foreign (apply foreign operation args))
       (t (tramp-run-real-handler operation args))))))


;; In Emacs, there is some concurrency due to timers.  If a timer
;; interrupts Tramp and wishes to use the same connection buffer as
;; the "main" Emacs, then garbage might occur in the connection
;; buffer.  Therefore, we need to make sure that a timer does not use
;; the same connection buffer as the "main" Emacs.  We implement a
;; cheap global lock, instead of locking each connection buffer
;; separately.  The global lock is based on two variables,
;; `tramp-locked' and `tramp-locker'.  `tramp-locked' is set to true
;; (with setq) to indicate a lock.  But Tramp also calls itself during
;; processing of a single file operation, so we need to allow
;; recursive calls.  That's where the `tramp-locker' variable comes in
;; -- it is let-bound to t during the execution of the current
;; handler.  So if `tramp-locked' is t and `tramp-locker' is also t,
;; then we should just proceed because we have been called
;; recursively.  But if `tramp-locker' is nil, then we are a timer
;; interrupting the "main" Emacs, and then we signal an error.

(defvar tramp-locked nil
  "If non-nil, then Tramp is currently busy.
Together with `tramp-locker', this implements a locking mechanism
preventing reentrant calls of Tramp.")

(defvar tramp-locker nil
  "If non-nil, then a caller has locked Tramp.
Together with `tramp-locked', this implements a locking mechanism
preventing reentrant calls of Tramp.")

(defun tramp-sh-file-name-handler (operation &rest args)
  "Invoke remote-shell Tramp file name handler.
Fall back to normal file name handler if no Tramp handler exists."
  (when (and tramp-locked (not tramp-locker))
    (signal 'file-error (list "Forbidden reentrant call of Tramp")))
  (let ((tl tramp-locked))
    (unwind-protect
	(progn
	  (setq tramp-locked t)
	  (let ((tramp-locker t))
	    (save-match-data
	      (let ((fn (assoc operation tramp-file-name-handler-alist)))
		(if fn
		    (apply (cdr fn) args)
		  (tramp-run-real-handler operation args))))))
      (setq tramp-locked tl))))

;;;###autoload
(defun tramp-completion-file-name-handler (operation &rest args)
  "Invoke tramp file name completion handler.
Falls back to normal file name handler if no tramp file name handler exists."
;;   (setq tramp-debug-buffer t)
;;   (tramp-message 1 "%s %s" operation args)
;;   (tramp-message 1 "%s %s\n%s"
;; 		 operation args (with-output-to-string (backtrace)))
  (let ((fn (assoc operation tramp-completion-file-name-handler-alist)))
    (if fn
	(save-match-data (apply (cdr fn) args))
      (tramp-completion-run-real-handler operation args))))

;;;###autoload
(put 'tramp-completion-file-name-handler 'safe-magic t)

;; Remove autoloaded handlers from file name handler alist.  Useful,
;; if tramp-syntax has been changed.
(let ((a1 (rassq 'tramp-completion-file-name-handler file-name-handler-alist))
      (a2 (rassq 'tramp-file-name-handler file-name-handler-alist)))
  (setq file-name-handler-alist
	(delete a1 (delete a2 file-name-handler-alist))))

;; Register in file name handler alist
;;;###autoload
(add-to-list 'file-name-handler-alist
	     (cons tramp-file-name-regexp 'tramp-file-name-handler))
;;;###autoload
(add-to-list 'file-name-handler-alist
	     (cons tramp-completion-file-name-regexp
		   'tramp-completion-file-name-handler))

;; If jka-compr is already loaded, move it to the front of
;; `file-name-handler-alist'.  On Emacs 22 this will not be
;; necessary anymore."
(let ((jka (rassoc 'jka-compr-handler file-name-handler-alist)))
  (when jka
    (setq file-name-handler-alist
	  (cons jka (delete jka file-name-handler-alist)))))


;;; Interactions with other packages:

;; -- complete.el --

;; This function contributed by Ed Sabol
(defun tramp-handle-expand-many-files (name)
  "Like `PC-expand-many-files' for tramp files."
  (with-parsed-tramp-file-name name nil
    (save-match-data
      (if (or (string-match "\\*" name)
	      (string-match "\\?" name)
	      (string-match "\\[.*\\]" name))
	  (save-excursion
	    (let (bufstr)
	      ;; CCC: To do it right, we should quote certain characters
	      ;; in the file name, but since the echo command is going to
	      ;; break anyway when there are spaces in the file names, we
	      ;; don't bother.
	      ;;-(let ((comint-file-name-quote-list
	      ;;-       (set-difference tramp-file-name-quote-list
	      ;;-                       '(?\* ?\? ?[ ?]))))
	      ;;-  (tramp-send-command
	      ;;-   method user host
	      ;;-   (format "echo %s" (comint-quote-filename localname))))
	      (tramp-send-command method user host (format "echo %s" localname))
	      (setq bufstr (buffer-substring (point-min)
					     (tramp-line-end-position)))
	      (goto-char (point-min))
	      (if (string-equal localname bufstr)
		  nil
		(insert "(\"")
		(while (search-forward " " nil t)
		  (delete-backward-char 1)
		  (insert "\" \""))
		(goto-char (point-max))
		(delete-backward-char 1)
		(insert "\")")
		(goto-char (point-min))
		(mapcar
		 (function (lambda (x)
			     (tramp-make-tramp-file-name method user host x)))
		 (read (current-buffer))))))
	(list (expand-file-name name))))))

;; Check for complete.el and override PC-expand-many-files if appropriate.
(eval-and-compile
  (defun tramp-save-PC-expand-many-files (name))); avoid compiler warning

(defun tramp-setup-complete ()
  (fset 'tramp-save-PC-expand-many-files
        (symbol-function 'PC-expand-many-files))
  (defun PC-expand-many-files (name)
    (if (tramp-tramp-file-p name)
        (funcall (symbol-function 'expand-many-files) name)
      (tramp-save-PC-expand-many-files name))))

;; Why isn't eval-after-load sufficient?
(if (fboundp 'PC-expand-many-files)
    (tramp-setup-complete)
  (eval-after-load "complete" '(tramp-setup-complete)))

;;; File name handler functions for completion mode

(defvar tramp-completion-mode nil
  "If non-nil, we are in file name completion mode.")

;; Necessary because `tramp-file-name-regexp-unified' and
;; `tramp-completion-file-name-regexp-unified' aren't different.
;; If nil, `tramp-completion-run-real-handler' is called (i.e. forwarding to
;; `tramp-file-name-handler'). Otherwise, it takes `tramp-run-real-handler'.
;; Using `last-input-event' is a little bit risky, because completing a file
;; might require loading other files, like "~/.netrc", and for them it
;; shouldn't be decided based on that variable. On the other hand, those files
;; shouldn't have partial tramp file name syntax. Maybe another variable should
;; be introduced overwriting this check in such cases. Or we change tramp
;; file name syntax in order to avoid ambiguities, like in XEmacs ...
;; In case of non unified file names it can be always true (and wouldn't be
;; necessary, because there are different regexp).
(defun tramp-completion-mode (file)
  "Checks whether method / user name / host name completion is active."
  (cond
   (tramp-completion-mode t)
   ((equal tramp-syntax 'sep) t)
;   ((string-match "^/.*:.*:$" file) nil)
;   ((string-match
;     (concat tramp-prefix-regexp
;      "\\(" tramp-method-regexp  "\\)" tramp-postfix-method-regexp "$")
;     file)
;    (member (match-string 1 file) (mapcar 'car tramp-methods)))
   ((or (equal last-input-event 'tab)
	;; Emacs
	(and (integerp last-input-event)
	     (not (event-modifiers last-input-event))
	     (or (char-equal last-input-event ?\?)
		 (char-equal last-input-event ?\t) ; handled by 'tab already?
		 (char-equal last-input-event ?\ )))
	;; XEmacs
	(and (featurep 'xemacs)
	     (not (event-modifiers last-input-event))
	     (or (char-equal
		  (funcall (symbol-function 'event-to-character)
			   last-input-event) ?\?)
		 (char-equal
		  (funcall (symbol-function 'event-to-character)
			   last-input-event) ?\t)
		 (char-equal
		  (funcall (symbol-function 'event-to-character)
			   last-input-event) ?\ ))))
    t)))

(defun tramp-completion-handle-file-exists-p (filename)
  "Like `file-exists-p' for tramp files."
  (if (tramp-completion-mode filename)
      (tramp-run-real-handler
       'file-exists-p (list filename))
    (tramp-completion-run-real-handler
     'file-exists-p (list filename))))

;; Localname manipulation in case of partial TRAMP file names.
(defun tramp-completion-handle-file-name-directory (file)
  "Like `file-name-directory' but aware of TRAMP files."
  (if (tramp-completion-mode file)
      "/"
    (tramp-completion-run-real-handler
     'file-name-directory (list file))))

;; Localname manipulation in case of partial TRAMP file names.
(defun tramp-completion-handle-file-name-nondirectory (file)
  "Like `file-name-nondirectory' but aware of TRAMP files."
  (substring
   file (length (tramp-completion-handle-file-name-directory file))))

;; Method, host name and user name completion.
;; `tramp-completion-dissect-file-name' returns a list of
;; tramp-file-name structures. For all of them we return possible completions.
(defun tramp-completion-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for partial tramp files."

  (unwind-protect
      ;; We need to reset `tramp-completion-mode'.
      (progn
	(setq tramp-completion-mode t)
	(let*
	    ((fullname (concat directory filename))
	     ;; possible completion structures
	     (v (tramp-completion-dissect-file-name fullname))
	     result result1)

	  (while v
	    (let* ((car (car v))
		   (method (tramp-file-name-method car))
		   (user (tramp-file-name-user car))
		   (host (tramp-file-name-host car))
		   (localname (tramp-file-name-localname car))
		   (m (tramp-find-method method user host))
		   (tramp-current-user user) ; see `tramp-parse-passwd'
		   all-user-hosts)

	      (unless localname        ;; Nothing to complete

		(if (or user host)

		    ;; Method dependent user / host combinations
		    (progn
		      (mapcar
		       (lambda (x)
			 (setq all-user-hosts
			       (append all-user-hosts
				       (funcall (nth 0 x) (nth 1 x)))))
		       (tramp-get-completion-function m))

		      (setq result (append result
	                (mapcar
			 (lambda (x)
			   (tramp-get-completion-user-host
			    method user host (nth 0 x) (nth 1 x)))
			 (delq nil all-user-hosts)))))

		  ;; Possible methods
		  (setq result
			(append result (tramp-get-completion-methods m)))))

	      (setq v (cdr v))))

	  ;; unify list, remove nil elements
	  (while result
	    (let ((car (car result)))
	      (when car (add-to-list 'result1 car))
	      (setq result (cdr result))))

	  ;; Complete local parts
	  (append
	   result1
	   (condition-case nil
	       (if result1
		   ;; "/ssh:" does not need to be expanded as hostname.
		   (tramp-run-real-handler
		    'file-name-all-completions (list filename directory))
		 ;; No method/user/host found to be expanded.
		 (tramp-completion-run-real-handler
		  'file-name-all-completions (list filename directory)))
	     (error nil)))))
    ;; unwindform
    (setq tramp-completion-mode nil)))

;; Method, host name and user name completion for a file.
(defun tramp-completion-handle-file-name-completion (filename directory)
  "Like `file-name-completion' for tramp files."
  (try-completion filename
   (mapcar 'list (file-name-all-completions filename directory))))

;; I misuse a little bit the tramp-file-name structure in order to handle
;; completion possibilities for partial methods / user names / host names.
;; Return value is a list of tramp-file-name structures according to possible
;; completions. If "localname" is non-nil it means there
;; shouldn't be a completion anymore.

;; Expected results:

;; "/x" "/[x"           "/x@" "/[x@"         "/x@y" "/[x@y"
;; [nil nil "x" nil]    [nil "x" nil nil]    [nil "x" "y" nil]
;; [nil "x" nil nil]
;; ["x" nil nil nil]

;; "/x:"                "/x:y"               "/x:y:"
;; [nil nil "x" ""]     [nil nil "x" "y"]    ["x" nil "y" ""]
;; "/[x/"               "/[x/y"
;; ["x" nil "" nil]     ["x" nil "y" nil]
;; ["x" "" nil nil]     ["x" "y" nil nil]

;; "/x:y@"              "/x:y@z"             "/x:y@z:"
;; [nil nil "x" "y@"]   [nil nil "x" "y@z"]  ["x" "y" "z" ""]
;; "/[x/y@"             "/[x/y@z"
;; ["x" nil "y" nil]    ["x" "y" "z" nil]
(defun tramp-completion-dissect-file-name (name)
  "Returns a list of `tramp-file-name' structures.
They are collected by `tramp-completion-dissect-file-name1'."

  (let* ((result)
	 (x-nil "\\|\\(\\)")
	 ;; "/method" "/[method"
	 (tramp-completion-file-name-structure1
	  (list (concat tramp-prefix-regexp "\\(" tramp-method-regexp x-nil "\\)$")
		1 nil nil nil))
	 ;; "/user" "/[user"
	 (tramp-completion-file-name-structure2
	  (list (concat tramp-prefix-regexp "\\(" tramp-user-regexp x-nil   "\\)$")
		nil 1 nil nil))
	 ;; "/host" "/[host"
	 (tramp-completion-file-name-structure3
	  (list (concat tramp-prefix-regexp "\\(" tramp-host-regexp x-nil   "\\)$")
		nil nil 1 nil))
	 ;; "/user@host" "/[user@host"
	 (tramp-completion-file-name-structure4
	  (list (concat tramp-prefix-regexp
			"\\(" tramp-user-regexp "\\)"   tramp-postfix-user-regexp
			"\\(" tramp-host-regexp x-nil   "\\)$")
		nil 1 2 nil))
	 ;; "/method:user" "/[method/user" "/method://user"
	 (tramp-completion-file-name-structure5
	  (list (concat tramp-prefix-regexp
			"\\(" tramp-method-regexp "\\)"	tramp-postfix-method-regexp
			"\\(" tramp-user-regexp x-nil   "\\)$")
		1 2 nil nil))
	 ;; "/method:host" "/[method/host" "/method://host"
	 (tramp-completion-file-name-structure6
	  (list (concat tramp-prefix-regexp
			"\\(" tramp-method-regexp "\\)" tramp-postfix-method-regexp
			"\\(" tramp-host-regexp x-nil   "\\)$")
		1 nil 2 nil))
	 ;; "/method:user@host" "/[method/user@host" "/method://user@host"
	 (tramp-completion-file-name-structure7
	  (list (concat tramp-prefix-regexp
			"\\(" tramp-method-regexp "\\)" tramp-postfix-method-regexp
			"\\(" tramp-user-regexp "\\)"   tramp-postfix-user-regexp
			"\\(" tramp-host-regexp x-nil   "\\)$")
		1 2 3 nil))
	 ;; "/method: "/method:/"
	 (tramp-completion-file-name-structure8
	  (list
	   (if (equal tramp-syntax 'url)
	       (concat tramp-prefix-regexp
		       "\\(" tramp-method-regexp "\\)"
		       "\\(" (substring tramp-postfix-method-regexp 0 1)
		       "\\|" (substring tramp-postfix-method-regexp 1 2) "\\)"
		       "\\(" "\\)$")
	     ;; Should not match if not URL syntax.
	     (concat tramp-prefix-regexp "/$"))
	   1 3 nil nil))
	 ;; "/method: "/method:/"
	 (tramp-completion-file-name-structure9
	  (list
	   (if (equal tramp-syntax 'url)
	       (concat tramp-prefix-regexp
		       "\\(" tramp-method-regexp "\\)"
		       "\\(" (substring tramp-postfix-method-regexp 0 1)
		       "\\|" (substring tramp-postfix-method-regexp 1 2) "\\)"
		       "\\(" "\\)$")
	     ;; Should not match if not URL syntax.
	     (concat tramp-prefix-regexp "/$"))
	   1 nil 3 nil)))

    (mapcar (lambda (regexp)
      (add-to-list 'result
	(tramp-completion-dissect-file-name1 regexp name)))
      (list
       tramp-completion-file-name-structure1
       tramp-completion-file-name-structure2
       tramp-completion-file-name-structure3
       tramp-completion-file-name-structure4
       tramp-completion-file-name-structure5
       tramp-completion-file-name-structure6
       tramp-completion-file-name-structure7
       tramp-completion-file-name-structure8
       tramp-completion-file-name-structure9
       tramp-file-name-structure))

    (delq nil result)))

(defun tramp-completion-dissect-file-name1 (structure name)
  "Returns a `tramp-file-name' structure matching STRUCTURE.
The structure consists of remote method, remote user,
remote host and localname (filename on remote host)."

  (save-match-data
    (when (string-match (nth 0 structure) name)
      (let ((method    (and (nth 1 structure)
			    (match-string (nth 1 structure) name)))
	    (user      (and (nth 2 structure)
			    (match-string (nth 2 structure) name)))
	    (host      (and (nth 3 structure)
			    (match-string (nth 3 structure) name)))
	    (localname (and (nth 4 structure)
			    (match-string (nth 4 structure) name))))
	(make-tramp-file-name
	 :method method
	 :user user
	 :host host
	 :localname localname)))))

;; This function returns all possible method completions, adding the
;; trailing method delimeter.
(defun tramp-get-completion-methods (partial-method)
  "Returns all method completions for PARTIAL-METHOD."
  (mapcar
   (lambda (method)
     (and method
	  (string-match (concat "^" (regexp-quote partial-method)) method)
	  ;; we must remove leading "/".
	  (substring (tramp-make-tramp-file-name method nil nil nil) 1)))
   (mapcar 'car tramp-methods)))

;; Compares partial user and host names with possible completions.
(defun tramp-get-completion-user-host (method partial-user partial-host user host)
  "Returns the most expanded string for user and host name completion.
PARTIAL-USER must match USER, PARTIAL-HOST must match HOST."
  (cond

   ((and partial-user partial-host)
    (if	(and host
	     (string-match (concat "^" (regexp-quote partial-host)) host)
	     (string-equal partial-user (or user partial-user)))
	(setq user partial-user)
      (setq user nil
	    host nil)))

   (partial-user
    (setq host nil)
    (unless
	(and user (string-match (concat "^" (regexp-quote partial-user)) user))
      (setq user nil)))

   (partial-host
    (setq user nil)
    (unless
	(and host (string-match (concat "^" (regexp-quote partial-host)) host))
      (setq host nil)))

   (t (setq user nil
	    host nil)))

  (unless (zerop (+ (length user) (length host)))
    ;; we must remove leading "/".
    (substring (tramp-make-tramp-file-name method user host nil) 1)))

(defun tramp-parse-rhosts (filename)
  "Return a list of (user host) tuples allowed to access.
Either user or host may be nil."

  (let (res)
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	(while (not (eobp))
	  (push (tramp-parse-rhosts-group) res))))
    res))

(defun tramp-parse-rhosts-group ()
   "Return a (user host) tuple allowed to access.
Either user or host may be nil."

   (let ((result)
	 (regexp
	  (concat
	   "^\\(" tramp-host-regexp "\\)"
	   "\\([ \t]+" "\\(" tramp-user-regexp "\\)" "\\)?")))

     (narrow-to-region (point) (tramp-line-end-position))
     (when (re-search-forward regexp nil t)
       (setq result (append (list (match-string 3) (match-string 1)))))
     (widen)
     (forward-line 1)
     result))

(defun tramp-parse-shosts (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."

  (let (res)
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	(while (not (eobp))
	  (push (tramp-parse-shosts-group) res))))
    res))

(defun tramp-parse-shosts-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."

   (let ((result)
	 (regexp (concat "^\\(" tramp-host-regexp "\\)")))

     (narrow-to-region (point) (tramp-line-end-position))
     (when (re-search-forward regexp nil t)
       (setq result (list nil (match-string 1))))
     (widen)
     (or
      (> (skip-chars-forward ",") 0)
      (forward-line 1))
     result))

(defun tramp-parse-sconfig (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."

  (let (res)
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	(while (not (eobp))
	  (push (tramp-parse-sconfig-group) res))))
    res))

(defun tramp-parse-sconfig-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."

   (let ((result)
	 (regexp (concat "^[ \t]*Host[ \t]+" "\\(" tramp-host-regexp "\\)")))

     (narrow-to-region (point) (tramp-line-end-position))
     (when (re-search-forward regexp nil t)
       (setq result (list nil (match-string 1))))
     (widen)
     (or
      (> (skip-chars-forward ",") 0)
      (forward-line 1))
     result))

(defun tramp-parse-shostkeys (dirname)
  "Return a list of (user host) tuples allowed to access.
User is always nil."

  (let ((regexp (concat "^key_[0-9]+_\\(" tramp-host-regexp "\\)\\.pub$"))
	(files (when (file-directory-p dirname) (directory-files dirname)))
	result)

    (while files
      (when (string-match regexp (car files))
	(push (list nil (match-string 1 (car files))) result))
      (setq files (cdr files)))
    result))

(defun tramp-parse-sknownhosts (dirname)
  "Return a list of (user host) tuples allowed to access.
User is always nil."

  (let ((regexp (concat "^\\(" tramp-host-regexp
			"\\)\\.ssh-\\(dss\\|rsa\\)\\.pub$"))
	(files (when (file-directory-p dirname) (directory-files dirname)))
	result)

    (while files
      (when (string-match regexp (car files))
	(push (list nil (match-string 1 (car files))) result))
      (setq files (cdr files)))
    result))

(defun tramp-parse-hosts (filename)
  "Return a list of (user host) tuples allowed to access.
User is always nil."

  (let (res)
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	(while (not (eobp))
	  (push (tramp-parse-hosts-group) res))))
    res))

(defun tramp-parse-hosts-group ()
   "Return a (user host) tuple allowed to access.
User is always nil."

   (let ((result)
	 (regexp (concat "^\\(" tramp-host-regexp "\\)")))

     (narrow-to-region (point) (tramp-line-end-position))
     (when (re-search-forward regexp nil t)
       (unless (char-equal (or (char-after) ?\n) ?:) ; no IPv6
	 (setq result (list nil (match-string 1)))))
     (widen)
     (or
      (> (skip-chars-forward " \t") 0)
      (forward-line 1))
     result))

;; For su-alike methods it would be desirable to return "root@localhost"
;; as default.  Unfortunately, we have no information whether any user name
;; has been typed already.  So we (mis-)use tramp-current-user as indication,
;; assuming it is set in `tramp-completion-handle-file-name-all-completions'.
(defun tramp-parse-passwd (filename)
  "Return a list of (user host) tuples allowed to access.
Host is always \"localhost\"."

  (let (res)
    (if (zerop (length tramp-current-user))
	'(("root" nil))
      (when (file-readable-p filename)
	(with-temp-buffer
	  (insert-file-contents filename)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (push (tramp-parse-passwd-group) res))))
      res)))

(defun tramp-parse-passwd-group ()
   "Return a (user host) tuple allowed to access.
Host is always \"localhost\"."

   (let ((result)
	 (regexp (concat "^\\(" tramp-user-regexp "\\):")))

     (narrow-to-region (point) (tramp-line-end-position))
     (when (re-search-forward regexp nil t)
       (setq result (list (match-string 1) "localhost")))
     (widen)
     (forward-line 1)
     result))

(defun tramp-parse-netrc (filename)
  "Return a list of (user host) tuples allowed to access.
User may be nil."

  (let (res)
    (when (file-readable-p filename)
      (with-temp-buffer
	(insert-file-contents filename)
	(goto-char (point-min))
	(while (not (eobp))
	  (push (tramp-parse-netrc-group) res))))
    res))

(defun tramp-parse-netrc-group ()
   "Return a (user host) tuple allowed to access.
User may be nil."

   (let ((result)
	 (regexp
	  (concat
	   "^[ \t]*machine[ \t]+" "\\(" tramp-host-regexp "\\)"
	   "\\([ \t]+login[ \t]+" "\\(" tramp-user-regexp "\\)" "\\)?")))

     (narrow-to-region (point) (tramp-line-end-position))
     (when (re-search-forward regexp nil t)
       (setq result (list (match-string 3) (match-string 1))))
     (widen)
     (forward-line 1)
     result))

(defun tramp-completion-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for tramp files."
  (let ((fullname (concat (or dir default-directory) name)))
    (if (tramp-completion-mode fullname)
	(tramp-run-real-handler
	 'expand-file-name (list name dir))
      (tramp-completion-run-real-handler
       'expand-file-name (list name dir)))))

;;; Internal Functions:

(defun tramp-maybe-send-script (method user host script name)
  "Define in remote shell function NAME implemented as SCRIPT.
Only send the definition if it has not already been done."
  (let ((scripts (tramp-get-connection-property
		  "scripts" nil method user host)))
    (unless (memq name scripts)
      (with-current-buffer (tramp-get-buffer method user host)
	(tramp-message 5 "Sending script `%s'..." name)
	;; The script could contain a call of Perl.  This is masked with `%s'.
	(tramp-send-command-and-check
	 method user host
	 (format "%s () {\n%s\n}" name
		 (format script (tramp-get-remote-perl method user host))))
	(tramp-set-connection-property
	 "scripts" (cons name scripts) method user host)
	(tramp-message 5 "Sending script `%s'...done." name)))))

(defun tramp-set-auto-save ()
  (when (and ;; ange-ftp has its own auto-save mechanism
	     (eq (tramp-find-foreign-file-name-handler (buffer-file-name))
		 'tramp-sh-file-name-handler)
             auto-save-default)
    (auto-save-mode 1)))
(add-hook 'find-file-hooks 'tramp-set-auto-save t)

(defun tramp-run-test (switch filename)
  "Run `test' on the remote system, given a SWITCH and a FILENAME.
Returns the exit code of the `test' program."
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (tramp-send-command-and-check
       method user host
       (format "%s %s %s"
	       (tramp-get-test-command method user host)
	       switch
               (tramp-shell-quote-argument localname))))))

(defun tramp-run-test2 (format-string file1 file2)
  "Run `test'-like program on the remote system, given FILE1, FILE2.
FORMAT-STRING contains the program name, switches, and place holders.
Returns the exit code of the `test' program.  Barfs if the methods,
hosts, or files, disagree."
  (unless (tramp-equal-remote file1 file2)
    (with-parsed-tramp-file-name (if (tramp-tramp-file-p file1) file1 file2) nil
      (tramp-error
       method user host 'file-error
       "tramp-run-test2 only implemented for same method, user, host")))
  (let* ((v1 (tramp-dissect-file-name file1))
         (v2 (tramp-dissect-file-name file2))
         (method1 (tramp-file-name-method v1))
         (method2 (tramp-file-name-method v2))
         (user1 (tramp-file-name-user v1))
         (user2 (tramp-file-name-user v2))
         (host1 (tramp-file-name-host v1))
         (host2 (tramp-file-name-host v2))
         (localname1 (tramp-file-name-localname v1))
         (localname2 (tramp-file-name-localname v2)))
    (save-excursion
      (tramp-send-command-and-check
       method1 user1 host1
       (format format-string
	       (tramp-shell-quote-argument localname1)
               (tramp-shell-quote-argument localname2))))))

(defun tramp-touch (file time)
  "Set the last-modified timestamp of the given file.
TIME is an Emacs internal time value as returned by `current-time'."
  (let ((touch-time (format-time-string "%Y%m%d%H%M.%S" time)))
    (if (tramp-tramp-file-p file)
	(with-parsed-tramp-file-name file nil
	  (let ((buf (tramp-get-buffer method user host)))
	    (unless (zerop (tramp-send-command-and-check
			    method user host
			    (format "touch -t %s %s"
				    touch-time
				    localname)))
	      (pop-to-buffer buf)
	      (tramp-error
	       method user host 'file-error
	       "touch failed, see buffer `%s' for details"
	       buf))))
      ;; It's a local file
      (with-temp-buffer
	(unless (zerop (call-process
			"touch" nil (current-buffer) nil "-t" touch-time file))
	      (pop-to-buffer (current-buffer))
	      (signal 'file-error "touch failed"))))))

(defun tramp-buffer-name (method user host)
  "A name for the connection buffer for USER at HOST using METHOD."
  (let ((lmethod (tramp-find-method method user host))
	(luser   (tramp-find-user   method user host))
	(lhost   (tramp-find-host   method user host)))
    (if luser
	(format "*tramp/%s %s@%s*" lmethod luser lhost)
      (format "*tramp/%s %s*" lmethod lhost))))

(defun tramp-get-buffer (method user host)
  "Get the connection buffer to be used for USER at HOST using METHOD."
  (with-current-buffer
      (get-buffer-create (tramp-buffer-name method user host))
    (setq buffer-undo-list t)
    (current-buffer)))

(defun tramp-get-connection-buffer (method user host)
  "Get the connection buffer to be used for USER at HOST using METHOD.
In case a second asynchronous communication has been started, it is different
from `tramp-get-buffer'."
  (or (tramp-get-connection-property "process-buffer" nil method user host)
      (tramp-get-buffer method user host)))

(defun tramp-get-connection-process (method user host)
  "Get the connection process to be used for USER at HOST using METHOD.
In case a second asynchronous communication has been started, it is different
from default one."
  (get-process
   (or (tramp-get-connection-property "process-name" nil method user host)
       (tramp-buffer-name method user host))))

(defun tramp-debug-buffer-name (method user host)
  "A name for the debug buffer for USER at HOST using METHOD."
  (let ((lmethod (tramp-find-method method user host))
	(luser   (tramp-find-user   method user host))
	(lhost   (tramp-find-host   method user host)))
    (if luser
	(format "*debug tramp/%s %s@%s*" lmethod luser lhost)
      (format "*debug tramp/%s %s*" lmethod lhost))))

(defun tramp-get-debug-buffer (method user host)
  "Get the debug buffer for USER at HOST using METHOD."
  (with-current-buffer
      (get-buffer-create (tramp-debug-buffer-name method user host))
    (when (bobp)
      (setq buffer-undo-list t)
      ;; Activate outline-mode
      (make-local-variable 'outline-regexp)
      (make-local-variable 'outline-level)
      (outline-mode)
      (setq outline-regexp "[0-9]+:[0-9]+:[0-9]+ [a-z0-9-]+ (\\([0-9]+\\)) #")
      (setq outline-level 'tramp-outline-level))
    (current-buffer)))

(defun tramp-outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.

The outline level is equal to the verbosity of the Tramp message."
  (1+ (string-to-number (match-string 1))))

(defun tramp-find-executable
  (method user host progname dirlist &optional ignore-tilde ignore-path)
  "Searches for PROGNAME in $PATH and all directories mentioned in DIRLIST.
First args METHOD, USER and HOST specify the connection, PROGNAME
is the program to search for, and DIRLIST gives the list of directories
to search.  If IGNORE-TILDE is non-nil, directory names starting
with `~' will be ignored. If IGNORE-PATH is non-nil, searches only
in DIRLIST.

Returns the absolute file name of PROGNAME, if found, and nil otherwise.

This function expects to be in the right *tramp* buffer."
  (let (result)
    ;; Check whether the executable is in $PATH. "which(1)" does not
    ;; report always a correct error code; therefore we check the
    ;; number of words it returns.
    (unless ignore-path
      (tramp-send-command
       method user host (format "which \\%s | wc -w" progname))
      (goto-char (point-min))
      (if (looking-at "^1$")
	(setq result (concat "\\" progname))))
    (unless result
      (when ignore-tilde
	;; Remove all ~/foo directories from dirlist.  In Emacs 20,
	;; `remove' is in CL, and we want to avoid CL dependencies.
	(let (newdl d)
	  (while dirlist
	    (setq d (car dirlist))
	    (setq dirlist (cdr dirlist))
	    (unless (char-equal ?~ (aref d 0))
	      (setq newdl (cons d newdl))))
	  (setq dirlist (nreverse newdl))))
      (tramp-send-command
       method user host
       (format (concat "while read d; "
		       "do if test -x $d/%s -a -f $d/%s; "
		       "then echo tramp_executable $d/%s; "
		       "break; fi; done <<'EOF'\n"
		       "%s\nEOF")
	       progname progname progname (mapconcat 'identity dirlist "\n")))
      (goto-char (point-max))
      (when (search-backward "tramp_executable " nil t)
	(skip-chars-forward "^ ")
	(skip-chars-forward " ")
	(setq result (buffer-substring (point) (tramp-line-end-position)))))
    result))

(defun tramp-set-remote-path (method user host var dirlist)
  "Sets the remote environment VAR to existing directories from DIRLIST.
I.e., for each directory in DIRLIST, it is tested whether it exists and if
so, it is added to the environment variable VAR.

Returns the list of existing directories."
  (tramp-message 5 (format "Setting $%s environment variable" var))
  (let ((existing-dirs
	 (delq
	  nil
	  (mapcar
	   (lambda (x)
	     (and (file-directory-p
		   (tramp-make-tramp-file-name method user host x))
		  x))
	   dirlist))))
    (tramp-send-command
     method user host
     (format "%s=%s; export %s"
	     var (mapconcat 'identity existing-dirs ":") var))
    existing-dirs))

;; -- communication with external shell --

(defun tramp-find-file-exists-command (method user host)
  "Find a command on the remote host for checking if a file exists.
Here, we are looking for a command which has zero exit status if the
file exists and nonzero exit status otherwise."
  (let ((existing "/")
        (nonexisting
	 (tramp-shell-quote-argument "/ this file does not exist "))
	result)
    ;; The algorithm is as follows: we try a list of several commands.
    ;; For each command, we first run `$cmd /' -- this should return
    ;; true, as the root directory always exists.  And then we run
    ;; `$cmd /this\ file\ does\ not\ exist ', hoping that the file indeed
    ;; does not exist.  This should return false.  We use the first
    ;; command we find that seems to work.
    ;; The list of commands to try is as follows:
    ;; `ls -d'          This works on most systems, but NetBSD 1.4
    ;;                  has a bug: `ls' always returns zero exit
    ;;                  status, even for files which don't exist.
    ;; `test -e'        Some Bourne shells have a `test' builtin
    ;;                  which does not know the `-e' option.
    ;; `/bin/test -e'   For those, the `test' binary on disk normally
    ;;                  provides the option.  Alas, the binary
    ;;                  is sometimes `/bin/test' and sometimes it's
    ;;                  `/usr/bin/test'.
    ;; `/usr/bin/test -e'       In case `/bin/test' does not exist.
    (unless (or
             (and (setq result
			(format "%s -e"
				(tramp-get-test-command method user host)))
		  (zerop (tramp-send-command-and-check
			  method user host
			  (format "%s %s" result existing)))
                  (not (zerop (tramp-send-command-and-check
			       method user host
			       (format "%s %s" result nonexisting)))))
             (and (setq result "/bin/test -e")
		  (zerop (tramp-send-command-and-check
			  method user host
			  (format "%s %s" result existing)))
                  (not (zerop (tramp-send-command-and-check
			       method user host
			       (format "%s %s" result nonexisting)))))
             (and (setq result "/usr/bin/test -e")
		  (zerop (tramp-send-command-and-check
			  method user host
			  (format "%s %s" result existing)))
                  (not (zerop (tramp-send-command-and-check
			       method user host
			       (format "%s %s" result nonexisting)))))
             (and (setq result
			(format "%s -d"
				(tramp-get-ls-command method user host)))
		  (zerop (tramp-send-command-and-check
			  method user host
			  (format "%s %s" result existing)))
                  (not (zerop (tramp-send-command-and-check
			       method user host
			       (format "%s %s" result nonexisting))))))
      (tramp-error
       method user host 'file-error
       "Couldn't find command to check if file exists"))
    result))


;; CCC test ksh or bash found for tilde expansion?
(defun tramp-find-shell (method user host)
  "Find a shell on the remote host which groks tilde expansion."
  (let ((shell nil))
    (tramp-send-command method user host "echo ~root")
    (cond
     ((string-match "^~root$" (buffer-string))
      (setq shell
	    ;; We don't need to change the buffer; `tramp-remote-path'
	    ;; is still global.
            (or (tramp-find-executable
		 method user host "bash" tramp-remote-path t)
                (tramp-find-executable
		 method user host "ksh" tramp-remote-path t)))
      (unless shell
        (tramp-error
	 method user host 'file-error
	 "Couldn't find a shell which groks tilde expansion"))
      ;; Find arguments for this shell.
      (let ((alist tramp-sh-extra-args)
	    item extra-args)
	(while (and alist (null extra-args))
	  (setq item (pop alist))
	  (when (string-match (car item) shell)
	    (setq extra-args (cdr item))))
	(when extra-args (setq shell (concat shell " " extra-args))))
      (tramp-message
       5 "Starting remote shell `%s' for tilde expansion..." shell)
      (tramp-send-command-internal
       method user host (concat "PS1='$ ' exec " shell))
      (tramp-message
       5 "Setting remote shell prompt...")
      ;; Douglas Gray Stephens <DGrayStephens@slb.com> says that we
      ;; must use "\n" here, not tramp-rsh-end-of-line.  Kai left the
      ;; last tramp-rsh-end-of-line, Douglas wanted to replace that,
      ;; as well.
      (tramp-send-command
       method user host
       (format "PS1='%s%s%s'; PS2=''; PS3=''"
	       tramp-rsh-end-of-line
	       tramp-end-of-output
	       tramp-rsh-end-of-line))
      (tramp-message
       5 "Setting remote shell prompt...done")
      )
     (t (tramp-message
	 5 "Remote `%s' groks tilde expansion, good"
	 (tramp-get-method-parameter method user host 'tramp-remote-sh))))))

;; ------------------------------------------------------------
;; -- Functions for establishing connection --
;; ------------------------------------------------------------

;; The following functions are actions to be taken when seeing certain
;; prompts from the remote host.  See the variable
;; `tramp-actions-before-shell' for usage of these functions.

(defun tramp-action-login (p method user host)
  "Send the login name."
  (tramp-message 3 "Sending login name `%s'" tramp-current-hop-user)
  (tramp-send-string method user host tramp-current-hop-user))

(defun tramp-action-password (p method user host)
  "Query the user for a password."
  (tramp-message 3 "Sending password")
  (tramp-enter-password p))

(defun tramp-action-succeed (p method user host)
  "Signal success in finding shell prompt."
  (throw 'tramp-action 'ok))

(defun tramp-action-permission-denied (p method user host)
  "Signal permission denied."
  (pop-to-buffer (tramp-get-connection-buffer method user host))
  (kill-process p)
  (throw 'tramp-action 'permission-denied))

(defun tramp-action-yesno (p method user host)
  "Ask the user for confirmation using `yes-or-no-p'.
Send \"yes\" to remote process on confirmation, abort otherwise.
See also `tramp-action-yn'."
  (save-window-excursion
    (pop-to-buffer (tramp-get-connection-buffer method user host))
    (unless (yes-or-no-p (match-string 0))
      (kill-process p)
      (throw 'tramp-action 'permission-denied))
    (tramp-send-string method user host "yes")))

(defun tramp-action-yn (p method user host)
  "Ask the user for confirmation using `y-or-n-p'.
Send \"y\" to remote process on confirmation, abort otherwise.
See also `tramp-action-yesno'."
  (save-window-excursion
    (pop-to-buffer (tramp-get-connection-buffer method user host))
    (unless (y-or-n-p (match-string 0))
      (kill-process p)
      (throw 'tramp-action 'permission-denied))
    (tramp-send-string method user host "y")))

(defun tramp-action-terminal (p method user host)
  "Tell the remote host which terminal type to use.
The terminal type can be configured with `tramp-terminal-type'."
  (tramp-message
   5 "Setting `%s' as terminal type." tramp-terminal-type)
  (tramp-send-string method user host tramp-terminal-type))

(defun tramp-action-process-alive (p method user host)
  "Check whether a process has finished."
  (unless (memq (process-status p) '(run open))
    (throw 'tramp-action 'process-died)))

(defun tramp-action-out-of-band (p method user host)
  "Check whether an out-of-band copy has finished."
  (cond ((and (memq (process-status p) '(stop exit))
	      (zerop (process-exit-status p)))
	 (tramp-message 3 "Process has finished.")
	 (throw 'tramp-action 'ok))
	((or (and (memq (process-status p) '(stop exit))
		  (not (zerop (process-exit-status p))))
	     (memq (process-status p) '(signal)))
	 ;; `scp' could have copied correctly, but set modes could have failed.
	 ;; This can be ignored.
	 (goto-char (point-min))
	 (if (re-search-forward tramp-operation-not-permitted-regexp nil t)
	     (progn
	       (tramp-message 5 "'set mode' error ignored.")
	       (tramp-message 3 "Process has finished.")
	       (throw 'tramp-action 'ok))
	   (goto-char (point-min))
	   (when (re-search-forward
		  "^.cp.?: \\(.+: Permission denied.?\\)$" nil t)
	     (tramp-error
	      method user host 'file-error "Remote host: %s" (match-string 1)))
	   (tramp-message 3 "Process has died.")
	   (throw 'tramp-action 'process-died)))
	(t nil)))

;; Functions for processing the actions.

(defun tramp-process-one-action (p method user host actions)
  "Wait for output from the shell and perform one action."
  (let (found item pattern action todo)
    (tramp-message
     3 "Waiting 60s for prompt from remote shell on host %s" host)
    (with-timeout (60 (throw 'tramp-action 'timeout))
      (while (not found)
	;; Reread output once all actions have been performed.
	;; Obviously, the output was not complete.
	(tramp-accept-process-output p 1)
	(with-current-buffer (process-buffer p) (goto-char (point-min)))
	(setq todo actions)
	(while todo
	  (with-current-buffer (process-buffer p) (goto-char (point-min)))
	  (setq item (pop todo))
	  (setq pattern (symbol-value (nth 0 item)))
	  (setq action (nth 1 item))
	  (tramp-message
	   5 "Looking for regexp \"%s\" from remote shell" pattern)
	  (when (with-current-buffer (process-buffer p)
		  (re-search-forward (concat pattern "\\'") nil t))
	    (save-match-data
	      (tramp-message 5 "Call `%s'" (symbol-name action)))
	    (setq found (funcall action p method user host)))))
      found)))

(defun tramp-process-actions (p method user host actions)
  "Perform actions until success."
  (let (exit)
    (while (not exit)
      (tramp-message 3 "Waiting for prompts from remote shell")
      (setq exit
	    (catch 'tramp-action
	      (tramp-process-one-action p method user host actions)
	      nil)))
    (tramp-message 9 "\n%s" (buffer-string))
    (unless (eq exit 'ok)
      (tramp-clear-passwd
       tramp-current-hop-method
       tramp-current-hop-user
       tramp-current-hop-host)
      (tramp-error method user host 'file-error "Login failed"))))

;; Functions to execute when we have seen the remote shell prompt but
;; before we exec the Bourne-ish shell.  Note that these commands
;; might be sent to any shell, not just a Bourne-ish shell.  This
;; means that the commands need to work in all shells.  (It is also
;; okay for some commands to just fail with an error message, but
;; please make sure that they at least don't crash the odd shell people
;; might be running...)
(defun tramp-process-initial-commands (p method user host commands)
  "Send list of commands to remote host, in order."
  (let (cmd)
    (while commands
      (setq cmd (pop commands))
      (tramp-message 5 "Sending command to remote shell: %s" cmd)
      (tramp-send-command-internal method user host cmd))))

;; Utility functions.

(defun tramp-accept-process-output
  (&optional process timeout timeout-msecs)
  "Like `accept-process-output' for Tramp processes.
This is needed in order to hide `last-coding-system-used', which is set
for process communication also."
  (tramp-message 10 "%s %s" process (process-status process))
  (with-current-buffer (process-buffer process)
    (let (buffer-read-only last-coding-system-used)
      (accept-process-output process timeout timeout-msecs)))
  (tramp-trace
   "\n%s" (with-current-buffer (process-buffer process) (buffer-string))))

(defun tramp-wait-for-regexp (proc timeout regexp)
  "Wait for a REGEXP to appear from process PROC within TIMEOUT seconds.
Expects the output of PROC to be sent to the current buffer.  Returns
the string that matched, or nil.  Waits indefinitely if TIMEOUT is
nil."
  (let ((found
	 (with-current-buffer (process-buffer proc)
	   (goto-char (point-min))
	   (re-search-forward regexp nil t)))
	(start-time (current-time)))
    (cond (timeout
           ;; Work around a bug in XEmacs 21, where the timeout
           ;; expires faster than it should.  This degenerates
           ;; to polling for buggy XEmacsen, but oh, well.
           (while (and (not found)
                       (< (tramp-time-diff (current-time) start-time)
                          timeout))
             (with-timeout (timeout)
               (while (not found)
                 (tramp-accept-process-output proc 1)
		 (unless (memq (process-status proc) '(run open))
		   (tramp-error
		    tramp-current-method tramp-current-user tramp-current-host
		    'file-error "Process has died"))
		 (with-current-buffer (process-buffer proc)
		   (goto-char (point-min))
		   (setq found (re-search-forward regexp nil t)))))))
	  (t
	   (while (not found)
	     (tramp-accept-process-output proc 1)
	     (unless (memq (process-status proc) '(run open))
	       (tramp-error
		tramp-current-method tramp-current-user tramp-current-host
		'file-error "Process has died"))
	     (with-current-buffer (process-buffer proc)
	       (goto-char (point-min))
	       (setq found (re-search-forward regexp nil t))))))
    (tramp-message
     9 "\n%s" (with-current-buffer (process-buffer proc) (buffer-string)))
    (when (not found)
      (if timeout
	  (tramp-error
	   tramp-current-method tramp-current-user tramp-current-host
	   'file-error "[[Regexp `%s' not found in %d secs]]"
	   regexp timeout)
	(tramp-error
	 tramp-current-method tramp-current-user tramp-current-host
	 'file-error "[[Regexp `%s' not found]]" regexp)))
    found))

(defun tramp-wait-for-shell-prompt (proc timeout)
  "Wait for the shell prompt to appear from process PROC within TIMEOUT seconds.
See `tramp-wait-for-regexp' for more details.
Shell prompt pattern is determined by variables `shell-prompt-pattern'
and `tramp-shell-prompt-pattern'."
  (tramp-wait-for-regexp
   proc timeout
   (format "\\(%s\\|%s\\)\\'"
	   shell-prompt-pattern tramp-shell-prompt-pattern)))

(defun tramp-barf-if-no-shell-prompt (proc timeout &rest error-args)
  "Wait for shell prompt and barf if none appears.
Looks at process PROC to see if a shell prompt appears in TIMEOUT
seconds.  If not, it produces an error message with the given ERROR-ARGS."
  (unless (tramp-wait-for-shell-prompt proc timeout)
    (pop-to-buffer (buffer-name))
    (apply 'error error-args)))

;; We don't call `tramp-send-string' in order to hide the password from the
;; debug buffer, and because end-of-line handling of the string
(defun tramp-enter-password (p)
  "Prompt for a password and send it to the remote end."
  (process-send-string
   p (concat (tramp-read-passwd)
	     (or (tramp-get-method-parameter
		  tramp-current-hop-method
		  tramp-current-hop-user
		  tramp-current-hop-host
		  'tramp-password-end-of-line)
		 tramp-default-password-end-of-line))))

;; HHH: Not Changed.  This might handle the case where USER is not
;;      given in the "File name" very poorly.  Then, the local
;;      variable tramp-current-user will be set to nil.
(defun tramp-pre-connection (method user host chunksize)
  "Do some setup before actually logging in.
METHOD, USER and HOST specify the connection."
  (set-buffer (tramp-get-buffer method user host))
  (set (make-local-variable 'tramp-current-method) method)
  (set (make-local-variable 'tramp-current-user)   user)
  (set (make-local-variable 'tramp-current-host)   host)
  (set (make-local-variable 'tramp-current-hop-method) method)
  (set (make-local-variable 'tramp-current-hop-user)   user)
  (set (make-local-variable 'tramp-current-hop-host)   host)
  (set (make-local-variable 'tramp-chunksize) chunksize)
  (set (make-local-variable 'inhibit-eol-conversion) nil)
  (tramp-cache-setup method user host))

(defun tramp-open-connection-setup-interactive-shell (p method user host)
  "Set up an interactive shell.
Mainly sets the prompt and the echo correctly.  P is the shell process
to set up.  METHOD, USER and HOST specify the connection."
  ;; Wait a bit in case the remote end feels like sending a little
  ;; junk first.  It seems that fencepost.gnu.org does this when doing
  ;; a Kerberos login.
  (tramp-process-initial-commands p method user host tramp-initial-commands)
  ;; It is useful to set the prompt in the following command because
  ;; some people have a setting for $PS1 which /bin/sh doesn't know
  ;; about and thus /bin/sh will display a strange prompt.  For
  ;; example, if $PS1 has "${CWD}" in the value, then ksh will display
  ;; the current working directory but /bin/sh will display a dollar
  ;; sign.  The following command line sets $PS1 to a sane value, and
  ;; works under Bourne-ish shells as well as csh-like shells.  Daniel
  ;; Pittman reports that the unusual positioning of the single quotes
  ;; makes it work under `rc', too.  We also unset the variable $ENV
  ;; because that is read by some sh implementations (eg, bash when
  ;; called as sh) on startup; this way, we avoid the startup file
  ;; clobbering $PS1.
  (tramp-send-command-internal
   method user host
   (format "exec env 'ENV=' 'PS1=$ ' %s"
	   (tramp-get-method-parameter method user host 'tramp-remote-sh))
   (format "remote `%s' to come up"
	   (tramp-get-method-parameter method user host 'tramp-remote-sh)))
  (tramp-message 5 "Setting up remote shell environment")
  (tramp-send-command-internal method user host "stty -inlcr -echo kill '^U'")
  ;; Ignore garbage after stty command.
  (tramp-send-command-internal method user host "echo foo")
  ;; Check whether the remote host suffers from buggy `send-process-string'.
  ;; This is known for FreeBSD (see comment in `send_process', file process.c).
  ;; I've tested sending 624 bytes successfully, sending 625 bytes failed.
  ;; Emacs makes a hack when this host type is detected locally.  It cannot
  ;; handle remote hosts, though.
  (when (or (null tramp-chunksize) (zerop tramp-chunksize))
    (tramp-message 5 "Checking remote host type for `send-process-string' bug")
    (tramp-send-command-internal method user host "(uname -sr) 2>/dev/null")
    (with-current-buffer (process-buffer p)
      (goto-char (point-min))
      (when (looking-at "FreeBSD")
	(tramp-message 5 "Set `tramp-chunksize' to 500")
	(setq tramp-chunksize 500))))

  ;; Try to set up the coding system correctly.
  ;; CCC this can't be the right way to do it.  Hm.
  (save-excursion
    (tramp-message 5 "Determining coding system")
    (tramp-send-command-internal method user host "echo foo ; echo bar")
    (if (featurep 'mule)
	(with-current-buffer (process-buffer p)
	  (goto-char (point-min))
	  ;; Use MULE to select the right EOL convention for communicating
	  ;; with the process.
	  (let* ((cs (or (process-coding-system p) (cons 'undecided 'undecided)))
		 cs-decode cs-encode)
	    (when (symbolp cs) (setq cs (cons cs cs)))
	    (setq cs-decode (car cs))
	    (setq cs-encode (cdr cs))
	    (unless cs-decode (setq cs-decode 'undecided))
	    (unless cs-encode (setq cs-encode 'undecided))
	    (setq cs-encode (tramp-coding-system-change-eol-conversion
			     cs-encode 'unix))
	    (when (search-forward "\r" nil t)
	      (setq cs-decode (tramp-coding-system-change-eol-conversion
			       cs-decode 'dos)))
	    (set-buffer-process-coding-system cs-decode cs-encode))
	;; Look for ^M and do something useful if found.
	(when (search-forward "\r" nil t)
	  ;; We have found a ^M but cannot frob the process coding system
	  ;; because we're running on a non-MULE Emacs.  Let's try
	  ;; stty, instead.
	  (tramp-send-command-internal method user host "stty -onlcr")))))
  (tramp-message 5 "Waiting 30s for `set +o vi +o emacs'")
  (tramp-send-command-internal method user host "set +o vi +o emacs")
  (tramp-message 5 "Setting shell prompt")
  ;; Douglas Gray Stephens <DGrayStephens@slb.com> says that we must
  ;; use "\n" here, not tramp-rsh-end-of-line.  We also manually frob
  ;; the last time we sent a command, to avoid tramp-send-command to send
  ;; "echo are you awake".
  (setq tramp-last-cmd-time (current-time))
  (tramp-send-command
   method user host
   (format "PS1='%s%s%s'; PS2=''; PS3=''"
	   tramp-rsh-end-of-line
           tramp-end-of-output
	   tramp-rsh-end-of-line)))

(defun tramp-post-connection (method user host)
  "Prepare a remote shell before being able to work on it.
METHOD, USER and HOST specify the connection.
Among other things, this finds a shell which groks tilde expansion,
tries to find an `ls' command which groks the `-n' option, sets the
locale to C and sets up the remote shell search path."
  ;; Search for a good shell before searching for a command which
  ;; checks if a file exists. This is done because Tramp wants to use
  ;; "test foo; echo $?" to check if various conditions hold, and
  ;; there are buggy /bin/sh implementations which don't execute the
  ;; "echo $?"  part if the "test" part has an error.  In particular,
  ;; the Solaris /bin/sh is a problem.  I'm betting that all systems
  ;; with buggy /bin/sh implementations will have a working bash or
  ;; ksh.  Whee...
  (tramp-find-shell method user host)
  ;; Set remote PATH variable.
  (set (make-local-variable 'tramp-remote-path)
       (tramp-set-remote-path method user host "PATH" tramp-remote-path))
  ;; Disable unexpected output.
  (tramp-send-command method user host "mesg n; biff n")
  ;; Set the environment.
  (tramp-message 5 "Setting default environment")
  (let ((env (copy-sequence tramp-remote-process-environment))
	item)
    (while env
      (setq item (split-string (car env) "="))
      (if (and (stringp (cadr item)) (not (string-equal (cadr item) "")))
	  (tramp-send-command
	   method user host
	   (format "%s=%s; export %s" (car item) (cadr item) (car item)))
	(tramp-send-command
	 method user host (format "unset %s" (car item))))
      (setq env (cdr env))))
  ;; Find the right encoding/decoding commands to use.
  (unless (tramp-method-out-of-band-p method user host)
    (tramp-find-inline-encoding method user host)))

;; CCC: We should either implement a Perl version of base64 encoding
;; and decoding.  Then we just use that in the last item.  The other
;; alternative is to use the Perl version of UU encoding.  But then
;; we need a Lisp version of uuencode.
;;
;; Old text from documentation of tramp-methods:
;; Using a uuencode/uudecode inline method is discouraged, please use one
;; of the base64 methods instead since base64 encoding is much more
;; reliable and the commands are more standardized between the different
;; Unix versions.  But if you can't use base64 for some reason, please
;; note that the default uudecode command does not work well for some
;; Unices, in particular AIX and Irix.  For AIX, you might want to use
;; the following command for uudecode:
;;
;;     sed '/^begin/d;/^[` ]$/d;/^end/d' | iconv -f uucode -t ISO8859-1
;;
;; For Irix, no solution is known yet.

(defconst tramp-local-coding-commands
  '((b64 base64-encode-region base64-decode-region)
    (uu  tramp-uuencode-region uudecode-decode-region)
    (pack
     "perl -e 'binmode STDIN; binmode STDOUT; print pack(q{u*}, join q{}, <>)'"
     "perl -e 'binmode STDIN; binmode STDOUT; print unpack(q{u*}, join q{}, <>)'"))
  "List of local coding commands for inline transfer.
Each item is a list that looks like this:

\(FORMAT ENCODING DECODING)

FORMAT is  symbol describing the encoding/decoding format.  It can be
`b64' for base64 encoding, `uu' for uu encoding, or `pack' for simple packing.

ENCODING and DECODING can be strings, giving commands, or symbols,
giving functions.  If they are strings, then they can contain
the \"%s\" format specifier.  If that specifier is present, the input
filename will be put into the command line at that spot.  If the
specifier is not present, the input should be read from standard
input.

If they are functions, they will be called with two arguments, start
and end of region, and are expected to replace the region contents
with the encoded or decoded results, respectively.")

(defconst tramp-remote-coding-commands
  '((b64 "mimencode -b" "mimencode -u -b")
    (b64 "mmencode -b" "mmencode -u -b")
    (b64 "recode data..base64" "recode base64..data")
    (b64 tramp-perl-encode-with-module tramp-perl-decode-with-module)
    (b64 tramp-perl-encode tramp-perl-decode)
    (uu  "uuencode xxx" "uudecode -o /dev/stdout")
    (uu  "uuencode xxx" "uudecode -o -")
    (uu  "uuencode xxx" "uudecode -p")
    (uu  "uuencode xxx" tramp-uudecode)
    (pack
     "perl -e 'binmode STDIN; binmode STDOUT; print pack(q{u*}, join q{}, <>)'"
     "perl -e 'binmode STDIN; binmode STDOUT; print unpack(q{u*}, join q{}, <>)'"))
  "List of remote coding commands for inline transfer.
Each item is a list that looks like this:

\(FORMAT ENCODING DECODING)

FORMAT is  symbol describing the encoding/decoding format.  It can be
`b64' for base64 encoding, `uu' for uu encoding, or `pack' for simple packing.

ENCODING and DECODING can be strings, giving commands, or symbols,
giving variables.  If they are strings, then they can contain
the \"%s\" format specifier.  If that specifier is present, the input
filename will be put into the command line at that spot.  If the
specifier is not present, the input should be read from standard
input.

If they are variables, this variable is a string containing a Perl
implementation for this functionality.  This Perl program will be transferred
to the remote host, and it is avalible as shell function with the same name.")

(defun tramp-find-inline-encoding (method user host)
  "Find an inline transfer encoding that works.
Goes through the list `tramp-local-coding-commands' and
`tramp-remote-coding-commands'."
  (unless
      (and
       (tramp-get-local-encoding method user host)
       (tramp-get-local-decoding method user host)
       (tramp-get-remote-encoding method user host)
       (tramp-get-remote-decoding method user host))
    (let ((local-commands tramp-local-coding-commands)
	  (magic "xyzzy")
	  loc-enc loc-dec rem-enc rem-dec	litem ritem found)
      (while (and local-commands (not found))
	(setq litem (pop local-commands))
	(catch 'wont-work-local
	  (let ((format (nth 0 litem))
		(remote-commands tramp-remote-coding-commands))
	    (setq loc-enc (nth 1 litem))
	    (setq loc-dec (nth 2 litem))
	    ;; If the local encoder or decoder is a string, the
	    ;; corresponding command has to work locally.
	    (if (not (stringp loc-enc))
		(tramp-message
		 5 "Checking local encoding function `%s'" loc-enc)
	      (tramp-message
	       5 "Checking local encoding command `%s' for sanity" loc-enc)
	      (unless (zerop (tramp-call-local-coding-command
			      loc-enc nil nil))
		(throw 'wont-work-local nil)))
	    (if (not (stringp loc-dec))
		(tramp-message
		 5 "Checking local decoding function `%s'" loc-dec)
	      (tramp-message
	       5 "Checking local decoding command `%s' for sanity" loc-dec)
	      (unless (zerop (tramp-call-local-coding-command
			      loc-dec nil nil))
		(throw 'wont-work-local nil)))
	    ;; Search for remote coding commands with the same format
	    (while (and remote-commands (not found))
	      (setq ritem (pop remote-commands))
	      (catch 'wont-work-remote
		(when (equal format (nth 0 ritem))
		  (setq rem-enc (nth 1 ritem))
		  (setq rem-dec (nth 2 ritem))
		  ;; Check if remote encoding and decoding commands can be
		  ;; called remotely with null input and output.  This makes
		  ;; sure there are no syntax errors and the command is really
		  ;; found.  Note that we do not redirect stdout to /dev/null,
		  ;; for two reaons: when checking the decoding command, we
		  ;; actually check the output it gives.  And also, when
		  ;; redirecting "mimencode" output to /dev/null, then as root
		  ;; it might change the permissions of /dev/null!
		  (when (not (stringp rem-enc))
		    (let ((name (symbol-name rem-enc)))
		      (while (string-match (regexp-quote "-") name)
			(setq name (replace-match "_" nil t name)))
		      (tramp-maybe-send-script
		       method user host (symbol-value rem-enc) name)
		      (setq rem-enc name)))
		  (tramp-message
		   5 "Checking remote encoding command `%s' for sanity" rem-enc)
		  (unless (zerop (tramp-send-command-and-check
				  method user host
				  (format "%s </dev/null" rem-enc) t))
		    (throw 'wont-work-remote nil))

		  (when (not (stringp rem-dec))
		    (let ((name (symbol-name rem-dec)))
		      (while (string-match (regexp-quote "-") name)
			(setq name (replace-match "_" nil t name)))
		      (tramp-maybe-send-script
		       method user host (symbol-value rem-dec) name)
		      (setq rem-dec name)))
		  (tramp-message
		   5 "Checking remote decoding command `%s' for sanity" rem-dec)
		  (unless (zerop (tramp-send-command-and-check
				  method user host
				  (format "echo %s | %s | %s"
					  magic rem-enc rem-dec) t))
		    (throw 'wont-work-remote nil))

		  (with-current-buffer
		      (tramp-get-connection-buffer method user host)
		    (goto-char (point-min))
		    (unless (looking-at (regexp-quote magic))
		      (throw 'wont-work-remote nil)))
		  (setq found t)))))))

      ;; Did we find something?  If not, issue error.
      (unless found
	(tramp-error
	 method user host 'file-error
	 "Couldn't find an inline transfer encoding")
	(tramp-kill-process method user host))

      ;; Set connection properties.
      (tramp-message 5 "Using local encoding `%s'" loc-enc)
      (tramp-set-local-encoding method user host loc-enc)
      (tramp-message 5 "Using local decoding `%s'" loc-dec)
      (tramp-set-local-decoding method user host loc-dec)
      (tramp-message 5 "Using remote encoding `%s'" rem-enc)
      (tramp-set-remote-encoding method user host rem-enc)
      (tramp-message 5 "Using remote decoding `%s'" rem-dec)
      (tramp-set-remote-decoding method user host rem-dec))))

(defun tramp-call-local-coding-command (cmd input output)
  "Call the local encoding or decoding command.
If CMD contains \"%s\", provide input file INPUT there in command.
Otherwise, INPUT is passed via standard input.
INPUT can also be nil which means `/dev/null'.
OUTPUT can be a string (which specifies a filename), or t (which
means standard output and thus the current buffer), or nil (which
means discard it)."
  (call-process
   tramp-encoding-shell			;program
   (when (and input (not (string-match "%s" cmd)))
     input)				;input
   (if (eq output t) t nil)		;output
   nil					;redisplay
   tramp-encoding-command-switch
   ;; actual shell command
   (concat
    (if (string-match "%s" cmd) (format cmd input) cmd)
    (if (stringp output) (concat "> " output) ""))))

(defun tramp-maybe-open-connection (method user host)
  "Maybe open a connection to HOST, logging in as USER, using METHOD.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  (let ((p (tramp-get-connection-process method user host))
	(process-environment (copy-sequence process-environment))
	target-alist choices item)

    ;; If too much time has passed since last command was sent, look
    ;; whether process is still alive.  If it isn't, kill it.  When
    ;; using ssh, it can sometimes happen that the remote end has hung
    ;; up but the local ssh client doesn't recognize this until it
    ;; tries to send some data to the remote end.  So that's why we
    ;; try to send a command from time to time, then look again
    ;; whether the process is really alive.
    (with-current-buffer (tramp-get-buffer method user host)
      (when (and tramp-last-cmd-time
		 (> (tramp-time-diff (current-time) tramp-last-cmd-time) 60)
		 p (processp p) (memq (process-status p) '(run open)))
	(tramp-send-command method user host "echo are you awake" t t)
	(unless (tramp-wait-for-output p 10)
	  (delete-process p)
	  (setq p nil))))

    ;; New connection must be opened.
    (unless (and p (processp p) (memq (process-status p) '(run open)))

      ;; Look for proxy hosts to be passed.
      (setq target-alist `((,(tramp-find-method method user host)
			    ,(tramp-find-user   method user host)
			    ,(tramp-find-host   method user host)))
	    choices tramp-default-proxies-alist)
      (while choices
	(setq item (pop choices))
	(when (and
	       ;; host
	       (string-match (or (nth 0 item) "")
			     (or (nth 2 (car target-alist)) ""))
	       ;; user
	       (string-match (or (nth 1 item) "")
			     (or (nth 1 (car target-alist)) "")))
	  (if (null (nth 2 item))
	      ;; No more hops needed.
	      (setq choices nil)
	    (with-parsed-tramp-file-name (nth 2 item) l
	      ;; Add the hop.
	      (add-to-list 'target-alist
			   `(,(tramp-find-method l-method l-user l-host)
			     ,(tramp-find-user   l-method l-user l-host)
			     ,(tramp-find-host   l-method l-user l-host)))
	      ;; Start next search.
	      (setq choices tramp-default-proxies-alist)))))

      ;; Foreign and out-of-band methods are not supported for multi-hops.
      (when (not (null (cdr target-alist)))
	(setq choices target-alist)
	(while choices
	  (setq item (pop choices))
	  (when
	      (or
	       (not
		(tramp-get-method-parameter
		 (nth 0 item) (nth 1 item) (nth 2 item) 'tramp-login-program))
	       (tramp-get-method-parameter
		(nth 0 item) (nth 1 item) (nth 2 item) 'tramp-copy-program))
	    (tramp-error
	     method user host 'file-error
	     "Method `%s' is not supported for multi-hops."
	     (tramp-find-method
	      (nth 0 item) (nth 1 item) (nth 2 item))))))

      ;; Start new process.
      (when (and p (processp p))
	(delete-process p))
      (tramp-pre-connection method user host tramp-chunksize)
      (setenv "TERM" tramp-terminal-type)
      (setenv "PS1" "$ ")
      (tramp-message
       3 "Opening connection for %s@%s using %s..."
       (tramp-find-user   method user host)
       (tramp-find-host   method user host)
       (tramp-find-method method user host))
      (let* ((process-connection-type tramp-process-connection-type)
	     (default-directory (tramp-temporary-file-directory))
	     (coding-system-for-read nil)
	     (p (start-process
		 (or (tramp-get-connection-property
		      "process-name" nil method user host)
		     (tramp-buffer-name method user host))
		 (tramp-get-connection-buffer method user host)
		 tramp-encoding-shell))
	     (first-hop t))

	;; Check whether process is alive.
	(tramp-set-process-query-on-exit-flag p nil)
	(tramp-message 3 "Waiting 60s for local shell to come up...")
	(tramp-barf-if-no-shell-prompt
	 p 60 "Couldn't find local shell prompt %s" tramp-encoding-shell)

	;; Now do all the connections as specified.
	(while target-alist
	  (let* ((l-method (nth 0 (car target-alist)))
		 (l-user (nth 1 (car target-alist)))
		 (l-host (nth 2 (car target-alist)))
		 (l-port nil)
		 (login-program
		  (tramp-get-method-parameter
		   l-method l-user l-host 'tramp-login-program))
		 (login-args
		  (tramp-get-method-parameter
		   l-method l-user l-host 'tramp-login-args))
		 (command login-program))

	    ;; Check for port number.  Until now, there's no need for handling
	    ;; like method, user, host.
	    (when (string-match tramp-host-with-port-regexp l-host)
	      (setq l-port (match-string 2 l-host)
		    l-host (match-string 1 l-host)))

	    ;; Set variables for computing the prompt for reading password
	    (setq tramp-current-hop-method
		  (tramp-find-method l-method l-user l-host)
		  tramp-current-hop-user
		  (tramp-find-user l-method l-user l-host)
		  tramp-current-hop-host
		  (tramp-find-host l-method l-user l-host))

	    ;; Replace login-args place holders.
	    (setq
	     l-host (or l-host "")
	     l-user (or l-user "")
	     l-port (or l-port "")
	     command
	     (concat
	      command " "
	      (mapconcat
	       '(lambda (x)
		  (setq
		   x (mapcar
		      '(lambda (y)
			 (format-spec
			  y `((?h . ,l-host) (?u . ,l-user) (?p . ,l-port))))
		      x))
		  (unless (member "" x) (mapconcat 'identity x " ")))
	       login-args " ")
	      ;; String to detect failed connection.  Every single word must
	      ;; be enclosed with '\"'; otherwise it is detected
	      ;; during connection setup.
	      ;; Local shell could be a Windows COMSPEC.  It doesn't know
	      ;; the ";" syntax, so we do exit in case of error.
	      (if first-hop
		  " || exit"
		"; echo \"Tramp\" \"connection\" \"closed\"; sleep 1"))
	     ;; We don't reach a Windows shell.  Could be initial only.
	     first-hop nil)

	    ;; Send the command.
	    (tramp-message 3 "Sending command `%s'" command)
	    (tramp-send-command method user host command t t)
	    (tramp-process-actions
	     p method user host tramp-actions-before-shell)
	    (tramp-message 3 "Found remote shell prompt on `%s'" l-host))
	  ;; Next hop.
	  (setq target-alist (cdr target-alist)))

	;; Make initial shell settings.
	(tramp-open-connection-setup-interactive-shell p method user host)
	(tramp-post-connection method user host)))))

(defun tramp-send-command
  (method user host command &optional neveropen nooutput)
  "Send the COMMAND to USER at HOST (logged in using METHOD).
Erases temporary buffer before sending the command.  If optional arg NEVEROPEN
is non-nil, never try to open the connection.  This is meant to be used from
`tramp-maybe-open-connection' only.  The function waits for output unless
NOOUTPUT is set."
  (unless neveropen (tramp-maybe-open-connection method user host))
  (set-buffer (tramp-get-buffer method user host))
  (tramp-message 9 "%s" command)
  (tramp-send-string method user host command)
  (unless nooutput
    (tramp-wait-for-output (tramp-get-connection-process method user host))))

(defun tramp-send-command-internal (method user host command &optional msg)
  "Send command to remote host and wait for success.
Sends COMMAND, then waits 30 seconds for shell prompt."
  (tramp-message 9 "%s" command)
  (tramp-send-string method user host command)
  (when msg
    (tramp-message 3 "Waiting 30s for %s..." msg))
  (tramp-barf-if-no-shell-prompt
   (tramp-get-connection-process method user host) 30
   "Couldn't `%s', see buffer `%s'" command (buffer-name)))

(defun tramp-wait-for-output (proc &optional timeout)
  "Wait for output from remote rsh command."
  (let ((found
	 (tramp-wait-for-regexp
	  proc timeout (format "^%s\r?$" (regexp-quote tramp-end-of-output)))))
    (when found
      (with-current-buffer (process-buffer proc)
	(let (buffer-read-only)
	  (goto-char (point-max))
	  (forward-line -2)
	  (delete-region (point) (point-max)))))
    (when (not found)
      (if timeout
	  (tramp-error
	   tramp-current-method tramp-current-user tramp-current-host
	   'file-error "[[Remote prompt `%s' not found in %d secs]]"
	   tramp-end-of-output timeout)
	(tramp-error
	 tramp-current-method tramp-current-user tramp-current-host
	 'file-error "[[Remote prompt `%s' not found]]"
	 tramp-end-of-output)))
    (goto-char (point-min))
    ;; Return value is whether end-of-output sentinel was found.
    found))

(defun tramp-send-command-and-check
  (method user host command &optional subshell)
  "Run COMMAND and check its exit status.
METHOD specifies how to log in (as USER) to the remote HOST.
Sends `echo $?' along with the COMMAND for checking the exit status.  If
COMMAND is nil, just sends `echo $?'.  Returns the exit status found.

If the optional argument SUBSHELL is non-nil, the command is executed in
a subshell, ie surrounded by parentheses."
  (tramp-send-command
   method user host
   (concat (if subshell "( " "")
	   command
	   (if command " 2>/dev/null; " "")
	   "echo tramp_exit_status $?"
	   (if subshell " )" " ")))
  (with-current-buffer (tramp-get-connection-buffer method user host)
    (goto-char (point-max))
    (unless (search-backward "tramp_exit_status " nil t)
      (tramp-error
       method user host 'file-error
       "Couldn't find exit status of `%s'" command))
    (skip-chars-forward "^ ")
    (prog1
     (read (current-buffer))
     (let ((buffer-read-only))
       (delete-region (match-beginning 0) (point-max))))))

(defun tramp-barf-unless-okay (method user host command fmt &rest args)
  "Run COMMAND, check exit status, throw error if exit status not okay.
Similar to `tramp-send-command-and-check' but accepts two more arguments
FMT and ARGS which are passed to `error'."
  (unless (zerop (tramp-send-command-and-check method user host command))
    (apply 'tramp-error method user host 'file-error fmt args)))

(defun tramp-send-command-and-read (method user host command)
  "Run COMMAND and return the output, which must be a Lisp expression.
METHOD specifies how to log in (as USER) to the remote HOST.  In
case there is no valid Lisp expression, it raises an error"
  (tramp-barf-unless-okay
   method user host command "`%s' returns with error" command)
  (with-current-buffer (tramp-get-connection-buffer method user host)
    ;; Read the expression.
    (goto-char (point-min))
    (condition-case nil
	(prog1 (read (current-buffer))
	  ;; Error handling.
	  (when (re-search-forward "\\S-" nil t) (error)))
      (error (tramp-error
	      method user host 'file-error
	      "`%s' does not return a valid Lisp expression: `%s'"
	      command (buffer-string))))))

;; It seems that Tru64 Unix does not like it if long strings are sent
;; to it in one go.  (This happens when sending the Perl
;; `file-attributes' implementation, for instance.)  Therefore, we
;; have this function which waits a bit at each line.
(defun tramp-send-string (method user host string)
  "Send the STRING to USER at HOST using METHOD.

The STRING is expected to use Unix line-endings, but the lines sent to
the remote host use line-endings as defined in the variable
`tramp-rsh-end-of-line'.  The communication buffer is erased before sending."
  (let ((proc (tramp-get-connection-process method user host)))
    (unless proc
      (tramp-error
       method user host 'file-error
       "Can't send string to remote host -- not logged in"))
    (with-current-buffer (tramp-get-buffer method user host)
      (setq tramp-last-cmd-time (current-time))
      (tramp-trace "%s" string))
    (with-current-buffer (tramp-get-connection-buffer method user host)
      ;; Clean up the buffer.  We cannot call `erase-buffer' because
      ;; narrowing might be in effect.
      (let (buffer-read-only) (delete-region (point-min) (point-max)))
      ;; replace "\n" by `tramp-rsh-end-of-line'
      (setq string
	    (mapconcat 'identity
		       (split-string string "\n")
		       tramp-rsh-end-of-line))
      (unless (or (string= string "")
		  (string-equal (substring string -1) tramp-rsh-end-of-line))
	(setq string (concat string tramp-rsh-end-of-line)))
      ;; send the string
      (if (and tramp-chunksize (not (zerop tramp-chunksize)))
	  (let ((pos 0)
		(end (length string)))
	    (while (< pos end)
	      (tramp-message
	       10 "Sending chunk from %s to %s"
	       pos (min (+ pos tramp-chunksize) end))
	      (process-send-string
	       proc (substring string pos (min (+ pos tramp-chunksize) end)))
	      (setq pos (+ pos tramp-chunksize))))
	(process-send-string proc string)))))

(defun tramp-send-eof (method user host)
  "Send EOF to the remote end.
METHOD, HOST and USER specify the connection."
  (let ((proc (tramp-get-connection-process method user host)))
    (unless proc
      (tramp-error
       method user host 'file-error
       "Can't send EOF to remote host -- not logged in"))
    (tramp-message 5 "Sending eof")
    (process-send-eof proc)))

(defun tramp-kill-process (method user host)
  "Kill the connection process used by Tramp.
METHOD, USER, and HOST specify the connection."
  (kill-process (tramp-get-connection-process method user host)))

(defun tramp-mode-string-to-int (mode-string)
  "Converts a ten-letter `drwxrwxrwx'-style mode string into mode bits."
  (let* ((mode-chars (string-to-vector mode-string))
         (owner-read (aref mode-chars 1))
         (owner-write (aref mode-chars 2))
         (owner-execute-or-setid (aref mode-chars 3))
         (group-read (aref mode-chars 4))
         (group-write (aref mode-chars 5))
         (group-execute-or-setid (aref mode-chars 6))
         (other-read (aref mode-chars 7))
         (other-write (aref mode-chars 8))
         (other-execute-or-sticky (aref mode-chars 9)))
    (save-match-data
      (logior
       (case owner-read
         (?r (tramp-octal-to-decimal "00400")) (?- 0)
         (t (error "Second char `%c' must be one of `r-'" owner-read)))
       (case owner-write
         (?w (tramp-octal-to-decimal "00200")) (?- 0)
         (t (error "Third char `%c' must be one of `w-'" owner-write)))
       (case owner-execute-or-setid
         (?x (tramp-octal-to-decimal "00100"))
         (?S (tramp-octal-to-decimal "04000"))
         (?s (tramp-octal-to-decimal "04100"))
         (?- 0)
         (t (error "Fourth char `%c' must be one of `xsS-'"
                   owner-execute-or-setid)))
       (case group-read
         (?r (tramp-octal-to-decimal "00040")) (?- 0)
         (t (error "Fifth char `%c' must be one of `r-'" group-read)))
       (case group-write
         (?w (tramp-octal-to-decimal "00020")) (?- 0)
         (t (error "Sixth char `%c' must be one of `w-'" group-write)))
       (case group-execute-or-setid
         (?x (tramp-octal-to-decimal "00010"))
         (?S (tramp-octal-to-decimal "02000"))
         (?s (tramp-octal-to-decimal "02010"))
         (?- 0)
         (t (error "Seventh char `%c' must be one of `xsS-'"
                   group-execute-or-setid)))
       (case other-read
         (?r (tramp-octal-to-decimal "00004")) (?- 0)
         (t (error "Eighth char `%c' must be one of `r-'" other-read)))
       (case other-write
         (?w (tramp-octal-to-decimal "00002")) (?- 0)
         (t (error "Nineth char `%c' must be one of `w-'" other-write)))
       (case other-execute-or-sticky
         (?x (tramp-octal-to-decimal "00001"))
         (?T (tramp-octal-to-decimal "01000"))
         (?t (tramp-octal-to-decimal "01001"))
         (?- 0)
         (t (error "Tenth char `%c' must be one of `xtT-'"
                   other-execute-or-sticky)))))))

(defun tramp-convert-file-attributes (method user host attr)
  "Convert file-attributes ATTR generated by perl script, stat or ls.
Convert file mode bits to string and set virtual device number.
Return ATTR."
  ;; Convert last access time
  (unless (listp (nth 4 attr))
    (setcar (nthcdr 4 attr)
	    (list (floor (nth 4 attr) 65536)
		  (floor (mod (nth 4 attr) 65536)))))
  ;; Convert last modification time
  (unless (listp (nth 5 attr))
    (setcar (nthcdr 5 attr)
	    (list (floor (nth 5 attr) 65536)
		  (floor (mod (nth 5 attr) 65536)))))
  ;; Convert last status change time
  (unless (listp (nth 6 attr))
    (setcar (nthcdr 6 attr)
	    (list (floor (nth 6 attr) 65536)
		  (floor (mod (nth 6 attr) 65536)))))
  ;; Convert file mode bits to string.
  (unless (stringp (nth 8 attr))
    (setcar (nthcdr 8 attr) (tramp-file-mode-from-int (nth 8 attr))))
  ;; Convert directory indication bit.
  (if (string-match "^d" (nth 8 attr))
      (setcar attr t)
    (if (and (listp (car attr)) (stringp (caar attr))
	     (string-match ".+ -> .\\(.+\\)." (caar attr)))
	(setcar attr (match-string 1 (caar attr)))
      (setcar attr nil)))
  ;; Set file's gid change bit.
  (setcar (nthcdr 9 attr)
	  (if (numberp (nth 3 attr))
	      (not (= (nth 3 attr)
		      (tramp-get-remote-gid method user host 'integer)))
	    (not (string-equal
		  (nth 3 attr)
		  (tramp-get-remote-gid method user host 'string)))))
  ;; Set virtual device number.
  (setcar (nthcdr 11 attr)
          (tramp-get-device method user host))
  attr)

(defun tramp-get-device (method user host)
  "Returns the virtual device number.
If it doesn't exist, generate a new one."
  (let ((string (tramp-make-tramp-file-name method user host "")))
    (unless (assoc string tramp-devices)
      (add-to-list 'tramp-devices
		   (list string (length tramp-devices))))
    (list -1 (nth 1 (assoc string tramp-devices)))))

(defun tramp-file-mode-from-int (mode)
  "Turn an integer representing a file mode into an ls(1)-like string."
  (let ((type	(cdr (assoc (logand (lsh mode -12) 15) tramp-file-mode-type-map)))
	(user	(logand (lsh mode -6) 7))
	(group	(logand (lsh mode -3) 7))
	(other	(logand (lsh mode -0) 7))
	(suid	(> (logand (lsh mode -9) 4) 0))
	(sgid	(> (logand (lsh mode -9) 2) 0))
	(sticky	(> (logand (lsh mode -9) 1) 0)))
    (setq user  (tramp-file-mode-permissions user  suid "s"))
    (setq group (tramp-file-mode-permissions group sgid "s"))
    (setq other (tramp-file-mode-permissions other sticky "t"))
    (concat type user group other)))


(defun tramp-file-mode-permissions (perm suid suid-text)
  "Convert a permission bitset into a string.
This is used internally by `tramp-file-mode-from-int'."
  (let ((r (> (logand perm 4) 0))
	(w (> (logand perm 2) 0))
	(x (> (logand perm 1) 0)))
    (concat (or (and r "r") "-")
	    (or (and w "w") "-")
	    (or (and suid x suid-text)	; suid, execute
		(and suid (upcase suid-text)) ; suid, !execute
		(and x "x") "-"))))	; !suid


(defun tramp-decimal-to-octal (i)
  "Return a string consisting of the octal digits of I.
Not actually used.  Use `(format \"%o\" i)' instead?"
  (cond ((< i 0) (error "Cannot convert negative number to octal"))
        ((not (integerp i)) (error "Cannot convert non-integer to octal"))
        ((zerop i) "0")
        (t (concat (tramp-decimal-to-octal (/ i 8))
                   (number-to-string (% i 8))))))


;;(defun tramp-octal-to-decimal (ostr)
;;  "Given a string of octal digits, return a decimal number."
;;  (cond ((null ostr) 0)
;;        ((string= "" ostr) 0)
;;        (t (let ((last (aref ostr (1- (length ostr))))
;;                 (rest (substring ostr 0 (1- (length ostr)))))
;;             (unless (and (>= last ?0)
;;                          (<= last ?7))
;;               (error "Not an octal digit: %c" last))
;;             (+ (- last ?0) (* 8 (tramp-octal-to-decimal rest)))))))
;; Kudos to Gerd Moellmann for this suggestion.
(defun tramp-octal-to-decimal (ostr)
  "Given a string of octal digits, return a decimal number."
  (let ((x (or ostr "")))
    ;; `save-match' is in `tramp-mode-string-to-int' which calls this.
    (unless (string-match "\\`[0-7]*\\'" x)
      (error "Non-octal junk in string `%s'" x))
    (string-to-number ostr 8)))

(defun tramp-shell-case-fold (string)
  "Converts STRING to shell glob pattern which ignores case."
  (mapconcat
   (lambda (c)
     (if (equal (downcase c) (upcase c))
         (vector c)
       (format "[%c%c]" (downcase c) (upcase c))))
   string
   ""))


;; ------------------------------------------------------------
;; -- TRAMP file names --
;; ------------------------------------------------------------
;; Conversion functions between external representation and
;; internal data structure.  Convenience functions for internal
;; data structure.

(defstruct tramp-file-name method user host localname)

(defun tramp-tramp-file-p (name)
  "Return t iff NAME is a tramp file."
  (save-match-data
    (string-match tramp-file-name-regexp name)))

(defun tramp-dissect-file-name (name)
  "Return a `tramp-file-name' structure.
The structure consists of remote method, remote user, remote host and
localname (file name on remote host)."
  (save-match-data
    (let ((match (string-match (nth 0 tramp-file-name-structure) name)))
      (unless match (error "Not a tramp file name: %s" name))
      (let ((method    (match-string (nth 1 tramp-file-name-structure) name))
	    (user      (match-string (nth 2 tramp-file-name-structure) name))
	    (host      (match-string (nth 3 tramp-file-name-structure) name))
	    (localname (match-string (nth 4 tramp-file-name-structure) name)))
	(make-tramp-file-name
	 :method method
	 :user (or user nil)
	 :host host
	 :localname localname)))))

(defun tramp-equal-remote (file1 file2)
  "Checks, whether the remote parts of FILE1 and FILE2 are identical.
The check depends on method, user and host name of the files.  If
one of the components is missing, the default values are used.
The local file name parts of FILE1 and FILE2 are not taken into
account.

Example:

  (tramp-equal-remote \"/ssh::/etc\" \"/<your host name>:/home\")

would yield `t'.  On the other hand, the following check results in nil:

  (tramp-equal-remote \"/sudo::/etc\" \"/su::/etc\")"
  (and (stringp (file-remote-p file1))
       (stringp (file-remote-p file2))
       (string-equal (file-remote-p file1) (file-remote-p file2))))

(defun tramp-find-default-method (user host)
  "Look up the right method to use in `tramp-default-method-alist'."
  (let ((choices tramp-default-method-alist)
	(method tramp-default-method)
	item)
    (while choices
      (setq item (pop choices))
      (when (and (string-match (or (nth 0 item) "") (or host ""))
		 (string-match (or (nth 1 item) "") (or user "")))
	(setq method (nth 2 item))
	(setq choices nil)))
    method))

(defun tramp-find-method (method user host)
  "Return the right method string to use.
This is METHOD, if non-nil. Otherwise, do a lookup in
`tramp-default-method-alist'."
  (or method (tramp-find-default-method user host)))

(defun tramp-find-default-user (method host)
  "Look up the right user to use in `tramp-default-user-alist'."
  (let ((choices tramp-default-user-alist)
	(user tramp-default-user)
	item)
    (while choices
      (setq item (pop choices))
      (when (and (string-match (or (nth 0 item) "") (or method ""))
		 (string-match (or (nth 1 item) "") (or host "")))
	(setq user (nth 2 item))
	(setq choices nil)))
    user))

(defun tramp-find-user (method user host)
  "Return the right user string to use.
This is USER, if non-nil. Otherwise, do a lookup in
`tramp-default-user-alist'."
  (or user (tramp-find-default-user method host)))

(defun tramp-find-default-host (method user)
  "Look up the right host to use."
  (or tramp-default-host (system-name)))

(defun tramp-find-host (method user host)
  "Return the right host string to use.
This is HOST, if non-nil. Otherwise, it is `tramp-default-host'."
  (or (and (> (length host) 1) host)
      (tramp-find-default-host method user)))

(defun tramp-make-tramp-file-name (method user host localname)
  "Constructs a tramp file name from METHOD, USER, HOST and LOCALNAME."
  ;; At least host delimeter must exist.
  (setq host (or host ""))
  (format-spec
   (concat tramp-prefix-format
	   (when method (concat "%m" tramp-postfix-method-format))
	   (when user   (concat "%u" tramp-postfix-user-format))
	   (when host   (concat "%h" tramp-postfix-host-format))
	   (when localname   (concat "%l")))
   `((?m . ,method) (?u . ,user) (?h . ,host) (?l . ,localname))))

(defun tramp-make-copy-program-file-name (user host localname)
  "Create a file name suitable to be passed to `rcp' and workalikes."
  (if user
      (format "%s@%s:%s" user host localname)
    (format "%s:%s" host localname)))

(defun tramp-method-out-of-band-p (method user host)
  "Return t if this is an out-of-band method, nil otherwise."
  (tramp-get-method-parameter
   (tramp-find-method method user host) user host 'tramp-copy-program))

;; Variables local to connection.

(defun tramp-get-ls-command (method user host)
  (with-connection-property method user host "ls"
    (save-excursion
      (with-current-buffer (tramp-get-connection-buffer method user host)
	(tramp-message 5 "Finding a suitable `ls' command")
	(or
	 (catch 'ls-found
	   (dolist (cmd '("ls" "gnuls" "gls"))
	     (let ((dl tramp-remote-path)
		   result)
	       (while
		   (and
		    dl
		    (setq result
			  (tramp-find-executable method user host cmd dl t t)))
		 ;; Check parameter.
		 (when (zerop (tramp-send-command-and-check
			       method user host (format "%s -lnd /" result)))
		   (throw 'ls-found result))
		 ;; Remove unneeded directories from path.
		 (while
		     (and
		      dl
		      (not
		       (string-equal
			result
			(concat (file-name-as-directory (car dl)) cmd))))
		   (setq dl (cdr dl)))
		 (setq dl (cdr dl))))))
	 (tramp-error
	  method user host 'file-error
	  "Couldn't find a proper `ls' command"))))))

(defun tramp-get-test-command (method user host)
  (with-connection-property method user host "test"
    (save-excursion
      (with-current-buffer (tramp-get-connection-buffer method user host)
	(tramp-message 5 "Finding a suitable `test' command")
	(if (zerop (tramp-send-command-and-check
		    method user host "test 0"))
	    "test"
	  (tramp-find-executable method user host "test" tramp-remote-path))))))

(defun tramp-get-test-nt-command (method user host)
  ;; Does `test A -nt B' work?  Use abominable `find' construct if it
  ;; doesn't.  BSD/OS 4.0 wants the parentheses around the command,
  ;; for otherwise the shell crashes.
  (with-connection-property method user host "test-nt"
    (save-excursion
      (or
       (progn
	 (tramp-send-command
	  method user host
	  (format "( %s / -nt / )" (tramp-get-test-command method user host)))
	 (goto-char (point-min))
	 (when (looking-at
		(format "\n%s\r?\n" (regexp-quote tramp-end-of-output)))
	   (format "%s %%s -nt %%s"
		   (tramp-get-test-command method user host))))
       (progn
	 (tramp-send-command
	  method user host
	  (format
	   "tramp_test_nt () {\n%s -n \"`find $1 -prune -newer $2 -print`\"\n}"
	   (tramp-get-test-command method user host)))
	 "tramp_test_nt %s %s")))))

(defun tramp-get-file-exists-command (method user host)
  (with-connection-property method user host "file-exists"
    (save-excursion
      (with-current-buffer (tramp-get-connection-buffer method user host)
	(tramp-message 5 "Finding command to check if file exists")
	(tramp-find-file-exists-command method user host)))))

(defun tramp-get-remote-ln (method user host)
  (with-connection-property method user host "ln"
    (save-excursion
      (with-current-buffer (tramp-get-connection-buffer method user host)
	(tramp-message 5 "Finding a suitable `ln' command")
	(tramp-find-executable method user host "ln" tramp-remote-path)))))

(defun tramp-get-remote-perl (method user host)
  (with-connection-property method user host "perl"
    (save-excursion
      (with-current-buffer (tramp-get-connection-buffer method user host)
	(tramp-message 5 "Finding a suitable `perl' command")
	(or (tramp-find-executable method user host "perl5" tramp-remote-path)
	    (tramp-find-executable
	     method user host "perl" tramp-remote-path))))))

(defun tramp-get-remote-stat (method user host)
  (with-connection-property method user host "stat"
    (save-excursion
      (with-current-buffer (tramp-get-connection-buffer method user host)
	(tramp-message 5 "Finding a suitable `stat' command")
	(let ((result (tramp-find-executable
		       method user host "stat" tramp-remote-path))
	      tmp)
	  ;; Check whether stat(1) returns usable syntax
	  (when result
	    (setq tmp
		  (condition-case nil
		      (tramp-send-command-and-read
		       method user host (format "%s -c '(\"%%N\")' /" result))
		    (error nil)))
	    (unless (and (listp tmp) (stringp (car tmp))
			 (string-match "^./.$" (car tmp)))
	      (setq result nil)))
	  result)))))

(defun tramp-get-remote-id (method user host)
  (with-connection-property method user host "id"
    (save-excursion
      (with-current-buffer (tramp-get-connection-buffer method user host)
	(tramp-message 5 "Finding POSIX `id' command")
	(or
	 (catch 'id-found
	   (let ((dl tramp-remote-path)
		 result)
	     (while
		 (and
		  dl
		  (setq result
			(tramp-find-executable method user host "id" dl t t)))
	       ;; Check POSIX parameter.
	       (when (zerop (tramp-send-command-and-check
			     method user host (format "%s -u" result)))
		 (throw 'id-found result))
	       ;; Remove unneeded directories from path.
	       (while
		   (and
		    dl
		    (not
		     (string-equal
		      result
		      (concat (file-name-as-directory (car dl)) "id"))))
		 (setq dl (cdr dl)))
	       (setq dl (cdr dl)))))
	 (tramp-error
	  method user host 'file-error
	  "Couldn't find a POSIX `id' command"))))))

(defun tramp-get-remote-uid (method user host id-format)
  (with-connection-property method user host (format "uid-%s" id-format)
    (save-excursion
      (tramp-send-command-and-read
       method user host
       (format "%s -u%s %s"
	       (tramp-get-remote-id method user host)
	       (if (equal id-format 'integer) "" "n")
	       (if (equal id-format 'integer)
		   "" "| sed -e s/^/\\\"/ -e s/\$/\\\"/"))))))

(defun tramp-get-remote-gid (method user host id-format)
  (with-connection-property method user host (format "gid-%s" id-format)
    (save-excursion
      (tramp-send-command-and-read
       method user host
       (format "%s -g%s %s"
	       (tramp-get-remote-id method user host)
	       (if (equal id-format 'integer) "" "n")
	       (if (equal id-format 'integer)
		   "" "| sed -e s/^/\\\"/ -e s/\$/\\\"/"))))))

;; Some predefined connection properties.
(defun tramp-set-remote-encoding (method user host rem-enc)
  (tramp-set-connection-property "remote-encoding" rem-enc method user host))
(defun tramp-get-remote-encoding (method user host)
  (tramp-get-connection-property "remote-encoding" nil method user host))

(defun tramp-set-remote-decoding (method user host rem-dec)
  (tramp-set-connection-property "remote-decoding" rem-dec method user host))
(defun tramp-get-remote-decoding (method user host)
  (tramp-get-connection-property "remote-decoding" nil method user host))

(defun tramp-set-local-encoding (method user host loc-enc)
  (tramp-set-connection-property "local-encoding" loc-enc method user host))
(defun tramp-get-local-encoding (method user host)
  (tramp-get-connection-property "local-encoding" nil method user host))

(defun tramp-set-local-decoding (method user host loc-dec)
  (tramp-set-connection-property "local-decoding" loc-dec method user host))
(defun tramp-get-local-decoding (method user host)
  (tramp-get-connection-property "local-decoding" nil method user host))

(defun tramp-get-method-parameter (method user host param)
  "Return the method parameter PARAM.
If the `tramp-methods' entry does not exist, use the variable PARAM
as default."
  (unless (boundp param)
    (tramp-error
     method user host 'file-error
     "Non-existing method parameter `%s'" param))
  (let ((entry (assoc param
		      (assoc (tramp-find-method method user host)
			     tramp-methods))))
    (if entry
	(second entry)
      (symbol-value param))))


;; Auto saving to a special directory.

(defun tramp-exists-file-name-handler (operation &rest args)
  (condition-case nil
      (let ((buffer-file-name "/")
	    (fnha file-name-handler-alist)
	    (check-file-name-operation operation)
	    (file-name-handler-alist
	     (list
	      (cons "/"
		    '(lambda (operation &rest args)
		       "Returns OPERATION if it is the one to be checked"
		       (if (equal check-file-name-operation operation)
			   operation
			 (let ((file-name-handler-alist fnha))
			   (apply operation args))))))))
	(eq (apply operation args) operation))
    (error nil)))

(unless (tramp-exists-file-name-handler 'make-auto-save-file-name)
  (defadvice make-auto-save-file-name
    (around tramp-advice-make-auto-save-file-name () activate)
    "Invoke `tramp-handle-make-auto-save-file-name' for tramp files."
    (if (and (buffer-file-name) (tramp-tramp-file-p (buffer-file-name)))
	(setq ad-return-value (tramp-handle-make-auto-save-file-name))
      ad-do-it)))

;; In Emacs < 22 and XEmacs < 21.5 autosaved remote files have
;; permission 0666 minus umask. This is a security threat.

(defun tramp-set-auto-save-file-modes ()
  "Set permissions of autosaved remote files to the original permissions."
  (let ((bfn (buffer-file-name)))
    (when (and (stringp bfn)
	       (tramp-tramp-file-p bfn)
	       (stringp buffer-auto-save-file-name)
	       (not (equal bfn buffer-auto-save-file-name)))
      (unless (file-exists-p buffer-auto-save-file-name)
	(write-region "" nil buffer-auto-save-file-name))
      ;; Permissions should be set always, because there might be an old
      ;; auto-saved file belonging to another original file.  This could
      ;; be a security threat.
      (set-file-modes buffer-auto-save-file-name
		      (or (file-modes bfn) (tramp-octal-to-decimal "0600"))))))

(unless (or (> emacs-major-version 21)
	    (and (featurep 'xemacs)
		 (= emacs-major-version 21)
		 (> emacs-minor-version 4)))
  (add-hook 'auto-save-hook 'tramp-set-auto-save-file-modes))

(defun tramp-subst-strs-in-string (alist string)
  "Replace all occurrences of the string FROM with TO in STRING.
ALIST is of the form ((FROM . TO) ...)."
  (save-match-data
    (while alist
      (let* ((pr (car alist))
             (from (car pr))
             (to (cdr pr)))
        (while (string-match (regexp-quote from) string)
          (setq string (replace-match to t t string)))
        (setq alist (cdr alist))))
    string))

;; ------------------------------------------------------------
;; -- Compatibility functions section --
;; ------------------------------------------------------------

(defun tramp-temporary-file-directory ()
  "Return name of directory for temporary files (compat function).
For Emacs, this is the variable `temporary-file-directory', for XEmacs
this is the function `temp-directory'."
  (cond ((boundp 'temporary-file-directory)
         (symbol-value 'temporary-file-directory))
        ((fboundp 'temp-directory)
         (funcall (symbol-function 'temp-directory))) ;pacify byte-compiler
        ((let ((d (getenv "TEMP"))) (and d (file-directory-p d)))
         (file-name-as-directory (getenv "TEMP")))
        ((let ((d (getenv "TMP"))) (and d (file-directory-p d)))
         (file-name-as-directory (getenv "TMP")))
        ((let ((d (getenv "TMPDIR"))) (and d (file-directory-p d)))
         (file-name-as-directory (getenv "TMPDIR")))
        ((file-exists-p "c:/temp") (file-name-as-directory "c:/temp"))
        (t (message (concat "Neither `temporary-file-directory' nor "
                            "`temp-directory' is defined -- using /tmp."))
           (file-name-as-directory "/tmp"))))

(defun tramp-read-passwd ()
  "Read a password from user (compat function).
Invokes `password-read' if available, `read-passwd' else."
  (let* ((key (tramp-make-tramp-file-name
	       tramp-current-hop-method
	       tramp-current-hop-user
	       tramp-current-hop-host ""))
	 (pw-prompt (format "Password for %s " key)))
    (if (functionp 'password-read)
	(let ((password (apply #'password-read	(list pw-prompt key))))
	  (apply #'password-cache-add (list key password))
	  password)
      (read-passwd pw-prompt))))

(defun tramp-clear-passwd (&optional method user host)
  "Clear password cache for connection related to current-buffer.
If METHOD, USER or HOST is given, take then for computing the key."
  (interactive)
  (when (functionp 'password-cache-remove)
    (unless (or method user host)
      ;; KEY must be computed from filename.
      (with-parsed-tramp-file-name
	  (or buffer-file-name list-buffers-directory "") l
	(setq method l-method user l-user host l-host)))
    (setq method (tramp-find-method method user host)
	  user   (tramp-find-user   method user host)
	  host   (tramp-find-host   method user host))
    (apply #'password-cache-remove
	   (list (tramp-make-tramp-file-name method user host "")))))

(defun tramp-time-diff (t1 t2)
  "Return the difference between the two times, in seconds.
T1 and T2 are time values (as returned by `current-time' for example).

NOTE: This function will fail if the time difference is too large to
fit in an integer."
  ;; Pacify byte-compiler with `symbol-function'.
  (cond ((and (fboundp 'subtract-time)
	      (fboundp 'float-time))
         (funcall (symbol-function 'float-time)
		  (funcall (symbol-function 'subtract-time) t1 t2)))
	((and (fboundp 'subtract-time)
	      (fboundp 'time-to-seconds))
         (funcall (symbol-function 'time-to-seconds)
		  (funcall (symbol-function 'subtract-time) t1 t2)))
        ((fboundp 'itimer-time-difference)
         (floor (funcall
		 (symbol-function 'itimer-time-difference)
		 (if (< (length t1) 3) (append t1 '(0)) t1)
		 (if (< (length t2) 3) (append t2 '(0)) t2))))
        (t
         ;; snarfed from Emacs 21 time-date.el; combining
	 ;; time-to-seconds and subtract-time
	 (let ((time  (let ((borrow (< (cadr t1) (cadr t2))))
                 (list (- (car t1) (car t2) (if borrow 1 0))
                       (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2))))))
	   (+ (* (car time) 65536.0)
	      (cadr time)
	      (/ (or (nth 2 time) 0) 1000000.0))))))

(defun tramp-coding-system-change-eol-conversion (coding-system eol-type)
  "Return a coding system like CODING-SYSTEM but with given EOL-TYPE.
EOL-TYPE can be one of `dos', `unix', or `mac'."
  (cond ((fboundp 'coding-system-change-eol-conversion)
         (apply #'coding-system-change-eol-conversion
                (list coding-system eol-type)))
        ((fboundp 'subsidiary-coding-system)
         (apply
          #'subsidiary-coding-system
          (list coding-system
                (cond ((eq eol-type 'dos) 'crlf)
                      ((eq eol-type 'unix) 'lf)
                      ((eq eol-type 'mac) 'cr)
                      (t
                       (error "Unknown EOL-TYPE `%s', must be %s"
                              eol-type
                              "`dos', `unix', or `mac'"))))))
        (t (error "Can't change EOL conversion -- is MULE missing?"))))

(defun tramp-split-string (string pattern)
  "Like `split-string' but omit empty strings.
In Emacs, (split-string \"/foo/bar\" \"/\") returns (\"foo\" \"bar\").
This is, the first, empty, element is omitted.  In XEmacs, the first
element is not omitted.

Note: this function has been written for `tramp-handle-file-truename'.
If you want to use it for something else, you'll have to check whether
it does the right thing."
  (delete "" (split-string string pattern)))

(defun tramp-set-process-query-on-exit-flag (process flag)
  "Specify if query is needed for process when Emacs is exited.
If the second argument flag is non-nil, Emacs will query the user before
exiting if process is running."
  (if (fboundp 'set-process-query-on-exit-flag)
      (funcall (symbol-function 'set-process-query-on-exit-flag) process flag)
    (funcall (symbol-function 'process-kill-without-query) process flag)))


;; ------------------------------------------------------------
;; -- Kludges section --
;; ------------------------------------------------------------

;; Currently (as of Emacs 20.5), the function `shell-quote-argument'
;; does not deal well with newline characters.  Newline is replaced by
;; backslash newline.  But if, say, the string `a backslash newline b'
;; is passed to a shell, the shell will expand this into "ab",
;; completely omitting the newline.  This is not what was intended.
;; It does not appear to be possible to make the function
;; `shell-quote-argument' work with newlines without making it
;; dependent on the shell used.  But within this package, we know that
;; we will always use a Bourne-like shell, so we use an approach which
;; groks newlines.
;;
;; The approach is simple: we call `shell-quote-argument', then
;; massage the newline part of the result.
;;
;; This function should produce a string which is grokked by a Unix
;; shell, even if the Emacs is running on Windows.  Since this is the
;; kludges section, we bind `system-type' in such a way that
;; `shell-quote-arguments'  behaves as if on Unix.
;;
;; Thanks to Mario DeWeerd for the hint that it is sufficient for this
;; function to work with Bourne-like shells.
;;
;; CCC: This function should be rewritten so that
;; `shell-quote-argument' is not used.  This way, we are safe from
;; changes in `shell-quote-argument'.
(defun tramp-shell-quote-argument (s)
  "Similar to `shell-quote-argument', but groks newlines.
Only works for Bourne-like shells."
  (let ((system-type 'not-windows))
    (save-match-data
      (let ((result (shell-quote-argument s))
	    (nl (regexp-quote (format "\\%s" tramp-rsh-end-of-line))))
	(when (and (>= (length result) 2)
		   (string= (substring result 0 2) "\\~"))
	  (setq result (substring result 1)))
	(while (string-match nl result)
	  (setq result (replace-match (format "'%s'" tramp-rsh-end-of-line)
				      t t result)))
	result))))

;; We currently (sometimes) use "[" and "]" in the filename format.
;; This means that Emacs wants to expand wildcards if
;; `find-file-wildcards' is non-nil, and then barfs because no
;; expansion could be found.  We detect this situation and do
;; something really awful: we have `file-expand-wildcards' return the
;; original filename if it can't expand anything.  Let's just hope
;; that this doesn't break anything else.
;; CCC: This check is now also really awful; we should search all
;; of the filename format, not just the prefix.
(when (string-match "\\[" tramp-prefix-format)
  (defadvice file-expand-wildcards (around tramp-fix activate)
    (let ((name (ad-get-arg 0)))
      (if (tramp-tramp-file-p name)
	  ;; If it's a Tramp file, dissect it and look if wildcards
	  ;; need to be expanded at all.
	  (let ((v (tramp-dissect-file-name name)))
	    (if (string-match "[[*?]" (tramp-file-name-localname v))
		(let ((res ad-do-it))
		  (setq ad-return-value (or res (list name))))
	      (setq ad-return-value (list name))))
	;; If it is not a Tramp file, just run the original function.
	(let ((res ad-do-it))
	  (setq ad-return-value (or res (list name))))))))

;; Tramp version is useful in a number of situations.

(defun tramp-version (arg)
  "Print version number of tramp.el in minibuffer or current buffer."
  (interactive "P")
  (if arg (insert tramp-version) (message tramp-version)))

;; Make the `reporter` functionality available for making bug reports about
;; the package. A most useful piece of code.

(unless (fboundp 'reporter-submit-bug-report)
  (autoload 'reporter-submit-bug-report "reporter"))

(defun tramp-bug ()
  "Submit a bug report to the TRAMP developers."
  (interactive)
  (require 'reporter)
  (catch 'dont-send
    (let ((reporter-prompt-for-summary-p t))
      (reporter-submit-bug-report
       tramp-bug-report-address		; to-address
       (format "tramp (%s)" tramp-version) ; package name and version
       (delq nil
	     `(;; Current state
	       tramp-current-method
	       tramp-current-user
	       tramp-current-host

	       ;; System defaults
	       tramp-auto-save-directory        ; vars to dump
	       tramp-default-method
	       tramp-default-method-alist
	       tramp-default-host
	       tramp-default-proxies-alist
	       tramp-default-user
	       tramp-default-user-alist
	       tramp-rsh-end-of-line
	       tramp-default-password-end-of-line
	       tramp-remote-path
	       tramp-login-prompt-regexp
	       ;; Mask non-7bit characters
	       (tramp-password-prompt-regexp . tramp-reporter-dump-variable)
	       tramp-wrong-passwd-regexp
	       tramp-yesno-prompt-regexp
	       tramp-yn-prompt-regexp
	       tramp-terminal-prompt-regexp
	       tramp-temp-name-prefix
	       tramp-file-name-structure
	       tramp-file-name-regexp
	       tramp-methods
	       tramp-end-of-output
	       tramp-local-coding-commands
	       tramp-remote-coding-commands
	       tramp-actions-before-shell
	       tramp-actions-copy-out-of-band
	       tramp-terminal-type
	       ;; Mask non-7bit characters
	       (tramp-shell-prompt-pattern . tramp-reporter-dump-variable)
	       tramp-chunksize
	       ,(when (boundp 'tramp-backup-directory-alist)
		  'tramp-backup-directory-alist)
	       ,(when (boundp 'tramp-bkup-backup-directory-info)
		  'tramp-bkup-backup-directory-info)

	       ;; Non-tramp variables of interest
	       ;; Mask non-7bit characters
	       (shell-prompt-pattern . tramp-reporter-dump-variable)
	       backup-by-copying
	       backup-by-copying-when-linked
	       backup-by-copying-when-mismatch
	       ,(when (boundp 'backup-by-copying-when-privileged-mismatch)
		  'backup-by-copying-when-privileged-mismatch)
	       ,(when (boundp 'password-cache)
		  'password-cache)
	       ,(when (boundp 'password-cache-expiry)
		  'password-cache-expiry)
	       ,(when (boundp 'backup-directory-alist)
		  'backup-directory-alist)
	       ,(when (boundp 'bkup-backup-directory-info)
		  'bkup-backup-directory-info)
	       file-name-handler-alist))

       'tramp-load-report-modules	; pre-hook
       'tramp-append-tramp-buffers	; post-hook
       "\
Enter your bug report in this message, including as much detail
as you possibly can about the problem, what you did to cause it
and what the local and remote machines are.

If you can give a simple set of instructions to make this bug
happen reliably, please include those.  Thank you for helping
kill bugs in TRAMP.

Another useful thing to do is to put

  (setq tramp-verbose 9
        tramp-debug-buffer t)

in the ~/.emacs file and to repeat the bug.  Then, include the
contents of the *tramp/foo* buffer and the *debug tramp/foo*
buffer in your bug report.

--bug report follows this line--
"))))

(defun tramp-reporter-dump-variable (varsym mailbuf)
  "Pretty-print the value of the variable in symbol VARSYM.
Used for non-7bit chars in strings."
  (let* ((reporter-eval-buffer (symbol-value 'reporter-eval-buffer))
	 (val (with-current-buffer reporter-eval-buffer
		(symbol-value varsym))))

    ;; There are characters to be masked.
    (when (and (boundp 'mm-7bit-chars)
	       (string-match
		(concat "[^" (symbol-value 'mm-7bit-chars) "]") val))
      (with-current-buffer reporter-eval-buffer
	(set varsym (concat "(base64-decode-string \""
			    (base64-encode-string val)
			    "\")"))))

    ;; Dump variable.
    (funcall (symbol-function 'reporter-dump-variable) varsym mailbuf)

    ;; Remove string quotation.
    (forward-line -1)
    (when (looking-at
	   (concat "\\(^.*\\)" "\""                       ;; \1 "
		   "\\((base64-decode-string \\)" "\\\\"  ;; \2 \
		   "\\(\".*\\)" "\\\\"                    ;; \3 \
		   "\\(\")\\)" "\"$"))                    ;; \4 "
      (replace-match "\\1\\2\\3\\4")
      (beginning-of-line)
      (insert " ;; variable encoded due to non-printable characters\n"))
    (forward-line 1)

    ;; Reset VARSYM to old value.
    (with-current-buffer reporter-eval-buffer
      (set varsym val))))

(defun tramp-load-report-modules ()
  "Load needed modules for reporting."

  ;; We load message.el and mml.el from Gnus.
  (if (featurep 'xemacs)
      (progn
	(load "message" 'noerror)
	(load "mml" 'noerror))
    (require 'message nil 'noerror)
    (require 'mml nil 'noerror))
  (when (functionp 'message-mode)
    (funcall (symbol-function 'message-mode)))
  (when (functionp 'mml-mode)
    (funcall (symbol-function 'mml-mode) t)))

(defun tramp-append-tramp-buffers ()
  "Append Tramp buffers and buffer local variables into the bug report."

  (goto-char (point-max))

  ;; Dump buffer local variables.
  (dolist (buffer
	   (delq nil
		 (mapcar
		  '(lambda (b)
		     (when (string-match "\\*tramp/" (buffer-name b)) b))
		  (buffer-list))))
    (let ((reporter-eval-buffer buffer)
	  (buffer-name (buffer-name buffer))
	  (elbuf (get-buffer-create " *tmp-reporter-buffer*")))
      (with-current-buffer elbuf
	(emacs-lisp-mode)
	(erase-buffer)
	(insert "\n(setq\n")
	(lisp-indent-line)
	(funcall (symbol-function 'reporter-dump-variable)
		 'buffer-name (current-buffer))
	(dolist (varsym-or-cons-cell (buffer-local-variables buffer))
	  (let ((varsym (or (car-safe varsym-or-cons-cell)
			    varsym-or-cons-cell)))
	    (when (string-match "tramp" (symbol-name varsym))
	      (funcall (symbol-function	'reporter-dump-variable)
		       varsym (current-buffer)))))
	(lisp-indent-line)
	(insert ")\n"))
      (insert-buffer-substring elbuf)))

  ;; Append buffers only when we are in message mode.
  (when (and
	 (eq major-mode 'message-mode)
	 (boundp 'mml-mode)
	 (symbol-value 'mml-mode))

    (let* ((tramp-buf-regexp "\\*\\(debug \\)?tramp/")
	   (buffer-list
	    (delq nil
		  (mapcar '(lambda (b)
		     (when (string-match tramp-buf-regexp (buffer-name b)) b))
			  (buffer-list))))
	   (curbuf (current-buffer)))

      ;; There is at least one Tramp buffer.
      (when buffer-list
	(switch-to-buffer (list-buffers-noselect nil))
	(delete-other-windows)
	(setq buffer-read-only nil)
	(goto-char (point-min))
	(while (not (eobp))
	  (if (re-search-forward tramp-buf-regexp (tramp-line-end-position) t)
	      (forward-line 1)
	    (forward-line 0)
	    (let ((start (point)))
	      (forward-line 1)
	      (kill-region start (point)))))
	(insert "
The buffer(s) above will be appended to this message.  If you
don't want to append a buffer because it contains sensible data,
or because the buffer is too large, you should delete the
respective buffer.  The buffer(s) will contain user and host
names.  Passwords will never be included there.")

	(when (and tramp-debug-buffer (>= tramp-verbose 9))
	  (insert "\n\n")
	  (let ((start (point)))
	    (insert "\
Please note that you have set `tramp-verbose' to a value of at
least 9.  Therefore, the contents of files might be included in
the debug buffer(s).")
	    (add-text-properties start (point) (list 'face 'italic))))

	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(goto-char (point-min))

	(if (y-or-n-p "Do you want to append the buffer(s)? ")
	    ;; OK, let's send.  First we delete the buffer list.
	    (progn
	      (kill-buffer nil)
	      (switch-to-buffer curbuf)
	      (goto-char (point-max))
	      (insert "\n\n")
	      (dolist (buffer buffer-list)
		(funcall (symbol-function 'mml-insert-empty-tag)
			 'part 'type "text/plain" 'encoding "base64"
			 'disposition "attachment" 'buffer (buffer-name buffer)
			 'description (buffer-name buffer)))
	      (set-buffer-modified-p nil))

	  ;; Don't send.  Delete the message buffer.
	  (set-buffer curbuf)
	  (set-buffer-modified-p nil)
	  (kill-buffer nil)
	  (throw 'dont-send nil))))))

(defalias 'tramp-submit-bug 'tramp-bug)

(provide 'tramp)

;; Make sure that we get integration with the VC package.
;; When it is loaded, we need to pull in the integration module.
;; This must come after (provide 'tramp) because tramp-vc.el
;; requires tramp.
(eval-after-load "vc"
  '(require 'tramp-vc))

;;; TODO:

;; * Allow putting passwords in the filename.
;;   This should be implemented via a general mechanism to add
;;   parameters in filenames.  There is currently a kludge for
;;   putting the port number into the filename for ssh and ftp
;;   files.  This could be subsumed by the new mechanism as well.
;;   Another approach is to read a netrc file like ~/.authinfo
;;   from Gnus.
;; * Handle nonlocal exits such as C-g.
;; * Autodetect if remote `ls' groks the "--dired" switch.
;; * Add fallback for inline encodings.  This should be used
;;   if the remote end doesn't support mimencode or a similar program.
;;   For reading files from the remote host, we can just parse the output
;;   of `od -b'.  For writing files to the remote host, we construct
;;   a shell program which contains only "safe" ascii characters
;;   and which writes the right bytes to the file.  We can use printf(1)
;;   or "echo -e" or the printf function in awk and use octal escapes
;;   for the "dangerous" characters.  The null byte might be a problem.
;;   On some systems, the octal escape doesn't work.  So we try the following
;;   two commands to write a null byte:
;;   dd if=/dev/zero bs=1 count=1
;;   echo | tr '\n' '\000'
;; * Cooperate with PCL-CVS.  It uses start-process, which doesn't
;;   work for remote files.
;; * Rewrite `tramp-shell-quote-argument' to abstain from using
;;   `shell-quote-argument'.
;; * Completion gets confused when you leave out the method name.
;; * In Emacs 21, `insert-directory' shows total number of bytes used
;;   by the files in that directory.  Add this here.
;; * Avoid screen blanking when hitting `g' in dired.  (Eli Tziperman)
;; * Make ffap.el grok Tramp filenames.  (Eli Tziperman)
;; * When logging in, keep looking for questions according to an alist
;;   and then invoke the right function.
;; * Case-insensitive filename completion.  (Norbert Goevert.)
;; * Running CVS remotely doesn't appear to work right.  It thinks
;;   files are locked by somebody else even if I'm the locking user.
;;   Sometimes, one gets `No CVSROOT specified' errors from CVS.
;;   (Skip Montanaro)
;; * Don't use globbing for directories with many files, as this is
;;   likely to produce long command lines, and some shells choke on
;;   long command lines.
;; * `vc-directory' does not work.  It never displays any files, even
;;   if it does show files when run locally.
;; * Allow correction of passwords, if the remote end allows this.
;;   (Mark Hershberger)
;; * How to deal with MULE in `insert-file-contents' and `write-region'?
;; * Grok `append' parameter for `write-region'.
;; * Test remote ksh or bash for tilde expansion in `tramp-find-shell'?
;; * abbreviate-file-name
;; * grok ~ in tramp-remote-path  (Henrik Holm <henrikh@tele.ntnu.no>)
;; * better error checking.  At least whenever we see something
;;   strange when doing zerop, we should kill the process and start
;;   again.  (Greg Stark)
;; * Provide a local cache of old versions of remote files for the rsync
;;   transfer method to use.  (Greg Stark)
;; * Remove unneeded parameters from methods.
;; * Invoke rsync once for copying a whole directory hierarchy.
;;   (Francesco Potort,Al(B)
;; * Should we set PATH ourselves or should we rely on the remote end
;;   to do it?
;; * Make it work for different encodings, and for different file name
;;   encodings, too.  (Daniel Pittman)
;; * Change applicable functions to pass a struct tramp-file-name rather
;;   than the individual items METHOD, USER, HOST, LOCALNAME.
;; * Clean up unused *tramp/foo* buffers after a while.  (Pete Forman)
;; * Progress reports while copying files.  (Michael Kifer)
;; * `Smart' connection method that uses inline for small and out of
;;   band for large files.  (Michael Kifer)
;; * Don't search for perl5 and perl.  Instead, only search for perl and
;;   then look if it's the right version (with `perl -v').
;; * When editing a remote CVS controlled file as a different user, VC
;;   gets confused about the file locking status.  Try to find out why
;;   the workaround doesn't work.
;; * Change `copy-file' to grok the case where the filename handler
;;   for the source and the target file are different.  Right now,
;;   it looks at the source file and then calls that handler, if
;;   there is one.  But since ange-ftp, for instance, does not know
;;   about Tramp, it does not do the right thing if the target file
;;   name is a Tramp name.
;; * Username and hostname completion.
;; ** If `partial-completion-mode' isn't loaded, "/foo:bla" tries to
;;    connect to host "blabla" already if that host is unique. No idea
;;    how to suppress. Maybe not an essential problem.
;; ** Try to avoid usage of `last-input-event' in `tramp-completion-mode'.
;; ** Extend `tramp-get-completion-su' for NIS and shadow passwords.
;; ** Unify `tramp-parse-{rhosts,shosts,sconfig,hosts,passwd,netrc}'.
;;    Code is nearly identical.
;; ** Decide which files to take for searching user/host names depending on
;;    operating system (windows-nt) in `tramp-completion-function-alist'.
;; ** Enhance variables for debug.
;; ** Add a learning mode for completion. Make results persistent.
;; * Allow out-of-band methods as _last_ multi-hop.

;; Functions for file-name-handler-alist:
;; diff-latest-backup-file -- in diff.el
;; dired-uncache -- this will be needed when we do insert-directory caching
;; file-name-as-directory -- use primitive?
;; file-name-sans-versions -- use primitive?
;; get-file-buffer -- use primitive
;; vc-registered

;;; arch-tag: 3a21a994-182b-48fa-b0cd-c1d9fede424a
;;; tramp.el ends here
