;;; -*- coding: iso-2022-7bit; -*-
;;; tramp-util.el --- Misc utility functions to use with Tramp

;; Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Kai Gro,A_(Bjohann <kai.grossjohann@gmx.net>
;;         Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, extensions, processes

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Some misc. utility functions that might go nicely with Tramp.
;; Mostly, these are kluges awaiting real solutions later on.

;;; Code:

(eval-when-compile (require 'cl))
(require 'tramp)

;; Define a Tramp minor mode. It's intention is to redefine some keys
;; for Tramp specific functions, like compilation.

(defvar tramp-minor-mode-map (make-sparse-keymap)
  "Keymap for Tramp minor mode.")

(define-minor-mode tramp-minor-mode "Tramp minor mode for utility functions."
  :group 'tramp
  :global nil
  :init-value nil
  :lighter " Tramp"
  :keymap tramp-minor-mode-map
  (setq tramp-minor-mode
	(and tramp-minor-mode (tramp-tramp-file-p default-directory))))

(add-hook 'find-file-hooks 'tramp-minor-mode t)
(add-hook 'dired-mode-hook 'tramp-minor-mode t)

(defun tramp-remap-command (old-command new-command)
  "Replaces bindings of OLD-COMMAND by NEW-COMMAND.
If remapping functionality for keymaps is defined, this happens for all
bindings.  Otherwise, only bindings active during invocation are taken
into account.  XEmacs menubar bindings are not changed by this."
  (if (functionp 'command-remapping)
      ;; Emacs 22
      (eval
       `(define-key tramp-minor-mode-map [remap ,old-command] new-command))
    ;; previous Emacs versions.
    (mapcar
     '(lambda (x)
	(define-key tramp-minor-mode-map x new-command))
     (where-is-internal old-command))))


;; Utility functions.

(unless (tramp-exists-file-name-handler 'start-process "" nil "ls")
  (defadvice start-process
    (around tramp-advice-start-process
	    (name buffer program &rest args)
	    activate)
    "Invoke `tramp-handle-start-process' for Tramp files."
    (if (eq (tramp-find-foreign-file-name-handler default-directory)
	    'tramp-sh-file-name-handler)
	(setq ad-return-value
	      (apply 'tramp-handle-start-process name buffer program args))
      ad-do-it)))

(unless (tramp-exists-file-name-handler 'call-process "ls")
  (defadvice call-process
    (around tramp-advice-call-process
	    (program &optional infile buffer display &rest args)
	    activate)
    "Invoke `tramp-handle-call-process' for Tramp files."
    (if (eq (tramp-find-foreign-file-name-handler default-directory)
	    'tramp-sh-file-name-handler)
	(setq ad-return-value
	      (apply 'tramp-handle-call-process
		     program infile buffer display args))
      ad-do-it)))

(if (not (fboundp 'file-remote-p))
    ;; Emacs 21
    (defalias 'file-remote-p (symbol-function 'tramp-handle-file-remote-p))
  (unless (tramp-exists-file-name-handler 'file-remote-p "/")
    ;; XEmacs 21
    (defadvice file-remote-p
      (around tramp-advice-file-remote-p (filename) activate)
      "Invoke `tramp-handle-file-remote-p' for Tramp files."
      (if (eq (tramp-find-foreign-file-name-handler (expand-file-name filename))
	      'tramp-sh-file-name-handler)
	  (setq ad-return-value
		(tramp-handle-file-remote-p filename))
	ad-do-it))))

;; compile.el parses the compilation output for file names.  It
;; expects them on the local machine.  This must be changed.

(add-hook
 'compilation-mode-hook
 '(lambda ()
    (set (make-local-variable 'comint-file-name-prefix)
	 (or (file-remote-p default-directory) ""))))

;; gud.el uses `gud-find-file' for specifying a file name function.
;; In XEmacs 21, 'gud must be required before calling `gdb'.
;; Otherwise, gdb.el is used, which is not supported.

(defun tramp-gud-file-name (filename)
  "Evaluate a file name to be loaded.
If it is an absolute file name, and not a remote one, prepend the remote part."
  (let ((filename (expand-file-name filename)))
    (setq filename
	  (if (file-remote-p filename)
	      ;; It is already expanded.
	      filename
	    ;; Prefix the Tramp remote file name.
	    (concat (file-remote-p default-directory) filename)))

    ;; Emacs 22 uses `gud-file-name' which we should do as well.
    ;; `gud-<MINOR-MODE>-directories' must be Tramp file names.
    (if (functionp 'gud-file-name)
	(funcall 'gud-file-name filename)
      filename)))

(defun tramp-gud-massage-args (args)
  "Set arguments of debugger on remote hosts.
They must be changed to be relative to the default directory.
Works only for relative file names and Tramp file names."
  (let ((default-directory (expand-file-name default-directory))
	(item args)
	file)
    (while (car item)
      ;; The expansion is performed for EVERY parameter, even for
      ;; non file names.  But this doesn't hurt, because it is
      ;; changed back to its original value afterwards.
      (setq file (expand-file-name (car item)))
      (when (string-lessp default-directory file)
	(setcar item (substring file (length default-directory))))
      (setq item (cdr item))))
  args)

(defun tramp-gud-setup ()
  (when (functionp 'gud-find-file)
    (set 'gud-find-file 'tramp-gud-file-name))

  (mapcar
   '(lambda (x)

      ;; (X)Emacs 21 use `gud-<MINOR-MODE>-find-file'.
      (eval
       `(defadvice ,(intern (format "gud-%s-find-file" x))
	  (before
	   ,(intern (format "tramp-advice-gud-%s-find-file" x))
	   (filename) activate)
	  "Invoke `tramp-gud-find-file' for Tramp files."
	  (when (eq (tramp-find-foreign-file-name-handler default-directory)
		    'tramp-sh-file-name-handler)
	    (ad-set-arg 0 (tramp-gud-file-name (ad-get-arg 0))))))

      ;; Arguments shall be trimmed to local file names.
      (eval
       `(defadvice ,(intern (format "gud-%s-massage-args" x))
	  (before
	   ,(intern (format "tramp-advice-gud-%s-massage-args" x))
	   (file args) activate)
	  "Invoke `tramp-gud-massage-args' for Tramp files."
	  (when (eq (tramp-find-foreign-file-name-handler
		     (expand-file-name (ad-get-arg 0)))
		    'tramp-sh-file-name-handler)
	    (ad-set-arg 0 (car (tramp-gud-massage-args (list (ad-get-arg 0)))))
	    (ad-set-arg 1 (tramp-gud-massage-args (ad-get-arg 1)))))))

   ;; So far, I've tested only gdb and perldb.
   ;; (X)Emacs
   '(gdb sdb dbx xdb perldb
   ;; Emacs
     pdb jdb
   ;; Emacs 22
     bashdb)))

(eval-after-load "gud" '(tramp-gud-setup))

(provide 'tramp-util)

;;; arch-tag: 500f9992-a44e-46d0-83a7-980799251808
;;; tramp-util.el ends here
