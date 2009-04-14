;;; icicles-face.el --- Faces for Icicles
;;
;; Filename: icicles-face.el
;; Description: Faces for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2007, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:19:43 2006
;; Version: 22.0
;; Last-Updated: Sun Feb 04 10:55:19 2007 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 244
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-face.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  customization groups and faces.  See `icicles.el' for
;;  documentation.
;;
;;  Groups defined here:
;;
;;    `Icicles', `Icicles-Buffers', `Icicles-Completions-Display',
;;    `Icicles-Key-Bindings', `Icicles-Key-Completion',
;;    `Icicles-Matching', `Icicles-Minibuffer-Display',
;;    `Icicles-Miscellaneous', `Icicles-Searching'.
;;
;;  Faces defined here:
;;
;;    `icicle-common-match-highlight-Completions',
;;    `icicle-complete-input',
;;    `icicle-completing-mustmatch-prompt-prefix',
;;    `icicle-completing-prompt-prefix',
;;    `icicle-Completions-instruction-1',
;;    `icicle-Completions-instruction-2',
;;    `icicle-current-candidate-highlight',
;;    `icicle-historical-candidate',
;;    `icicle-match-highlight-Completions',
;;    `icicle-match-highlight-minibuffer', `icicle-prompt-suffix',
;;    `icicle-search-current-input',
;;    `icicle-search-main-regexp-current',
;;    `icicle-search-main-regexp-others', `icicle-special-candidate',
;;    `icicle-whitespace-highlight', `minibuffer-prompt'.

 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Change log")
;;  (@> "Groups, organized alphabetically")
;;  (@> "Faces, organized alphabetically")

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;(@* "Change log")
;;
;; 2006/12/22 dadams
;;    Renamed group icicles to Icicles.
;;    Added Icicles subgroups, and assigned them instead of group Icicles:
;;      -Buffers, -Completions-Display, -Key-Bindings, -Key-Completion, -Matching,
;;      -Minibuffer-Display, -Miscellaneous, -Searching.
;; 2006/11/06 dadams
;;    icicle-search-highlight-all-flag -> icicle-search-highlight-threshold (integer)
;; 2006/10/16 dadams
;;     icicle-special-candidate: changed background from Pink to #DB17FFF4E581.
;; 2006/10/04 dadams
;;     Added: icicle-special-candidate.
;; 2006/08/13 dadams
;;     Added: icicle-completing-prompt-prefix.
;; 2006/07/16 dadams
;;     Added dark-background face suggestions from Le Wang - thx.
;; 2006/06/30 dadams
;;     Added: minibuffer-prompt for Emacs < 22 (e.g. Emacs 21.4 has propertize).
;; 2006/04/28 dadams
;;     Added: icicle-whitespace-highlight.
;; 2006/04/14 dadams
;;     Renamed icicle-search-refined-regexp to icicle-search-current-input.
;; 2006/04/07 dadams
;;     Added: icicle-search-main-regexp-others.
;;     Renamed: icicle-search-main-regexp to icicle-search-main-regexp-current.
;; 2006/03/27 dadams
;;     Added: icicle-search-*-regexp.
;; 2006/03/22 dadams
;;     Renamed: icicle-root-highlight-* to icicle-match-highlight-*.
;; 2006/03/21 dadams
;;     Added: icicle-common-match-highlight-Completions.
;;     icicle-root-highlight-Completions: Changed default face.
;; 2006/03/08 dadams
;;     Added: icicle-current-candidate-highlight.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
;;(@* "Groups, organized alphabetically")

;;; Groups, organized alphabetically ---------------------------------

(defgroup Icicles nil
  "Minibuffer input completion and cycling of completion candidates."
  :prefix "icicle-"
  :group 'completion :group 'convenience :group 'help :group 'apropos
  :group 'dabbrev :group 'matching :group 'minibuffer :group 'recentf
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Commentary" "icicles")
  )

(defgroup Icicles-Buffers nil
  "Icicles preferences related to buffers."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Commentary" "icicles")
  )

(defgroup Icicles-Completions-Display nil
  "Icicles preferences related to display of completion candidates."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and lIcicles ibrary versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Commentary" "icicles")
  )

(defgroup Icicles-Key-Bindings nil
  "Icicles preferences related to key bindings."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Commentary" "icicles")
  )

(defgroup Icicles-Key-Completion nil
  "Icicles preferences related to key completion (`icicle-complete-keys')."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Commentary" "icicles")
  )

(defgroup Icicles-Matching nil
  "Icicles preferences related to matching input for completion."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Commentary" "icicles")
  )

(defgroup Icicles-Minibuffer-Display nil
  "Icicles preferences related to minibuffer display during completion."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Commentary" "icicles")
  )

(defgroup Icicles-Miscellaneous nil
  "Miscellaneous Icicles preferences."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Commentary" "icicles")
  )

(defgroup Icicles-Searching nil
  "Icicles preferences related to searching."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/icicles.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Icicles")
  :link '(emacs-commentary-link :tag "Commentary" "icicles")
  )

 
;;(@* "Faces, organized alphabetically")

;;; Faces, organized alphabetically ----------------------------------

(defface icicle-common-match-highlight-Completions '((t (:foreground "magenta3")))
  "*Face used to highlight candidates common match, in *Completions*."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-complete-input
  '((((background dark)) (:background "CadetBlue"))
    (t (:foreground "DarkGreen")))
  "*Face used to highlight input when it is complete."
  :group 'Icicles-Minibuffer-Display :group 'faces)

(defface icicle-completing-mustmatch-prompt-prefix
    '((((type x w32 mac graphic) (class color))
       (:box (:line-width 1 :color "Cyan") :foreground "Cyan" :background "Red"))
      (t (:inverse-video t)))
  "*Face used to highlight `icicle-completing-mustmatch-prompt-prefix'.
This highlighting is done in the minibuffer.
Not used for versions of Emacs before version 21."
  :group 'Icicles-Minibuffer-Display :group 'faces)

(defface icicle-completing-prompt-prefix
    '((((type x w32 mac graphic) (class color))
       (:box (:line-width 1 :color "Red") :foreground "Red" :background "Cyan"))
      (t (:inverse-video t)))
  "*Face used to highlight `icicle-completing-prompt-prefix'.
This highlighting is done in the minibuffer.
Not used for versions of Emacs before version 21."
  :group 'Icicles-Minibuffer-Display :group 'faces)

(defface icicle-Completions-instruction-1
  '((((background dark)) (:foreground "DeepSkyBlue"))
    (t (:foreground "Blue")))
  "*Face used to highlight first line of *Completions* buffer."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-Completions-instruction-2 '((t (:foreground "Red")))
  "*Face used to highlight second line of *Completions* buffer."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-current-candidate-highlight
  '((((background dark)) (:background "CadetBlue"))
    (t (:background "CadetBlue1")))
  "*Face used to highlight the current candidate, in *Completions*."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-historical-candidate
  '((((background dark)) (:background "Blue" :foreground "White"))
    (t (:foreground "Blue")))
  "*Face used to highlight *Completions* candidates that have been used."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-match-highlight-Completions '((t (:foreground "Red3")))
  "*Face used to highlight root that was completed, in *Completions*."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-match-highlight-minibuffer '((t (:underline t)))
  "*Face used to highlight root that was completed, in minibuffer."
  :group 'Icicles-Minibuffer-Display :group 'faces)

(defface icicle-prompt-suffix
  '((((type x w32 mac graphic) (class color) (background dark))
     (:box (:line-width 2 :style pressed-button) :foreground "DarkSlateBlue"))
    (((type x w32 mac graphic) (class color))
     (:box (:line-width 2 :style pressed-button) :foreground "DarkBlue"))
    (t (:inverse-video t)))
  "*Face used to highlight `icicle-prompt-suffix', in the minibuffer."
  :group 'Icicles-Minibuffer-Display :group 'faces)

(defface icicle-search-current-input '((t (:foreground "Black" :background "Green")))
  "*Face used to highlight what your current input matches.
This highlighting is during Icicles searching."
  :group 'Icicles-Searching :group 'faces)

(defface icicle-search-main-regexp-current
  '((((background dark)) (:background "DodgerBlue"))
    (t (:background "misty rose")))
  "*Face used to highlight current match of your original regexp.
This highlighting is during Icicles searching."
  :group 'Icicles-Searching :group 'faces)

(defface icicle-search-main-regexp-others
  '((((background dark)) (:background "SeaGreen"))
    (t (:background "CadetBlue1")))
  "*Face used to highlight other matches of your original regexp.
If user option `icicle-search-highlight-threshold' is less than one,
then this face is not used.
This highlighting is during Icicles searching."
  :group 'Icicles-Searching :group 'faces)

(defface icicle-special-candidate
    '((((background dark)) (:background "#DB17FFF4E581")) ; A pale green.
      (t (:background "#DB17FFF4E581")))
  "*Face used to highlight *Completions* candidates that are special.
The meaning of special is that their names match
`icicle-special-candidate-regexp'."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-whitespace-highlight '((t (:background "Magenta")))
  "*Face used to highlight initial whitespace in minibuffer input."
  :group 'Icicles-Minibuffer-Display :group 'faces)

;; This is defined in `faces.el', Emacs 22.  This is for Emacs < 22.  This is used
;; only for versions of Emacs that have `propertize' but don't have this face.
(unless (facep 'minibuffer-prompt)
  (defface minibuffer-prompt '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
    "Face for minibuffer prompts."
    :group 'basic-faces))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-face.el ends here
