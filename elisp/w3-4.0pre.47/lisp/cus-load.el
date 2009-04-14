;;; cus-load.el --- automatically extracted custom dependencies
;;
;;; Code:

(put 'w3-printing 'custom-loads '("w3-cus"))
(put 'w3-menus 'custom-loads '("w3-cus" "w3-menu"))
(put 'w3-java 'custom-loads '("w3-java"))
(put 'w3-images 'custom-loads '("w3-cus" "w3-display"))
(put 'url-news 'custom-loads '("url-news"))
(put 'url-history 'custom-loads '("url-vars"))
(put 'w3-hooks 'custom-loads '("w3-cus"))
(put 'w3-parsing 'custom-loads '("w3-cus"))
(put 'url 'custom-loads '("url-vars" "url" "url-gw" "url-irc" "url-news"))
(put 'url-cache 'custom-loads '("url-vars" "url-cache"))
(put 'url-gateway 'custom-loads '("url-gw"))
(put 'w3-advanced 'custom-loads '("w3-cus"))
(put 'ssl 'custom-loads '("ssl"))
(put 'url-mime 'custom-loads '("url-vars"))
(put 'url-hairy 'custom-loads '("url-vars"))
(put 'w3 'custom-loads '("url-vars" "w3-cus" "w3-script" "w3-java"))
(put 'i18n 'custom-loads '("url-vars"))
(put 'comm 'custom-loads '("ssl"))
(put 'url-cookie 'custom-loads '("url-vars" "url-cookie"))
(put 'hypermedia 'custom-loads '("url-vars" "w3-cus"))
(put 'w3-scripting 'custom-loads '("w3-script"))
(put 'socks 'custom-loads '("socks"))
(put 'faces 'custom-loads '("font"))
(put 'url-file 'custom-loads '("url-vars" "url-cache"))
(put 'w3-files 'custom-loads '("w3-cus"))
(put 'w3-display 'custom-loads '("w3-cus"))
(put 'processes 'custom-loads '("socks"))
;;; These are for handling :version.  We need to have a minimum of
;;; information so `custom-changed-variables' could do its job.  
;;; For both groups and variables we have to set `custom-version'.
;;; For variables we also set the `standard-value' and for groups
;;; `group-documentation' (which is shown in the customize buffer), so
;;; we don't have to load the file containing the group.

;;; `custom-versions-load-alist' is an alist that has as car a version
;;; number and as elts the files that have variables that contain that
;;; version. These files should be loaded before showing the
;;; customization buffer that `customize-changed-options' generates.


;;; This macro is used so we don't modify the information about
;;; variables and groups if it's already set. (We don't know when
;;; cus-load.el is going to be loaded and at that time some of the
;;; files might be loaded and some others might not).
(defmacro custom-put-if-not (symbol propname value)
  `(unless (get ,symbol ,propname)
     (put ,symbol ,propname ,value)))


(defvar custom-versions-load-alist nil
 "For internal use by custom.")

(provide 'cus-load)

;;; Local Variables:
;;; version-control: never
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:
;;; cus-load.el ends here
