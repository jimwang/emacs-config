
;;;### (autoloads (font-set-face-font x-font-build-cache font-default-size-for-device
;;;;;;  font-default-encoding-for-device font-default-registry-for-device
;;;;;;  font-default-family-for-device font-default-object-for-device
;;;;;;  font-default-font-for-device font-create-object) "font" "font.el"
;;;;;;  (15365 13621))
;;; Generated autoloads from font.el

(autoload (quote font-create-object) "font" nil nil nil)

(autoload (quote font-default-font-for-device) "font" nil nil nil)

(autoload (quote font-default-object-for-device) "font" nil nil nil)

(autoload (quote font-default-family-for-device) "font" nil nil nil)

(autoload (quote font-default-registry-for-device) "font" nil nil nil)

(autoload (quote font-default-encoding-for-device) "font" nil nil nil)

(autoload (quote font-default-size-for-device) "font" nil nil nil)

(autoload (quote x-font-build-cache) "font" nil nil nil)

(autoload (quote font-set-face-font) "font" nil nil nil)

;;;***

;;;### (autoloads (url-register-auth-scheme url-get-authentication)
;;;;;;  "url-auth" "url-auth.el" (15365 13621))
;;; Generated autoloads from url-auth.el

(autoload (quote url-get-authentication) "url-auth" "\
Return an authorization string suitable for use in the WWW-Authenticate
header in an HTTP/1.0 request.

URL    is the url you are requesting authorization to.  This can be either a
       string representing the URL, or the parsed representation returned by
       `url-generic-parse-url'
REALM  is the realm at a specific site we are looking for.  This should be a
       string specifying the exact realm, or nil or the symbol 'any' to
       specify that the filename portion of the URL should be used as the
       realm
TYPE   is the type of authentication to be returned.  This is either a string
       representing the type (basic, digest, etc), or nil or the symbol 'any'
       to specify that any authentication is acceptable.  If requesting 'any'
       the strongest matching authentication will be returned.  If this is
       wrong, its no big deal, the error from the server will specify exactly
       what type of auth to use
PROMPT is boolean - specifies whether to ask the user for a username/password
       if one cannot be found in the cache" nil nil)

(autoload (quote url-register-auth-scheme) "url-auth" "\
Register an HTTP authentication method.

TYPE     is a string or symbol specifying the name of the method.   This
         should be the same thing you expect to get returned in an Authenticate
         header in HTTP/1.0 - it will be downcased.
FUNCTION is the function to call to get the authorization information.  This
         defaults to `url-?-auth', where ? is TYPE
RATING   a rating between 1 and 10 of the strength of the authentication.
         This is used when asking for the best authentication for a specific
         URL.  The item with the highest rating is returned." nil nil)

;;;***

;;;### (autoloads (url-cache-expired url-cache-extract url-is-cached
;;;;;;  url-store-in-cache) "url-cache" "url-cache.el" (15365 13621))
;;; Generated autoloads from url-cache.el

(autoload (quote url-store-in-cache) "url-cache" "\
Store buffer BUFF in the cache" nil nil)

(autoload (quote url-is-cached) "url-cache" "\
Return non-nil if the URL is cached." nil nil)

(autoload (quote url-cache-extract) "url-cache" "\
Extract FNAM from the local disk cache" nil nil)

(autoload (quote url-cache-expired) "url-cache" "\
Return t iff a cached file has expired." nil nil)

;;;***

;;;### (autoloads (url-cookie-handle-set-cookie url-cookie-retrieve
;;;;;;  url-cookie-write-file url-cookie-parse-file) "url-cookie"
;;;;;;  "url-cookie.el" (15365 13621))
;;; Generated autoloads from url-cookie.el

(autoload (quote url-cookie-parse-file) "url-cookie" nil nil nil)

(autoload (quote url-cookie-write-file) "url-cookie" nil nil nil)

(autoload (quote url-cookie-retrieve) "url-cookie" "\
Retrieves all the netscape-style cookies for a specified HOST and PATH" nil nil)

(autoload (quote url-cookie-handle-set-cookie) "url-cookie" nil nil nil)

;;;***

;;;### (autoloads (url-open-stream url-gateway-nslookup-host) "url-gw"
;;;;;;  "url-gw.el" (15365 13621))
;;; Generated autoloads from url-gw.el

(autoload (quote url-gateway-nslookup-host) "url-gw" "\
Attempt to resolve the given HOSTNAME using nslookup if possible." t nil)

(autoload (quote url-open-stream) "url-gw" "\
Open a stream to a host" nil nil)

;;;***

;;;### (autoloads (url-mail) "url-mail" "url-mail.el" (15365 13621))
;;; Generated autoloads from url-mail.el

(autoload (quote url-mail) "url-mail" nil t nil)

;;;***

;;;### (autoloads (url-ns-user-pref url-ns-prefs isInNet isResolvable
;;;;;;  dnsResolve dnsDomainIs isPlainHostName) "url-ns" "url-ns.el"
;;;;;;  (15365 13621))
;;; Generated autoloads from url-ns.el

(autoload (quote isPlainHostName) "url-ns" nil nil nil)

(autoload (quote dnsDomainIs) "url-ns" nil nil nil)

(autoload (quote dnsResolve) "url-ns" nil nil nil)

(autoload (quote isResolvable) "url-ns" nil nil nil)

(autoload (quote isInNet) "url-ns" nil nil nil)

(autoload (quote url-ns-prefs) "url-ns" nil nil nil)

(autoload (quote url-ns-user-pref) "url-ns" nil nil nil)

;;;***

;;;### (autoloads (url-retrieve url-popup-info url-get-url-at-point
;;;;;;  url-do-setup url-setup-save-timer url-buffer-visiting url-normalize-url
;;;;;;  url-file-attributes) "url" "url.el" (15365 13621))
;;; Generated autoloads from url.el

(autoload (quote url-file-attributes) "url" "\
Return a list of attributes of URL.
Value is nil if specified file cannot be opened.
Otherwise, list elements are:
 0. t for directory, string (name linked to) for symbolic link, or nil.
 1. Number of links to file.
 2. File uid.
 3. File gid.
 4. Last access time, as a list of two integers.
  First integer has high-order 16 bits of time, second has low 16 bits.
 5. Last modification time, likewise.
 6. Last status change time, likewise.
 7. Size in bytes. (-1, if number is out of range).
 8. File modes, as a string of ten letters or dashes as in ls -l.
    If URL is on an http server, this will return the content-type if possible.
 9. t iff file's gid would change if file were deleted and recreated.
10. inode number.
11. Device number.

If file does not exist, returns nil." nil nil)

(autoload (quote url-normalize-url) "url" "\
Return a 'normalized' version of URL.  This strips out default port
numbers, etc." nil nil)

(autoload (quote url-buffer-visiting) "url" "\
Return the name of a buffer (if any) that is visiting URL." nil nil)

(autoload (quote url-setup-save-timer) "url" "\
Reset the history list timer." t nil)

(autoload (quote url-do-setup) "url" "\
Do setup - this is to avoid conflict with user settings when URL is
dumped with emacs." nil nil)

(autoload (quote url-get-url-at-point) "url" "\
Get the URL closest to point, but don't change your
position. Has a preference for looking backward when not
directly on a symbol." nil nil)

(autoload (quote url-popup-info) "url" "\
Retrieve the HTTP/1.0 headers and display them in a temp buffer." nil nil)

(autoload (quote url-retrieve) "url" "\
Retrieve a document over the World Wide Web.
The document should be specified by its fully specified
Uniform Resource Locator.  No parsing is done, just return the
document as the server sent it.  The document is left in the
buffer specified by url-working-buffer.  url-working-buffer is killed
immediately before starting the transfer, so that no buffer-local
variables interfere with the retrieval.  HTTP/1.0 redirection will
be honored before this function exits." nil nil)

;;;***

;;;### (autoloads (w3-about) "w3-about" "w3-about.el" (15365 13621))
;;; Generated autoloads from w3-about.el

(autoload (quote w3-about) "w3-about" nil nil nil)

;;;***

;;;### (autoloads (w3-region) "w3-display" "w3-display.el" (15365
;;;;;;  13621))
;;; Generated autoloads from w3-display.el

(autoload (quote w3-region) "w3-display" "\
Parse and display the region of this buffer between ST and ND." t nil)

;;;***

;;;### (autoloads (w3-do-text-entry w3-form-resurrect-widgets w3-form-add-element)
;;;;;;  "w3-forms" "w3-forms.el" (15365 13621))
;;; Generated autoloads from w3-forms.el

(autoload (quote w3-form-add-element) "w3-forms" nil nil nil)

(autoload (quote w3-form-resurrect-widgets) "w3-forms" nil nil nil)

(autoload (quote w3-do-text-entry) "w3-forms" nil nil nil)

;;;***

;;;### (autoloads (w3-hotlist-add-document w3-hotlist-add-document-at-point
;;;;;;  w3-use-hotlist w3-parse-hotlist w3-hotlist-append w3-hotlist-rename-entry
;;;;;;  w3-hotlist-delete w3-hotlist-refresh w3-hotlist-apropos w3-read-html-bookmarks)
;;;;;;  "w3-hot" "w3-hot.el" (15365 13621))
;;; Generated autoloads from w3-hot.el

(autoload (quote w3-read-html-bookmarks) "w3-hot" "\
Import an HTML file into the Emacs-w3 format." t nil)

(autoload (quote w3-hotlist-apropos) "w3-hot" "\
Show hotlist entries matching REGEXP." t nil)

(autoload (quote w3-hotlist-refresh) "w3-hot" "\
Reload the default hotlist file into memory" t nil)

(autoload (quote w3-hotlist-delete) "w3-hot" "\
Deletes a document from your hotlist file" t nil)

(autoload (quote w3-hotlist-rename-entry) "w3-hot" "\
Rename a hotlist item" t nil)

(autoload (quote w3-hotlist-append) "w3-hot" "\
Append a hotlist to the one in memory" t nil)

(autoload (quote w3-parse-hotlist) "w3-hot" "\
Read in the hotlist specified by FNAME" nil nil)

(autoload (quote w3-use-hotlist) "w3-hot" "\
Possibly go to a link in your W3/Mosaic hotlist.
This is part of the emacs World Wide Web browser.  It will prompt for
one of the items in your 'hotlist'.  A hotlist is a list of often
visited or interesting items you have found on the World Wide Web." t nil)

(autoload (quote w3-hotlist-add-document-at-point) "w3-hot" "\
Add the document pointed to by the hyperlink under point to the hotlist." t nil)

(autoload (quote w3-hotlist-add-document) "w3-hot" "\
Add this documents url to the hotlist" t nil)

;;;***

;;;### (autoloads (w3-hotindex-query w3-hotindex-delete-entry w3-hotindex-rename-entry
;;;;;;  w3-hotindex-rm-key w3-hotindex-add-key) "w3-hotindex" "w3-hotindex.el"
;;;;;;  (15365 13621))
;;; Generated autoloads from w3-hotindex.el

(autoload (quote w3-hotindex-add-key) "w3-hotindex" "\
*Add a keyword to an item in w3-hotindex. Completion is done
on the list of all keywords." t nil)

(autoload (quote w3-hotindex-rm-key) "w3-hotindex" "\
*Remove a keyword from an item of w3-hotindex." t nil)

(autoload (quote w3-hotindex-rename-entry) "w3-hotindex" "\
Renames an entry in the HotIndex. Intended to be called from 
w3-hotlist-rename-entry. OLD should equal the entry to be renamed.
Case is therefore important." nil nil)

(autoload (quote w3-hotindex-delete-entry) "w3-hotindex" "\
Deletes an entry in the HotIndex. Intended to be called from 
w3-hotlist-delete. OLD should equal the entry to be deleted.
Case is therefore important." nil nil)

(autoload (quote w3-hotindex-query) "w3-hotindex" "\
Query the HotIndex for KEY." t nil)

;;;***

;;;### (autoloads (w3-show-dvi w3-parse-tree-to-latex) "w3-latex"
;;;;;;  "w3-latex.el" (15365 13621))
;;; Generated autoloads from w3-latex.el

(autoload (quote w3-parse-tree-to-latex) "w3-latex" nil nil nil)

(autoload (quote w3-show-dvi) "w3-latex" "\
Uses xdvi to show DVI file created from `w3-parse-tree-to-latex'." t nil)

;;;***

;;;### (autoloads (w3-print-url-under-point w3-print-this-url) "w3-print"
;;;;;;  "w3-print.el" (15365 13621))
;;; Generated autoloads from w3-print.el

(autoload (quote w3-print-this-url) "w3-print" "\
Print out the current document (in LaTeX format)" t nil)

(autoload (quote w3-print-url-under-point) "w3-print" "\
Print out the url under point (in LaTeX format)" t nil)

;;;***

;;;### (autoloads (w3-table-setup-keys w3-table-speak-current-table-column)
;;;;;;  "w3-speak-table" "w3-speak-table.el" (15365 13621))
;;; Generated autoloads from w3-speak-table.el

(autoload (quote w3-table-speak-current-table-column) "w3-speak-table" "\
Speak current table column. Prefix arg can be used to specify the desired table nesting." t nil)

(autoload (quote w3-table-setup-keys) "w3-speak-table" "\
Setup emacspeak table browsing keys in w3 mode" nil nil)

;;;***

;;;### (autoloads (w3-display-stylesheet w3-handle-style) "w3-style"
;;;;;;  "w3-style.el" (15365 13621))
;;; Generated autoloads from w3-style.el

(autoload (quote w3-handle-style) "w3-style" nil nil nil)

(autoload (quote w3-display-stylesheet) "w3-style" "\
Display the stylesheet for the current document." t nil)

;;;***

;;;### (autoloads (w3-prev-document w3-next-document w3-follow-link
;;;;;;  w3-follow-link-other-frame w3-do-setup w3 w3-version w3-preview-this-buffer
;;;;;;  w3-follow-url-at-point w3-follow-url-at-point-other-frame
;;;;;;  w3-maybe-follow-link w3-maybe-follow-link-mouse w3-fetch
;;;;;;  w3-fetch-other-frame w3-find-file w3-open-local) "w3" "w3.el"
;;;;;;  (15365 13621))
;;; Generated autoloads from w3.el

(autoload (quote w3-open-local) "w3" "\
Find a local file, and interpret it as a hypertext document.
It will prompt for an existing file or directory, and retrieve it as a
hypertext document." t nil)

(autoload (quote w3-find-file) "w3" "\
Find a local file, and interpret it as a hypertext document.
It will prompt for an existing file or directory, and retrieve it as a
hypertext document." t nil)

(autoload (quote w3-fetch-other-frame) "w3" "\
Attempt to follow the hypertext reference under point in a new frame.
With prefix-arg P, ignore viewers and dump the link straight
to disk." t nil)

(autoload (quote w3-fetch) "w3" "\
Retrieve a document over the World Wide Web.
Defaults to URL of the current document, if any.
With prefix argument, use the URL of the hyperlink under point instead." t nil)

(autoload (quote w3-maybe-follow-link-mouse) "w3" "\
Maybe follow a hypertext link under point.
If there is no link under point, this will try using
url-get-url-at-point" t nil)

(autoload (quote w3-maybe-follow-link) "w3" "\
Maybe follow a hypertext link under point.
If there is no link under point, this will try using
url-get-url-at-point" t nil)

(autoload (quote w3-follow-url-at-point-other-frame) "w3" "\
Follow the URL under PT, defaults to link under (point)" t nil)

(autoload (quote w3-follow-url-at-point) "w3" "\
Follow the URL under PT, defaults to link under (point)" t nil)

(autoload (quote w3-preview-this-buffer) "w3" "\
See what this buffer will look like when its formatted as HTML.
HTML is the HyperText Markup Language used by the World Wide Web to
specify formatting for text.  More information on HTML can be found at
ftp.w3.org:/pub/www/doc." t nil)

(autoload (quote w3-version) "w3" "\
Show the version number of W3 in the minibuffer.
If optional argument HERE is non-nil, insert info at point." t nil)

(autoload (quote w3) "w3" "\
Retrieve the default World Wide Web home page.
The World Wide Web is a global hypertext system started by CERN in
Switzerland in 1991.

The home page is specified by the variable w3-default-homepage.  The
document should be specified by its fully specified Uniform Resource
Locator.  The document will be parsed as HTML (if appropriate) and
displayed in a new buffer." t nil)

(autoload (quote w3-do-setup) "w3" "\
Do setup - this is to avoid conflict with user settings when W3 is
dumped with emacs." nil nil)

(autoload (quote w3-follow-link-other-frame) "w3" "\
Attempt to follow the hypertext reference under point in a new frame.
With prefix-arg P, ignore viewers and dump the link straight
to disk." nil nil)

(autoload (quote w3-follow-link) "w3" "\
Attempt to follow the hypertext reference under point.
With prefix-arg P, ignore viewers and dump the link straight
to disk." t nil)

(autoload (quote w3-next-document) "w3" nil t nil)

(autoload (quote w3-prev-document) "w3" nil t nil)

;;;***

(provide 'w3-auto)
