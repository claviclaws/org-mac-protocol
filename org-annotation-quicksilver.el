;;; org-annotation-quicksilver --- OS X only. Allows creation of org
;;; notes from other applications via Quicksilver and remember-mode.
;;
;; Author: Christopher Suckling: suckling AT gmail DOT com
;;
;; Version: 0.624
;;
;;; Commentary
;; 
;; Opens a *remember* buffer from any AppleScript enabled application,
;; linking to the open document and, were possible, setting the initial
;; content of the *remember* buffer to the document's selected
;; text. Features dynamic selection of remember templates from
;; Quicksilver and transparant creation of org notes without switching
;; focus from the source application.
;;
;; This file is an extended verison of org-annotation-helper.el by
;; [bzg] and [dmg]. It incorporates slightly modified functions from
;; org-remember.el by Carsten Dominik.
;;
;;; Requirements
;;
;; OS X 10.5 (untested on previous versions, but should work) 
;;
;; org-mode
;; remember-mode 
;;
;; org-annotation-quicksilver helper AppleScripts (included in 
;; this archive): 
;; org-link.scpt 
;; org-note.scpt
;; org-remember.scpt 
;; Optional: org-mairix-display.scpt and org-mairix-link.sh (if this 
;; package is not already obscure enough, if you also happen to use
;; mutt in a Termial.app window and use mairix to index your maildir
;; folders, then this may be useful)
;;
;; Quicksilver (http://code.google.com/p/blacktree-alchemy/) 
;;
;;; Installation
;;
;; Copy this file to your load path and byte compile.
;;
;; Add 
;;
;; (require 'org-annotation-quicksilver) 
;;
;; to your .emacs.
;;
;; Please see org-annotation-quicksilver.org for full installation
;; instructions for the helper AppleScripts and *remember* buffer
;; templates.

(require 'url)
(require 'remember)

(autoload 'url-unhex-string "url")

(defun org-qs-remember-annotation ()
  (if (string= noAnnotation "true")
      (progn
	(plist-put org-store-link-plist :annotation "")
	(plist-get org-store-link-plist :annotation))
    (plist-put org-store-link-plist :annotation orglink)
    (plist-get org-store-link-plist :annotation)))

(defun org-annotation-helper-remember (theLink &optional theText theTemplate noAnnotation remember-or-file)
  "Process an externally passed remember:// style url.

This function will initialise a *remember* buffer for further annotation, passing a link and the selection as initial content for the buffer.

URLSTRING consists of a protocol part and a url and title,
separated by ::remember::

The protocol types currently recognized are:

remember://     start `remember' with the url, title and selection (if any).
annotation://   squirrel away a link of the form [[url][title]] that can
                be used later with \\[org-insert-link]."
  (interactive)
  (let ((remember-annotation-functions '(org-qs-remember-annotation)))
    ;; The `parse-url' functions break on the embedded url,
    ;; since our format is fixed we'll split the url ourselves.
    (if (string-match  "^\\([^:]*\\):\\(/*\\)\\(.*\\)" theLink)
	(let* ((proto (match-string 1 theLink))
	       (url_title_region (match-string 3 theLink))
	       (splitparts (split-string url_title_region "::remember::"))
	       (url (url-unhex-string (car splitparts)))
	       (type (if (string-match "^\\([a-z]+\\):" url) 
			 (match-string 1 url)))
	       (title (cadr splitparts))
	       (region (url-unhex-string (caddr splitparts)))
	       (regtext (concat region "\n\n" theText))
	       orglink)
	  (setq title (if (> (length title) 0) (url-unhex-string title)))
	  (setq orglink (org-make-link-string url title))
	  (if (string= remember-or-file "remember")
	      (org-store-link-props :annotation orglink			      
				    :initial region
				    :type type
				    :link url
				    :region region
				    :description title)
	    (org-store-link-props :annotation orglink
				  :initial regtext
				  :type type
				  :link url
				  :region regtext
				  :description title))
	  (if (string= noAnnotation "true")
	      (if (string= remember-or-file "remember")
		  (progn
		    (plist-put org-store-link-plist :annotation "")
		    (plist-put org-store-link-plist :initial nil))
		(plist-put org-store-link-plist :annotation "")))
	  (setq org-stored-links
		(cons (list url title) org-stored-links))
	  ;; Inelegant access of %a and %i in templates: requires
	  ;; duplication of two org-remember functions
	  (raise-frame)
	  (cond ((equal proto "remember")
		 (let* ((remember-mode-hook 'org-remember-apply-template)
			(org-remember-templates 
			 (append '(("QS Inbox" ?q "\n* QS %U%?\n%i\n%a" (concat org-directory "inbox.org") "Inbox")) org-remember-templates)))
		   (org-remember-qs nil theTemplate))
		 (if (string= remember-or-file "file")
		     (org-remember-finalize)))
		((equal proto "annotation")
		 (message "Copied '%s' to the kill-ring." orglink)
		 (kill-new orglink))
		(t (error "unrecognized org-helper protocol"))))
      (error "could not parse argument"))))


(defun org-remember-qs (&optional goto org-force-remember-template-char)
  "Call `remember'.  If this is already a remember buffer, re-apply template.
If there is an active region, make sure remember uses it as initial content
of the remember buffer.

When called interactively with a `C-u' prefix argument GOTO, don't remember
anything, just go to the file/headline where the selected template usually
stores its notes.  With a double prefix arg `C-u C-u', go to the last
note stored by remember.

Lisp programs can set ORG-FORCE-REMEMBER-TEMPLATE-CHAR to a character
associated with a template in `org-remember-templates'.

Differs from org-mode's org-remember by passing the initial
region to remember and calling org-remember-apply-template-qs if
necessary."
  (interactive "P")
  (org-require-remember)
  (cond
   ((equal goto '(4)) (org-go-to-remember-target))
   ((equal goto '(16)) (org-remember-goto-last-stored))
   (t
    ;; set temporary variables that will be needed in
    ;; `org-select-remember-template'
    (setq org-select-template-temp-major-mode major-mode)
    (setq org-select-template-original-buffer (current-buffer))
    (let ((annotation (plist-get org-store-link-plist :annotation))
	  (initial (plist-get org-store-link-plist :initial)))
      (if org-remember-mode
	  (progn
	    (when (< (length org-remember-templates) 2)
	      (error "No other template available"))
	    (erase-buffer)
	    (org-remember-apply-template)
	    (message "Press C-c C-c to remember data"))
	(org-do-remember initial))))))

(provide 'org-annotation-quicksilver)