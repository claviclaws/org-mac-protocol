;;; org-annotation-quicksilver --- OS X only. Allows creation of org
;;; notes from other applications via Quicksilver and remember-mode.
;;
;; Author: Christopher Suckling: suckling AT gmail DOT com
;;
;; Version: 0.609
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

(defun org-annotation-helper-remember (theLink &optional theText theTemplate noAnnotation)
  "Process an externally passed remember:// style url.

This function will initialise a *remember* buffer for further annotation, passing a link and the selection as initial content for the buffer.

URLSTRING consists of a protocol part and a url and title,
separated by ::remember::

The protocol types currently recognized are:

remember://     start `remember' with the url, title and selection (if any).
annotation://   squirrel away a link of the form [[url][title]] that can
                be used later with \\[org-insert-link]."
  (interactive)
  (let ((remember-annotation-functions nil))
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
	       orglink)
	  (setq title (if (> (length title) 0) (url-unhex-string title)))
	  (setq orglink (org-make-link-string url title))
	  (org-store-link-props :annotation orglink			      
				:initial region
				:type type
				:link url
				:region region
				:description title)
	  (if (string= noAnnotation "true")
	      (progn (plist-put org-store-link-plist :annotation "")
	       (plist-put org-store-link-plist :initial nil)))
	  (setq org-stored-links
		(cons (list url title) org-stored-links))
	  ;; Inelegant access of %a and %i in templates: requires
	  ;; duplication of two org-remember functions
	  (raise-frame)
	  (cond ((equal proto "remember")
		 (let* ((remember-mode-hook 'org-remember-apply-template-qs))
		   (org-remember-qs nil theTemplate)))
		((equal proto "annotation")
		 (message "Copied '%s' to the kill-ring." orglink)
		 (kill-new orglink))
		(t (error "unrecognized org-helper protocol"))))
      (error "could not parse argument")))
  )


(defun org-annotation-helper-file (theLink &optional theText theTemplate noAnnotation)
  "Process an externally passed remember:// style url.

This function will initalise then file a *remember* buffer
containing the link, selected content and supplied text without
any further prompting.

URLSTRING consists of a protocol part and a url and title,
separated by ::remember::

The protocol types currently recognized are:

remember://     start `remember' with the url, title and selection (if any).
annotation://   squirrel away a link of the form [[url][title]] that can
                be used later with \\[org-insert-link]."
  (interactive)
  (let ((remember-annotation-functions nil))
    ;; The `parse-url' functions break on the embedded url,
    ;; since our format is fixed we'll split the url ourselves.
    (if (string-match  "^\\([^:]*\\):\\(/*\\)\\(.*\\)" theLink)
	(let* ((protocol (match-string 1 theLink))
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
	  (org-store-link-props :annotation orglink
				:initial regtext
				:type type
				:link url
				:region regtext
				:description title)
	  (if (string= noAnnotation "true")
	      (plist-put org-store-link-plist :annotation ""))	  
	  (setq org-stored-links
		(cons (list url title) org-stored-links))
	  ;; Inelegant access of %a and %i in templates: requires
	  ;; duplication of two org-remember functions
	  (raise-frame)
	  (cond ((equal protocol "remember")
		 (let* ((remember-mode-hook 'org-remember-apply-template-qs))
		   (org-remember-qs nil theTemplate))
		 (org-ctrl-c-ctrl-c))
		((equal protocol "annotation")
		 (message "Copied '%s' to the kill-ring." orglink)
		 (kill-new orglink))
		(t (error "unrecognized org-helper protocol"))))
      (error "could not parse argument")))
  )

(defun org-remember-apply-template-qs (&optional use-char skip-interactive)
  "Initialize *remember* buffer with template, invoke `org-mode'.
This function should be placed into `remember-mode-hook' and in fact requires
to be run from that hook to function properly.

Differs from org-mode's org-remember-apply-template in that it
doesn't override org-store-link-plist :annotation and :initial with
remember-mode's values. It extracts values for these properties from
the plist created by the org-annotation-helper- functions."
  (when (and (boundp 'initial) (stringp initial))
    (setq initial (org-no-properties initial))
    (remove-text-properties 0 (length initial) '(read-only t) initial))
  (if org-remember-templates
      (let* ((entry (org-select-remember-template use-char))
	     (ct (or org-overriding-default-time (org-current-time)))
	     (dct (decode-time ct))
	     (ct1
	      (if (< (nth 2 dct) org-extend-today-until)
		  (encode-time 0 59 23 (1- (nth 3 dct)) (nth 4 dct) (nth 5 dct))
		ct))
	     (tpl (car entry))
	     (plist-p (if org-store-link-plist t nil))
	     (file (if (and (nth 1 entry) 
			    (or (and (stringp (nth 1 entry))
				     (string-match "\\S-" (nth 1 entry)))
				(functionp (nth 1 entry))))
		       (nth 1 entry)
		     org-default-notes-file))
	     (headline (nth 2 entry))
	     (v-c (and (> (length kill-ring) 0) (current-kill 0)))
	     (v-x (or (org-get-x-clipboard 'PRIMARY)
		      (org-get-x-clipboard 'CLIPBOARD)
		      (org-get-x-clipboard 'SECONDARY)))
	     (v-t (format-time-string (car org-time-stamp-formats) ct))
	     (v-T (format-time-string (cdr org-time-stamp-formats) ct))
	     (v-u (concat "[" (substring v-t 1 -1) "]"))
	     (v-U (concat "[" (substring v-T 1 -1) "]"))
	     ;; `initial' and `annotation' are bound in `remember'
	     (v-i (if (boundp 'initial) initial))
	     (v-a (if (and (boundp 'annotation) annotation)
		      (if (equal annotation "[[]]") "" annotation)
		    ""))
	     (clipboards (remove nil (list v-i
					   (org-get-x-clipboard 'PRIMARY)
					   (org-get-x-clipboard 'CLIPBOARD)
					   (org-get-x-clipboard 'SECONDARY)
					   v-c)))
	     (v-A (if (and v-a
			   (string-match "\\[\\(\\[.*?\\]\\)\\(\\[.*?\\]\\)?\\]" v-a))
		      (replace-match "[\\1[%^{Link description}]]" nil nil v-a)
		    v-a))
	     (v-n user-full-name)
	     (v-k (if (marker-buffer org-clock-marker)
		      (substring-no-properties org-clock-heading)))
	     (v-K (if (marker-buffer org-clock-marker)
		      (org-make-link-string
		       (buffer-file-name (marker-buffer org-clock-marker))
		       org-clock-heading)))
	     v-I
	     (org-startup-folded nil)
	     (org-inhibit-startup t)
	     org-time-was-given org-end-time-was-given x
	     prompt completions char time pos default histvar)

	(when (functionp file)
	  (setq file (funcall file)))
	(when (and file (not (file-name-absolute-p file)))
	  (setq file (expand-file-name file org-directory)))
	;; 	(setq org-store-link-plist
	;; 	      (append (list :annotation v-a :initial v-i)
	;; 		      org-store-link-plist))
	(unless tpl (setq tpl "") (message "No template") (ding) (sit-for 1))
	(erase-buffer)
	(if (equal v-a "") 
	    (setq v-a (plist-get org-store-link-plist :annotation)))
	(insert (substitute-command-keys
		 (format
"## %s  \"%s\" -> \"* %s\"
## C-u C-c C-c  like C-c C-c, and immediately visit note at target location
## C-0 C-c C-c  \"%s\" -> \"* %s\"
## %s  to select file and header location interactively.
## C-2 C-c C-c  as child of the currently clocked item
## To switch templates, use `\\[org-remember]'.  To abort use `C-c C-k'.\n\n"
		  (if org-remember-store-without-prompt "C-1 C-c C-c" "        C-c C-c")
		  (if org-remember-store-without-prompt "    C-c C-c" "    C-1 C-c C-c")
		  (abbreviate-file-name (or file org-default-notes-file))
		  (or headline "")
		  (or (car org-remember-previous-location) "???")
		  (or (cdr org-remember-previous-location) "???")
		  (if org-remember-store-without-prompt "C-1 C-c C-c" "        C-c C-c"))))
	(insert tpl)
	(goto-char (point-min))

	;; Simple %-escapes
	(while (re-search-forward "%\\([tTuUaiAcxkKI]\\)" nil t)
	  (when (and initial (equal (match-string 0) "%i"))
	    (save-match-data
	      (let* ((lead (buffer-substring
			    (point-at-bol) (match-beginning 0))))
		(setq v-i (mapconcat 'identity
				     (org-split-string initial "\n")
				     (concat "\n" lead))))))
	  (replace-match
	   (or (eval (intern (concat "v-" (match-string 1)))) "")
	   t t))

	;; %[] Insert contents of a file.
	(goto-char (point-min))
	(while (re-search-forward "%\\[\\(.+\\)\\]" nil t)
	  (let ((start (match-beginning 0))
		(end (match-end 0))
		(filename (expand-file-name (match-string 1))))
	    (goto-char start)
	    (delete-region start end)
	    (condition-case error
		(insert-file-contents filename)
	      (error (insert (format "%%![Couldn't insert %s: %s]"
				     filename error))))))
	;; %() embedded elisp
	(goto-char (point-min))
	(while (re-search-forward "%\\((.+)\\)" nil t)
	  (goto-char (match-beginning 0))
	  (let ((template-start (point)))
	    (forward-char 1)
	    (let ((result
		   (condition-case error
		       (eval (read (current-buffer)))
		     (error (format "%%![Error: %s]" error)))))
	      (delete-region template-start (point))
	      (insert result))))

	;; From the property list
	(when plist-p
	  (goto-char (point-min))
	  (while (re-search-forward "%\\(:[-a-zA-Z]+\\)" nil t)
	    (and (setq x (or (plist-get org-store-link-plist
					(intern (match-string 1))) ""))
		 (replace-match x t t))))

	;; Turn on org-mode in the remember buffer, set local variables
	(let ((org-inhibit-startup t)) (org-mode))
	(org-set-local 'org-finish-function 'org-remember-finalize)
	(if (and file (string-match "\\S-" file) (not (file-directory-p file)))
	    (org-set-local 'org-default-notes-file file))
	(if headline
	    (org-set-local 'org-remember-default-headline headline))
	;; Interactive template entries
	(goto-char (point-min))
	(while (re-search-forward "%^\\({\\([^}]*\\)}\\)?\\([gGtTuUCLp]\\)?" nil t)
	  (setq char (if (match-end 3) (match-string 3))
		prompt (if (match-end 2) (match-string 2)))
	  (goto-char (match-beginning 0))
	  (replace-match "")
	  (setq completions nil default nil)
	  (when prompt
	    (setq completions (org-split-string prompt "|")
		  prompt (pop completions)
		  default (car completions)
		  histvar (intern (concat
				   "org-remember-template-prompt-history::"
				   (or prompt "")))
		  completions (mapcar 'list completions)))
	  (cond
	   ((member char '("G" "g"))
	    (let* ((org-last-tags-completion-table
		    (org-global-tags-completion-table
		     (if (equal char "G") (org-agenda-files) (and file (list file)))))
		   (org-add-colon-after-tag-completion t)
		   (ins (completing-read
			 (if prompt (concat prompt ": ") "Tags: ")
			 'org-tags-completion-function nil nil nil
			 'org-tags-history)))
	      (setq ins (mapconcat 'identity
				  (org-split-string ins (org-re "[^[:alnum:]_@]+"))
				  ":"))
	      (when (string-match "\\S-" ins)
		(or (equal (char-before) ?:) (insert ":"))
		(insert ins)
		(or (equal (char-after) ?:) (insert ":")))))
	   ((equal char "C")
	    (cond ((= (length clipboards) 1) (insert (car clipboards)))
		  ((> (length clipboards) 1)
		   (insert (read-string "Clipboard/kill value: "
					(car clipboards) '(clipboards . 1)
					(car clipboards))))))
	   ((equal char "L")
	    (cond ((= (length clipboards) 1)
		   (org-insert-link 0 (car clipboards)))
		  ((> (length clipboards) 1)
		   (org-insert-link 0 (read-string "Clipboard/kill value: "
						   (car clipboards)
						   '(clipboards . 1)
						   (car clipboards))))))
	   ((equal char "p")
	    (let*
		((prop (substring-no-properties prompt))
		 (allowed (with-current-buffer
			      (get-buffer (file-name-nondirectory file))
			    (org-property-get-allowed-values nil prop 'table)))
		 (existing (with-current-buffer
			       (get-buffer (file-name-nondirectory file))
			     (mapcar 'list (org-property-values prop))))
		 (propprompt (concat "Value for " prop ": "))
		 (val (if allowed
			  (org-completing-read propprompt allowed nil
					       'req-match)
			(org-completing-read propprompt existing nil nil
					     "" nil ""))))
	      (org-set-property prop val)))
	   (char
	    ;; These are the date/time related ones
	    (setq org-time-was-given (equal (upcase char) char))
	    (setq time (org-read-date (equal (upcase char) "U") t nil
				      prompt))
	    (org-insert-time-stamp time org-time-was-given
				   (member char '("u" "U"))
				   nil nil (list org-end-time-was-given)))
	   (t
	    (insert (org-completing-read
		     (concat (if prompt prompt "Enter string")
			     (if default (concat " [" default "]"))
			     ": ")
		     completions nil nil nil histvar default)))))
	(goto-char (point-min))
	(if (re-search-forward "%\\?" nil t)
	    (replace-match "")
	  (and (re-search-forward "^[^#\n]" nil t) (backward-char 1))))
    (let ((org-inhibit-startup t)) (org-mode))
    (org-set-local 'org-finish-function 'org-remember-finalize))
  (when (save-excursion
	  (goto-char (point-min))
	  (re-search-forward "%&" nil t))
    (replace-match "")
    (org-set-local 'org-jump-to-target-location t))
  (when (save-excursion
	  (goto-char (point-min))
	  (re-search-forward "%!" nil t))
    (replace-match "")
    (add-hook 'post-command-hook 'org-remember-finish-immediately 'append)))

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
    (if (eq org-finish-function 'org-remember-finalize)
	(progn
	  (when (< (length org-remember-templates) 2)
	    (error "No other template available"))
	  (erase-buffer)
	  (let ((annotation (plist-get org-store-link-plist :annotation))
		(initial (plist-get org-store-link-plist :initial)))
	    (org-remember-apply-template-qs))
	  (message "Press C-c C-c to remember data"))
      (if (org-region-active-p)
	  (org-do-remember (buffer-substring (point) (mark)))
	(let ((annotation (plist-get org-store-link-plist :annotation))
	      (initial (plist-get org-store-link-plist :initial)))	  
	  (org-do-remember initial)))))))


(provide 'org-annotation-quicksilver)