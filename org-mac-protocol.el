;; Credit: Jack Moffitt http://metajack.im/2008/12/30/gtd-capture-with-emacs-orgmode/
;; org-protocol

(add-to-list 'org-protocol-protocol-alist
	     '("org-mac-remember"
	       :protocol "mac-remember"
	       :function org-mac-protocol-remember
	       :kill-client t))

(add-to-list 'org-remember-templates
	     '("AppleScript remember" ?y "* %?\n\n  Source: %u, %c\n\n  %i" "~/Sites/org/inbox.org" "Notes"))
(add-to-list 'org-remember-templates
	     '("AppleScript note" ?z "* %?\n\n  Source: %u\n" "~/Sites/org/inbox.org" "Notes"))

(defvar org-mac-protocol-app nil)

(defadvice remember-finalize (after delete-remember-frame activate)  
    "Advise remember-finalize to close the frame if it is the mac-remember frame"  
    (when (equal "*mac-remember*" (frame-parameter nil 'name))
      (mapc
       (lambda (x)
	 (when (not (equal (frame-parameter x 'name) "*mac-remember*"))
	   (make-frame-visible x)))
       (frame-list))
      (mapc
       (lambda (x)
	 (when (equal (frame-parameter x 'name) "*mac-remember*")
	   (delete-frame x)))
       (frame-list))
      (do-applescript
       (concat
	"tell application \"" org-mac-protocol-app "\"\n"
	"activate\n"
	"end tell"))))  

  (defadvice remember-destroy (after delete-remember-frame activate)  
    "Advise remember-destroy to close the frame if it is the mac-rememeber frame"  
    (when (equal "*mac-remember*" (frame-parameter nil 'name))
      (mapc
       (lambda (x)
	 (when (not (equal (frame-parameter x 'name) "*mac-remember*"))
	   (make-frame-visible x)))
       (frame-list))  
      (mapc
       (lambda (x)
	 (when (equal (frame-parameter x 'name) "*mac-remember*")
	   (delete-frame x)))
       (frame-list))
      (do-applescript
       (concat
	"tell application \"" org-mac-protocol-app "\"\n"
	"activate\n"
	"end tell"))))  

  (defadvice org-remember-apply-template (before delete-non-remember-windows activate)
    "Advise org-remember to delete non-remember windows if it is
    in the remember frame"
    (when (equal "*mac-remember*" (frame-parameter nil 'name))
      (delete-other-windows)))
  

(defun org-mac-protocol-remember (data)
  "Process an org-protocol://as-remember:// style url.
Pops up a small Emacs frame containing a remember buffer. Calls
org-protocol-remember. Destroys the frame after the remember
buffer is filed."

  (let* ((parts (org-protocol-split-data data nil ":"))
	 (info (car parts))
	 (app (org-protocol-unhex-string (cadr parts))))

    (setq org-mac-protocol-app app)

    (make-frame '((name . "*mac-remember*") (width . 80) (height . 20)))
    (select-frame-by-name "*mac-remember*")
    (let ((junk (mapc
		 (lambda (x)
		   (when (not (equal (frame-parameter x 'name) "*mac-remember*"))
		     (make-frame-invisible x)))
		 (frame-list)))))
  
    (do-applescript
     (concat
      "tell application \"Emacs\"\n"
      "activate\n"
      "end tell"))

    ;;  (frame-center) http://emacsbugs.donarmstrong.com/cgi-bin/bugreport.cgi?bug=3233
    ;; (frame-center)
    
    (org-protocol-remember info))
  nil)

(provide 'org-mac-protocol.el)