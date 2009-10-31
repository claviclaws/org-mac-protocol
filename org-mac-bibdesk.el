;;; org-mac-bibdesk.el - Open bibtex links in BibDesk

(require 'org-bibtex)

(org-add-link-type "bibdesk" 'org-mac-bibdesk-open)
;; no need for a store link function as this is dealt with from the
;; BibDesk end? Or should we have a store link function so I can store
;; BibDesk links from a bibtex file aready open in Emacs?
;; Set is as customizable option like in the man example?


(defun org-mac-bibdesk-open (uri)
  "Visit the bibtex entry on PATH in BibDesk"
  (let* ((key (when (string-match "::\\(.+\\)\\'" uri)
		   (match-string 1 uri)))
	 (document (substring path 0 (match-beginning 0))))    
    (do-applescript
     (concat
      "tell application \"BibDesk.app\"\n"
         "activate\n"
         "set theDoc to \"" document "\"\n"
         "set theKey to \"" key "\"\n"
         "open theDoc\n"
            "tell document 1 of application \"BibDesk\"\n"
            "set somePubs to (publications whose cite key contains theKey)\n"
            "set the selection to somePubs\n"
            "set theSelection to the selection\n"
            "set thePub to get item 1 of theSelection\n"
            "show thePub\n"
         "end tell\n"
      "end tell"))
    ))


(provide 'org-mac-bibdesk)