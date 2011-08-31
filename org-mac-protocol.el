;;; org-mac-protocol.el -- process events triggered by the
;;; org-mac-protocol suite of AppleScripts

;; Copyright (C) 2009, 2010 Christopher Suckling

;; Authors: Christopher Suckling <suckling at gmail dot com>

;; This file is Free Software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Version: 0.634
;; Keywords: outlines, hyperlinks, wp

;;; Commentary
;;
;; This file provides org-protocol handlers and various hacks to
;; produce a nicely centered, pop-up remember frame for links passed
;; to emacsclient by the org-mac-protocol suite of AppleScripts. It
;; also defines new org-mode hyperlink types for opening files in
;; various OS X applications.

;;; Installation and Usage
;;
;; Please see org-mac-protocol.org

;;; Credit
;;
;; Portions of this code are developed from a blog post by Jack Moffitt:
;; http://metajack.im/2008/12/30/gtd-capture-with-emacs-orgmode/

(require 'cl)
(require 'org-protocol)
(require 'org-bibtex)

;;; Define org-mode hyperlink types
;;
;; BibDesk

(org-add-link-type "bibdesk" 'org-mac-bibdesk-open)

(defun org-mac-bibdesk-open (uri)
  "Visit bibtex entry in BibDesk"
  (let* ((key (when (string-match "::\\(.+\\)\\'" uri)
		(match-string 1 uri)))
	 (document (substring uri 0 (match-beginning 0))))    
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
      "end tell"))))

;; Skim

(org-add-link-type "skim" 'org-mac-skim-open)

(defun org-mac-skim-open (uri)
  "Visit page of pdf in Skim"
  (let* ((page (when (string-match "::\\(.+\\)\\'" uri)
		 (match-string 1 uri)))
	 (document (substring uri 0 (match-beginning 0))))
    (do-applescript
     (concat
      "tell application \"Skim\"\n"
         "activate\n"
	 "set theDoc to \"" document "\"\n"
		     "set thePage to " page "\n"
	 "open theDoc\n"
	 "go document 1 to page thePage of document 1\n"
      "end tell"))))

;; Pages

(org-add-link-type "pages" 'org-mac-pages-open)

(defun org-mac-pages-open (uri)
  "Visit page of document in Pages. Reliant on character offset
from start of document, so link may degrade if document is
edited"
  (let* ((charoff (when (string-match "::\\(.+\\)\\'" uri)
		 (match-string 1 uri)))
	 (document (substring uri 0 (match-beginning 0))))
    (do-applescript
     (concat
      "tell application \"Pages\"\n"
         "activate\n"
	 "set theDoc to \"" document "\"\n"
	 "set theCharOff to " charoff "\n"
	 "open theDoc\n"
	 "select character theCharOff of document 1\n"
      "end tell"))))

;; Numbers

(org-add-link-type "numbers" 'org-mac-numbers-open)

(defun org-mac-numbers-open (uri)
  "Visit range of spreadsheet in Numbers. Although the correct
cells will be selected, it may be necessary to change sheets
manually"
  (let* ((sheet (when (string-match "::\\(.+\\)::\\(.+\\)::\\(.+\\)\\'" uri)
		  (match-string 1 uri)))
	 (table (when (string-match "::\\(.+\\)::\\(.+\\)::\\(.+\\)\\'" uri)
		  (org-protocol-unhex-string (match-string 2 uri))))
	 (range (when (string-match "::\\(.+\\)::\\(.+\\)::\\(.+\\)\\'" uri)
		  (match-string 3 uri)))
	 (document (substring uri 0 (match-beginning 0))))
    (do-applescript
     (concat
      "tell application \"Numbers\"\n"
         "activate\n"
	 "set theDoc to \"" document "\"\n"
	 "set theSheet to \"" sheet "\"\n"
	 "set theTable to \"" table "\"\n"
	 "set theRange to \"" range "\"\n"
	 "open theDoc\n"
	 "tell document 1\n"
	    "tell sheet (theSheet as integer)\n"
	       "tell table theTable\n"
	          "set selection range to range theRange\n"
	       "end tell\n"
	    "end tell\n"
	 "end tell\n"   
       "end tell"))))


;; Keynote

(org-add-link-type "keynote" 'org-mac-keynote-open)

(defun org-mac-keynote-open (uri)
  "Visit slide in Keynote"
  (let* ((slide (when (string-match "::\\(.+\\)\\'" uri)
		  (match-string 1 uri)))
	 (document (substring uri 0 (match-beginning 0))))
    (do-applescript
     (concat
      "tell application \"Keynote\"\n"
          "activate\n"
          "set theDoc to \"" document "\"\n"
          "set theSlide to " slide "\n"
          "open theDoc\n"
          "show slide theSlide of slideshow 1\n"
      "end tell"))))


;; iTunes

(org-add-link-type "iTunes" 'org-mac-iTunes-open)

(defun org-mac-iTunes-open (uri)
  "Visit track in iTunes"
  (let* ((track (when (string-match "[0-9a-zA-Z]+\\'" uri)
		  (match-string 0 uri))))
    (do-applescript
     (concat
      "tell application \"iTunes\"\n"
         "activate\n"
	 "set theTrack to \"" track "\"\n"
	 "set view of front browser window to user playlist \"Music\" of source \"Library\"\n"
	 "play (some track of playlist \"Music\" of source \"Library\" whose persistent ID is theTrack)\n"
	 "playpause\n"
      "end tell"
	 ))))


;; Address Book
;; Support based on code by Alexander Poslavsky

(org-add-link-type "address" 'org-mac-addressbook-open)

(defun org-mac-addressbook-open (uri)
  "Applescript to open correct address-book entry"
  (do-applescript
   (concat
    "set theID to \"" uri "\"\n"

    "tell application \"System Events\"\n"
        "activate\n"
        "try\n"
            "open location \"addressbook\:\/\/\" \& theID\n"
            "on error\n"
            "display dialog \"no matching contact found\"\n"
        "end try\n"
    "end tell"
    )))


;;; Variables and advice for managing *remember* frames

;; updated for org-capture hooks

(defvar width-of-display (x-display-pixel-width)
  "For some reason, (x-display-pixel-width) returns corrupted
values when called during (org-mac-protocol-remember); this
ensures that correct value is returned")
(setq width-of-display (x-display-pixel-width))

(defvar height-of-display (x-display-pixel-height)
  "For some reason, (x-display-pixel-height) returns corrupted
values when called during (org-mac-protocol-remember); this
ensures that correct value is returned")
(setq height-of-display (x-display-pixel-height))

(defvar org-mac-protocol-app nil
  "The application that AppleScript will activate
after the *remember* buffer is killed")

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
  "Advise remember-destroy to close the frame if it is the
mac-rememeber frame"
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
  "Advise org-remember to delete non-remember windows if it is in
the remember frame"
  (when (equal "*mac-remember*" (frame-parameter nil 'name))
    (delete-other-windows)))

(add-hook 'org-capture-mode-hook
	  '(lambda ()
	     (when (equal "*mac-remember*" (frame-parameter nil 'name))
	       (delete-other-windows))))  

(add-hook 'org-capture-after-finalize-hook
	  '(lambda ()	     
	     (unless (boundp 'org-refile-for-capture)
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
		   "end tell"))))))

(add-hook 'org-after-refile-insert-hook
	  '(lambda ()
	     (when (boundp 'org-refile-for-capture)
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
		   "end tell"))))))

;;; Define org-protocol protocols

(add-to-list 'org-protocol-protocol-alist
	     '("org-mac-remember"
	       :protocol "mac-remember"
	       :function org-mac-protocol-remember
	       :kill-client t))

(add-to-list 'org-protocol-protocol-alist
	     '("org-mac-safari-tabs"
	       :protocol "safari-tabs"
	       :function org-mac-safari-tabs
	       :kill-client t))

(defun org-mac-protocol-remember (data)
  "Process an org-protocol://mac-remember:// scheme URL.
Pop up a small, centred Emacs frame containing a remember
buffer. Destroys the frame after the remember buffer is filed and
return to originating application. This code is
essentially (org-protocol-remember) with an additional :shortdesc
link property"

  (let* ((elements (org-protocol-split-data data nil ":"))
	 (info (car elements))
	 (app (org-protocol-unhex-string (cadr elements)))
	 (parts (org-protocol-split-data info t))
	 (template (or (and (= 1 (length (car parts))) (pop parts))
		       org-protocol-default-template-key))
	 (url (org-protocol-sanitize-uri (car parts)))
	 (type (if (string-match "^\\([a-z]+\\):" url)
		   (match-string 1 url)))
	 (title (or (cadr parts) ""))
	 (shorttitle (or (caddr parts) ""))
	 (region (or (cadddr parts) ""))
	 (orglink (org-make-link-string
		   url (if (string-match "[^[:space:]]" title) title url)))
	 (org-capture-link-is-already-stored t)
	 remember-annotation-functions)
    
    (setq org-mac-protocol-app app)

    (do-applescript
     (concat
      "tell application \"Emacs\"\n"
      "activate\n"
      "end tell"))

    (make-frame '((name . "*mac-remember*") (width . 80) (height . 20)))
    (mapc (lambda (x)
	    (if (not (equal (frame-parameter x 'name) "*mac-remember*"))
		(make-frame-invisible x)
	      (let ((center-left
		     (/ (- width-of-display (frame-pixel-width)) 2))
		    (center-top
		     (/ (- (- height-of-display 22) (frame-pixel-height)) 2)))
		(set-frame-parameter nil 'top center-top)
		(set-frame-parameter nil 'left center-left))))
	  (frame-list))
    (select-frame-by-name "*mac-remember*")

    (setq org-stored-links
	  (cons (list url title) org-stored-links))
    (kill-new orglink)    
    (org-store-link-props :type type
			  :link url
			  :description title
			  :shortdesc shorttitle
			  :initial region)
    (message "protocol: %s" (plist-get org-store-link-plist :shortdesc))
    
    (if (boundp 'org-capture-templates)
	(org-capture nil template)
      (org-remember nil (string-to-char template))))
  nil) 

(defun org-mac-safari-tabs (data)
  "Process an org-protocol://safari-tabs:// scheme URL.
Inserts a formatted list of hyperlinks to the tabs open in the
front Safari window"  
  (let* ((elements (org-protocol-split-data data nil "::"))	 
	 ;; discard the last element, as it is blank
	 (links (butlast elements)))
    (mapc (lambda (x)
	    (org-protocol-store-link x))
	  links))
  nil)


(provide 'org-mac-protocol)
