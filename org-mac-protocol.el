;;; org-mac-protocol.el -- process events triggered by the
;;; org-mac-protocol suite of AppleScripts

;; Copyright (C) 2009 Christopher Suckling

;; Author: Christopher Suckling <suckling at gmail dot com>

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

;; Version: 0.628
;; Keywords: outlines, hyperlinks, wp

;;; Commentary
;;
;; This file provides an org-protocol handler and various hacks to
;; produce a nicely centered, pop-up remember frame for links passed
;; to emacsclient by the org-mac-protocol suite of AppleScripts.
;;
;; Installation
;;
;; Move this file to your elisp load-path and add
;; (require 'org-mac-protocol)
;; to your .emacs
;;
;; Usage
;;
;; You must create two org-remember-templates. Please see the
;; org-mac-protocol manual for sample templates. If you change the
;; keys to trigger these templates, you *must* edit the relevant lines
;; in org-remember.scpt and org-note.scpt. Please see the
;; documentation to org-protocol.el and the org-mode manual for
;; further assistance with creating templates.
;;
;; Credit
;;
;; Portions of this code are developed from a blog post by Jack Moffitt:
;; http://metajack.im/2008/12/30/gtd-capture-with-emacs-orgmode/

(require 'org-protocol)
(require 'org-bibtex)

;; Define hyperlink types

;; BibDesk

(org-add-link-type "bibdesk" 'org-mac-bibdesk-open)

(defun org-mac-bibdesk-open (uri)
  "Visit the bibtex entry on URI in BibDesk"
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
  "Visit the linked page of the pdf in Skim"
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


;; iTunes

(org-add-link-type "iTunes" 'org-mac-iTunes-open)

(defun org-mac-iTunes-open (uri)
  "Visit the linked track in iTunes"
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

;; Define org-protocol protocols

(add-to-list 'org-protocol-protocol-alist
	     '("org-mac-remember"
	       :protocol "mac-remember"
	       :function org-mac-protocol-remember
	       :kill-client t)
	     '("org-mac-safari-tabs"
	       :protocol "safari-tabs"
	       :function org-mac-safari-tabs
	       :kill-client t))

(defvar org-mac-protocol-app nil
  "The application that AppleScript will acticate
after the remember buffer is killed")
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


(defun org-mac-protocol-remember (data)
  "Process an org-protocol://mac-remember:// scheme URL.
Pops up a small Emacs frame containing a remember buffer. Calls
org-protocol-remember. Destroys the frame after the remember
buffer is filed)"

  (let* ((parts (org-protocol-split-data data nil ":"))
	 (info (car parts))
	 (app (org-protocol-unhex-string (cadr parts))))

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
    (org-protocol-remember info))
  nil)

(defun org-mac-safari-tabs (data)
  "Process an org-protocol://safari-tabs:// scheme URL.
Inserts a formatted list of hyperlinks to the tabs open in the
front Safari window"  
  (message data)
  nil)

(provide 'org-mac-protocol)