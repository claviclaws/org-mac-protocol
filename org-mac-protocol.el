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

(add-to-list 'org-protocol-protocol-alist
	     '("org-mac-remember"
	       :protocol "mac-remember"
	       :function org-mac-protocol-remember
	       :kill-client t))

(defvar org-mac-protocol-app nil
  "The application that AppleScript will acticate
after the remember buffer is killed")
(defvar width-of-display (x-display-pixel-width)
  "For some reason, (x-display-pixel-width) returns corrupted
values when called during (org-mac-protocol-remember); this
ensures that correct value is returned")
(defvar height-of-display (x-display-pixel-height)
  "For some reason, (x-display-pixel-height) returns corrupted
values when called during (org-mac-protocol-remember); this
ensures that correct value is returned")

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

(provide 'org-mac-protocol)