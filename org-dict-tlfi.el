;;; org-dict-tlfi.el --- Dictionary parse engine of the TLFI -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Firmin Martin

;; Author: Firmin Martin
;; Maintainer: Firmin Martin
;; Version: 0.1
;; Keywords: convenience, dictionary
;; URL: https://www.github.com/firmart/org-dict
;; Package-Requires: ((emacs "25.1") (org "9.3") (cl-lib "0.5"))


;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a dictionary parse engine for the French dictionary
;; TLFi available on CNRTL (https://cnrtl.fr/definition/).

;;; Code:
(require 'org-dict-core)
(require 'cl-lib)
;;; Custom group
;;;; General settings
;;; Internal variables
;;;;   Internal functions
(defun org-dict-tlfi--total-entries (dom)
  "From a word's DOM, return the number of entries. 
One word could have multiple entries (as noun, as adjective, etc.)"
  ;; #vtoolbar li
  (let ((entries-selector '((:attrs ((id . "vtoolbar")))
			    (=> (:tag li)))))
    (length (org-dict--dom-select dom entries-selector))))

(defun org-dict-tlfi--current-entry-name (dom)
  "From a word's DOM, return the current entry title."
  ;; #vitemselected a
  (let* ((title-selector '((:attrs ((id . "vitemselected")))
			   (=> (:tag a))))
	 (title-node (car (org-dict--dom-select dom title-selector)))
	 (superscript-node (car (org-dict--dom-select title-node '((:tag sup)))))
	 (superscript (caddr superscript-node)))
    (when superscript
	(org-dict--dom-replace-node title-node superscript-node (concat " (" superscript ")")))
    (dom-texts title-node "")))

(defun org-dict-tlfi--dom-list (dom url)
  "Given the word's URL and its DOM, returns the DOM of all entries."
  (let ((dom-list (list dom)))
    (append dom-list
	    (cl-loop for i from 1 to (1- (org-dict-tlfi--total-entries dom))
		     ;; fetch <tlfi-url>/<word>/<nth-entry>
		     for entry-url = (concat url "/" (number-to-string i))
		     collect (org-dict--url-to-dom entry-url) into remaining-dom
		     finally return remaining-dom))))

(defun org-dict-tlfi--replace-exemples (dom)
  ;; span.tlf_cexemple
  (let ((cexemple-nodes (org-dict--dom-select dom '((:tag span :attrs ((class . "tlf_cexemple")))))))
    (mapc (lambda (node)
	    (let* ((content (dom-texts node ""))
		   (unumbered-content (replace-regexp-in-string "^[0-9]+\\. \\(.*\\)" "\\1" content)))
	      (org-dict--dom-replace-node dom node (concat "\n#+BEGIN_QUOTE\n" unumbered-content "\n#+END_QUOTE\n"))))
	  cexemple-nodes)))

(defun org-dict-tlfi--remove-cdevette (dom)
  ;; TODO org-dict-dom--select-all and org-dict-dom--select
  ;; #lexicontent div.tlf_cvedette
  (let ((cvedette-node (car (org-dict--dom-select dom '((:attrs ((id . "lexicontent")))
							(=> (:tag div :attrs ((class . "tlf_cvedette")))))))))
    (dom-remove-node dom cvedette-node)))

(defun org-dict-tlfi--replace-numbering (dom)
  (let ;; span.tlf_cplan b
      ((cplan-nodes (org-dict--dom-select dom '((:tag span :attrs ((class . "tlf_cplan")))
						(=> (:tag b))))))
    (mapc (lambda (node)
	    ;; TODO Replace (caddr node) with something more sophisticated
	    (org-dict--dom-replace-node dom node (concat "\n\n" (caddr node))))
	  cplan-nodes)))

(defun org-dict-tlfi--remove-italic (dom)
  "Remove some <i> which are right before <sup>. 

/word/^{exp} doesn't render well in Org mode."
  (let* ((superscript-nodes (org-dict--dom-select dom '((:tag i)
							(+ (:tag sup)))))
	 (superscript-siblings-list (mapcar
				     (lambda (superscript-node)
				       (list superscript-node (dom-children (dom-parent dom superscript-node))))
				     superscript-nodes))
	 (italic-nodes (mapcar (lambda (superscript-siblings)
				 (cl-loop with superscript-node = (car superscript-siblings)
					  with siblings = (cadr superscript-siblings)
					  for node in siblings
					  with previous-node
					  when (and previous-node
						    (not (stringp previous-node))
						    (eq superscript-node node)
						    (eq 'i (car previous-node)))
					  return previous-node
					  do (setq previous-node node)))
			       superscript-siblings-list)))
    (mapc (lambda (node)
	    (when node
    	      (org-dict--dom-replace-node dom node (caddr node))))
    	  italic-nodes)))

(defun org-dict-tlfi--replace-scripts (dom)
  "Replace <sup> to superscript markup of Org mode."
  (let* ((all-superscript-nodes (org-dict--dom-select dom '((:tag sup))))
	(cplan-superscript-nodes (org-dict--dom-select dom '((:attrs ((class . "tlf_cplan")))
							     (> (:tag sup)))))
	(target-superscript-nodes (cl-set-difference all-superscript-nodes cplan-superscript-nodes)))
    
    (mapc (lambda (node)
	    (org-dict--dom-replace-node dom node (concat "^{" (if (stringp (caddr node))
								  (caddr node)
								(dom-texts (caddr node) ""))
							 "}")))
	  target-superscript-nodes)))

(defun org-dict-tlfi--node-replace-with-surround (dom node char)
  "Surround NODE's text content with CHAR."
  (let* ((content (caddr node))
	 (replaced-content (replace-regexp-in-string "^\\([ ,.]+?\\)?\\(.*?\\)\\([ ,.]+\\)?$" (format "\\1​%s\\2%s​\\3" char char) content)))
    ;;(message "node:%s, class:%s, content:'%s', replaced-content:'%s'" (car node) (org-dict--dom-node-class node) content replaced-content)
    (org-dict--dom-replace-node dom node replaced-content)))

(defun org-dict-tlfi--replace-markup (dom)
  "Replace HTML markup into DOM markup."
  (let* ((all-bold-nodes (org-dict--dom-select dom '((:tag b))))
	 (cplan-bold-nodes (org-dict--dom-select dom '((:attrs ((class . "tlf_cplan")))
						       (=> (:tag b)))))
	 (target-bold-nodes (cl-set-difference all-bold-nodes cplan-bold-nodes))
	 (all-italic-nodes (org-dict--dom-select dom '((:tag i))))
	 (cplan-italic-nodes (org-dict--dom-select dom '((:attrs ((class . "tlf_cplan")))
							 (=> (:tag i)))))
	 (target-italic-nodes (cl-set-difference all-italic-nodes cplan-italic-nodes))
	 (target-smallcaps-nodes (org-dict--dom-select dom '((:attrs ((class . "tlf_smallcaps")))))))

    (mapc (lambda (node)
	   (org-dict-tlfi--node-replace-with-surround dom node "*"))
	   target-bold-nodes)
    (mapc (lambda (node)
	   (org-dict-tlfi--node-replace-with-surround dom node "*"))
	  target-smallcaps-nodes)
    (mapc (lambda (node)
	   (org-dict-tlfi--node-replace-with-surround dom node "/"))
	  target-italic-nodes)))

(defvar org-dict-tlfi--cplan-regexps '("^[0-9].*Section\\.$"
					"^\\([I]\\{1,3\\}\\|[I]?V\\|V[I]\\{1,3\\}\\|[I]?X\\)\\.[ ]?−$"
					"^[A-H]\\.[ ]?−$" "^[0-9]\\.[ ]?$" "^[a-z])$"
					"[αβγδϵζηθικλ])"))

(defun org-dict-tlfi--parse-cplan (string)
  "Parse TLFi numbering STRING. Return its associated level."
  (cl-loop with regexps = org-dict-tlfi--cplan-regexps
	   with level = 0
	   for regexp in regexps 
	   when (string-match regexp string) return level
	   do (setq level (1+ level))
	   finally (error "Cannot match tlf_cplan numbering: %s" string)))

(defun org-dict-tlfi--space-or-newline (node)
  "Depending NODE's class, return space or newline."
  (if (or (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_csyntagme"))) node)
	  (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cdefinition"))) node)
	  (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_csynonyme"))) node))
      " "
    "\n"))

(defun org-dict-tlfi--create-section-title (title current-depth numbering)
  "Given CURRENT-DEPTH and TITLE, create a section's title."
  (if (<= current-depth 2) 
      ;; Org mode heading
      (format "%s %s\n" (make-string (+ current-depth 2) ?*) title)
    ;; Org mode numbered list
    (format "%s%s. %s " (make-string (* (- current-depth 2) org-dict-indentation-width) ? ) numbering title)))

;; Source of sigles http://www.languefrancaise.net/forum/viewtopic.php?id=11703
(defun org-dict-tlfi--parse-parah (parah-node)
  "Parse TLFi recursive div node of class tlf_parah."
  (cl-loop for node in (dom-children parah-node)
	   with level-numbering = 1
	   with current-depth
	   with title
	   with title-settled?
	   if (stringp node)
	       collect (replace-regexp-in-string "^[ ]*$" "" node)
	   else if (org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_parah"))) node)
	       if (not title-settled?) do (setq title-settled? t) end
	       and collect (org-dict-tlfi--create-section-title title current-depth level-numbering)
	       and collect (apply #'concat (flatten-tree (org-dict-tlfi--parse-parah node)))
	       and do (setq level-numbering (1+ level-numbering))
	   else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cplan"))) node)
	        do (setq current-depth (org-dict-tlfi--parse-cplan (dom-texts node "")))
	   else if (or (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cemploi"))) node)
                       (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_ccrochet"))) node))
		    if (not title-settled?) do (setq title (concat title (dom-texts node "")))
		    else collect (concat (apply #'concat (flatten-tree (dom-texts node ""))) " ")
		    end
	   else if (not title-settled?) do (setq title-settled? t) and
		    collect (org-dict-tlfi--create-section-title title current-depth level-numbering)
		end and
		collect (apply #'concat (flatten-tree (dom-texts node ""))) and
		collect (org-dict-tlfi--space-or-newline node)
	   end))

(defun org-dict-tlfi--parse-entry-content (dom)
  (let ((article-node (org-dict--dom-select dom ;; #lexicontent > .div
						      '((:attrs ((id . "lexicontent")))
							(> (:tag div))))))
  ;; (org-dict-tlfi--remove-cdevette article-node)
  (org-dict-tlfi--replace-markup article-node)
  (org-dict-tlfi--remove-italic article-node)
  (org-dict-tlfi--replace-exemples article-node)
  (org-dict-tlfi--replace-scripts article-node)
  ;; (org-dict-tlfi--replace-numbering article-node)
  (apply #'concat
	 (flatten-tree
	  (cl-loop for node in (dom-children article-node)
		   with top-level-numbering = 1
		   if (org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_parah"))) node)
		     ;; do (mapc (lambda (child) (message "node:'%s', class:%s" (if (stringp child) child (car child)) (when (not (stringp child)) (org-dict--dom-node-class child)))) (dom-children node)) and
	               ;; do (message "node:%s, class:%s, level-numbering:%s" (car node) (org-dict--dom-node-class node) top-level-numbering) and
                       collect (org-dict-tlfi--parse-parah node) and
                       do (setq top-level-numbering (1+ top-level-numbering))
                   else if (org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_cvedette"))) node)
		       do 'nothing
		   else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_ccrochet"))) node)
		       collect (concat (apply #'concat (flatten-tree (dom-texts node ""))) "\n\n")
		   else collect (dom-texts node ""))))))

(defun org-dict-tlfi--parse-entry (dom)
  "Parse a single word's entry whose the dom is DOM into an org buffer string."
  (let ((entry-name (org-dict-tlfi--current-entry-name dom)))
    (with-temp-buffer 
      (org-mode)
      (org-insert-heading)
      (insert entry-name "\n")
      (insert
       (org-dict--remove-redundant-spaces
	(org-dict-tlfi--parse-entry-content dom)))
      (insert "\n")
      ;; TODO add etymology
      ;; TODO use org-element API to fill paragraph and list
      (org-dict--fill-region (point-min) (point-max))
      (buffer-substring (point-min) (point-max)))))

;;; Interactive functions
(defun org-dict-tlfi-parse (dom &optional url)
  "Parse a DOM coming from TLFI and outputs in Org."
  (let* ((dom-list (org-dict-tlfi--dom-list dom url)))
    (mapcar #'org-dict-tlfi--parse-entry dom-list)))

;;; org-dict-tlfi.el ends here
(provide 'org-dict-tlfi)
