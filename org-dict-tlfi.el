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
(require 'cl-lib)
(require 'org-dict-core)
;;; Custom group
;;;; General settings
(defcustom org-dict-tlfi-heading-max-depth 2
  "The maximal depth of produced Org headings.

All nodes of a greater depth will become a numbered items instead of an
Org heading.  This variable accepts integer ranging from 0 to 5.
The least is the top level numbering:
- 0 for '1re Section'
- 1 for 'I.  -'
- 2 for 'A.  -'
- 3 for '1.'
- 4 for 'a)'
- 5 for 'α)'"
  :type 'integer
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

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
  ;; #lexicontent span.tlf_cmot
  (let* ((title-selector '((:attrs ((id . "lexicontent")))
			   (=> (:tag div :attrs ((class . "tlf_cvedette"))))))
	 (title-node (car (org-dict--dom-select dom title-selector)))
	 (word-node (car (org-dict--dom-select title-node '((:tag span :attrs ((class . "tlf_cmot")))))))
	 (word (caddr word-node))
	 (superscript-node (car (org-dict--dom-select word-node '((:tag sup)))))
	 (superscript (caddr superscript-node))
	 (pos-node (car (org-dict--dom-select title-node '((:tag span :attrs ((class . "tlf_ccode")))))))
	 (part-of-speech (propertize (caddr pos-node) 'font-lock-face 'org-dict-pos-face)))

    (concat word
	    (when superscript
	      (concat " (" superscript ") "))
	    part-of-speech)))

(defun org-dict-tlfi--dom-list (dom url)
  "Given the word's URL and its DOM, return the DOM of all entries."
  (let ((dom-list (list dom)))
    (append dom-list
	    (cl-loop for i from 1 to (1- (org-dict-tlfi--total-entries dom))
		     ;; fetch <tlfi-url>/<word>/<nth-entry>
		     for entry-url = (concat url "/" (number-to-string i))
		     collect (org-dict--url-to-dom entry-url) into remaining-dom
		     finally return remaining-dom))))

(defun org-dict-tlfi--remove-italic (dom)
  "In DOM, remove some <i> which are right before <sup>.

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
  "In DOM, replace <sup> to superscript markup of Org mode."
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
  "In DOM, surround NODE's text content with CHAR."
  (when-let* ((content (caddr node))
	 (replaced-content (replace-regexp-in-string "^\\([ ,.]+?\\)?\\(.*?\\)\\([ ,.]+\\)?$" (format "\\1​%s\\2%s​\\3" char char) content)))
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
	 (all-smallcaps-nodes (org-dict--dom-select dom '((:attrs ((class . "tlf_smallcaps")))))))

    (mapc (lambda (node)
	   (org-dict-tlfi--node-replace-with-surround dom node "*"))
	   target-bold-nodes)
    (mapc (lambda (node)
	   (org-dict-tlfi--node-replace-with-surround dom node "*"))
	  all-smallcaps-nodes)
    (mapc (lambda (node)
	   (org-dict-tlfi--node-replace-with-surround dom node "/"))
	  target-italic-nodes)))

(defvar org-dict-tlfi--cplan-regexps '("^[ ]*?\\([0-9]+?.*Section\\)\\.[ ]*?$"
					"^[ ]*?\\([I]\\{1,3\\}\\|[I]?V\\|V[I]\\{1,3\\}\\|[I]?X\\)\\.[ ]*?−?[ ]*?$"
					"^[ ]*?\\([A-H]\\)\\.[ ]*?−[ ]*?$"
					"^[ ]*?\\([0-9]+?\\)\\.[ ]*?−?$"
					"^[ ]*?\\([a-z]\\))[ ]*?$"
					"^[ ]*?\\([αβγδϵζηθικλ]\\))[ ]*?"))

(defun org-dict-tlfi--parse-cplan-string (string)
  "Parse TLFi numbering STRING.  Return its string representation."
  (cl-loop for regexp in org-dict-tlfi--cplan-regexps
	   when (string-match regexp string) return (replace-regexp-in-string regexp "\\1" string)
	   finally (error "Cannot parse tlf_cplan numbering: '%s'" string)))

(defun org-dict-tlfi--parse-cplan-depth (string)
  "Parse TLFi numbering STRING.  Return its associated depth."
  (cl-loop with depth = 0
	   for regexp in org-dict-tlfi--cplan-regexps
	   when (string-match regexp string) return depth
	   do (setq depth (1+ depth))
	   finally (error "Cannot match tlf_cplan numbering: '%s'" string)))

(defun org-dict-tlfi--emploi-crochet-domaine-p (node)
  "Return non-nil when NODE's class is tlf_c{emploi, crochet, domaine}."
  (or (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cemploi"))) node)
      (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_ccrochet"))) node)
      (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cdomaine"))) node)))

(defun org-dict-tlfi--space-or-newline (node)
  "Depending NODE's class, return space or newline."
  (if (or (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_csyntagme"))) node)
	  (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cdefinition"))) node)
	  (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_csynonyme"))) node))
      " "
    "\n"))

(defun org-dict-tlfi--create-section-title (title root-depth current-depth numbering)
  "Create a Org mode section's title.

Based on TITLE, ROOT-DEPTH (the depth of the first node visited),
CURRENT-DEPTH and NUMBERING"
  (let ((title (if (not title) "" title)))
  (if (<= current-depth org-dict-tlfi-heading-max-depth)
      ;; Org mode heading. Keep TLFi numbering for reference purpose.
      (format "%s (%s) %s\n" (make-string (- (+ current-depth org-dict-tlfi-heading-max-depth) root-depth) ?*) numbering title)
    ;; Org mode numbered list
    (format "%s%s. %s " (make-string (* (- current-depth (1+ org-dict-tlfi-heading-max-depth)) org-dict-indentation-width) ? ) numbering title))))

(defun org-dict-tlfi--parse-cemploi (cemploi-node)
  (propertize (dom-texts cemploi-node "") 'font-lock-face 'org-dict-use-face))

(defun org-dict-tlfi--parse-ccrochet (ccrochet-node)
  (propertize (dom-texts ccrochet-node "") 'font-lock-face 'org-dict-comment-face))

(defun org-dict-tlfi--parse-cdomaine (cdomaine-node)
  (propertize (dom-texts cdomaine-node "") 'font-lock-face 'org-dict-domain-face))

(defun org-dict-tlfi--parse-csyntagme (csyntagme-node)
  (propertize (dom-texts csyntagme-node "") 'font-lock-face 'org-dict-syntagma-face))

;; TODO propertize differently Synon. & Anton.
(defun org-dict-tlfi--parse-csynonime (csynonime-node)
  "Parse node of class 'csynonime'."
  (propertize (dom-texts csynonime-node "") 'font-lock-face 'org-dict-synonym-face))

(defun org-dict-tlfi--parse-paraputir (paraputir-node)
  (let ((children (dom-children paraputir-node)))
    (apply #'concat
	   (cl-loop for node in children
		    collect (cond
			     ((stringp node) (org-dict-tlfi--parse-string node))
			     ((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cemploi"))) node)
			      (concat (org-dict-tlfi--parse-cemploi node) "\n"))
			     ((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_ccrochet"))) node)
			      (concat (org-dict-tlfi--parse-ccrochet node) " "))
			     ((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_csynonime"))) node)
			      (org-dict-tlfi--parse-csynonime node))
			     ((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_csyntagme"))) node)
			      (org-dict-tlfi--parse-csyntagme node))
			     ((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cdomaine"))) node)
			      (org-dict-tlfi--parse-cdomaine node))
                             ((org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_parah"))) node)
                              (org-dict-tlfi--parse-parah node))
                             ((org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_parsynt"))) node)
                              (org-dict-tlfi--parse-parsynt node))
		             ((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cexemple"))) node)
                              (org-dict-tlfi--parse-cexemple node))
			     (t (dom-texts node "")))))))

(defun org-dict-tlfi--parse-tabulation (tabulation-node)
  (let ((children (dom-children tabulation-node)))
    (apply #'concat
	   (cl-loop for node in children
		    collect (cond
			     ((stringp node) (org-dict-tlfi--parse-string node))
		             ((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cexemple"))) node)
                              (org-dict-tlfi--parse-cexemple node))
			     (t (dom-texts node "")))))))

(defun org-dict-tlfi--parse-string (str)
  "Parse a STR node."
  (replace-regexp-in-string "^[ ]*$" "" str))

;; TODO propertize author, bbg, date
;; TODO take account of depth
(defun org-dict-tlfi--parse-cexemple (node)
  (let* ((content (dom-texts node ""))
	 (unumbered-content (replace-regexp-in-string "^[0-9]+\\.[ ]*\\(.*\\)" "\\1" content))
	 (target-content (replace-regexp-in-string "− \\(.*\\)" "\\1" unumbered-content)))
    (concat "\n#+BEGIN_QUOTE\n"
	    (propertize target-content 'font-lock-face 'org-dict-example-face)
	    "\n#+END_QUOTE\n")))

;; Source of sigles http://www.languefrancaise.net/forum/viewtopic.php?id=11703
(defun org-dict-tlfi--parse-parah (parah-node &optional root-depth)
  "Parse TLFi recursive div.tlf_parah node PARAH-NODE.

ROOT-DEPTH is used to correctly determine the Org heading and numbered list depth."
  (let* ((children (dom-children parah-node))
	 (total-nodes (length children)))
  (apply #'concat (cl-loop for node in children
       with node-count = 0
       with root-depth = root-depth
       with current-depth
       with numbering-string
       with title
       with title-settled?
       do (setq node-count (1+ node-count))
       if (stringp node)
            collect (org-dict-tlfi--parse-string node)
       else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cplan"))) node)
            do (setq current-depth (org-dict-tlfi--parse-cplan-depth (dom-texts node ""))) and
            do (setq numbering-string (org-dict-tlfi--parse-cplan-string (dom-texts node ""))) and
            when (not root-depth) do (setq root-depth current-depth) end
	    ;; Potential HTML nodes used to make an Org heading
       else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cexemple"))) node)
            if (not title-settled?)
                do (setq title-settled? t) and
                collect (org-dict-tlfi--create-section-title title root-depth current-depth numbering-string)
            end and
	    collect "\n" and
            collect (org-dict-tlfi--parse-cexemple node)
       else if (org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_parsynt"))) node)
            if (not title-settled?)
                do (setq title-settled? t) and
                collect (org-dict-tlfi--create-section-title title root-depth current-depth numbering-string)
            end and
	    collect "\n" and
            collect (org-dict-tlfi--parse-parsynt node)
       else if (org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_tabulation"))) node)
            if (not title-settled?)
                do (setq title-settled? t) and
                collect (org-dict-tlfi--create-section-title title root-depth current-depth numbering-string)
            end and
	    collect "\n" and
            collect (org-dict-tlfi--parse-tabulation node)
       else if (org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_paraputir"))) node)
            if (not title-settled?)
                do (setq title-settled? t) and
                collect (org-dict-tlfi--create-section-title title root-depth current-depth numbering-string)
            end and
	    collect "\n\n" and
            collect (concat (org-dict-tlfi--parse-paraputir node) " ")
       else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_csyntagme"))) node)
            if (not title-settled?)
                do (setq title-settled? t) and
                collect (org-dict-tlfi--create-section-title title root-depth current-depth numbering-string)
            end and
	    collect "\n\n" and
            collect (concat (org-dict-tlfi--parse-csyntagme node) " ")
       else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_csynonime"))) node)
            if (not title-settled?)
                do (setq title-settled? t) and
                collect (org-dict-tlfi--create-section-title title root-depth current-depth numbering-string)
            end and
	    collect "\n\n" and
            collect (concat (org-dict-tlfi--parse-csynonime node) " ")
       else if (org-dict-tlfi--emploi-crochet-domaine-p node)
            if (not title-settled?)
                do (setq title (concat title " " (dom-texts node "")))
            else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cemploi"))) node)
		collect (org-dict-tlfi--parse-cemploi node)
            else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_ccrochet"))) node)
                collect (concat (org-dict-tlfi--parse-ccrochet node) "\n")
            else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cdomaine"))) node)
		collect (org-dict-tlfi--parse-cdomaine node)
            end
       else if (org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_parah"))) node)
            if (not title-settled?)
                do (setq title-settled? t) and
                collect (org-dict-tlfi--create-section-title title root-depth current-depth numbering-string)
            end and
	    collect "\n" and
            collect (org-dict-tlfi--parse-parah node root-depth)
       else
            if (not title-settled?)
                do (setq title-settled? t) and
                collect (org-dict-tlfi--create-section-title title root-depth current-depth numbering-string)
            end and
            collect (dom-texts node "") and
            collect (org-dict-tlfi--space-or-newline node)
       end
       if (= total-nodes node-count)
           if (not title-settled?)
               do (setq title-settled? t) and
               collect (org-dict-tlfi--create-section-title title root-depth current-depth numbering-string)
	   end and
	   collect "\n"
       end))))

(defun org-dict-tlfi--parse-parsynt (parsynt-node)
  (let ((children (dom-children parsynt-node)))
    (apply #'concat
	   (cl-loop for node in children
		    collect (cond
			     ((stringp node) (org-dict-tlfi--parse-string node))
		             ((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_csyntagme"))) node)
                              (org-dict-tlfi--parse-csyntagme node))
			     (t (dom-texts node "")))))))

(defun org-dict-tlfi--parse-entry-content (dom)
  "Parse a single TLFi entry of DOM."
  (let* ((article-node (org-dict--dom-select dom ;; #lexicontent > .div
						      '((:attrs ((id . "lexicontent")))
							(> (:tag div)))))
	 (nodes (dom-children article-node))
	 (total-nodes (length nodes)))
  (org-dict-tlfi--replace-markup article-node)
  (org-dict-tlfi--remove-italic article-node)
  (org-dict-tlfi--replace-scripts article-node)
  (let ((result (cl-loop for node in nodes
		   if (stringp node) do 'nothing
                   else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cemploi"))) node)
                       collect (concat (org-dict-tlfi--parse-cemploi node) "\n")
                   else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_ccrochet"))) node)
                       collect (concat (org-dict-tlfi--parse-ccrochet node) "\n")
                   else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_paraputir"))) node)
                       collect (org-dict-tlfi--parse-paraputir node)
                   else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cdomaine"))) node)
                       collect (concat (org-dict-tlfi--parse-cdomaine node) "\n")
		   else if (org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_parah"))) node)
                       collect (org-dict-tlfi--parse-parah node)
		   else if (org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_parsynt"))) node)
                       collect (org-dict-tlfi--parse-parsynt node)
                   else if (org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_tabulation"))) node)
                       collect (org-dict-tlfi--parse-tabulation node)
                   else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cexemple"))) node)
                       collect (org-dict-tlfi--parse-cexemple node)
                   else if (org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_cvedette"))) node)
		       do 'nothing
		   else collect (concat (dom-texts node "") "\n\n")
	           end)))
  (apply #'concat (flatten-tree result)))))

(defun org-dict-tlfi--parse-entry (dom)
  "Parse a single word's entry whose the dom is DOM into an org buffer string."
  (let ((entry-name (downcase (org-dict-tlfi--current-entry-name dom))))
    (with-temp-buffer
      (org-mode)
      (org-insert-heading)
      (insert entry-name "\n")
      (insert (org-dict-tlfi--parse-entry-content dom))
      (insert "\n")
      ;; TODO parse correctly addendum (etymology etc.)
      (org-dict--org-fill-whole-buffer)
      (buffer-substring (point-min) (point-max)))))

;;; Main functions
(defun org-dict-tlfi-parse (dom base-url)
  "Parse a DOM coming from TLFI and outputs the result in Org mode.

BASE-URL must be provided to find other entries of the word,
in which case all of them will be parsed."
  (let ((dom-list (org-dict-tlfi--dom-list dom base-url)))
    (mapcar #'org-dict-tlfi--parse-entry dom-list)))

(defun org-dict-tlfi-not-found-p (dom)
  "Given a DOM, return non-nil if TLFi cannot find the entry, nil otherwise."
  (not (org-dict--dom-select dom ;; #lexicontent > .div
			'((:attrs ((id . "lexicontent")))
			  (> (:tag div))))))

;;; org-dict-tlfi.el ends here
(provide 'org-dict-tlfi)
