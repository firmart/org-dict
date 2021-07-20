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

(defcustom org-dict-tlfi-heading-max-length 80
  "The maximal length of a Org heading.

If the heading is too long, it will be moved to the content."
  :type 'integer
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

;;; Internal variables
(defvar org-dict-tlfi--surround-regexp "^[[:space:]\n]*\\(\\)\\(.*?\\)\\([,: ]*\\)?$")

(defvar org-dict-tlfi--numbering-regexps '("\\([0-9]+?.*Section\\)"
					"\\([I]\\{1,3\\}\\|[I]?V\\|V[I]\\{1,3\\}\\|[I]?X\\)"
					"\\([A-H]\\)"
					"\\([0-9]+?\\)"
					"\\([a-z]\\)"
					"\\([αβγδϵζηθικλ]\\)"))

(defvar org-dict-tlfi--ref-regex (format "^\\(%s? ?%s?\\)"
					 "\\(Section\\)"
					 (string-join (cdr org-dict-tlfi--numbering-regexps) "? ?")))

(defvar org-dict-tlfi--cplan-regexps (list
				      (format "%s%s%s" "^[ ]*?" (nth 0 org-dict-tlfi--numbering-regexps) "\\.?[ ]*?$")
				      (format "%s%s%s" "^[ ]*?" (nth 1 org-dict-tlfi--numbering-regexps) "\\.[ ]*?−?[ ]*?$")
				      (format "%s%s%s" "^[ ]*?" (nth 2 org-dict-tlfi--numbering-regexps) "\\.[ ]*?−?[ ]*?$")
				      (format "%s%s%s" "^[ ]*?" (nth 3 org-dict-tlfi--numbering-regexps) "\\.[ ]*?−?$")
				      (format "%s%s%s" "^[ ]*?" (nth 4 org-dict-tlfi--numbering-regexps) ")[ ]*?$")
				      (format "%s%s%s" "^[[:space:]\n]*?" (nth 5 org-dict-tlfi--numbering-regexps) ")[ ]*?")))

(defvar org-dict-tlfi--potential-link-regexps '("[vV]\\."))
;;; Service
(defvar org-dict-tlfi-service '(:dict tlfi
			              :name "TLFi"
			              :url "https://cnrtl.fr/definition/%s"
			              :parser org-dict-tlfi-parse
			              :not-found-p org-dict-tlfi-not-found-p))
;;; Internal functions
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
	 (entry-number-string (when superscript
				(concat " (" (if (stringp superscript) superscript (dom-texts superscript "")) ")")))
	 (pos-node (car (org-dict--dom-select title-node '((:tag span :attrs ((class . "tlf_ccode")))))))
	 (part-of-speech (propertize (caddr pos-node) 'font-lock-face 'org-dict-pos-face)))
    (format "%s%s %s" word (or entry-number-string "") part-of-speech)))

(defun org-dict-tlfi--dom-list (dom url)
  "Given the word's URL and its DOM, return the DOM of all entries."
  (let ((dom-list (list dom)))
    (append dom-list
	    (cl-loop for i from 1 to (1- (org-dict-tlfi--total-entries dom))
		     ;; fetch <tlfi-url>/<word>/<nth-entry>
		     for entry-url = (concat url "/" (number-to-string i))
		     collect (org-dict--url-to-dom entry-url) into remaining-dom
		     finally return remaining-dom))))

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

(defun org-dict-tlfi--is-title-component-p (node)
  "Return non-nil when NODE's class is tlf_c{emploi, crochet, domaine, etc.}."
  (or (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cemploi"))) node)
      (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_ccrochet"))) node)
      (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cdomaine"))) node)
      (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cconstruction"))) node)))

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
      (format "%s%s. %s " (org-dict-tlfi--item-indentation current-depth) numbering title))))

(defun org-dict-tlfi--parse-cemploi (cemploi-node)
  (propertize (dom-texts cemploi-node "") 'font-lock-face 'org-dict-use-face))

(defun org-dict-tlfi--parse-cdefinition (cdefinition-node)
  (propertize (dom-texts cdefinition-node "") 'font-lock-face 'org-dict-definition-face))

(defun org-dict-tlfi--emphasis-p (node)
  (let ((tag (car node)))
    (or (eq tag 'i)
	(eq tag 'b))))

(defun org-dict-tlfi--sup-p (node)
  (let ((tag (car node)))
    (eq tag 'sup)))

(defun org-dict-tlfi--potential-link-match (str)
  (cl-loop for regex in org-dict-tlfi--potential-link-regexps
	   if (string-match regex str) return regex
	   finally return nil))

(defun org-dict-tlfi--make-link (pre-string nodes regex pattern)
  ;; <nodes> := <pre-string><desc><post-string>
  ;; <pre-string> := <pre-text><left-inner-text>
  ;; <post-string> := <right-inner-text><post-text>
  (let* ((n0 (nth 0 nodes))
	 (n1 (nth 1 nodes))
	 (n2 (nth 2 nodes))
	 (n3 (nth 3 nodes))
	 (pre-text-end (string-match regex pre-string))
	 (pre-text (substring pre-string 0 pre-text-end))
	 (left-inner-text (substring pre-string pre-text-end (length pre-string)))
	 word sup post-string desc post-text-beg ref post-text-end right-inner-text post-text)

    (setq word (nth 2 n0))
    (cond ((eq pattern 'v-section)
	   (setq sup (nth 2 n2))
	   (setq post-string n3))
	  ((eq pattern 'v-normal)
	   (setq post-string n1)))

    (unless (eq pattern 'it-word)
      (setq post-text-beg (string-match org-dict-tlfi--ref-regex post-string))
      (unless post-text-beg
	(error "TLFi: cannot find the end of the interlink in string %S" post-string))

      (setq ref (match-string 1 post-string))
      (setq post-text-end (+ post-text-beg (length ref)))
      (setq right-inner-text (substring post-string 0 post-text-end))
      (setq post-text (substring post-string post-text-end (length post-string))))

    ;; Cannot display superscript in Org link.
    (setq desc (concat left-inner-text word (when sup (format "%s%s " n1 sup)) right-inner-text))
    (format "%s%s%s" pre-text
	    (org-make-link-string
	     (format "%s:%s::%s" org-dict-tlfi-link-type word ref)
		     desc)
	     post-text)))

(defun org-dict-tlfi--parse-probe-link-end (nodes)
  ;; v-section :: ("v."|"V.") <italic-word> <number-string> ("re"|"e") <section-ref>
  ;; v-normal :: ("v."|"V.") <italic-word> <ref>
  ;; TODO v-word-nth :: ("v."|"V.") <italic-word> <bold-number>
  ;; TODO v-section-nth :: ("v."|"V.") <italic-word> <bold-number> <number-string> ("re"|"e") <section-ref>
  ;; TODO v-normal-nth :: ("v."|"V.") <italic-word> <bold-number> <ref>
  ;; TODO it-section :: <italic-word> <section-ref>
  ;; TODO it-normal :: <italic-word> <ref>
  ;; it-word :: <italic-word>
  (let ((unknown '(:pattern unknown)))
    (if (eq (car (nth 0 nodes)) 'i)
	(cond ((and (and (stringp (nth 1 nodes))
			 (string-match "^[[:space:]]*?[0-9][[:space:]]*?$" (nth 1 nodes)))
		    (eq (car (nth 2 nodes)) 'sup)
		    (and (stringp (nth 3 nodes))
			 (string-match "^Section.*$" (nth 3 nodes))))
	       '(:pattern v-section :count 4))
	      ((and (stringp (nth 1 nodes)))
	       (if (string-match org-dict-tlfi--ref-regex (nth 1 nodes))
		   '(:pattern v-normal :count 2)
		'(:pattern it-word :count 1)))
	      (t unknown))
      unknown)))

(defun org-dict-tlfi--parse-text-node (tnode)
  "Parse TLFi's HTML text node (node that is not recursive)."
  (apply #'concat
	 (cl-loop with children = (dom-children tnode)
		  while children
		  for node = (pop children)
		  for result = (cond ((stringp node)
				      (cond ((stringp (car children)) node)
					    ;; If the next node is a <i> or a <b>
					    ((org-dict-tlfi--emphasis-p (car children))
					     ;; Check if the current node contains "v." or "V."
					     (let ((regex (org-dict-tlfi--potential-link-match node))
						    result pattern count nodes)
					       (if regex
						   ;; If so we have an interlink: we pop out all nodes to the end of the link
						   (progn
						     (setq result (org-dict-tlfi--parse-probe-link-end children))
						     (setq pattern (plist-get result :pattern))
						     (setq count (plist-get result :count))
						     ;; and return the Org link
						     (if (eq pattern 'unknown)
							 node
						       (setq nodes (cl-subseq children 0 count))
						       (setq children (cl-subseq children count))
						       (org-dict-tlfi--make-link node nodes regex pattern)))
						 node)))
					    ((org-dict-tlfi--sup-p (car children))
					     (format "%s^{%s} " node (dom-texts (pop children) "")))
					    (t node)))
				     (t (dom-texts node "")))
		  collect result)))

(defun org-dict-tlfi--parse-ccrochet (node)
  (let ((text (org-dict-tlfi--parse-text-node node)))
    (propertize text 'font-lock-face 'org-dict-comment-face)))

(defun org-dict-tlfi--parse-cdomaine (node)
  (let ((text (org-dict-tlfi--parse-text-node node)))
    (propertize text 'font-lock-face 'org-dict-domain-face)))

(defun org-dict-tlfi--parse-csyntagme (node)
  (let ((text (org-dict-tlfi--parse-text-node node)))
    (propertize text 'font-lock-face 'org-dict-syntagma-face)))

(defun org-dict-tlfi--parse-cconstruction (node)
  (let ((text (org-dict-tlfi--parse-text-node node)))
    text))

;; TODO propertize differently Synon. & Anton.
(defun org-dict-tlfi--parse-csynonime (csynonime-node)
  "Parse node of class 'tlf_csynonime'.

The misspelling comes from TLFi HTML source code."
  (propertize (dom-texts csynonime-node "") 'font-lock-face 'org-dict-synonym-face))

(defun org-dict-tlfi--parse-paraputir (paraputir-node current-depth)
  (let ((children (dom-children paraputir-node)))
    (apply #'concat
	   (cl-loop with first-node? = t
	            for node in children
		    if first-node?
		        if (stringp node)
			    collect (format "%s- " (org-dict-tlfi--item-indentation current-depth t))
			else
			    do (error "TLFi: paraputir's first children is not a string")
			end
		    else
		        collect (org-dict-tlfi--general-parse node t current-depth)
		    end
		    do (setq first-node? nil)))))

(defun org-dict-tlfi--parse-tabulation (tabulation-node current-depth)
  (let ((children (dom-children tabulation-node)))
    (apply #'concat
	   (cl-loop for node in children
		    collect (cond
			     ((stringp node) (org-dict-tlfi--parse-string node))
		             ((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cexemple"))) node)
                              (org-dict-tlfi--parse-cexemple node current-depth))
			     (t (dom-texts node "")))))))

(defun org-dict-tlfi--parse-string (str)
  "Parse a STR node."
  (if (string= " " str) ""
    (replace-regexp-in-string "[[:space:]\n]+" " " str)))

;; TODO
(defun org-dict-tlfi--parse-cothers (node)

  )

(defun org-dict-tlfi--item-indentation (current-depth &optional nested?)
  (let ((indent-level (+ current-depth
			 (if nested? 1 0)
			 (- (1+ org-dict-tlfi-heading-max-depth)))))
    (make-string
     (* (if (> indent-level 0) indent-level 0)
	org-dict-indentation-width)
     ? )))

;; TODO propertize author, bbg, date
;; TODO take account of depth
(defun org-dict-tlfi--parse-cexemple (node current-depth)
  (let* ((content (dom-texts node ""))
	 (unumbered-content (replace-regexp-in-string "^[0-9]+\\.[ ]*\\(.*\\)" "\\1" content))
	 (target-content (replace-regexp-in-string "− \\(.*\\)" "\\1" unumbered-content))
	 (indentation (org-dict-tlfi--item-indentation current-depth t)))
    (format "\n%s#+BEGIN_QUOTE\n%s%s\n%s#+END_QUOTE\n"
	    indentation
	    indentation
	    (propertize target-content 'font-lock-face 'org-dict-example-face)
	    indentation)))

;; Source of sigles http://www.languefrancaise.net/forum/viewtopic.php?id=11703
(defun org-dict-tlfi--parse-parah (parah-node &optional root-depth)
  "Parse TLFi recursive div.tlf_parah node PARAH-NODE.

ROOT-DEPTH is used to correctly determine the Org heading and numbered list depth."
  (let* ((children (dom-children parah-node))
	 (total-nodes (length children)))
    (apply #'concat
	   (cl-loop for node in children
                    with node-count = 0
                    with root-depth = root-depth
                    with current-depth
                    with numbering-string
                    with title
                    with title-settled?
                    do (setq node-count (1+ node-count))
                    if (stringp node)
                         if (not title-settled?)
                             do (setq title (concat title (org-dict-tlfi--parse-string node)))
                    	    else
                             collect (org-dict-tlfi--parse-string node)
                    	    end
                    else if (org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cplan"))) node)
                         do (setq current-depth (org-dict-tlfi--parse-cplan-depth (dom-texts node ""))) and
                         do (setq numbering-string (org-dict-tlfi--parse-cplan-string (dom-texts node ""))) and
                         when (not root-depth) do (setq root-depth current-depth) end
                    else if (org-dict-tlfi--is-title-component-p node)
                         if (not title-settled?)
                             do (let ((new-title (concat title " " (org-dict-tlfi--general-parse node t current-depth))))
                                  (if (> (length new-title) org-dict-tlfi-heading-max-length)
                             	 (setq title-settled? t)
                                    (setq title new-title)))
                             and if title-settled?
                                 collect (org-dict-tlfi--create-section-title title root-depth current-depth numbering-string)
                                 end
                             end and
                             if title-settled?
                                 collect (org-dict-tlfi--general-parse node nil current-depth)
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
                         collect (org-dict-tlfi--general-parse node nil current-depth)
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

(defun org-dict-tlfi--general-parse (node &optional inline? current-depth)
  "Parse a TLFi HTML NODE. Line breaks depend on INLINE?."
  (cond ((stringp node) (org-dict-tlfi--parse-string node))
	((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cemploi"))) node)
	 (concat (org-dict-tlfi--parse-cemploi node) (if inline? " " "\n")))
	((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_ccrochet"))) node)
	 (concat (org-dict-tlfi--parse-ccrochet node) (if inline? " " "\n\n")))
	((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_csynonime"))) node)
	 (org-dict-tlfi--parse-csynonime node))
	((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cdefinition"))) node)
	 (concat (org-dict-tlfi--parse-cdefinition node) " "))
	((org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_paraputir"))) node)
	 (concat "\n\n" (org-dict-tlfi--parse-paraputir node current-depth) "\n"))
	((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_csyntagme"))) node)
	 (concat (org-dict-tlfi--parse-csyntagme node) (if inline? " " "\n")))
	((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cdomaine"))) node)
	 (concat (org-dict-tlfi--parse-cdomaine node) (if inline? " " "\n")))
	((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cconstruction"))) node)
	 (org-dict-tlfi--parse-cconstruction node))
	((org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_parah"))) node)
	 (concat "\n" (org-dict-tlfi--parse-parah node)))
	((org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_parsynt"))) node)
	 (org-dict-tlfi--parse-parsynt node))
	((org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_tabulation"))) node)
	 (org-dict-tlfi--parse-tabulation node current-depth))
	((org-dict--dom-node-simple-selector-p '(:tag span :attrs ((class . "tlf_cexemple"))) node)
	 (org-dict-tlfi--parse-cexemple node current-depth))
	((org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_cvedette"))) node)
	 "")
	((org-dict--dom-node-simple-selector-p '(:tag div :attrs ((class . "tlf_parothers"))) node)
	 (concat (dom-texts node "") (if inline? "" "\n")))
	(t (concat (dom-texts node "") (if inline? "" "\n")))))

(defun org-dict-tlfi--parse-entry-content (dom)
  "Parse a single TLFi entry of DOM."
  (let* ((article-node (org-dict--dom-select dom ;; #lexicontent > .div
						      '((:attrs ((id . "lexicontent")))
							(> (:tag div)))))
	 (nodes (dom-children article-node))
	 (total-nodes (length nodes)))
  (let ((result (cl-loop for node in nodes
		   collect (org-dict-tlfi--general-parse node))))
  (apply #'concat result))))

(defun org-dict-tlfi--parse-entry (dom)
  "Parse a single word's entry whose the dom is DOM into an org buffer string."
  (let ((entry-name (downcase (org-dict-tlfi--current-entry-name dom))))
    (with-temp-buffer
      (org-mode)
      (auto-fill-mode)
      (org-insert-heading)
      (insert entry-name "\n")
      (insert (org-dict-tlfi--parse-entry-content dom))
      (insert "\n")
      ;; TODO parse correctly addendum (etymology etc.)
      (org-dict--fill-whole-buffer '(quote-block paragraph item))
      (buffer-substring (point-min) (point-max)))))

;;; Org link
(defvar org-dict-tlfi-link-type "org-dict-tlfi")
(org-link-set-parameters org-dict-tlfi-link-type :follow #'org-dict-tlfi-follow)
;; TODO: Looks like when more dictionaries will come up
;; 1. we could move this function in org-dict-core.el
;; 2. and rename it as org-dict--follow-builder which would take
;;    a link parser and a service as parameters.
(defun org-dict-tlfi-follow (link &optional arg)
  (let* ((link-data (org-dict-tlfi--parse-link link))
	 (word (plist-get link-data :word))
	 (ref (plist-get link-data :ref)))
    (org-dict--setup-follow)
    (org-dict--parse word org-dict-tlfi-service ref)))

;; TODO
(defun org-dict-tlfi--parse-link (link)

  )
;;; Main functions
(defun org-dict-tlfi-parse (dom base-url)
  "Parse a DOM coming from TLFi and outputs the result in Org mode.

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
