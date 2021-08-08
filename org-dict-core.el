;;; org-dict-core.el --- Utilities to write parse engines -*- lexical-binding: t; -*-

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

;; This file provides utilities to write parse engine.

;;; Code:
;;; Custom group
;;;; General settings
(defcustom org-dict-indentation-width 4
  "The number of spaces used for indentation."
  :type 'integer
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

;;;; Faces
(defface org-dict-definition-face
  '((t (:inherit font-lock-string-face :foreground "NavajoWhite")))
  "Face for the definition of a word."
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

(defface org-dict-use-face
  '((t (:inherit font-lock-comment-face :foreground "LightSkyBlue")))
  "Face for the use of a word (e.g. archaic, literary, etc.)"
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

(defface org-dict-syntagma-face
  '((t (:inherit font-lock-comment-face :foreground "yellow")))
  "Face for syntagma of a definition."
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

(defface org-dict-synonym-face
  '((t (:inherit font-lock-comment-face :foreground "MediumPurple")))
  "Face for synonyms or antonyms."
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

(defface org-dict-pos-face
'((t (:weight bold :box 1 :foreground "#61afef" :background "#38394c" :inherit org-level-1)))
  "Face for part of speech."
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

(defface org-dict-example-face
  '((t (:inherit font-lock-string-face :foreground "aquamarine")))
  "Face for examples of the definition."
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

(defface org-dict-example-source-face
  '((t (:inherit org-dict-example-face :underline t)))
  "Face for example's sources of the definition."
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

(defface org-dict-domain-face
  '((t (:inherit org-special-keyword)))
  "Face for the domain of study of the definition."
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

(defface org-dict-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for comments, annotations, notes."
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

;;; Internal functions
;;;; Org link utilities

;; TODO setup follow/back stack as help-mode
(defun org-dict--setup-follow ()
  (unless (eq (current-buffer)
	      (get-buffer org-dict-buffer))
    (switch-to-buffer (get-buffer-create org-dict-buffer)))
  (read-only-mode -1)
  (erase-buffer))
;;;; Parsing
(defun org-dict--parse (word service &optional ref)
  "Parse query result of WORD from the dictionary SERVICE."
  (let* ((url (org-dict--word-url word service))
	 (dom (org-dict--url-to-dom url))
	 (parser (plist-get service :parser))
	 (positioner (plist-get service :positioner))
	 (not-found-p (plist-get service :not-found-p)))
    (if (or (not dom)
	    (and not-found-p (funcall not-found-p dom)))
	(error "Cannot find definition of '%s' in %s" word (plist-get service :name))
      (mapc #'insert (funcall parser dom url))
      ;; FIXME: in a multi-dictionaries context, this may not be enough
      (when (and ref positioner)
	(funcall positioner ref)))))
;;;; Utilities
(defun org-dict--fold-element (types)
  "Fold Org elements of TYPES in the whole Org buffer."
  (interactive
   (list '(quote-block)))
  (org-element-map
      (org-element-parse-buffer 'element)
      types
    (lambda (node)
      (goto-char (org-element-property :begin node))
      (org-cycle))))

(defun org-dict--fill-whole-buffer (types)
  "Fill Org elements of TYPES in the whole Org buffer."
  (interactive (list 'paragraph))
  (org-with-wide-buffer
   (mapc (lambda (elt-beg)
	   (goto-char elt-beg)
	   (org-fill-paragraph nil t))
	 (reverse
	  (org-element-map (org-element-parse-buffer 'element)
	      types
	    (lambda (elt)
	      (when-let* ((target-point (org-element-property :contents-begin elt)))
		target-point)))))))

(defun org-dict--remove-redundant-spaces (str)
  "Remove redundant spaces in STR which are ignored by HTML or LaTeX rendering."
  (string-trim
   (replace-regexp-in-string
    "\\( \\)+" " " str)))

(defun org-dict--insert-string-with-properties (str)
  "Insert STR with its text properties (even with font-lock-mode enabled).

Required Emacs >= 28 due to the use of `object-intervals'."
  (if (and (>= emacs-major-version 28)
	   font-lock-mode)
      (mapc (lambda (text-prop)
	      (let ((beg (car text-prop))
		    (end (cadr text-prop))
		    (props (caddr text-prop)))
		(cl-loop while props
			 for prop = (pop props)
			 for value = (pop props)
			 when (eq prop 'face)
			   do (put-text-property beg end 'font-lock-face value str))))
	    (object-intervals str)))
  (insert str))

;;;; DOM related
(defun org-dict--url-to-dom (url)
  "Given a URL, return the DOM of the fetched response (when HTTP status = 200)."
  (let* ((buffer (url-retrieve-synchronously url t t))
	 (code (url-http-symbol-value-in-buffer 'url-http-response-status buffer)))
    (when (= code 200)
      (with-current-buffer buffer
        ;; Move to the end of the headers
        (goto-char (point-min))
        (re-search-forward "^\r?\n" nil t)
        (backward-char 1)
	(libxml-parse-html-region (point) (point-max))))))

(defun org-dict--dom-replace-node (dom node new-node)
  "Replace NODE in DOM with the NEW-NODE.

Before using this function, make sure that:
- NODE is not the root node (`dom-parent' would fail);
- NODE is not a string node (`dom-parent' limitation)."

  (let ((parent (dom-parent dom node)))
    (dom-add-child-before parent new-node node)
    (dom-remove-node parent node)))

(defun org-dict--dom-replace-nodes (dom nodes new-nodes)
  "As `org-dict--dom-replace-node', but with two lists of NODES and NEW-NODES.

DOM is an HTML DOM."
  (cl-loop for node in nodes
	   for new-node in new-nodes
	   do (org-dict--dom-replace-node dom node new-node)))

(defun org-dict--dom-node-class (node)
  "Return NODE class as a list."
  (let ((node-class (dom-attr node 'class)))
    (when node-class (split-string node-class))))

(defun org-dict--dom-node-class-p (classes node)
  "Return t whenever NODE has CLASSES as classes.

 CLASSES can be given as a list of string for multiple classes or
 as a string for a single class."
  (let ((node-classes (org-dict--dom-node-class node)))
    (if (listp classes)
 	(cl-subsetp classes node-classes :test #'string=)
      (member classes node-classes))))

(defun org-dict--dom-node-simple-selector-p (simple-selector node)
  "Return t whenever NODE match a SIMPLE-SELECTOR query."
  (and (not (stringp node))
       (or (not simple-selector) ;; nil corresponds to the universal selector
	   (and (let ((tag (plist-get simple-selector :tag)))
		  (or (not tag) (eq tag (car node))))
		(cl-loop for (key . value) in (plist-get simple-selector :attrs)
			 with match-result
			 if (not key)
			   do (error "Attribute selector must have a key")
			 else if (not value)
			   do (setq match-result (member key (mapcar #'car (dom-attributes node))))
			 else if (eq key 'class)
			   do (setq match-result (org-dict--dom-node-class-p (split-string value) node))
			 else
			   do (setq match-result (member (cons key value) (dom-attributes node)))
			 if (not match-result) return nil
			 finally return t)))))

(defun org-dict--dom-by-attrs (dom attrs)
  "Return nodes from DOM that match attributes ATTRS.

Unlike `dom-by-class' which matches nodes using regexp,
this function matches nodes whose classes contain those given in ATTRS.
E.g. '((class . \"bottombox\")) will match nodes having attribute '((class . \"box bottombox\"))"
  (let* ((attrs-keys (remove nil (mapcar #'car attrs))))
    (if (not attrs-keys)
	dom
      (dom-search dom
		  (lambda (node)
		    (org-dict--dom-node-simple-selector-p `(:attrs ,attrs) node))))))

(defun org-dict--dom-simple-select (dom-or-nodes selector)
  "Given a simple SELECTOR, return its query result on DOM-OR-NODES.

Specifically,
- if DOM-OR-NODES is a DOM (i.e. not a nodes list), the result is the query of a
  simple SELECTOR.
- if DOM-OR-NODES is a nodes list (say result from a selector s1), then the
  result corresponds to 's1 SELECTOR' (namely the result of the descendant
  combinator of s1 and SELECTOR)."

   ;; If there is only a single node, wrap it in a list.

(cl-remove-duplicates
   (cl-loop with nodes = (if (org-dict--dom-p dom-or-nodes) (list dom-or-nodes) dom-or-nodes)
	    with tag = (plist-get selector :tag)
	    with attrs = (plist-get selector :attrs)
	    with id = (alist-get 'id attrs)
	    for dom in nodes
	    append (cond (id (let ((id-nodes (dom-by-id dom (concat "^" id "$"))))
			       (when (org-dict--dom-node-simple-selector-p selector (car id-nodes)) id-nodes)))
			 (attrs (let ((attrs-nodes (org-dict--dom-by-attrs dom attrs)))
				    (if tag (cl-remove-if-not
					     (lambda (node) (eq tag (car node)))
					     attrs-nodes)
				      attrs-nodes)))
			 (tag (dom-by-tag dom tag))))))

(defun org-dict--dom-p (dom)
  "Return t whenever DOM is a dom (and not a list of nodes)."
  (symbolp (car dom)))

(defun org-dict--select-child (dom nodes selector)
  "Given NODES (the query result of s1) on the DOM, return 's1 > SELECTOR'."
  (let* ((tag (plist-get selector :tag))
	 (attrs (plist-get selector :attrs))
	 (id (alist-get 'id attrs))
	 (class (alist-get 'class attrs))
	 (child-nodes (cl-mapcan #'dom-non-text-children nodes)))
    (cl-remove-if-not (lambda (node)
			(org-dict--dom-node-simple-selector-p selector node))
		      child-nodes)))

(defun org-dict--dom-nodes-subsequent-sibling (dom nodes &optional adjacent?)
  "Given NODES from DOM, return a list of subsequent siblings of them.

If ADJACENT? is non-nil, then hold only one subsequent sibling."
  (cl-remove-duplicates
   (cl-loop for node in nodes
	    for siblings = (dom-non-text-children (dom-parent dom node))
	    append (cl-loop for sibling in siblings
			    with node-seen?
			    if node-seen? collect sibling
			    until (and node-seen? adjacent?)
			    if (eq sibling node) do (setq node-seen? t)))))

(defun org-dict--select-subsequent-sibling (dom nodes selector &optional adjacent?)
  "Given NODES (the query result of s1) on the DOM, return 's1 ~ SELECTOR'.

If ADJACENT? is non-nil, return the result of 's1 + SELECTOR'."
  (let* ((sibling-nodes (org-dict--dom-nodes-subsequent-sibling dom nodes adjacent?)))
    (cl-remove-if-not (lambda (node)
			(org-dict--dom-node-simple-selector-p selector node))
		      sibling-nodes)))

(defun org-dict--dom-select (dom selector)
  "Select DOM node given CSS attribute/combinator SELECTOR.

A SELECTOR has the following syntax:
<selector> := '(<simple_selector> <combinator_selector>...)
<simple_selector> := '(:tag <tag> :attrs <attributes>)
<combinator_selector> := '(<combinator> <simple_selector>)
where
<tag> is a symbol
<combinator> is one of '=>, '>, '+, '~
<attributes> is an alist of key (symbol) value (string)

Simple selectors:
- 'e' corresponds to '(:tag e)
- '#myid' corresponds to '(:attrs ((id . \"myid\")))
- '.myclass' corresponds to '(:attrs ((class . \"myclass\")))

Attribute selectors (only e[key] and e[key~=value] is supported):
- '[key]', corresponds to (:attrs ((key)))
- 'e[key]', corresponds to (:tag e :attrs ((key)))
- 'e[key~=\"value\"]', corresponds to (:tag e :attrs ((key . \"value\")))
- 'e#myid', which is equivalent to 'e[id~=myid]', corresponds to
  '(:tag e :attrs ((id . \"myid\"))).
- 'e.class1.class2', which is equivalent to 'e[class~=class1][class~=class2]',
  corresponds to '(:tag e :attrs ((class . \"class1 class2\"))).

Combinator selectors (space, >, +, ~ combinators are supported):
- 'e1 e2' corresponds to '(=> e1 e2)
- 'e1 > e2' corresponds to '(> e1 e2)
- 'e1 + e2' corresponds to '(+ e1 e2)
- 'e1 ~ e2' corresponds to '(~ e1 e2)

Besides, selectors are composable, e.g. 'ul > li#selected + li > a' corresponds
to
   '((:tag ul)
    (> (:tag li :attrs ((id \"selected\"))))
    (+ (:tag li))
    (> (:tag a)))"
  (cl-loop with combinator-selectors = (cdr selector)
	   with base-selector = (car selector)
	   with nodes = (org-dict--dom-simple-select dom base-selector)
	   for combinator-selector in combinator-selectors
	   for combinator = (car combinator-selector)
	   for selector = (cadr combinator-selector)
	   do (setq nodes (cond ((eq combinator '=>) (org-dict--dom-simple-select nodes selector))
				((eq combinator '>)  (org-dict--select-child dom nodes selector))
				((eq combinator '+)  (org-dict--select-subsequent-sibling dom nodes selector t))
				((eq combinator '~)  (org-dict--select-subsequent-sibling dom nodes selector))))
	   finally return nodes))

(defun org-dict--replace-node-in-dom (dom rules)
  "Given a DOM, do node transformation following the RULES.

Each rule of RULES is a list (<selector> <transformer>) where <selector> selects
a node to work on and <transformer> transforms that node to a new one.  Finally,
the selected nodes are replaced by the new nodes in the DOM."
  (cl-loop for rule in rules
	   for selector = (car rule)
	   for transformer = (cadr rule)
	   for nodes = (org-dict--dom-select dom selector)
	   for new-nodes = (mapcar #'transformer nodes)
	   do (org-dict--dom-replace-nodes dom nodes new-nodes)))

;;; org-dict-core.el ends here
(provide 'org-dict-core)
