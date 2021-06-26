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
;;; Internal variables
;;; Internal functions
(defun org-dict--url-to-dom (url)
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
- NODE is not the root node (dom-parent would fail);
- NODE is not a string node (dom-parent limitation)."

  (let ((parent (dom-parent dom node)))
    (dom-add-child-before parent new-node node)
    (dom-remove-node parent node)))

(defun org-dict--dom-replace-nodes (dom nodes new-nodes)
  "As `org-dict--dom-replace-node', but accepts two lists of nodes."
  (cl-loop for node in nodes
	   for new-node in new-nodes
	   (org-dict--dom-replace-node dom node new-node)))

(defun org-dict--dom-by-class (dom classname) ;
  "Return nodes from DOM that matches CLASSNAME.

Unlike `dom-by-class' which matches nodes using CLASSNAME as a regexp, 
this function matches nodes whose one of its class is exactly CLASSNAME.
So, no need to call `dom-by-class' using complex regexp to say 'one of its class
is CLASSNAME'.
"
  (dom-search dom (lambda (node)
		    (let* ((node-class (dom-attr node 'class))
			   (node-classes (when node-class (split-string node-class))))
		      (member classname node-classes)))))

(defun org-dict--dom-select-base (dom-or-nodes selector)
  ;; If there is only a single node, wrap it in a list.
  ;; Use a heuristics to distinguish both cases.
  (let ((nodes (if (symbolp (car dom-or-nodes)) (list dom-or-nodes) dom-or-nodes)))
    (cl-remove-duplicates
     (cl-loop for dom in nodes
	      for tag = (plist-get selector :tag)
	      for attrs = (plist-get selector :attrs)
	      for id = (car (alist-get 'id attrs))
	      for class = (car (alist-get 'class attrs))
	      append (cond (id (let* (;; #id selector
				      (id-node (when id (car (dom-by-id dom (concat "^" id "$")))))
				      (node-class (when (and class id-node) (dom-attr id-node 'class)))
				      (node-classes (when (and class node-class) (split-string node-class)))
				      ;; .class#id selector
				      (class-id-node (when (and id-node
								(or (not class)
								    (member class node-classes)))
						       id-node))
				      ;; e.class#id selector
				      (tag-class-id-node (when (and class-id-node
								    (or (not tag) 
									(eq tag (car class-id-node))))
							   class-id-node)))
				 (when tag-class-id-node (list tag-class-id-node))))
			   (class (let* (;; .class selector
					 (class-nodes (when class (org-dict--dom-by-class dom class)))
					 ;; e.class selector
					 (tag-class-nodes (when class-nodes
							    (if tag (cl-remove-if-not
								     (lambda (node) (eq tag (car node)))
								     class-nodes)
							      class-nodes))))
				    tag-class-nodes))
			   (tag (dom-by-tag dom tag)))))))

(defun org-dict--dom-select (dom-or-nodes selector)
  "Select DOM node given CSS attribute/combinator SELECTOR.

A <selector> has the following syntax: 
'(:tag <tag> :attrs <attributes>) | 
'(<combinator> (:tag <tag> :attrs <attributes>) <selector>).

Simple selectors:
- 'e' corresponds to '(:tag e)
- '#myid' corresponds to '(:attrs ((id \"myid\")))
- '.myclass' corresponds to '(:attrs ((class \"myclass\")))
- 'e.#myid' corresponds to '(:tag e :attrs ((id \"myid\")))

Attribute selectors (only e[key~=value] is supported):
- 'e#myid', which is equivalent to 'e[id~=myid]', corresponds to 
  '(:tag e :attrs ((id \"myid\"))).
- 'e.class1.class2', which is equivalent to 'e[class~=class1][class~=class2]',
  corresponds to '(:tag e :attrs ((class \"class1\") (class \"class2\"))).

Combinator selectors (space, >, +, ~ combinators are supported):
- 'e1 e2' corresponds to '(descendant e1 e2)
- 'e1 > e2' corresponds to '(child e1 e2)
- 'e1 + e2' corresponds to '(adj-sibling e1 e2)
- 'e1 ~ e2' corresponds to '(gen-sibling e1 e2)

Besides, selectors are composable, e.g. 'ul > li#selected + li > a' corresponds 
to 

    '(child (:tag ul)
	(adj-sibling
	    (:tag li :attrs ((id \"selected\")))
	    (child (:tag li)
		(:tag a))))
"
  (if (or (plist-member selector :tag)
	  (plist-member selector :attrs))
      ;; simple selector
      (org-dict--dom-select-base dom selector)
    ;; combinator selector
    (let* ((combinator (car selector)))
      (cond ((eq combinator 'descendant))
	    ((eq combinator 'child))
	    ((eq combinator 'adj-sibling))
	    ((eq combinator 'gen-sibling))) 
      )))

(defun org-dict--replace-node-in-dom (dom rules)
  "Given a DOM, do node transformation following the RULES.

Each rule of RULES is a list (<selector> <transformer>) where <selector> selects
a node to work on and <transformer> transforms that node to a new one. Finally,
the selected nodes are replaced by the new nodes in the DOM.
"
  (cl-loop for rule in rules
	   for selector = (car rule)
	   for transformer = (cadr rule)
	   for nodes = (org-dict--dom-select dom selector)
	   for new-nodes = (mapcar #'transformer nodes)
	   do (org-dict--dom-replace-nodes dom nodes new-nodes)))

;;; org-dict-core.el ends here
(provide 'org-dict-core)
