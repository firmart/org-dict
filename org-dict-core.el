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
;; TODO check HTTP code
(defun org-dict--url-to-dom (url)
  (with-current-buffer (url-retrieve-synchronously url t t)
    (libxml-parse-html-region (point-min) (point-max))))

(defun org-dict--dom-replace-node (dom node new-node)
  "Replace NODE in DOM with the NEW-NODE.

Before using this function, make sure that:
- NODE is not the root node (dom-parent would fail);
- NODE is not a string node (dom-parent limitation)."

  (let ((parent (dom-parent dom node)))
    (dom-add-child-before parent new-node node)
    (dom-remove-node parent node)))

(defun org-dict--dom-select (dom selector)
  "Select DOM node given CSS attribute/combinator SELECTOR.

A <selector> has the following syntax: 
'(<element> <attributes>) | '(<combinator> (<element> <attributes>)) <selector>).

Attributes:
- 'e#myid', which is equivalent to 'e[id~=myid]', corresponds to 
  '(e ((id \"myid\"))).
- 'e.class1.class2', which is equivalent to 'e[class~=class1][class~=class2]',
  corresponds to '(e ((class \"class1\") (class \"class2\"))).

Combinators:
- 'e1 e2' corresponds to '(descendant e1 e2)
- 'e1 > e2' corresponds to '(child e1 e2)
- 'e1 + e2' corresponds to '(adj-sibling e1 e2)
- 'e1 ~ e2' corresponds to '(gen-sibling e1 e2)

Besides, selectors are composable, e.g. 'ul > li#selected + li > a' corresponds 
to 

    '(child (ul nil)
	(adj-sibling
	    (li ((id \"selected\")))
	    (child (li nil)
		(a nil))))
"
  )

;; (selector transformer) => (node new-node)
(defun org-dict--replace-node-in-dom (dom rules)
  "Given a DOM, do node transformation following the RULES.

Each rule of RULES is a list (<selector> <transformer>) where <selector> selects
a node to work on and <transformer> transforms that node to a new one. Finally,
the selected nodes are replaced by the new nodes in the DOM.
"
  (cl-loop for rule in rules
	   for selector = (car rule)
	   for transformer = (cadr rule)
	   for node = (org-dict--dom-select dom selector)
	   for new-node = (transformer node)
	   do (org-dict--dom-replace-node dom node new-node)))

;;; org-dict-core.el ends here
(provide 'org-dict-core)
