;;; org-dict-cntrl.el --- Dictionary parse engine of the CNTRL -*- lexical-binding: t; -*-

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

;; This file provides a dictionary parse engine for the French dictionaries
;; (TLFi for now) available on CNTRL (https://cnrtl.fr/definition/).

;;; Code:
(require 'org-dict-core)
(require 'cl-lib)
;;; Custom group
;;;; General settings
;;; Internal variables
;;;;  Internal functions
(defun org-dict-cntrl--total-entries (dom)
  "From a word's DOM, return the number of entries. 
One word could have multiple entries (as noun, as adjective, etc.)"
  (length (dom-by-tag (dom-by-id dom "vtoolbar") 'li)))

(defun org-dict-cntrl--current-entry-name (dom)
  "From a word's DOM, return the current entry title."
  (dom-texts (dom-by-id dom "vitemselected")))

(defun org-dict-cntrl--dom-list (dom url)
  "Given the word's URL and its DOM, returns the DOM of all entries."
  (let ((dom-list (list dom)))
    (append dom-list
	    (cl-loop for i from 1 to (1- (org-dict-cntrl--total-entries dom))
		     ;; fetch <tlfi-url>/<word>/<nth-entry>
		     for entry-url = (concat url "/" (number-to-string i))
		     collect (org-dict--url-to-dom entry-url) into remaining-dom
		     finally return remaining-dom))))

(defun org-dict-cntrl--parse-entry-content (dom)
  (dom-texts (dom-by-id dom "lexicontent")))

(defun org-dict-cntrl--parse-entry (dom)
  "Parse a single word's entry whose the dom is DOM into an org buffer string."
  (let* ((entry-name (org-dict-cntrl--current-entry-name dom)))
  (with-temp-buffer
    (org-mode)
    (org-insert-heading)
    (insert entry-name "\n")
    (insert (org-dict-cntrl--parse-entry-content dom))
    (buffer-substring (point-min) (point-max)))))

;;; Interactive functions
(defun org-dict-cntrl-parse (dom &optional url)
  "Parse a DOM coming from a CNTRL dictionary and outputs in Org."
  (let* ((dom-list (org-dict-cntrl--dom-list dom url)))
    (mapcar #'org-dict-cntrl--parse-entry dom-list)))

;;; org-dict-cntrl.el ends here
(provide 'org-dict-cntrl)
