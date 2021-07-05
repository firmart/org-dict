;;; org-dict.el --- Dictionary formatted in Org buffer -*- lexical-binding: t; -*-

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

;; Org-dict is endowed with the power of Org-mode to bring the dictionary
;; experience to the next level.  It increases readability,
;; reusability and the speed to consult online dictionaries.


;;; Code:

;; Built-in Emacs lib
(require 'org)
(require 'ox-html)
(require 'cl-lib) 
;; Dictionary parse engines
(require 'org-dict-core)
(require 'org-dict-tlfi)

;;; Custom group
;;;; General settings

(defgroup org-dict nil
  "Org-dict Settings"
  :group 'org
  :package-version '(org-dict . "0.1"))

(defcustom org-dict-buffer "*org-dict*"
  "Buffer used by Org-dict to display dictionary query result."
  :type 'string
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

(defcustom org-dict-services
  '((:dict tlfi
     :name "TLFi"
     :url "https://cnrtl.fr/definition/%s"
     :parser org-dict-tlfi-parse
     :not-found-p org-dict-tlfi-not-found-p))
  "List of dictionary's service available in Org-dict.

Each service must contain the following key-values:
- `:dict' : The dictionary symbol.
- `:name' : The dictionary name (used as a Org heading).
- `:url' : The query url where %s denotes the word to search.
- `:parser' : The parser function of the dictionary which takes a DOM returned
by `libxml-parse-html-region' and a URL.
- `:not-found-p' : A predicate that returns non-nil whenever the given DOM
  indicates a failed query. It will be used to error out early in case the web page 
doesn't response with a 404 error when a word entry cannot be found.
"
  :type 'plist
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

(defcustom org-dict-default-dictionary 'tlfi
  "The default dictionary used by Org-dict."
  :type 'symbol
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

(defcustom org-dict-default-lang 'fr
  "The default language used by Org-dict."
  :type 'symbol
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

(defcustom org-dict-by-langs
  '((fr tlfi))
  "An alist of (<lang-symbol> . <dict-symbols>).

<lang-symbol> must be an ISO 639-1 language code.
<dict-symbols> is a list of dictionaries symbols appearing in `org-dict-dictionaries'.

This alist is used to display results gathered from all dictionaries of a
specific language. 
See `\\[universal-argument] \\[universal-argument]' `org-dict-at-point'."
  :type '(alist
          :key-type (symbol :tag "Language")
          :value-type ((repeat symbol) :tag "List of dictionaries"))
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

;;; Internal variables
;;; Internal functions
(defun org-dict--parse (word service)
  "Parse a dictionary and return the result"
  (let* ((url (format (plist-get service :url) (downcase word)))
	 (dom (org-dict--url-to-dom url))
	 (parser (plist-get service :parser))
	 (not-found-p (plist-get service :not-found-p)))
    (if (or (not dom)
	    (funcall not-found-p dom))
	(error "Cannot find definition of '%s' in %s" word (plist-get service :name))
      (mapc #'insert (funcall parser dom url)))))

;;; Interactive functions
;; TODO: integration with pdf-view, guess-language
(defun org-dict (word &optional dict-or-dicts arg)
  "Query the definition of WORD in DICT-OR-DICTS.

DICT-OR-DICTS is either a symbol or a list of symbols. They must match
the field `:dict' of a service in `org-dict-services'.

With a `\\[universal-argument]' prefix argument, it prompts to choose 
a specific dictionary.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, 
it prompts to choose a language and display in the buffer `org-dict-buffer' 
all results gathered from dictionaries of that language.
"
  (interactive "MWord: \ni\nP")
  (let* ((dicts (or (when dict-or-dicts
		      (if (symbolp dict-or-dicts)
			  (list dict-or-dicts)
			dict-or-dicts))
		    ;; Prompt a dictionary
		    (when (equal arg '(4))
		      (list (completing-read "Dictionary: "
					     (mapcar (lambda (s) (plist-get s :symbol))
						     org-dict-services))))
		    ;; Prompt a language and return dictionaries of that language
		    (when (equal arg '(16))
		      (alist-get (intern (completing-read "Language: " org-dict-by-langs))
				 org-dict-by-langs))
		    ;; Default dictionary or dictionaries of default language
		    (list org-dict-default-dictionary)
		    (car (alist-get org-dict-default-lang org-dict-by-langs))
		    (error "There is not available dictionary")))
	 (dict-services (mapcar (lambda (dict)
				  (cl-loop for service in org-dict-services
					   when (eq dict (plist-get service :dict))
					   return service))
				dicts))
	 (urls-parsers (mapcar (lambda (service)
				 (list (format (plist-get service :url) (downcase word))
				       (plist-get service :parser)))
			       dict-services))
	 (org-use-sub-superscripts t)
	 (org-cycle-global-status 'overview)
	 (old-org-emphasis-regexp-components org-emphasis-regexp-components))

    (when (get-buffer org-dict--buffer)
      (kill-buffer org-dict--buffer))

    (with-current-buffer (get-buffer-create org-dict--buffer)
      (org-mode)
      ;; Turn off some time-consuming minor mode
      ;; (Seriously, spell check a dictionary ???)
      (when (fboundp 'flyspell-mode) (flyspell-mode -1))
      ;; Allow markup to span over 10 lines
      (setcar (nthcdr 4 org-emphasis-regexp-components) 9)
      (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
      (mapc (lambda (service) (org-dict--parse word service))
	    dict-services)
      (read-only-mode)
      (goto-char (point-min))
      (org-global-cycle)
      (org-set-emph-re 'org-emphasis-regexp-components old-org-emphasis-regexp-components))
    (pop-to-buffer org-dict--buffer)))

(defun org-dict-at-point (&optional arg)
  "Search a word at point using Org-dict.

With a `\\[universal-argument]' prefix argument, prompt to choose 
a specific dictionary.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, 
prompt to choose a language and display in the buffer `org-dict-buffer' 
all results gathered from dictionaries of that language.
"
  (interactive)
  (let ((word (substring-no-properties
               (thing-at-point 'word))))
    (org-dict word)))

;;; org-dict.el ends here
(provide 'org-dict)
