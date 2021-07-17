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
(require 'org-element)
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
  `(,org-dict-tlfi-service)
  "List of dictionary's service available in Org-dict.

Each service must contain the following key-values:
- `:dict' : The dictionary symbol.
- `:name' : The dictionary name (used as a Org heading).
- `:url' : The query url where %s denotes the word to search.
- `:parser' : The parser function of the dictionary which takes a DOM returned
by `libxml-parse-html-region' and a URL.

The following key-values are optional:
- `:not-found-p' : A predicate that returns non-nil whenever the given DOM
indicates a failed query.  It will be used to error out early in case the web
page doesn't response with a 404 error when a word entry cannot be found."
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
<dict-symbols> is a list of dictionaries symbols appearing in
`org-dict-dictionaries'.

This alist is used to display results gathered from all dictionaries of a
specific language.
See `\\[universal-argument] \\[universal-argument]' `org-dict-at-point'."
  :type '(alist
          :key-type (symbol :tag "Language")
          :value-type ((repeat symbol) :tag "List of dictionaries"))
  :group 'org-dict
  :package-version '(org-dict . "0.1"))

;;; Internal variables
(defvar org-dict--current-services)
(defvar org-dict--current-word)
;;; Internal functions

(defun org-dict--word-url (word service)
  (format (plist-get service :url) (downcase word)))

(defun org-dict--service-by-dict (name services)
  (cl-loop for service in services
	   when (eq name (plist-get service :dict))
	   return service))

;;; Interactive functions
(defun org-dict-open-url ()
  "Open in a browser one of the dictionaries being used."
  (interactive)
  (if (and (get-buffer org-dict-buffer)
	   org-dict--current-word
	   org-dict--current-services)
      (browse-url
       (if (= 1 (length org-dict--current-services))
	   (org-dict--word-url org-dict--current-word (car org-dict--current-services))
	 (org-dict--word-url org-dict--current-word
			     (org-dict--service-by-dict
			      (completing-read "Select dictionary to open: "
					       (mapcar (lambda (s) (plist-get s :dict))
						       org-dict--current-services))
			      org-dict--current-services))))
    (error "Org-dict is currently not in use")))

(defun org-dict-open-all-url ()
  "Open in a browser all dictionaries being used."
  (interactive)
  (if (and (get-buffer org-dict-buffer)
	   org-dict--current-word
	   org-dict--current-services)
      (mapc (lambda (service)
	      (browse-url
	       (org-dict--word-url org-dict--current-word service)))
	    org-dict--current-services)
    (error "Org-dict is currently not in use")))

;; TODO: integration with pdf-view, guess-language
(defun org-dict (word &optional dict-or-dicts arg)
  "Query the definition of WORD in DICT-OR-DICTS.

DICT-OR-DICTS is either a symbol or a list of symbols.  They must match
the field `:dict' of a service in `org-dict-services'.

With a `\\[universal-argument]' prefix argument ARG, it prompts to choose
a specific dictionary.

With a `\\[universal-argument] \\[universal-argument]' prefix argument ARG,
it prompts to choose a language and display in the buffer `org-dict-buffer'
all results gathered from dictionaries of that language."
  (interactive "MWord: \ni\nP")
  (let* ((dicts (cond (dict-or-dicts
		       (if (symbolp dict-or-dicts)
			   (list dict-or-dicts)
			 dict-or-dicts))
		      ;; Prompt a dictionary
		      ((equal arg '(4))
		       (list (completing-read "Dictionary: "
					      (mapcar (lambda (s) (plist-get s :dict))
						      org-dict-services))))
		      ;; Prompt a language and return dictionaries of that language
		      ((equal arg '(16))
		       (alist-get (intern (completing-read "Language: " org-dict-by-langs))
				  org-dict-by-langs))
		      ;; Default dictionary or dictionaries of default language
		      (org-dict-default-dictionary
		       (list org-dict-default-dictionary))
		      ((and org-dict-default-lang org-dict-by-langs)
		       (car (alist-get org-dict-default-lang org-dict-by-langs)))
		      (t (error "There is not available dictionary"))))
	 (dict-services (mapcar
			 (lambda (dict)
			   (org-dict--service-by-dict dict org-dict-services))
			 dicts))
	 ;; Put these under a local variables heading
	 (org-use-sub-superscripts t)
	 (org-cycle-global-status 'overview)
	 (old-org-emphasis-regexp-components org-emphasis-regexp-components)
	 (org-list-allow-alphabetical t))

    ;; Update Org element regex to honour single alphabetical bullet
    (org-element-update-syntax)

    (when (get-buffer org-dict-buffer)
      (kill-buffer org-dict-buffer))

    (with-current-buffer (get-buffer-create org-dict-buffer)
      (org-mode)
      ;; Turn off some time-consuming minor mode
      ;; (Seriously, spell check a dictionary ???)
      (when (fboundp 'flyspell-mode) (flyspell-mode -1))

      ;; Allow markup to span over 10 lines
      (setcar (nthcdr 4 org-emphasis-regexp-components) 9)
      (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
      ;;(condition-case err
	  (progn
	    (mapc (lambda (service) (org-dict--parse word service))
		  dict-services)
	    (setq org-dict--current-word word)
	    (setq org-dict--current-services dict-services)
	    (read-only-mode)
	    (org-global-cycle)
	    (org-element-map (org-element-parse-buffer 'element) 'quote-block
	      (lambda (node) (goto-char (org-element-property :begin node)) (org-cycle)))
	    (org-set-emph-re 'org-emphasis-regexp-components old-org-emphasis-regexp-components)
	    (pop-to-buffer org-dict-buffer)
	    (goto-char (point-min))
	    (org-cycle))
	;;(error (kill-buffer org-dict-buffer)
        ;;       (error "%s" (error-message-string err))))
      )))

(defun org-dict-at-point (&optional arg)
  "Search a word at point using Org-dict.

With a `\\[universal-argument]' prefix argument ARG, prompt to choose
a specific dictionary.

With a `\\[universal-argument] \\[universal-argument]' prefix argument ARG,
prompt to choose a language and display in the buffer `org-dict-buffer'
all results gathered from dictionaries of that language."
  (interactive)
  (when-let* ((word-at-point (or (when (use-region-p)
				   (buffer-substring
				    (region-beginning)
				    (region-end)))
				 (thing-at-point 'word)))
	      (word (substring-no-properties word-at-point)))
    (org-dict word)))

;;; org-dict.el ends here
(provide 'org-dict)
