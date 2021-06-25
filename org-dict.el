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

;; Org-dict is a dictionary utility which uses the power of Org-mode to increase
;; readability and to provide reusability.

;;; Code:

;; Built-in Emacs lib
(require 'org)
(require 'ox-html)
(require 'cl-lib) 
;; Dictionary parse engines
(require 'org-dict-core)
(require 'org-dict-cntrl)

;;; Custom group
;;;; General settings

(defgroup org-dict nil
  "Org-dict Settings"
  :group 'org
  :package-version '(org-dict . "0.1"))

(defcustom org-dict-dictionaries nil
  "List of dictionaries used in output by default."
  :type '(repeat string)
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

;;; Internal variables
(defvar org-dict--buffer "*org-dict*")
;;; Internal functions
(defun org-dict--parse (url parser)
  "Parse a dictionary and return the result"
  (let ((dom (org-dict--url-to-dom url)))
    (when (get-buffer org-dict--buffer)
      (kill-buffer org-dict--buffer))
    (with-current-buffer (get-buffer-create org-dict--buffer)
      (org-mode)
      (mapc #'insert
	    (funcall parser dom url))
      (read-only-mode)
      (switch-to-buffer org-dict--buffer))))

;;; Interactive functions
(defun org-dict ()
  "Org-dict entry-point"
  ())
;;; org-dict.el ends here
(provide 'org-dict)
