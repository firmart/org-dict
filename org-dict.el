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

;; built-in Emacs lib
(require 'org)
(require 'ox-html)
(require 'cl-lib)     ;; Common-lisp emulation library

;;; Custom group
;;;; General settings

(defgroup org-dict nil
  "Org-dict Settings"
  :group 'org
  :package-version '(org-dict . "0.1"))

(defcustom org-dict-dictionaries nil
  ""
  :type  'boolean
  :group 'org-glaux
  :package-version '(org-glaux . "0.1"))

;;; Interactive functions
;;; Internal functions
;;; org-dict.el ends here
(provide 'org-dict)
