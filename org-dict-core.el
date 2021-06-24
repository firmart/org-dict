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
(defun org-dict-core-dom (url)
  (with-current-buffer (url-retrieve-synchronously url t t)
    (libxml-parse-html-region (point-min) (point-max))))

;;; org-dict-core.el ends here
(provide 'org-dict-core)
