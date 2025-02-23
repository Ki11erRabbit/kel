;;; kel-ex.el --- Command for prompt mode -*- lexical-binding: t; -*-
;; Author Alec Davis <unlikelytitan at gmail.com>
;; Maintainter Alec Davis <unlikelytitan at gmail.com>

;; Version: 0.1.0

;;
;; This file is NOT a part of GNU Emacs

;;; License GPL3

;; This file is a part of Kel.
;;
;; Kel is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Kel is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Kel.  If not, see <http://www.gnu.org/licenses/>.


(require 'kel-vars)

;;; Code:


(defun my-programmable-collection (str pred action)
  "str: the string being entered
pred: a filter function
action: a flag that determines what action does"
  (let ((coll kel-prompt-commands))
    (cond
     ((eq action nil)
      (try-completion str coll pred))
     ((eq action t)
      (all-completions str coll pred))
     ((eq action 'lambda)
      (test-completion str coll pred))
     ((consp action)
      (completion-boundaries str coll pred (cdr action)))
     ((eq action 'metadata)
      (completion-metadata str coll pred)))))


(defun kel-edit (filename)
  "opens a file for editing, if it is already open, then go to that buffer"
  (find-file filename))




(provide 'kel-ex)
;;; kel-ex.el ends here
