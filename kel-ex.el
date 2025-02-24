;;; kel-ex.el --- Command processing for prompt mode -*- lexical-binding: t; -*-
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

(require 'subr-x)

(require 'kel-vars)


;;; Code:


(defun my-programmable-collection (str pred action)
  "str: the string being entered
pred: a filter function
action: a flag that determines what action does"
  (cond
   ((or (> (length (split-string (string-trim str) " ")) 1) (eq (length (split-string (string-trim-left str) " ")) 2));; There is a command and at least one arg for it
    (let* ((split (split-string (string-trim str) " "))
           (command (car split))
           (command-args (kel-get-command-args command)))
      (cond
       ((eq action nil) (kel-args-match command-args (cdr split)))
       ((eq action t) (let ((current-arg (kel-get-current-arg command-args (cdr split)))
                            (arg-type (kel-get-current-arg-type command-args (cdr split))))
                        (pcase arg-type ; TODO: handle optional argument
                          (`(,optional, "file") (directory-files (default-directory))) 
                          (`(,optional, "number") '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")) 
                          (`(,optional, "buffer") (mapcar (function buffer-name) (buffer-list))))))
       ((eq action 'lambda) (let ((current-arg (kel-get-current-arg command-args (cdr split)))
                            (arg-type (kel-get-current-arg-type command-args (cdr split))))
                        (pcase arg-type
                          (`(,optional, "file") (kel-get-file-possible-match current-arg)) 
                          (`(,optional, "number") nil) 
                          (`(,optional, "buffer") (kel-get-buffer-possible-match current-arg)))))
       ((consp action) ; TODO: check to make sure it has the right behaviors
        (completion-boundaries str coll pred (cdr action)))
       ((eq action 'metadata); TODO: check to make sure it has the right behaviors
        (completion-metadata str coll pred)))))
   ((or (eq (length (split-string (string-trim str) " ")) 1) (eq (length (split-string (string-trim str) " ")) 0))
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
          (completion-metadata str coll pred)))))))
       
                        
(defun kel-args-match (current-arg-list command-spec-list)
  "todo: make this try to finish the completion and not just provide the default values"
  (if (eq (length current-arg-list) (length command-spec-list))
      t
    (let ((pair (kel-get-arg-type (nth (- (length current-arg-list) 1) command-spec-list))))
      (pcase pair
        (`(,optional "file") (directory-files (default-directory)))
        (`(,optional "number") '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
        (`(,optional "buffer") (mapcar (function buffer-name) (buffer-list)))))))

(defun kel-get-current-arg (current-arg-list command-spec-list)
  "TODO: check to see if each arg matches the spec. If it does, move on. Otherwise, return that arg"
  (nth (- (length current-arg-list) 1) command-spec-list))

(defun kel-get-current-arg-type (current-arg-list command-spec-list)
  (kel-get-arg-type (nth (- (length current-arg-list) 1) command-spec-list)))

(defun kel-get-arg-type (command-spec)
  "converts a completion type to a pair of whether or not it is optional and the name of the completion"
  (pcase command-spec
    ("file" (cons nil "file"))
    ("file?" (cons t "file"))
    ("number" (cons nil "number"))
    ("number?" (cons t "number"))
    ("buffer" (cons nil "buffer"))
    ("buffer?" (cons t "buffer"))))


(defun kel-get-file-possible-match (current-arg)
  "TODO: add logic to see if there is a file that matches the name, otherwise return nil"
  nil)


(defun kel-get-buffer-possible-match (current-arg)
  "TODO: add logic to see if there is a buffer that matches the name, otherwise return nil"
  nil)

(defun kel-edit (filename)
  "opens a file for editing, if it is already open, then go to that buffer"
  (find-file filename))




(provide 'kel-ex)
;;; kel-ex.el ends here
