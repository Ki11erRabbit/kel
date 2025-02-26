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
   ((or (> (length (split-string (string-trim str) " ")) 1) (>= (length (split-string (string-trim-left str) " ")) 2));; There is a command and at least one arg for it
    (let* ((first-split (split-string (string-trim str) " "))
           (second-split nil)
           (split (progn (dolist (item first-split)
                           (message (format "split item: %s" item))
                           (unless (equal item "")
                             (setq second-split (cons item second-split))))
                         (reverse second-split)))
           (command (car split))
           (command-args (kel-get-command-args command)))
      (message "command and at least one arg")
      (message (format "command and arg: %s" split))
      (cond
       ((eq action nil) (progn (message "action nil") (kel-args-match (cdr split) command-args)))
       ((eq action t) (let ((current-arg (kel-get-current-arg (cdr split) command-args))
                            (arg-type (kel-get-current-arg-type (cdr split) command-args )))
                        (message "action t")
                        (message (format "arg-type: %s" arg-type))
                        (pcase arg-type ; TODO: handle optional argument
                          (`(,optional . "file") (kel-get-directory-files current-arg))

                          (`(,optional . "number") '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")) 
                          (`(,optional . "buffer") (mapcar (function buffer-name) (buffer-list))))))
       ((eq action 'lambda) (let ((current-arg (kel-get-current-arg (cdr split) command-args))
                                  (arg-type (kel-get-current-arg-type (cdr split) command-args)))
                              (message "action: lambda")
                              (message (format "\tpred: %s" pred))
                              (message (format "\targ-type: %s" arg-type))
                              (message (format "\targ: %s" (car (cdr split))))
                              (pcase arg-type
                                (`(,optional . "file") (kel-get-file-possible-match current-arg))
                                (`(,optional . "number") nil) 
                                (`(,optional . "buffer") (kel-get-buffer-possible-match (cdr split))))))
       ((consp action) ; TODO: check to make sure it has the right behaviors
        (let ((suffix (cdr action)))
          (message (format "boundaries: %s" suffix))
          (message (format "boundaries: %s" str))
          (if (string-equal suffix "")
              `(boundaries ,(- (length str) (length suffix)). ,(length suffix));; TODO: fix this so that it can not duplicate completions
            `(boundaries ,(length str). 0)))); TODO: figure out what this should be
        ; just a simple (index . index)
       ((eq action 'metadata); TODO: check to make sure it has the right behaviors
        `(metadata
          (affixation-function . kel-affixation-function)
          (group-function . kel-group-function)))
       )))
   ((or (eq (length (split-string (string-trim str) " ")) 1) (eq (length (split-string (string-trim str) " ")) 0))
    (let ((coll kel-prompt-commands))
      (message "no commands yet")
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
  "todo: make this try to finish the completion and not just provide nil"
  (if (eq (length current-arg-list) (length command-spec-list))
      t
    (let ((current-arg (kel-get-current-arg current-arg-list command-spec-list))
          (pair (kel-get-arg-type (nth (kel-get-current-arg-index current-arg-list command-spec-list) command-spec-list))))
      (pcase pair
        (`(,optional . "file") nil); TODO: return longest common substring
        (`(,optional . "number") nil)
        (`(,optional . "buffer") nil)))))

(defun kel-get-current-arg-index (current-arg-list command-spec-list)
  "TODO: check to see if each arg matches the spec. If it does, move on. Otherwise, return that arg"
  (let ((index 0)
        (is-good t))
    (while (and (< index (length current-arg-list)) is-good)
      (let ((pair (kel-get-arg-type (nth index command-spec-list))))
        (pcase pair
          (`(,optional . "file") (if (<= (length (kel-get-file-match (nth index current-arg-list))) 1) t (setq is-good nil)))
          (`(,optional . "number") (if (string-match-p "^-?\\(?:0\\|[1-9][0-9]*\\)$" (nth index current-arg-list)) t (setq is-good nil)))
          (`(,optional . "buffer") (error "todo")))
        (setq index (+ index 1))))
    (- index 1)))


(defun kel-get-current-arg (current-arg-list command-spec-list)
  "TODO: check to see if each arg matches the spec. If it does, move on. Otherwise, return that arg"
  (nth (kel-get-current-arg-index current-arg-list command-spec-list) current-arg-list))

(defun kel-get-current-arg-type (current-arg-list command-spec-list)
  (message (format "length: %s" (length current-arg-list)))
  (message (format "lists args: %s \n commands: %s" current-arg-list command-spec-list))
  (kel-get-arg-type (nth (kel-get-current-arg-index current-arg-list command-spec-list) command-spec-list)))

(defun kel-get-arg-type (command-spec)
  "converts a completion type to a pair of whether or not it is optional and the name of the completion"
  (message (format "command-spec: %s" command-spec))
  (pcase command-spec
    ("file" (cons nil "file"))
    ("file?" (cons t "file"))
    ("number" (cons nil "number"))
    ("number?" (cons t "number"))
    ("buffer" (cons nil "buffer"))
    ("buffer?" (cons t "buffer"))))

(defun kel-get-file-match (current-arg)
  "TODO: add logic to see if there is a file that matches the name, otherwise return nil"
  (let ((possibilities nil))
    (dolist (item (kel-get-directory-files current-arg))
      (message (format "trying '%s' and '%s'" current-arg item))
      (when (or (equal current-arg item) (and (not (null current-arg)) (string-match-p (regexp-quote current-arg) item)))
        (setq possibilities (cons item possibilities))))
    possibilities))

(defun kel-get-file-possible-match (current-arg)
  "TODO: add logic to see if there is a file that matches the name, otherwise return nil"
  (let ((possibilities nil))
    (dolist (item (kel-get-directory-files current-arg))
      (message (format "trying '%s' and '%s'" current-arg item))
      (when (or (equal current-arg item) (and (not (null current-arg)) (string-match-p (regexp-quote current-arg) item)))
        (setq possibilities (cons item possibilities))))
    (message (format "file possible-match: %s" possibilities))
    (when (consp possibilities) t)))


(defun kel-get-buffer-possible-match (current-arg)
  "TODO: add logic to see if there is a buffer that matches the name, otherwise return nil"
  nil)


(defun kel-group-function (completion transform)
  (if (null transform)
      nil
    completion))
  
(defun kel-affixation-function (completions)
  (let ((output nil))
    (dolist (item completions)
      (setq output (cons `(,item "" "") output)))
    (reverse output)))


(defun kel-get-directory-files (&optional dir)
  "get the files in a directory, if dir is nil or empty string then take dir and remove the non-dir part TODO: catch errors here"
  (message (format "default-directory: %s" default-directory))
  (message (format "dir: %s" dir))
  (if (or (null dir) (equal dir ""))
      (directory-files default-directory)
    (let ((split (let ((acc nil)) (dolist (item (split-string dir "/"))
                                    (unless (equal item "")
                                      (setq acc (cons item acc))))
                      (reverse acc)))
          (acc default-directory)
          (skip nil))
      (dolist (item split)
        (unless skip
          (if (file-directory-p (concat acc item))
              (setq acc (concat acc item "/"))
            (message "at end")
            (setq skip t))))
      (message (format "directory files acc: %s" acc))
      (directory-files acc))))

      
      

(defun kel-edit (filename)
  "opens a file for editing, if it is already open, then go to that buffer"
  (find-file filename))




(provide 'kel-ex)
;;; kel-ex.el ends here
