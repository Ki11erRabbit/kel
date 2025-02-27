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
(require 'seq)

(require 'kel-vars)
(require 'kel-util)


;;; Code:


(defun my-programmable-collection (str pred action)
  "str: the string being entered
pred: a filter function
action: a flag that determines what action does"
  (cond
   ((or (> (length (kel-split-string (string-trim str))) 1) (>= (length (kel-split-string (string-trim-left str))) 2));; There is a command and at least one arg for it TODO: fix split to handle escaped spaces
    (let* ((first-split (kel-split-string (string-trim str)))
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
       ((eq action t) (progn (message "action t") (let ((current-arg (kel-get-current-arg (cdr split) command-args))
                            (arg-type (kel-get-current-arg-type (cdr split) command-args )))
                        
                        (message (format "arg-type: %s" arg-type))
                        (pcase arg-type ; TODO: handle optional argument
                          (`(,optional . "file") (let ((triple (kel-get-file-match current-arg)))
                                                   (car (cdr (cdr triple)))))
                          (`(,optional . "number") '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")) 
                          (`(,optional . "buffer") (mapcar (function buffer-name) (buffer-list)))))))
       ((eq action 'lambda) (progn (message "action: lambda") (let ((current-arg (kel-get-current-arg (cdr split) command-args))
                                  (arg-type (kel-get-current-arg-type (cdr split) command-args)))
                              
                              (message (format "\tpred: %s" pred))
                              (message (format "\targ-type: %s" arg-type))
                              (message (format "\targ: %s" (car (cdr split))))
                              (pcase arg-type
                                (`(,optional . "file") (kel-get-file-possible-match current-arg))
                                (`(,optional . "number") nil) 
                                (`(,optional . "buffer") (kel-get-buffer-possible-match (cdr split)))))))
       ((consp action) ; TODO: check to make sure it has the right behaviors
        (let ((suffix (cdr action))
              (current-arg (kel-get-current-arg (cdr split) command-args))
              (arg-type (kel-get-current-arg-type (cdr split) command-args )))
          (message (format "boundaries: %s" suffix))
          (message (format "boundaries: %s" str))
          (pcase arg-type
            (`(,optional . "file") `(boundaries ,(kel-calculate-file-start-boundry str current-arg) . ,(length suffix)))
            (_ (if (string-equal suffix "")
              `(boundaries ,(- (length str) (length current-arg)) . ,(length suffix));; TODO: fix this so that it can handle a successful completion
            `(boundaries ,(length str). 0))))
          )); TODO: figure out what this should be
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
        (`(,optional . "number") (if (string-match-p "^-?\\(?:0\\|[1-9][0-9]*\\)$" (nth index current-arg-list)) t nil))
        (`(,optional . "buffer") nil)))))

(defun kel-get-current-arg-index (current-arg-list command-spec-list)
  "TODO: check to see if each arg matches the spec. If it does, move on. Otherwise, return that arg"
  (let ((index 0)
        (is-good t))
    (while (and (< index (length current-arg-list)) is-good)
      (let ((pair (kel-get-arg-type (nth index command-spec-list))))
        (pcase pair
          (`(,optional . "file") (let* ((file-triple (kel-get-file-match (nth index current-arg-list)))
                                        (prefix (car file-triple))
                                        (rest (car (cdr file-triple)))
                                        (files (car (cdr (cdr file-triple))))
                                        (reduce (seq-reduce (lambda (acc x) (or acc x)) (mapcar (lambda (file) (and (file-exists-p (concat prefix file)) (equal rest file))) files) nil)))
                                   (unless reduce
                                     (setq is-good nil)
                                     (setq index (- index 1)))))
          (`(,optional . "number") (if (string-match-p "^-?\\(?:0\\|[1-9][0-9]*\\)$" (nth index current-arg-list)) t (setq is-good nil)(setq index (- index 1))))
          (`(,optional . "buffer") (error "todo")))
        (setq index (+ index 1))))
    (message (format "arg-index: %s" index))
    index))


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
    ("?file" (cons t "file"))
    ("number" (cons nil "number"))
    ("?number" (cons t "number"))
    ("buffer" (cons nil "buffer"))
    ("?buffer" (cons t "buffer"))))

(defun kel-get-file-match (current-arg)
  "Get files from the current arg that match the first non-file suffix.
Returns a list of the directory prefix, the rest of the current-arg, and the files in that directory."
  (let* ((possibilities nil)
         (triple (kel-get-directory-files current-arg))
         (rest (car triple))
         (prefix (car (cdr triple)))
         (files (car (cdr (cdr triple)))))
    (dolist (item files)
      (message (format "trying '%s' and '%s'" rest item))
      (if (null rest)
          (progn (message "rest is nil") (setq possibilities (cons item possibilities)))
        (when (or (equal rest item) (and (not (null rest)) (string-match-p (regexp-quote rest) item)))
          (setq possibilities (cons item possibilities)))))
    (progn (message (format "possiblities: %s" possibilities)) `(,prefix ,rest ,possibilities))))

(defun kel-get-file-possible-match (current-arg)
  "If the result of kel-get-file-match has any files that match the rest of the current arg, return t, otherwise return nil"
  (let* ((triple (kel-get-file-match current-arg))
         (rest (car (cdr triple)))
         (files (car (cdr (cdr triple))))
         (reduce (seq-reduce (lambda (acc file) (or acc (string-match-p (regexp-quote rest) file))) files nil)))
    (when reduce t)))


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
  "get a pair that is the remaining portion of the dir string and the files in that directory. The remaining portion is the first non-successful directory match."
  (message (format "default-directory: %s" default-directory))
  (message (format "dir: %s" dir))
  (cond
   ((or (null dir) (equal dir "")) (cons nil (directory-files default-directory)))
   ((string-match-p (regexp-quote "..") dir) (let ((split (let ((acc nil)) (dolist (item (split-string dir "/"))
                                    (unless (equal item "")
                                      (setq acc (cons item acc))))
                        (reverse acc)))
            (acc default-directory))
        (while (and (file-directory-p (concat acc (car split) "/")) (consp split))
          (setq acc (concat acc (car split) "/"))
          (setq split (cdr split)))
        `( ,(car split) ,acc ,(directory-files acc))))
   ((kel-is-at-root dir) (let ((split (let ((acc nil)) (dolist (item (split-string dir "/"))
                                    (unless (equal item "")
                                      (setq acc (cons item acc))))
                        (reverse acc)))
            (acc "/"))
        (while (and (file-directory-p (concat acc (car split) "/")) (consp split))
          (setq acc (concat acc (car split) "/"))
          (setq split (cdr split)))
        `(,(car split) ,acc ,(directory-files acc))))
   (t (let ((split (let ((acc nil)) (dolist (item (split-string dir "/"))
                                    (unless (equal item "")
                                      (setq acc (cons item acc))))
                        (reverse acc)))
            (acc default-directory))
        (while (and (file-directory-p (concat acc (car split) "/")) (consp split))
          (setq acc (concat acc (car split) "/"))
          (setq split (cdr split)))
        `(,(car split) ,acc ,(directory-files acc))))))

(defun kel-is-at-root (dir)
  (cond
   ((eq system-type 'windows-nt) (string-match-p "^\\`[a-zA-Z]:[/\\]\\'.*" dir))
   (t (string-match-p (regexp-quote "/") dir))))
      
(defun kel-calculate-file-start-boundry (str dir)
  "Gets the start boundary for various types of directories"
  (cond
   ((or (null dir) (equal dir "")) (length str))
   ((string-match-p (regexp-quote "../") dir) (- (length str) (- (length dir) 3)))
   ((kel-is-at-root dir) (- (length str) (- (length dir) 1))); TODO: make this work with windows
   (t (- (length str) (length dir)))))
      

(defun kel-edit (filename)
  "opens a file for editing, if it is already open, then go to that buffer"
  (find-file filename))




(provide 'kel-ex)
;;; kel-ex.el ends here
