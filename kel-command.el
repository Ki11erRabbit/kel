;;; kel-command.el --- Commands -*- lexical-binding t -*-
;; Author Alec Davis <unlikelytitan at gmail.com>
;; Maintainter Alec Davis <unlikelytitan at gmail.com>

;; Version: 0.1.0

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



;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(require 'kel-vars)
(require 'kel-util)
;(require 'kel-visual)
(require 'array)


;; Movement

(defun kel-backward-char ()
  "Move to the left."
  (interactive)
  (kel-deactivate-mark)
  (backward-char (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-select-backward-char ()
  "Move to the left and select."
  (interactive)
  (kel-set-mark-if-inactive)
  (backward-char (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-forward-char ()
  "Move to the right."
  (interactive)
  (kel-deactivate-mark)
  (forward-char (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-select-forward-char ()
  "Move to the right and select."
  (interactive)
  (kel-set-mark-if-inactive)
  (forward-char (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-previous-line ()
  "Move up."
  (interactive)
  (kel-deactivate-mark)
  (previous-line (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-select-previous-line ()
  "Move up and select."
  (interactive)
  (kel-set-mark-if-inactive)
  (previous-line (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-next-line ()
  "Move down."
  (interactive)
  (kel-deactivate-mark)
  (next-line (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-select-next-line ()
  "Move down and select."
  (interactive)
  (kel-set-mark-if-inactive)
  (next-line (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-forward-word ()
  "Select the word and following whitespace on the right of the end of each selection"
  (interactive)
  (kel-deactivate-mark)
  (kel-forward-word-util (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-select-forward-word ()
  "Select the word and following whitespace on the right of the end of each selection"
  (interactive)
  (kel-forward-word-util (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-backward-word ()
  "select preceding whitespaces and the word on the left of the end of each selection"
  (interactive)
  (kel-deactivate-mark)
  (kel-backward-word-util (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-select-backward-word ()
  "select preceding whitespaces and the word on the left of the end of each selection"
  (interactive)
  (kel-backward-word-util (if (equal current-prefix-arg nil) 1 current-prefix-arg)))


(defun kel-forward-symbol ()
  "select preceding whitespaces and the word on the right of the end of each selection"
  (interactive)
  (kel-set-mark-here)
  (forward-symbol (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-select-forward-symbol ()
  "select preceding whitespaces and the word on the right of the end of each selection"
  (interactive)
  (kel-set-mark-if-inactive)
  (forward-symbol (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-to-char (arg char)
  "select to the next occurrence of given character"
  (interactive "p\ncSelect to char: ")
  (kel-deactivate-mark)
  (kel-select-to-char-util arg char))

(defun kel-select-to-char (arg char)
  "select to the next occurrence of given character"
  (interactive "p\ncSelect to char: ")
  (kel-set-mark-if-inactive)
  (kel-select-to-char-util))

(defun kel-up-to-char (arg char)
  "select until the next occurrence of given character"
  (interactive "p\ncSelect up to char: ")
  (kel-deactivate-mark)
  (kel-select-up-to-char-util (if (equal current-prefix-arg nil) 1 current-prefix-arg) char))

(defun kel-select-up-to-char (arg char)
  "select until the next occurrence of given character"
  (interactive "p\ncSelect up to char: ")
  (kel-set-mark-if-inactive)
  (kel-select-up-to-char-util (if (equal current-prefix-arg nil) 1 current-prefix-arg) char))

(defun kel-line ()
  (interactive)
  (kel-line-util))

(defun kel-select-line ()
  (interactive)
  (kel-line-util))

(defun kel-select-buffer ()
  (interactive)
  (mark-whole-buffer))

(defun kel-select-to-line-begin ()
  (interactive)
  (kel-set-mark-here)
  (beginning-of-line))

(defun kel-select-to-line-end ()
  (interactive)
  (kel-set-mark-here)
  (end-of-line))

(defun kel-page-up ()
  (interactive)
  (kel-deactivate-mark)
  (scroll-up-command (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-page-down ()
  (interactive)
  (kel-deactivate-mark)
  (scroll-down-command (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-half-page-up ()
  (interactive)
  (kel-deactivate-mark)
  (kel-scroll-up-half-page))

(defun kel-half-page-down ()
  (interactive)
  (kel-deactivate-mark)
  (kel-scroll-down-half-page))

(defun kel-reduce-selection ()
  "reduce selections to their cursor"
  (interactive)
  (kel-deactivate-mark))

(defun kel-flip-selection ()
  "flip the direction of each selection"
  (interactive)
  (kel-flip-selection-util))

(defun kel-make-selection-forward ()
  "ensure selections are in forward direction (cursor after anchor)"
  (interactive)
  (kel-make-selection-forward-util))
                                   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STATE TOGGLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kel-insert-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((kel-insert-mode-p)
    (kel--switch-state 'normal))))


(defun kel-insert ()
  "Move to the start of selection, switch to INSERT state."
  (interactive)
    ;(meow--direction-backward)
    ;(meow--cancel-selection)
  (kel--switch-state 'insert)
  (when kel-select-on-insert
    (setq-local kel--insert-pos (point))))

(provide 'kel-command)
;;; kel-command.el ends here
