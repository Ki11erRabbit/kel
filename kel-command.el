;;; kel-command.el --- Commands -*- lexical-binding: t; -*-
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
(require 'simple)
(require 'electric)
(require 'multiple-cursors)

(require 'kel-vars)
(require 'kel-util)
(require 'kel-hooks)
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

(defun kel-to-char (arg char &optional count)
  "select to the next occurrence of given character"
  (interactive "p\ncSelect to char: ")
  (kel-set-mark-here)
  (kel-select-to-char-util (if count count (if (equal current-prefix-arg nil) 1 current-prefix-arg)) char)
  (kel-set-normal-last-selection-command (lambda () (kel-to-char arg char (if (equal current-prefix-arg nil) 1 current-prefix-arg)))))

(defun kel-select-to-char (arg char &optional count)
  "select to the next occurrence of given character"
  (interactive "p\ncSelect to char: ")
  (kel-set-mark-if-inactive)
  (kel-select-to-char-util (if count count (if (equal current-prefix-arg nil) 1 current-prefix-arg)) char)
  (kel-set-normal-last-selection-command (lambda () (kel-select-to-char arg char (if (equal current-prefix-arg nil) 1 current-prefix-arg)))))

(defun kel-up-to-char (arg char &optional count)
  "select until the next occurrence of given character"
  (interactive "p\ncSelect up to char: ")
  (kel-set-mark-here)
  (kel-select-up-to-char-util (if count count (if (equal current-prefix-arg nil) 1 current-prefix-arg)) char)
  (kel-set-normal-last-selection-command (lambda () (kel-up-to-char arg char (if (equal current-prefix-arg nil) 1 current-prefix-arg)))))

(defun kel-select-up-to-char (arg char &optional count)
  "select until the next occurrence of given character"
  (interactive "p\ncSelect up to char: ")
  (kel-set-mark-if-inactive)
  (kel-select-up-to-char-util (if count count (if (equal current-prefix-arg nil) 1 current-prefix-arg)) char)
  (kel-set-normal-last-selection-command (lambda () (kel-select-up-to-char arg char (if (equal current-prefix-arg nil) 1 current-prefix-arg)))))

(defun kel-to-char-reverse (arg char &optional count)
  "select to the previous occurrence of given character"
  (interactive "p\ncSelect to char: ")
  (kel-set-mark-here)
  (kel-select-to-char-reverse-util (if count count (if (equal current-prefix-arg nil) 1 current-prefix-arg)) char)
  (kel-set-normal-last-selection-command (lambda () (kel-to-char-reverse arg char (if (equal current-prefix-arg nil) 1 current-prefix-arg)))))

(defun kel-select-to-char-reverse (arg char &optional count)
  "select to the previous occurrence of given character"
  (interactive "p\ncSelect to char: ")
  (kel-set-mark-if-inactive)
  (kel-select-to-char-reverse-util (if count count (if (equal current-prefix-arg nil) 1 current-prefix-arg)) char)
  (kel-set-normal-last-selection-command (lambda () (kel-select-to-char-reverse arg char (if (equal current-prefix-arg nil) 1 current-prefix-arg)))))

(defun kel-up-to-char-reverse (arg char &optional count)
  "select until the previous occurrence of given character"
  (interactive "p\ncSelect up to char: ")
  (kel-set-mark-here)
  (kel-select-up-to-char-reverse-util (if count count (if (equal current-prefix-arg nil) 1 current-prefix-arg)) char)
  (kel-set-normal-last-selection-command (lambda () (kel-up-to-char-reverse arg char (if (equal current-prefix-arg nil) 1 current-prefix-arg)))))

(defun kel-select-up-to-char-reverse (arg char &optional count)
  "select until the previous occurrence of given character"
  (interactive "p\ncSelect up to char: ")
  (kel-set-mark-if-inactive)
  (kel-select-up-to-char-reverse-util (if count count (if (equal current-prefix-arg nil) 1 current-prefix-arg)) char)
  (kel-set-normal-last-selection-command (lambda () (kel-select-up-to-char-reverse arg char (if (equal current-prefix-arg nil) 1 current-prefix-arg)))))

(defun kel-last-object-or-char-selection ()
  (interactive)
  (funcall (kel-get-normal-last-selection-command)))

(defun kel-select-next-matching-pair ()
  "select to the next sequence enclosed by matching characters"
  (interactive)
  (kel-select-closing-pair (char-after (point))))

(defun kel-extend-next-matching-pair ()
  "select to the next sequence enclosed by matching characters"
  (interactive)
  (kel-match-closing-pair (char-after (point))))

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

(defun kel-yank-and-delete ()
  "yank and delete selections"
  (interactive)
  (kel-yank-and-delete-util))

(defun kel-last-insert-mode-change ()
  "repeat last insert mode change (i, a, or c, including the inserted text)"
  (interactive)
  (kel-last-insert-mode-change-util))


(defun kel-delete ()
  "delete selections (not yanking)"
  (interactive)
  (kel-delete-util))

;; TODO: add adding lines above and below cursor

(defun kel-yank ()
  "yank selections"
  (interactive)
  (kill-ring-save 0 0 (region-bounds)))

(defun kel-paste-after ()
  "paste after the end of each selection"
  (interactive)
  (when (> (region-end) (point))
    (exchange-point-and-mark))
  (yank))

(defun kel-paste-before ()
  "paste before the beginning of each selection"
  (interactive)
  (when (<= (region-end) (point))
    (exchange-point-and-mark))
  (yank))

;; TODO: use multiple cursors to implement paste all and end and start

(defun kel-replace-selection ()
  "replace selections with yanked text"
  (interactive)
  (kel-delete-util)
  (yank))

;; TODO: implement replace selections with every yanked text

(defun kel-replace-character (arg char)
  "replace each character with the next entered one"
  (interactive "p\ncenter char to replace with")
  (delete-char 1)
  (insert char)
  (backward-char))

(defun kel-join-lines ()
  "join selected lines"
  (interactive)
  (join-line 1))

;; TODO: figure out how to use this one
(defun kel-join-lines-with-spaces ()
  "join selected lines and select spaces inserted in place of line breaks"
  (interactive)
  (replace-regexp "\n" " "))

;; TODO figure out what merge contiguous selections together means

;; TODO add duplicate each selection, merge overlapping selections

(defun kel-indent-selection ()
  "indent selected lines"
  (interactive)
  (kel-indent (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

;; TODO: indent selected lines, including empty lines

(defun kel-unindent-selection ()
  "unindent selected lines"
  (interactive)
  (kel-unindent (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

;; TODO: unindent selected lines, don't remove incomplete indents

;; TODO: implement undo, redo, moving in history with undo-fu

;; TODO: figure out how to save selection changes with multiple cursors

;; TODO: figure out how to align selections with multiple cursors.

(defun kel-to-lower-case ()
  "to lower case"
  (interactive)
  (kel-lowercase))

(defun kel-to-upper-case ()
  "to upper case"
  (interactive)
  (kel-uppercase))

;; TODO: figure out how to swap casing

(defun kel-tabs->spaces ()
  "convert tabs to spaces in each selection, uses the buffer tabstop option or the count parameter for tabstop"
  (interactive)
  (message "tabs->spaces")
  (kel-convert-tabs-to-spaces (if (equal current-prefix-arg nil) 0 current-prefix-arg)))

(defun kel-spaces->tabs ()
  "convert spaces to tabs in each selection, uses the buffer tabstop option or the count parameter for tabstop"
  (interactive)
  (kel-convert-spaces-to-tabs (if (equal current-prefix-arg nil) 0 current-prefix-arg)))
;; TODO: unselect whitespace from multiple cursors

;; TODO: rotate selections content, which includes count


;; Multiple Selections

(defun kel-match-selection (arg regex)
  "create a selection for each match of the given regex (selects the count capture if it is given)"
  (interactive "p\nsselect: ")
  (kel-match-selection-regex regex (if (equal current-prefix-arg nil) nil current-prefix-arg)))

(defun kel-split-selection (arg regex)
  (interactive "p\nssplit: ")
  (kel-split-selection-regex regex (if (equal current-prefix-arg nil) nil current-prefix-arg)))

(defun kel-split-selections-line-boundry ()
  "select first and last characters of each selection. Currently does not work"
  (interactive)
  (kel-mc-split-region (region-beginning) (region-end) "\n"))

(defun kel-select-first-last ()
  "select first and last characters of each selection"
  (interactive)
  (kel-set-selections-to-start-end))

(defun kel-duplicate-selections-following-lines ()
  "duplicate selections on the lines that follow them"
  (interactive)
  (mc/mark-next-lines (if (equal current-prefix-arg nil) 0 current-prefix-arg)))

(defun kel-duplicate-selections-preceding-lines ()
  "duplicate selections on the lines that precede them"
  (interactive)
  (mc/mark-previous-lines (if (equal current-prefix-arg nil) 0 current-prefix-arg)))

(defun kel-clear-selections ()
  "clear selections to only keep the main one"
  (interactive)
  (mc/maybe-multiple-cursors-mode))

(defun kel-pipe-replace-only-success ()
  "pipe each selection to the given shell command and keep the ones for which the shell returned 0."
  (interactive)
  (let ((command (read-string "keep pipe:")))
    (kel-pipe-only-success command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STATE TOGGLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Insert Mode

(defun kel-attach-post-command-hook ()
  (add-hook 'post-command-hook #'kel-insert-post-command-hook))

(defun kel-detach-post-command-hook ()
  (remove-hook 'post-command-hook #'kel-insert-post-command-hook))

(defun kel-insert-exit ()
  "Switch to NORMAL state."
  (interactive)
  (kel-detach-post-command-hook)
  (cond
   ((kel-insert-mode-p)
    (kel--switch-state 'normal))))


(defun kel-insert-before ()
  "enter insert mode before selections"
  (interactive)
  (kel-reset-last-insert-commands)
  (kel--switch-state 'insert)
  (kel-attach-post-command-hook)
  (when (and (use-region-p) (<= (region-end) (point)))
      (exchange-point-and-mark)))
  
(defun kel-insert-after ()
  "enter insert mode after selections"
  (interactive)
  (kel-reset-last-insert-commands)
  (kel-attach-post-command-hook)
  (kel--switch-state 'insert)
  (if (and (use-region-p) (> (region-end) (point)))
      (exchange-point-and-mark)
    (kel-set-mark-if-inactive)
    (forward-char)))

(defun kel-insert-yank-and-delete ()
  "yank and delete selections and enter insert mode"
  (interactive)
  (kel-reset-last-insert-commands)
  (kel-add-insert-command 'delete-selection)
  (kel-yank-and-delete-util)
  (kel--switch-state 'insert)
  (kel-attach-post-command-hook))
  
(defun kel-insert-delete ()
  "yank and delete selections and enter insert mode"
  (interactive)
  (kel-reset-last-insert-commands)
  (kel-add-insert-command 'delete-selection)
  (kel-delete-util)
  (kel--switch-state 'insert)
  (kel-attach-post-command-hook))

(defun kel-insert-line-start ()
  "enter insert mode at the beginning of the lines containing the start of each selection"
  (interactive)
  (kel-reset-last-insert-commands)
  (when (<= (region-end) (point)) (exchange-point-and-mark))
  (back-to-indentation)
  (kel--switch-state 'insert)
  (kel-attach-post-command-hook))

(defun kel-insert-line-end ()
  "enter insert mode at the end of the lines containing the end of each selection"
  (interactive)
  (kel-reset-last-insert-commands)
  (when (>= (region-end) (point)) (exchange-point-and-mark))
  (move-end-of-line)
  (kel--switch-state 'insert)
  (kel-attach-post-command-hook))

(defun kel-insert-new-line-below ()
  "enter insert mode in a new line (or in a given count of new lines) below the end of each selection"
  (interactive)
  (kel-reset-last-insert-commands)
  (end-of-line)
  (dotimes (_ (if (equal current-prefix-arg nil) 1 current-prefix-arg))
    (electric-newline-and-maybe-indent))
  (kel--switch-state 'insert)
  (kel-attach-post-command-hook))
  
(defun kel-insert-new-line-above ()
  "enter insert mode in a new line (or in a given count of new lines) above the beginning of each selection"
  (interactive)
  (kel-reset-last-insert-commands)
  (beginning-of-line)
  (dotimes (_ (if (equal current-prefix-arg nil) 1 current-prefix-arg))
    (newline)
    (forward-line -1))
  (kel--switch-state 'insert)
  (kel-attach-post-command-hook))

(provide 'kel-command)
;;; kel-command.el ends here
