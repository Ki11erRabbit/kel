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

;; Changes through external programs

(defun kel-pipe-replace ()
  "pipe each selection through the given external filter program and replace the selection with its output."
  (interactive)
  (let ((command (read-string "pipe:")))
    (kel-shell-pipe command)))

(defun kel-pipe-ignore ()
  "pipe each selection through the given external filter program and ignore its output."
  (interactive)
  (let ((command (read-string "pipe-to:")))
    (kel-shell-pipe-ignore command)))

(defun kel-pipe-before ()
  "insert and select command output before each selection."
  (interactive)
  (let ((command (read-string "insert-output:")))
    (kel-shell-pipe-before command)))   

(defun kel-pipe-after ()
  "append and select command output after each selection."
  (interactive)
  (let ((command (read-string "append-output:")))
    (kel-shell-pipe-before command)))

;;Goto Commands

(defun kel-goto-line-begin ()
  "go to line begin"
  (interactive)
  (when (kel-is-goto-selection-set)
    (kel-set-mark-if-inactive))
  (move-beginning-of-line nil)
  (kel-goto-exit))

(defun kel-goto-line-end ()
  "go to line end"
  (interactive)
  (when (kel-is-goto-selection-set)
    (kel-set-mark-if-inactive))
  (move-end-of-line nil)
  (kel-goto-exit))     

(defun kel-goto-line-indent ()
  "go to non blank line start"
  (interactive)
  (when (kel-is-goto-selection-set)
    (kel-set-mark-if-inactive))
  (move-to-tab-stop)
  (kel-goto-exit))

(defun kel-goto-buffer-start ()
  "goto the first line"
  (interactive)
  (when (kel-is-goto-selection-set)
    (kel-set-mark-if-inactive))
  (goto-line 0)
  (move-beginning-of-line)
  (kel-goto-exit))

(defun kel-goto-buffer-end ()
  "goto the last line"
  (interactive)
  (when (kel-is-goto-selection-set)
    (kel-set-mark-if-inactive))
  (end-of-buffer)
  (move-beginning-of-line)
  (kel-goto-exit))

(defun kel-goto-buffer-end-char ()
  "go to last char of last line"
  (interactive)
  (when (kel-is-goto-selection-set)
    (kel-set-mark-if-inactive))
  (end-of-buffer)
  (kel-goto-exit))

(defun kel-goto-window-top ()
  "go to the first displayed line"
  (interactive)
  (when (kel-is-goto-selection-set)
    (kel-set-mark-if-inactive))
  (move-to-window-line 0)
  (move-beginning-of-line)
  (kel-goto-exit))

(defun kel-goto-window-middle ()
  "go to the middle displayed line"
  (interactive)
  (when (kel-is-goto-selection-set)
    (kel-set-mark-if-inactive))
  (move-to-window-line (/ (window-total-height) 2))
  (move-beginning-of-line)
  (kel-goto-exit))

(defun kel-goto-window-bottom ()
  "go to the last displayed line"
  (interactive)
  (when (kel-is-goto-selection-set)
    (kel-set-mark-if-inactive))
  (move-to-window-line (window-total-height))
  (move-beginning-of-line)
  (kel-goto-exit))

(defun kel-goto-last-buffer ()
  "go to the previous (alternate) buffer"
  (interactive)
  (previous-buffer)
  (kel-goto-exit))

(defun kel-goto-file-name ()
  "open the file whose name is selected"
  (interactive)
  (if (use-region-p)
      (let ((file-name (buffer-substring (region-beginning) (region-end))))
        (find-file file-name))
    (find-file (make-string (char-after) 1))))

;; TODO: goto last buffer modification position

;; View commands

(defun kel-center-vertically ()
  "center the main selection in the window (vertically)"
  (interactive)
  (recenter)
  (kel-view-exit))

(defun kel-center-horizontally ()
  "center the main selection in the window (horizontally)"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid))))
  (kel-view-exit))

(defun kel-scroll-selection-top ()
  "scroll to put the main selection on the top line of the window"
  (interactive)
  (let ((line (line-number-at-pos)))
    (scroll-up-line (- line 1))
  (kel-view-exit)))

(defun kel-scroll-selection-bottom ()
  "scroll to put the main selection on the bottom line of the window"
  (interactive)
  (let ((line (line-number-at-pos)))
    (scroll-down-line (- line 1)))
  (kel-view-exit))

(defun kel-scroll-left ()
  "scroll the window count columns left"
  (interactive)
  (scroll-right (kel-get-view-count))
  (kel-view-exit))

(defun kel-scroll-down ()
  "scroll the window count line downward"
  (interactive)
  (scroll-up (kel-get-view-count))
  (kel-view-exit))

(defun kel-scroll-up ()
  "scroll the window count line upward"
  (interactive)
  (scroll-down (kel-get-view-count))
  (kel-view-exit))

(defun kel-scroll-right ()
  "scroll the window count columns right"
  (interactive)
  (scroll-left (kel-get-view-count))
  (kel-view-exit))

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
  (mc/mark-next-lines (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-duplicate-selections-preceding-lines ()
  "duplicate selections on the lines that precede them"
  (interactive)
  (mc/mark-previous-lines (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-clear-selections ()
  "clear selections to only keep the main one"
  (interactive)
  (mc/maybe-multiple-cursors-mode))

(defun kel-keep-match-regex ()
  "keep selections that match the given regex"
  (interactive)
  (let ((regex (read-string "keep matching:")))
    (kel-keep-match regex)))

(defun kel-remove-match-regex ()
  "clear selections that match the given regex"
  (interactive)
  (let ((regex (read-string "keep not matching:")))
    (kel-remove-match regex)))

(defun kel-pipe-replace-only-success ()
  "pipe each selection to the given shell command and keep the ones for which the shell returned 0."
  (interactive)
  (let ((command (read-string "keep pipe:")))
    (kel-pipe-only-success command)))

(defun kel-cycle-forwards ()
  "rotate main selection (the main selection becomes the next one)"
  (interactive)
  (kel-move-selection-forwards (if (equal current-prefix-arg nil) 1 current-prefix-arg)))

(defun kel-cycle-backwards ()
  "rotate main selection backward (the main selection becomes the previous one)"
  (interactive)
  (kel-move-selection-backwards (if (equal current-prefix-arg nil) 1 current-prefix-arg)))


;; Prompt commands

(defun kel-process-command ()
  "reads in a command and executes it
TODO: make this have tab completion"
  (interactive)
  (let ((command (read-from-minibuffer ":")))
    (kel-parse-execute-command command))
  (kel-prompt-exit))

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


(defun kel-goto-exit ()
  "Switch to NORMAL state."
  (interactive)
  (kel-goto-selection-disable)
  (cond
   ((kel-goto-mode-p)
    (kel--switch-state 'normal))))

(defun kel-goto-mode-start ()
  "When a count is specified, send the anchor to the given line, otherwise enter goto-mode"
  (interactive)
  (if (equal current-prefix-arg nil)
      (progn (kel--switch-state 'goto) (kel-goto-selection-disable))
    (mc/disable-multiple-cursors-mode)
    (kel-deactivate-mark)
    (goto-line current-prefix-arg)))

(defun kel-goto-select-mode-start ()
  "When a count is specified, send the anchor to the given line, otherwise enter goto-mode"
  (interactive)
  (if (equal current-prefix-arg nil)
      (progn (kel--switch-state 'goto) (kel-goto-selection-enable))
    (mc/disable-multiple-cursors-mode)
    (kel-set-mark-if-inactive)
    (goto-line current-prefix-arg)))


(defun kel-view-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((kel-view-mode-p)
    (kel--switch-state 'normal))))

(defun kel-view-mode-start ()
  "enter view-mode"
  (interactive)
  (kel-set-view-count (if (equal current-prefix-arg nil) 1 current-prefix-arg))
  (kel--switch-state 'view))


(defun kel-prompt-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((kel-prompt-mode-p)
    (kel--switch-state 'normal))))

(defun kel-prompt-mode-start ()
  "enter command mode"
  (interactive)
  (kel--switch-state 'prompt)
  (message "processing command")
  (kel-process-command))

(provide 'kel-command)
;;; kel-command.el ends here
