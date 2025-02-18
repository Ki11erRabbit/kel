;;; kel-util.el --- Utilities -*- lexical-binding t -*-
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

(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'color)

(require 'kel-vars)
(require 'kel-keymap)

;;

(defun kel-set-mark-if-inactive ()
  (interactive)
  (unless (use-region-p) (set-mark (point))))

(defun kel-set-mark-here ()
  (interactive)
  (set-mark (point)))

(defun kel-deactivate-mark ()
  (interactive)
  (deactivate-mark))

(defun kel-forward-word-util (count)
  (interactive)
  (forward-word count)
  (backward-word)
  (kel-set-mark-if-inactive)
  (forward-word))

(defun kel-backward-word-util (count)
  (interactive)
  (backward-word count)
  (forward-word)
  (kel-set-mark-if-inactive)
  (backward-word))

(defun kel-scroll-down-half-page ()
  "scroll down half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
    (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) (move-to-window-line nil))
      ((= ln lmax) (recenter (window-end)))
      (t (progn
           (move-to-window-line -1)
           (recenter))))))

(defun kel-scroll-up-half-page ()
  "scroll up half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
    (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) nil)
      ((= ln lmax) (move-to-window-line nil))
      (t (progn
           (move-to-window-line 0)
           (recenter))))))

(defun kel-flip-selection-util ()
  (exchange-point-and-mark))

(defun kel-make-selection-forward-util ()
  (let ((regions-end (region-end)))
    (when (> regions-end (point))
      (kel-set-mark-here)
      (exchange-point-and-mark)
      (goto-char regions-end))))

(defun kel-yank-and-delete-util ()
  (kill-region))

(defun kel-delete-util ()
  (delete-region))


(defvar kel-last-t-or-f ?f
  "Using t or f command sets this variable.")

(defvar-local kel-last-char-selected-to " "
  "This variable is updated by kel-select-to-char.")

(defun kel-select-up-to-char-util (count char)
  "Select up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (setq kel-last-char-selected-to char)
  (setq kakoune-last-t-or-f ?t)
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
	    (search-forward (char-to-string char) nil nil count)
	  (backward-char direction))
    (point)))

(defun kel-select-to-char-util (count char)
  "Select up to, and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (setq kel-last-char-select-to char)
  (setq kel-last-t-or-f ?f)
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
        (search-forward (char-to-string char) nil nil count))
    (point)))


(defun kel-select-up-to-char-reverse-util (count char)
  "Select up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (setq kel-last-char-selected-to char)
  (setq kakoune-last-t-or-f ?t)
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
	    (search-backward (char-to-string char) nil nil count)
	  (forward-char direction))
    (point)))

(defun kel-select-to-char-reverse-util (count char)
  "Select up to, and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (setq kel-last-char-select-to char)
  (setq kel-last-t-or-f ?f)
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
        (search-backward (char-to-string char) nil nil count))
    (point)))


(defun kel-line-util ()
  "Select the current line."
  (interactive)
  (let ((regions-end (region-end)))
  (if (use-region-p)
      (exchange-point-and-mark)
    nil)
  (beginning-of-line)
  (set-mark (point))
  (goto-char regions-end)
  (forward-line)
  (goto-char (- (point) 1))))


(defun kel-indent (count)
  "TODO: make work with multiple cursors"
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) 2)
             (setq deactivate-mark nil))
    (let ((beg (save-excursion (beginning-of-line) (point)))
          (end (save-excursion (forward-line count) (point))))
      (indent-rigidly beg end 2))))

(defun kel-unindent (count)
  "TODO: make work with multiple cursors"
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) -2)
             (setq deactivate-mark nil))
    (let ((beg (save-excursion (beginning-of-line) (point)))
          (end (save-excursion (forward-line count) (point))))
      (indent-rigidly beg end -2))))



(defun kel-lowercase ()
  (if (use-region-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-region (point) (+ 1 (point)))))

(defun kel-uppercase ()
  (if (use-region-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-region (point) (1+ (point)))))

;; Modes

(defvar kel-normal-mode)

(defun kel--execute-kbd-macro (kbd-macro)
  "Execute KBD-MACRO."
  (when-let* ((ret (key-binding (read-kbd-macro kbd-macro))))
    (cond
     ((commandp ret)
      (setq this-command ret)
      (call-interactively ret))

     ((and (not kel-use-keypad-when-execute-kbd) (keymap ret))
      (set-transient-map ret nil nil))

     ((and kel-use-keypad-when-execute-kbd (keymap ret))
      (kel-keypad-start-with kbd-macro)))))

(defun kel-insert-mode-p ()
  "Whether insert mode is enabled."
  (bound-and-true-p kel-insert-mode))

(defun kel-motion-mode-p ()
  "Whether motion mode is enabled."
  (bound-and-true-p kel-motion-mode))

(defun kel-normal-mode-p ()
  "Whether normal mode is enabled."
  (bound-and-true-p kel-normal-mode))

(defun kel-goto-mode-p ()
  "Whether goto mode is enabled."
  (bound-and-true-p kel-goto-mode))

(defun kel-view-mode-p ()
  "Whether view mode is enabled."
  (bound-and-true-p kel-view-mode))

(defun kel-prompt-mode-p ()
  "Whether prompt mode is enabled."
  (bound-and-true-p kel-prompt-mode))

(defun kel-object-mode-p ()
  "Whether object mode is enabled."
  (bound-and-true-p kel-object-mode))

(defun kel-user-mode-p ()
  "Whether user mode is enabled."
  (bound-and-true-p kel-user-mode))

(defun kel--disable-current-state ()
  (when kel--current-state
    ;(message "%s" kel--current-state)
    (funcall (alist-get kel--current-state kel-state-mode-alist) -1)
    (setq kel--current-state nil)))

(defun kel--state-p (state)
  (funcall (intern (concat "kel-" (symbol-name state) "-mode-p"))))

(defun kel--current-state ()
 kel--current-state)

(defun kel--get-state-name (state)
  "Get the name of the current state.

Looks up the state in kel-replace-state-name-list"
  (alist-get state kel-replace-state-name-list))

(defun kel--switch-state (state &optional no-hook)
  "Switch to STATE execute kel-switch-state-hook' unless NO-HOOK is non-nil."
  (unless (eq state (kel--current-state))
    (let ((mode (alist-get state kel-state-mode-alist)))
      (funcall mode 1))
    (unless (bound-and-true-p no-hook)
      (run-hook-with-args 'kel-switch-state-hook state))))


(defun kel--on-window-state-change (&rest _args)
  "Update cursor style after switching window."
  ;(kel--update-cursor)
                                        ;(kel--update-indicator))
  nil)

(defun kel--on-exit ()
  (unless (display-graphic-p)
    (send-string-to-terminal "\e[2 q")))



(defun kel--minibuffer-setup ()
  (local-set-key (kbd "<escape>") #'kel-minibuffer-quit)
  (setq-local kel-normal-mode nil)
  )



(defun kel--init-buffers ()
  "Enable kel in existing buffers."
  (dolist (buf (buffer-list))
    (unless (minibufferp buf)
      (with-current-buffer buf
        (kel--enable)))))

(provide 'kel-util)
;;; kel-util.el ends here
