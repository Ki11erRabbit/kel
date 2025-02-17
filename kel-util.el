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


(defun kel--init-buffers ()
  "Enable kel in existing buffers."
  (dolist (buf (buffer-list))
    (unless (minibufferp buf)
      (with-current-buffer buf
        (kel--enable)))))

(provide 'kel-util)
;;; kel-util.el ends here
