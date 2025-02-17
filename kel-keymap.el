;;; kel-keymap.el --- Default keybindings for Kel -*- lexical-binding t -*-
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



;;; Code:

(require 'kel-vars)

(declare-function kel-describe-key "kel-command")
(declare-function kel-end-or-call-kmacro "kel-command")
(declare-function kel-end-kmacro "kel-command")

(defvar kel-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [remap describe-key] #'kel-describe-key)
    keymap)
  "Global keymap for Kel.")

(defvar kel-normal-state-keymap
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "i") 'kel-insert)
    (define-key keymap (kbd "h") 'kel-backward-char)
    (define-key keymap (kbd "H") 'kel-select-backward-char)
    (define-key keymap (kbd "j") 'kel-next-line)
    (define-key keymap (kbd "J") 'kel-select-next-line)
    (define-key keymap (kbd "k") 'kel-previous-line)
    (define-key keymap (kbd "K") 'kel-select-previous-line)
    (define-key keymap (kbd "l") 'kel-forward-char)
    (define-key keymap (kbd "L") 'kel-select-forward-char)
    (define-key keymap (kbd "w") 'kel-forward-word)
    (define-key keymap (kbd "W") 'kel-select-forward-word)
    (define-key keymap (kbd "b") 'kel-backward-word)
    (define-key keymap (kbd "B") 'kel-select-backward-word)
    (define-key keymap (kbd "e") 'kel-forward-symbol)
    (define-key keymap (kbd "E") 'kel-select-forward-symbol)
    (define-key keymap (kbd "f") 'kel-to-char)
    (define-key keymap (kbd "F") 'kel-select-to-char)
    (define-key keymap (kbd "t") 'kel-up-to-char)
    (define-key keymap (kbd "T") 'kel-select-up-to-char)
    (define-key keymap (kbd "x") 'kel-line)
    (define-key keymap (kbd "X") 'kel-select-line)
    (define-key keymap (kbd "M-h") 'kel-select-to-line-begin)
    (define-key keymap (kbd "M-l") 'kel-select-to-line-end)
    (define-key keymap (kbd "<home>") 'kel-select-to-line-begin)
    (define-key keymap (kbd "<end>") 'kel-select-to-line-end)
    (define-key keymap (kbd "%") 'kel-select-buffer)
    (define-key keymap (kbd "C-b") 'kel-page-up)
    (define-key keymap (kbd "C-f") 'kel-page-down)
    (define-key keymap (kbd "<pageup>") 'kel-select-to-line-begin)
    (define-key keymap (kbd "<pagedown>") 'kel-select-to-line-end)
    (define-key keymap (kbd "C-u") 'kel-half-page-up)
    (define-key keymap (kbd "C-d") 'kel-half-page-down)
    (define-key keymap (kbd ";") 'kel-reduce-selection)
    (define-key keymap (kbd "M-;") 'kel-flip-selection)
    (define-key keymap (kbd "M-:") 'kel-make-selection-forward)
    (define-key keymap (kbd "0") 'digit-argument)
    (define-key keymap (kbd "1") 'digit-argument)
    (define-key keymap (kbd "2") 'digit-argument)
    (define-key keymap (kbd "3") 'digit-argument)
    (define-key keymap (kbd "4") 'digit-argument)
    (define-key keymap (kbd "5") 'digit-argument)
    (define-key keymap (kbd "6") 'digit-argument)
    (define-key keymap (kbd "7") 'digit-argument)
    (define-key keymap (kbd "8") 'digit-argument)
    (define-key keymap (kbd "9") 'digit-argument)
    (define-key keymap (kbd "-") 'digit-argument)
    (define-key keymap [remap kmacro-end-or-call-macro] #'kel-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'kel-end-kmacro)
    keymap)
  "Keymap for Kel normal state.")

(defvar kel-insert-state-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap [escape] 'kel-insert-exit)
    (define-key keymap [remap kmacro-end-or-call-macro] #'kel-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'kel-end-kmacro)
    keymap)
  "Keymap for Kel insert state.")

(defvar kel-goto-state-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap [escape] 'kel-goto-exit)
    (define-key keymap [remap kmacro-end-or-call-macro] #'kel-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'kel-end-kmacro)
    keymap)
  "Keymap for Kel Goto state.")
    
(defvar kel-view-state-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap [escape] 'kel-view-exit)
    (define-key keymap [remap kmacro-end-or-call-macro] #'kel-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'kel-end-kmacro)
    keymap)
  "Keymap for Kel View state.")

(defvar kel-prompt-state-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap [escape] 'kel-prompt-exit)
    (define-key keymap [remap kmacro-end-or-call-macro] #'kel-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'kel-end-kmacro)
    keymap)
  "Keymap for Kel Prompt state.")

(defvar kel-object-state-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap [escape] 'kel-object-exit)
    (define-key keymap [remap kmacro-end-or-call-macro] #'kel-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'kel-end-kmacro)
    keymap)
  "Keymap for Kel object state.")

(defvar kel-user-state-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap [escape] 'kel-user-exit)
    (define-key keymap [remap kmacro-end-or-call-macro] #'kel-end-or-call-kmacro)
    (define-key keymap [remap kmacro-end-macro] #'kel-end-kmacro)
    keymap)
  "Keymap for Kel user state.")

(defvar kel-motion-state-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [escape] 'kel-last-buffer)
    keymap)
  "Keymap for Kel motion state.")

(defvar kel-keymap-alist
  `((insert . ,kel-insert-state-keymap)
    (normal . ,kel-normal-state-keymap)
    (motion . ,kel-motion-state-keymap))
  "Alist of symbols of state names to keymaps.")


(provide 'kel-keymap)
;;; kel-keymap.el ends here
