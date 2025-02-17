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
