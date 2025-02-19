;;; kel-vars.el --- Variables -*- lexical-binding: t; -*-
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

(defgroup kel nil
  "Custom group for kel."
  :group 'kel-module)

;; Behaviors

(defcustom kel-mode-state-list
  '((conf-mode . normal)
    (fundamental-mode . normal)
    (prog-mode . normal)
    (text-mode . normal))
  "A list of rules, each is (major-mode . init-state).

The init-state can be any state, including custom ones."
  :group 'kel
  :type '(alist :key-type (sexp :tag "Major-mode")
                :value-type (symbol :tag "Initial state")))

(defvar kel-state-mode-alist
  '((normal . kel-normal-mode)
    (insert . kel-insert-mode)
    ;(goto . kel-goto-mode)
    ;(motion . kel-motion-mode)
    ;(view . kel-view-mode)
    ;(prompt . kel-prompt-mode)
    ;(object . kel-object-mode)
                                        ;(user . meow-user-mode))
    )
  "Alist of kel states -> modes")


(defcustom kel-replace-state-name-list
  '((normal . "NORMAL")
    ;(motion . "MOTION")
    (insert . "INSERT")
    ;(goto . "GOTO")
    ;(view . "VIEW")
    ;(prompt . "PROMPT")
    ;(object . "OBJECT")
                                        ;(user . "USER"))
    )
  "A list of mappings for how to display state in indicator."
  :group 'kel
  :type '(alist :key-type (symbol :tag "Kel state")
                :value-type (string :tag "Indicator")))


(defcustom kel-matching-pairs
  '((?\( . '(?\) 1))
    (?\) . '(?\( -1))
    (?{ . '(?} 1))
    (?} . '(?{ -1))
    (?\[ . '(?\] 1))
    (?\] . '(?\[ -1))
    (?< . '(?> 1))
    (?> . '(?< -1)))
  "A list of codepoints that are to be treated as matching pairs for the m command."
  :group 'kel
  :type '(alist :key-type (char :tag "kel pair")
                :value-type (pair :tag "match and direction")))

(defun kel-get-matching-pair (char)
  (nth 1 (alist-get char kel-matching-pairs)))


;;; Internal variables

(defvar-local kel--current-state 'normal
  "A symbol represent current state.")

(defvar-local kel--normal-last-selection-command (lambda () (interactive) nil)
  "The last selection action. Either object or f/t")

(defun kel-set-normal-last-selection-command (func)
  (setq kel--normal-last-selection-command func))

(defun kel-get-normal-last-selection-command ()
  kel--normal-last-selection-command)

(defvar-local kel--insert-last-input ""
  "This variable records everything from insert mode including actions from c")

(defun kel-append-insert-last-input (value)
  (setq kel--insert-last-input (concat kel--insert-last-input value)))

(defun kel-reset-insert-last-input ()
  (setq kel--insert-last-input ""))
;; TODO: make a var that stores everything done in insert mode that comes from i, a, and c

(provide 'kel-vars)
;;; kel-vars.el ends here
