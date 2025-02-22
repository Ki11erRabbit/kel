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

(defun kel-set-tab-stop ()
  (setq-default tab-width 8))

(defun kel-get-tab-stop ()
  tab-width)

(defvar-local kel--indent-width 4
  "width (in spaces) used for indentation, 0 means a tab character")

(defun kel-set-indent-width (size)
  (setq kel--indent-width size))

(defun kel-get-indent-width ()
  kel--indent-width)

;;; Internal variables

(defvar-local kel--current-state 'normal
  "A symbol represent current state.")

(defvar-local kel--normal-last-selection-command (lambda () (interactive) nil)
  "The last selection action. Either object or f/t")

(defun kel-set-normal-last-selection-command (func)
  (setq kel--normal-last-selection-command func))

(defun kel-get-normal-last-selection-command ()
  kel--normal-last-selection-command)

(defvar-local kel--last-insert-commands nil
  "This variable records actions from insert mode including actions from c")

(defun kel-add-insert-command (command &optional value)
  "Adds a command to kel--last-insert-commands.
Valid commands include:
`delete-selection` for deleting the current selection.
`delete-forward` for deleting n chars forward
`delete-backward` for deleting n chars backward
`insert-char` for inserting a particular character
`insert-string` for inserting a string."
  ;(message (format "%s %s" command value))
  (setq kel--last-insert-commands (cons (cons command value) kel--last-insert-commands)))

(defun kel-reset-last-insert-commands ()
  (setq kel--last-insert-commands nil))

(defun kel-get-last-insert-commands ()
  kel--last-insert-commands)

(defvar-local kel--goto-selection-enabled nil
  "This variable records if we entered goto mode with a selection")

(defun kel-is-goto-selection-set ()
  kel--goto-selection-enabled)

(defun kel-goto-selection-enable ()
  (setq kel--goto-selection-enabled t))

(defun kel-goto-selection-disable ()
  (setq kel--goto-selection-enabled nil))

(defvar-local kel--view-count 1
  "This variable holds the prefix number for view mode")

(defun kel-set-view-count (value)
  (setq kel--view-count value))

(defun kel-get-view-count ()
  kel--view-count)

(provide 'kel-vars)
;;; kel-vars.el ends here
