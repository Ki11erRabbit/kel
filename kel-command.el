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
