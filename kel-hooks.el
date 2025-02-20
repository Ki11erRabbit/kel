;;; kel-hooks.el --- Hooks for Kel -*- lexical-binding: t; -*-
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

(defun kel-insert-post-command-hook ()
  (let* ((keys (this-command-keys-vector))
         (key-list (append keys nil)))
    (pcase key-list
      (`(127) (kel-add-insert-command 'delete-backward 1))
      (`('deletechar) (kel-add-insert-command 'delete-forward 1))
      (`() nil)
      (c (kel-add-insert-command 'insert-char (car c))))))


(provide 'kel-hooks)
;;; kel-hooks.el ends here
