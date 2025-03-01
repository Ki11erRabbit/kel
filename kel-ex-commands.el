;;; kel-ex-commands.el --- Kel commands for ex -*- lexical-binding: t; -*-
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

(require 'kel-util)

(defun kel-quit (&optional force)
  (condition-case nil
      (kel-window-delete)
    (error
     (if (and (bound-and-true-p server-buffer-clients)
              (fboundp 'server-edit)
              (fboundp 'server-buffer-done))
         (if force
             (server-buffer-done (current-buffer))
           (server-edit))
       (condition-case nil
           (delete-frame)
         (error
          (condition-case nil
              (tab-bar-close-tab)
            (error
             (if force
                 (kill-emacs)
               (save-buffers-kill-emacs))))))))))

(defun kel-quit-force ()
  (kel-quit t))

(defun kel-write (&optional file-name)
  (let ((bufname (buffer-file-name (buffer-base-buffer))))
    (if file-name
        (if (string= file-name bufname)
            (save-buffer)
          (write-file file-name))
      (save-buffer))))


(defun kel-edit (file-name &optional line column)
  (find-file file-name)
  (when line (goto-line line))
  (when column (move-to-column column)))

(provide 'kel-ex-commands)
;;; kel-ex-commands.el ends here
