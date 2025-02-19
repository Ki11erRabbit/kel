;;; kel-core.el --- Core functionality -*- lexical-binding: t; -*-
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

(require 'cl-lib)
(require 'subr-x)

(require 'kel-util)
(require 'kel-command)
(require 'kel-vars)
(require 'kel-helpers)

(kel-define-state normal
                  "Kel NORMAL state minor mode."
                  :keymap kel-normal-state-keymap)

(kel-define-state insert
                  "Kel INSERT state minor mode."
                  :keymap kel-insert-state-keymap)

; TODO: add other modes

;;;###autoload
(define-minor-mode kel-mode
  "Kel minor mode.

This minor mode is used by kel-global-mode, should not be enabled directly."
  :init-value nil
  :interactive nil
  :global nil
  :keymap kel-keymap
  (if kel-mode
      (kel--enable)
    (kel--disable)))

;;;###autoload
(define-global-minor-mode kel-global-mode kel-mode
  (lambda ()
    (unless (minibufferp)
      (kel-mode 1)))
  :group 'kel
  (if kel-mode
      (kel--global-enable)
    (kel--global-disable)))

(defun kel--enable ()
  "Enable Kel.

This function will switch to the proper state for current major
mode. Firstly, the variable `kel-mode-state-list' will be used.
If current major mode derived from any mode from the list,
specified state will be used.  When no result is found, give a
test on the commands bound to the keys a-z. If any of the command
names contains \"self-insert\", then NORMAL state will be used.
Otherwise, MOTION state will be used.

Note: When this function is called, NORMAL state is already
enabled.  NORMAL state is enabled globally when
`kel-global-mode' is used, because in `fundamental-mode',
there's no chance for kel to call an init function."
  (let ((state (kel--mode-get-state)))
    (kel--disable-current-state)
    (kel--switch-state state t)))

(defun kel--disable ()
  "Disable Kel."
  (mapc (lambda (state-mode) (funcall (cdr state-mode) -1)) kel-state-mode-alist)
  ;(when (secondary-selection-exist-p)
                                        ; (kel--cancel-second-selection))
  )

(defun kel--global-enable ()
  "Enable kel globally."
  (setq-default kel-normal-mode t)
  (kel--init-buffers)
  (add-hook 'window-state-change-functions #'kel--on-window-state-change)
  (add-hook 'minibuffer-setup-hook #'kel--minibuffer-setup)
  ;(add-hook 'pre-command-hook 'kel--highlight-pre-command)
  ;(add-hook 'post-command-hook 'meow--maybe-toggle-beacon-state)
  ;(add-hook 'suspend-hook 'kel--on-exit)
  ;(add-hook 'suspend-resume-hook 'kel--update-cursor)
  (add-hook 'kill-emacs-hook 'kel--on-exit)
  (add-hook 'desktop-after-read-hook 'kel--init-buffers)

  ;(meow--enable-shims)
  ;; meow-esc-mode fix ESC in TUI
  ;(meow-esc-mode 1)
  ;; raise Meow keymap priority
  ;(add-to-ordered-list 'emulation-mode-map-alists
  ;                     `((meow-motion-mode . ,meow-motion-state-keymap)))
  ;(add-to-ordered-list 'emulation-mode-map-alists
  ;                     `((meow-normal-mode . ,meow-normal-state-keymap)))
  ;(add-to-ordered-list 'emulation-mode-map-alists
  ;                     `((meow-beacon-mode . ,meow-beacon-state-keymap)))
  ;(when meow-use-cursor-position-hack
  ;  (setq redisplay-highlight-region-function #'meow--redisplay-highlight-region-function)
  ;  (setq redisplay-unhighlight-region-function #'meow--redisplay-unhighlight-region-function))
  ;(meow--prepare-face)
                                        ;(advice-add 'enable-theme :after 'kel--enable-theme-advice))
  )

(defun kel--global-disable ()
  "Disable Kel globally."
  (setq-default kel-normal-mode nil)
  (remove-hook 'window-state-change-functions #'kel--on-window-state-change)
  (remove-hook 'minibuffer-setup-hook #'kel--minibuffer-setup)
  ;(remove-hook 'pre-command-hook 'kel--highlight-pre-command)
  ;(remove-hook 'post-command-hook 'meow--maybe-toggle-beacon-state)
  (remove-hook 'suspend-hook 'kel--on-exit)
  ;(remove-hook 'suspend-resume-hook 'meow--update-cursor)
  ;(remove-hook 'kill-emacs-hook 'kel--on-exit)
  (remove-hook 'desktop-after-read-hook 'kel--init-buffers)
  ;(meow--disable-shims)
  ;(meow--remove-modeline-indicator)
  ;; (when meow-use-cursor-position-hack
  ;;   (setq redisplay-highlight-region-function meow--backup-redisplay-highlight-region-function)
  ;;   (setq redisplay-unhighlight-region-function meow--backup-redisplay-unhighlight-region-function))
  ;(meow-esc-mode -1)
                                        ;(advice-remove 'enable-theme 'meow--enable-theme-advice))
  )


(provide 'kel-core)
;;; kel-core.el ends here
