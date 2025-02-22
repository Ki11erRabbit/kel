;;; kel-util.el --- Utilities -*- lexical-binding: t; -*-
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
(require 'multiple-cursors)

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

(defun kel-last-insert-mode-change-util ()
  (let ((commands (kel-get-last-insert-commands)))
    (kel-last-insert-mode-change-applicator commands)))

(defun kel-last-insert-mode-change-applicator (commands)
  (pcase commands
    (`() nil)
    (_ (progn
         (kel-last-insert-mode-change-applicator (cdr commands))
         (pcase (car commands)
           (`(delete-selection . _) (delete-region))
           (`(delete-forward . ,n) (delete-forward-char n))
           (`(delete-backward . ,n) (delete-backward-char n))
           (`(insert-char . ,c) (insert c))
           (`(insert-string . ,s) (insert s)))))))
  

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
  (let ((direction (if (>= count 0) 1 -1)))
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
  (let ((direction (if (>= count 0) 1 -1)))
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
  (let ((direction (if (>= count 0) 1 -1)))
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
  (let ((direction (if (>= count 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
        (search-backward (char-to-string char) nil nil count))
    (point)))


(defun kel-select-closing-pair (char)
  "finds a matching pair character from the kel-matching-pairs variable"
  (let* ((pair (kel-get-matching-pair char))
        (location nil)
        (index 1)
        (direction-mod (if (= (car (cdr pair)) 1) (lambda (x y) (+ x y)) (lambda (x y) (- x y))))
        (fetch-char (if (= (car (cdr pair)) 1) (lambda (x) (char-after x)) (lambda (x) (char-before x)))))
    (if (= (car (cdr pair)) 1) (set-mark (point)) (set-mark (+ (point) 1)))
    (setq location (kel-get-closing-pair char))
    (when location
      (if (= (car (cdr pair)) 1) (goto-char location) (goto-char (- location 1))))
    (unless location
      (message "no selections remaining"))))

(defun kel-match-closing-pair (char)
  "finds a matching pair character from the kel-matching-pairs variable"
  (let* ((pair (kel-get-matching-pair char))
        (location nil)
        (index 1)
        (direction-mod (if (= (car (cdr pair)) 1) (lambda (x y) (+ x y)) (lambda (x y) (- x y))))
        (fetch-char (if (= (car (cdr pair)) 1) (lambda (x) (char-after x)) (lambda (x) (char-before x)))))
    (unless (use-region-p) (if (= (car (cdr pair)) 1) (set-mark (point)) (set-mark (+ (point) 1))))
    (setq location (kel-get-closing-pair char))
    (when location
      (if (= (car (cdr pair)) 1) (goto-char location) (goto-char (- location 1))))
    (unless location
      (message "no selections remaining"))))

(defun kel-get-closing-pair (char)
  "finds a matching pair character from the kel-matching-pairs variable"
  (let* ((pair (kel-get-matching-pair char))
        (location nil)
        (index 1)
        (direction-mod (if (= (car (cdr pair)) 1) (lambda (x y) (+ x y)) (lambda (x y) (- x y))))
        (fetch-char (if (= (car (cdr pair)) 1) (lambda (x) (char-after x)) (lambda (x) (char-before x))))
        (matching-pair (car pair))
        (stack '()))
    (while (and (not location) (and (<= (funcall direction-mod (point) index) (buffer-size)) (>= (funcall direction-mod (point) index) 0)))
      (if (eq (funcall fetch-char (funcall direction-mod (point) index)) matching-pair)
          (if (> (length stack) 0)
              (setq stack (cdr stack))
            (setq location (funcall direction-mod (point) index)))
        (when (eq (funcall fetch-char (funcall direction-mod (point) index)) char)
          (setq stack (cons 1 stack))))
      (setq index (+ index 1)))
    location))

(defun kel-line-util ()
  "Select the current line."
  (beginning-of-line)
  (set-mark (point))
  (forward-line)
  (goto-char (- (point) 1)))


(defun kel-indent (count)
  "TODO: make work with multiple cursors"
  (let* ((indent-width (kel-get-indent-width))
         (width (* indent-width count)))
    (if width
        (if (use-region-p) ;; if width is greater than 1
            (progn (indent-rigidly (region-beginning) (region-end) width)
                   (setq deactivate-mark nil))
          (let ((beg (save-excursion (beginning-of-line) (point)))
                (end (save-excursion (forward-line count) (point))))
            (indent-rigidly beg end (- 0 width))))
      (if (use-region-p) ;; if width is 0 meaning that we use tabs
          (progn (indent-region (region-beginning) (region-end) width)
                 (setq deactivate-mark nil))
        (let ((beg (save-excursion (beginning-of-line) (point)))
              (end (save-excursion (forward-line count) (point))))
          (indent beg end (- 0 width)))))))

(defun kel-unindent (count)
  "TODO: make work with multiple cursors"
  (let* ((indent-width (kel-get-indent-width))
         (width (* indent-width count)))
    (if width
        (if (use-region-p) ;; if width is greater than 1
            (progn (indent-rigidly (region-beginning) (region-end) (- 0 width))
                   (setq deactivate-mark nil))
          (let ((beg (save-excursion (beginning-of-line) (point)))
                (end (save-excursion (forward-line count) (point))))
            (indent-rigidly beg end (- 0 width))))
      (if (use-region-p) ;; if width is 0 meaning that we use tabs
          (progn (indent-region (region-beginning) (region-end) (- 0 width))
                 (setq deactivate-mark nil))
        (let ((beg (save-excursion (beginning-of-line) (point)))
              (end (save-excursion (forward-line count) (point))))
          (indent beg end (- 0 width)))))))



(defun kel-lowercase ()
  (if (use-region-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-region (point) (+ 1 (point)))))

(defun kel-uppercase ()
  (if (use-region-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-region (point) (1+ (point)))))

(defun kel-convert-tabs-to-spaces (count)
  (let ((tab-size (if (> count 0) count (kel-get-tab-stop))))
    (if (use-region-p)
        (replace-regexp "\	" (make-string tab-size ?\s))
      (if (eq (char-after (point)) ?\t)
          (progn (delete-char 1) (insert (make-string tab-size ?\s)) (backward-char tab-size))
        nil))))

(defun kel-convert-spaces-to-tabs (count)
  (let ((tab-size (if (> count 0) count (kel-get-tab-stop))))
    (if (use-region-p)
        (replace-regexp (concat "\\" (make-string tab-size ?\s)) "	")
      nil)))

;; Changes through external programs

(defun kel-shell-pipe (command)
  (mc/for-each-cursor-ordered
   (shell-command-on-region (mc/cursor-beg cursor)
                            (mc/cursor-end cursor)
                            command
                            nil
                            1)))

(defun kel-shell-pipe-ignore (command)
  (mc/for-each-cursor-ordered
   (with-output-to-string
     (shell-command-on-region (mc/cursor-beg cursor)
                              (mc/cursor-end cursor)
                              command
                              standard-output))))

(defun kel-shell-pipe-before (command)
  (mc/for-each-cursor-ordered
   (mc/save-excursion
    (goto-char (mc/cursor-beg cursor))
    (insert (with-output-to-string
              (shell-command-on-region (mc/cursor-beg cursor)
                                       (mc/cursor-end cursor)
                                       command
                                       standard-output))))))
(defun kel-shell-pipe-after (command)
  (mc/for-each-cursor-ordered
   (mc/save-excursion
    (goto-char (mc/cursor-end cursor))
    (insert (with-output-to-string
              (shell-command-on-region (mc/cursor-beg cursor)
                                       (mc/cursor-end cursor)
                                       command
                                       standard-output))))))

;; Multiple Cursors

(defun kel-match-selection-regex (regex &optional group)
  "TODO: implement getting nth regex capture group"
  (if (not (region-active-p))
      (error "nothing selected")
    (kel-mc/mark-all-in-region-regexp regex (region-beginning) (region-end))))
          
(defun kel-mc/mark-all-in-region-regexp (regex beg end)
  "Find and mark all the parts in the region matching the given regexp."
  (let ((search regex)
        (case-fold-search nil))
    (if (string= search "")
        (message "Mark aborted")
      (progn
        (mc/remove-fake-cursors)
        (goto-char beg)
        (let ((lastmatch))
          (while (and (< (point) end) ; can happen because of (forward-char)
                      (search-forward-regexp search end t))
            (push-mark (match-beginning 0))
            (mc/create-fake-cursor-at-point)
            (setq lastmatch (point))
            (when (= (point) (match-beginning 0))
              (forward-char)))
          (unless lastmatch
            (error "Search failed for %S" search)))
        (goto-char (match-end 0))
        (if (< (mc/num-cursors) 3)
            (mc/disable-multiple-cursors-mode)
          (mc/pop-state-from-overlay (mc/furthest-cursor-before-point))
          (multiple-cursors-mode 1))))))

(defun kel-split-selection-regex (regex &optional group)
  "TODO: implement getting the nth regex capture group"
  (if (not (region-active-p))
      (error "nothing selected")
    (kel-mc/mark-all-except-region-regexp regex (region-beginning) (region-end))))

(defun kel-mc/mark-all-except-region-regexp (regex beg end)
  "Find and mark all the parts in the region matching the given regexp."
  (let ((search regex)
        (case-fold-search nil))
    (if (string= search "")
        (message "Mark aborted")
      (progn
        (mc/remove-fake-cursors)
        (goto-char beg)
        (let ((lastmatch))
          (push-mark beg)
          (while (and (< (point) end) ; can happen because of (forward-char)
                      (search-forward-regexp search end t))
            (backward-char 2)
            (mc/create-fake-cursor-at-point)
            (forward-char 2)
            (setq lastmatch (point))
            (when (= (point) (match-beginning 0)) 
              (forward-char))
            (push-mark (match-end 0)))
          (unless lastmatch
            (error "Search failed for %S" search)))
        (push-mark (point))
        (goto-char end)
        (mc/create-fake-cursor-at-point)
        (if (< (mc/num-cursors) 3)
            (mc/disable-multiple-cursors-mode)
          (mc/pop-state-from-overlay (mc/furthest-cursor-before-point))
          (multiple-cursors-mode 1))))))


(defun kel-mc-split-region (beg end search)
  "Split region each time SEARCH occurs between BEG and END.

This can be thought of as an inverse to `mc/mark-all-in-region'."
  (let ((case-fold-search nil))
    (if (string= search "")
        (user-error "Empty search term")
      (progn
        (mc/remove-fake-cursors)
        (goto-char beg)
        (push-mark beg)
        (while (search-forward search end t)
          (save-excursion
            (goto-char (match-beginning 0))
            (mc/create-fake-cursor-at-point))
          (push-mark (match-end 0)))
        (unless (= (point) end)
          (goto-char end))
        (mc/maybe-multiple-cursors-mode)))))

(defun kel-set-selections-to-start-end ()
  "Gets the region of each cursor, removes all cursors, and puts new cursors at the ends of those selections"
  (let ((cursor-pos (cons (cons (region-beginning) (region-end)) nil))
        (len 0))
    (mc/for-each-fake-cursor 
     (setq cursor-pos (cons (cons (mc/cursor-beg cursor) (mc/cursor-end cursor)) cursor-pos)))
    (setq len (length cursor-pos))
    (mc/remove-fake-cursors)
    (deactivate-mark)
    (dolist (pair cursor-pos)
      (goto-char (car pair))
      (mc/create-fake-cursor-at-point)
      (goto-char (cdr pair))
      (if (> len 1)
          (mc/create-fake-cursor-at-point)
        nil)
      (setq len (- len 1)))))


(defun kel-keep-match (regex)
  "Keep the selection if the regex matches within a cursor region
TODO: ensure that selections are kept"
  (let ((cursors nil)
        (cursor-direction nil)
        (last-matching-pair nil)
        (last-matching-cursor nil))
    (mc/for-each-cursor-ordered
     (setq cursors (cons (cons (mc/cursor-beg cursor) (mc/cursor-end cursor)) cursors))
     (setq cursor-direction (if (< (overlay-get cursor 'point) (overlay-get cursor 'mark)) 'beg (if (> (overlay-get cursor 'point) (overlay-get cursor 'mark)) 'end nil))))
    (mc/remove-fake-cursors)
    (deactivate-mark)
    (setq cursors (reverse cursors))
    (dolist (pair cursors)
      (goto-char (car pair))
      (if (search-forward-regexp regex (cdr pair) t)
          (progn
            (setq last-matching-pair pair)
          (pcase cursor-direction
            ('beg (progn
                    (setq last-matching-cursor (mc/create-fake-cursor-at-point))
                    (push-mark (cdr pair))))
            ('end (progn
                    (push-mark (point))
                    (goto-char (cdr pair))
                    (setq last-matching-cursor (mc/create-fake-cursor-at-point))))))
        nil))
    (if last-matching-pair
        (pcase cursor-direction
          ('beg (progn
                  (mc/remove-fake-cursor last-matching-cursor)
                  (goto-char (car last-matching-pair))
                  (set-mark (cdr last-matching-pair))))
            ('end (progn
                  (mc/remove-fake-cursor last-matching-cursor)
                  (set-mark (point))
                  (goto-char (cdr last-matching-pair)))))
      nil)
  (mc/maybe-multiple-cursors-mode)))
            
 (defun kel-remove-match (regex)
  "clear selections that match the given regex
TODO: ensure that selections are kept"
  (let ((cursors nil)
        (cursor-direction nil)
        (last-matching-pair nil)
        (last-matching-cursor nil))
    (mc/for-each-cursor-ordered
     (setq cursors (cons (cons (mc/cursor-beg cursor) (mc/cursor-end cursor)) cursors))
     (setq cursor-direction (if (< (overlay-get cursor 'point) (overlay-get cursor 'mark)) 'beg (if (> (overlay-get cursor 'point) (overlay-get cursor 'mark)) 'end nil))))
    (mc/remove-fake-cursors)
    (deactivate-mark)
    (setq cursors (reverse cursors))
    (dolist (pair cursors)
      (goto-char (car pair))
      (if (search-forward-regexp regex (cdr pair) t)
          nil
        (progn
            (setq last-matching-pair pair)
          (pcase cursor-direction
            ('beg (progn
                    (setq last-matching-cursor (mc/create-fake-cursor-at-point))
                    (push-mark (cdr pair))))
            ('end (progn
                    (push-mark (point))
                    (goto-char (cdr pair))
                    (setq last-matching-cursor (mc/create-fake-cursor-at-point))))))))
    (if last-matching-pair
        (pcase cursor-direction
          ('beg (progn
                  (mc/remove-fake-cursor last-matching-cursor)
                  (goto-char (car last-matching-pair))
                  (set-mark (cdr last-matching-pair))))
            ('end (progn
                  (mc/remove-fake-cursor last-matching-cursor)
                  (set-mark (point))
                  (goto-char (cdr last-matching-pair)))))
      nil)
  (mc/maybe-multiple-cursors-mode)))             
   

(defun kel-pipe-only-success (command)
  "pipes the output of a command only if it succeeds"
  (mc/for-each-cursor-ordered
   (let* ((buffer-text (buffer-substring (mc/cursor-beg cursor) (mc/cursor-end cursor)))
         (output
          (shell-command-to-string (format "out=$(echo \"%s\" | %s) && echo \"$out\"" buffer-text command))))
     (message (format "message was: %s" output))
     (when (> (length output) 0)
       (mc/save-excursion
        (goto-char (mc/cursor-beg cursor))
        (delete-region (mc/cursor-beg cursor) (mc/cursor-end cursor))
        (insert output))))))

(defun kel-move-selection-forwards (count)
  (dotimes (i count)
    (mc/cycle-forward)))
    
(defun kel-move-selection-backwards (count)
  (dotimes (i count)
    (mc/cycle-backward)))

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
