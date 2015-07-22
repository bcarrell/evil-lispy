;;; evil-lispy.evil Mode

;; Copyright (C) 2015 Brandon Carrell

;; Author: Brandon Carrell <brandoncarrell@gmail.com>
;; URL: https://github.com/bcarrell/evil-lispy
;; Version: 0.0.1
;; Keywords: lisp

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; evil-lispy defines a minor mode and an additional Evil state for editing
;; Lisp code.  The goal is to encourage a workflow where you can hop between
;; Lispy State for making structured edits using Lispy bindings and the rest
;; of the standard Evil states for general editing.  Where it makes sense,
;; this package redefines a few Lispy bindings, which can be turned on or off
;; by a variable.

;; In addition to providing the Lispy state, this package will do its best to
;; keep your buffer balanced while writing Lisp code.  To that end, it will
;; advise common Evil operations and block them if they will break your buffer
;; or modify them slightly in order to keep everything balanced.
;;
;; It also provides several useful keybindings for editing Lisp in Normal mode.
;;
;; There are several ways to activate Lispy state, denoted as <LISP> in your
;; modeline, all from Normal mode:
;;
;; ) will hop to the right paren and activate Lispy state
;; ( will hop to the left paren and activate Lispy state
;; >i will hop to the right paren and activate Lispy state
;; <i will hop to the left paren and activate Lispy state
;; gv will mark the current symbol and activate Lispy state

;; Once in Lispy state, use ESC to return to Normal mode.

;;; Code:

(require 'evil)
(require 'lispy)

(put 'evil-define-state 'lisp-indent-function 'defun)

;; ——— Customization ———————————————————————————————————————————————————————————

(defgroup evil-lispy nil
  "Evil integration with Lispy."
  :group 'lispy)

(defcustom evil-lispy-modified-operators t
  "Set to t to use the `evil-lispy' versions of Vim operators like d, c, y, D,
etc.  These provide a safe version that should respect the balance of the
buffer."
  :group 'evil-lispy
  :type 'boolean)


;; ——— State ———————————————————————————————————————————————————————————————————

(evil-define-state lispy
  "An evil state for Lispy, a precision editing mode for Lisp."
  :tag "<L>"
  :message "Entering Lispy state!"
  :cursor ("red" box)
  :suppress-keymap t
  :entry-hook (evil-lispy-state-entry)
  :exit-hook (evil-lispy-state-exit)
  nil)

(defun evil-lispy-state-entry ()
  (remove-hook 'activate-mark-hook #'evil-visual-activate-hook t)
  (lispy-mode 1))

(defun evil-lispy-state-exit ()
  (when (region-active-p) (deactivate-mark))
  (add-hook 'activate-mark-hook #'evil-visual-activate-hook nil t)
  (lispy-mode -1))

(defun evil-lispy-enter-state (direction extra-direction)
  "Return a lambda which enters Lispy state at the DIRECTION side of
the current form.  DIRECTION must be either 'left or 'right."
  (let ((f (intern (concat "lispy-" (symbol-name direction))))
        (g (intern (concat "lispy-" (symbol-name extra-direction)))))
    `(lambda ()
       (interactive)
       (when (looking-at lispy-left) (forward-char))
       (let ((pos (point)))
         (,f 1)
         (when (eq (point) pos) (,g 1)))
       (evil-lispy-state))))

(fset 'evil-lispy-enter-state-left (evil-lispy-enter-state 'left 'backward))
(fset 'evil-lispy-enter-state-right (evil-lispy-enter-state 'right 'forward))

(defun evil-lispy-enter-marked-state ()
  "Enters `lispy-state' with the current symbol under point marked."
  (interactive)
  (evil-lispy-state)
  (lispy-mark-symbol))

(defun evil-lispy-enter-visual-state ()
  "If we're in visual state, enter `lispy-state' with the current region
selected."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end))
        (pos (point)))
    (evil-lispy-state)
    (set-mark (if (eq pos start) end start))))

(defun evil-lispy-enter-insert-state (direction extra-direction)
  "Return a lambda which enters Insert state at the DIRECTION side of
the current form.  DIRECTION must be either 'left or 'right."
  `(lambda ()
     (interactive)
     (funcall (evil-lispy-enter-state ',direction ',extra-direction))
     (evil-insert-state)
     (cond
      ((eq ',direction 'left)
       (forward-char)
       (unless (looking-at "\s")
         (insert ?\s)
         (backward-char)))
      ((eq ',direction 'right)
       (backward-char)
       (unless (looking-back "\s")
         (insert ?\s))))))

(fset 'evil-lispy-enter-insert-state-left
      (evil-lispy-enter-insert-state 'left 'backward))
(fset 'evil-lispy-enter-insert-state-right
      (evil-lispy-enter-insert-state 'right 'forward))

;; ——— Mode ————————————————————————————————————————————————————————————————————

(defvar evil-lispy-mode-map (make-sparse-keymap))

(define-minor-mode evil-lispy-mode
  "A minor mode for integrating Evil and Lispy."
  :lighter " evil-lispy"
  :keymap evil-lispy-mode-map
  :after-hook (evil-normal-state))

;; ——— Text objects ————————————————————————————————————————————————————————————

(evil-define-text-object evil-lispy--outer-form-object (&optional count beg end type)
  (let ((bounds (lispy--bounds-list)))
    (when bounds
      (evil-range (car bounds) (cdr bounds)))))

(evil-define-text-object evil-lispy--inner-form-object (&optional count beg end type)
  (let ((bounds (lispy--bounds-list)))
    (evil-range (1+ (car bounds)) (1- (cdr bounds)))))

(define-key evil-inner-text-objects-map "f" 'evil-lispy--inner-form-object)
(define-key evil-outer-text-objects-map "f" 'evil-lispy--outer-form-object)

;; ——— Operations ——————————————————————————————————————————————————————————————

(defun evil-lispy-describe ()
  (interactive)
  (save-excursion
    (lispy-mark-symbol)
    (lispy-describe-inline)))

;; ——— Operators ———————————————————————————————————————————————————————————————

(defun evil-lispy--current-line-string ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun evil-lispy--balanced-p (start end)
  "Predicate to check if a range contains balanced boundaries.
Useful for checking if a delete will break balance.  Returns t if start -> end
is balanced."
  (condition-case condition
      (let ((s (buffer-substring-no-properties start end)))
        (with-temp-buffer
          (insert s)
          (check-parens))
        t)
    (error nil)))

(defun evil-lispy--line-deletion-bounds ()
  "Return a pair of positions representing the bounds of a line deletion."
  (let* ((line (evil-lispy--current-line-string))
         (line-length (length line))
         (curpos (current-column))
         (chars-to-delete (with-temp-buffer
                            (insert line)
                            (move-to-column curpos)
                            (lispy-kill)
                            (- line-length
                               (length (evil-lispy--current-line-string))))))
    (cons (point) (+ (point) chars-to-delete))))

(defun evil-lispy--reconcile-bounds (req-bnds bnds)
  (let ((req-beg (car req-bnds))
        (req-end (cdr req-bnds))
        (bnds-beg (1+ (car bnds)))
        (bnds-end (1- (cdr bnds))))
    (cons (if (< req-beg bnds-beg)
              bnds-beg
            req-beg)
          (if (> req-end bnds-end)
              bnds-end
            req-end))))

(defun evil-lispy--safe-operator (op orig beg end type reg yank-handler)
  "Wrapper for creating safe variants of Evil operations."
  (let* ((bnd-op (if (lispy--in-string-p)
                     #'lispy--bounds-string
                   #'lispy--bounds-list))
         (cur-bnds (save-excursion
                     (goto-char orig)
                     (funcall bnd-op))))
    (cond
     ;; We're not changing anything or the operation is balanced
     ((or (= end beg) (evil-lispy--balanced-p beg end))
      (funcall op beg end type reg yank-handler))

     ;; We're not in a form and it's unbalanced
     ((null cur-bnds)
      (error "Couldn't reconcile bounds of unbalanced operation"))

     ;; Destination is in another form or string
     ;; We could refuse the operation, but let's try massaging it
     ;; and accomplish something, even if it's not the entire
     ;; request
     (t
      (let* ((revised-beg (save-excursion
                            (goto-char beg)
                            (cl-loop until
                                     (equal cur-bnds (funcall bnd-op))
                                     do
                                     (forward-char)
                                     finally return
                                     (if (eq type 'line)
                                         (point)
                                       (1+ (point))))))
             (revised-end (save-excursion
                            (goto-char end)
                            (cl-loop until
                                     (equal cur-bnds (funcall bnd-op))
                                     do
                                     (backward-char)
                                     finally return
                                     (if (> end orig)
                                         (1+ (point))
                                       (point))))))
        (funcall op
                 revised-beg
                 revised-end
                 type
                 reg
                 yank-handler)
        (goto-char revised-beg))))))

(evil-define-operator evil-lispy-delete (beg end type reg yank-handler orig)
  "A balanced version of `evil-delete'."
  (interactive "<R><x><y>" (list (point)))
  (evil-lispy--safe-operator #'evil-delete orig beg end type reg yank-handler))

(evil-define-operator evil-lispy-yank (beg end type reg yank-handler orig)
  "A balanced version of `evil-yank'."
  (interactive "<R><x><y>" (list (point)))
  (evil-lispy--safe-operator #'evil-yank orig beg end type reg yank-handler))

(evil-define-operator evil-lispy-change (beg end type reg yank-handler orig)
  "A balanced version of `evil-change'."
  (interactive "<R><x><y>" (list (point)))
  (evil-lispy--safe-operator #'evil-change orig beg end type reg yank-handler))

(evil-define-operator evil-lispy-change-line (beg end type reg yank-handler)
  "Delete to end of line using `lispy-kill' and change to `evil-insert-state'."
  :motion nil
  (interactive "<R><x>")
  (let ((bnds (evil-lispy--line-deletion-bounds)))
    (evil-change (car bnds)
                 (cdr bnds)
                 type
                 reg
                 yank-handler)))

(evil-define-operator evil-lispy-delete-line (beg end type reg yank-handler)
  "Delete to end of line using `lispy-kill'."
  :motion nil
  (interactive "<R><x>")
  (let ((bnds (evil-lispy--line-deletion-bounds)))
    (evil-delete (car bnds)
                 (cdr bnds)
                 type
                 reg
                 yank-handler)))

(evil-define-operator evil-lispy-yank-line (beg end type reg yank-handler)
  "Safe yank to end of line."
  :motion nil
  :move-point nil
  (interactive "<R><x>")
  (let ((bnds (evil-lispy--line-deletion-bounds)))
    (evil-yank (car bnds)
               (cdr bnds)
               type
               reg
               yank-handler)))


;; ——— Keys ————————————————————————————————————————————————————————————————————

(define-key evil-lispy-state-map [escape] 'evil-normal-state)

;; ——— Entering state ——————————————————
(evil-define-key 'normal evil-lispy-mode-map
  "(" #'evil-lispy-enter-state-left
  ")" #'evil-lispy-enter-state-right
  "gv" #'evil-lispy-enter-marked-state
  "<i" #'evil-lispy-enter-insert-state-left
  "<I" #'evil-lispy-enter-insert-state-left
  ">i" #'evil-lispy-enter-insert-state-right
  ">I" #'evil-lispy-enter-insert-state-right)

(evil-define-key 'visual evil-lispy-mode-map
  (kbd "RET") #'evil-lispy-enter-visual-state)

;; ——— Editing operations ——————————————
(when evil-lispy-modified-operators
  (evil-define-key 'normal evil-lispy-mode-map
    "D" #'evil-lispy-delete-line
    "C" #'evil-lispy-change-line
    "Y" #'evil-lispy-yank-line
    "d" #'evil-lispy-delete
    "c" #'evil-lispy-change
    "y" #'evil-lispy-yank))

(evil-define-key 'normal evil-lispy-mode-map
  "K" #'evil-lispy-describe
  (kbd "M-k") #'lispy-kill-sentence
  (kbd "C-1") #'evil-lispy-describe
  (kbd "C-2") #'lispy-arglist-inline)

;; ——— Insert operations ———————————————
(evil-define-key 'insert evil-lispy-mode-map
  "(" #'lispy-parens
  "[" #'lispy-brackets
  "{" #'lispy-braces
  ")" #'lispy-right-nostring
  "\"" #'lispy-quotes
  (kbd "DEL") #'lispy-delete-backward
  (kbd "M-k") #'lispy-kill-sentence
  (kbd "C-1") #'lispy-describe-inline
  (kbd "C-2") #'lispy-arglist-inline)

(provide 'evil-lispy)

;;; evil-lispy.el ends here
