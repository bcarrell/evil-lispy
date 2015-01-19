;;; evil-lispy.el --- Lispy for Evil Mode

;; Copyright (C) 2014 Brandon Carrell

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
;; Lisp code.  The goal is to encourage a workflow where you can hop into
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
;; gv will mark the current symbol and activate Lispy state
;; << will hop to the beginning of the current defun and activate Lispy state
;; >> will hop to the end of the current defun and activate Lispy state

;; Once in Lispy state, use C-g or ESC to return to Normal mode.

;;; Code:

(require 'evil)
(require 'lispy)

;; ——— Mode ————————————————————————————————————————————————————————————————————

(define-minor-mode evil-lispy-mode
  "A minor mode for integrating Evil and Lispy."
  :lighter " eLY"
  :keymap (make-sparse-keymap)
  :after-hook (evil-normal-state))


;; ——— Helpers —————————————————————————————————————————————————————————————————

(defun evil-lispy--in-special-p ()
  (cond
   ((looking-at lispy-left) t)
   ((looking-back lispy-right) t)
   (t nil)))

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

(defun evil-lispy--looking-at-empty-p ()
  "Predicate that checks if we're looking at an empty form."
  (cond
   ((looking-at "()") t)
   ((looking-at "\\[\\]") t)
   ((looking-at "\\{\\}") t)
   (t nil)))

(defun evil-lispy--inside-empty-p ()
  "Predicate that checks if we're inside an empty form in normal mode."
  (cond
   ((and (looking-at "\)")
         (looking-back "\(")) t)
   ((and (looking-at "\\]")
         (looking-back "\\[")) t)
   ((and (looking-at "\}")
         (looking-back "\{")) t)))


;; ——— State ———————————————————————————————————————————————————————————————————

(evil-define-state lispy
  "An evil state for Lispy, a precision editing mode for Lisp."
  :tag "<LISP>"
  :cursor ("red" box)
  :suppress-keymap t

  ;; When in Lispy state, don't use Evil visual mode
  (if (evil-lispy-state-p)
      (remove-hook 'activate-mark-hook 'evil-visual-activate-hook t)
    (add-hook 'activate-mark-hook 'evil-visual-activate-hook nil t)))

;; Enter state
(defun evil-lispy--enter-state-right ()
  "Enter lispy state on the right bound of the form."
  (interactive)
  (unless (evil-lispy--in-special-p)
    (lispy-out-forward 1))
  (evil-lispy-state))

(defun evil-lispy--enter-state-left ()
  "Enter lispy state on the left bound of the form."
  (interactive)
  (unless (evil-lispy--in-special-p)
    (lispy-out-backward 1))
  (evil-lispy-state))

(defun evil-lispy--enter-state-with-symbol-marked ()
  "Enter lispy state with a mark."
  (interactive)
  (evil-lispy-state)
  (lispy-mark-symbol))

(defun evil-lispy--enter-state-at-beginning-of-defun ()
  (interactive)
  (lispy-beginning-of-defun)
  (evil-lispy-state))

(defun evil-lispy--enter-state-at-end-of-defun ()
  (interactive)
  (lispy-beginning-of-defun)
  (lispy-different)
  (evil-lispy-state))

;; Exit state
(defun evil-lispy--exit-state ()
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (deactivate-mark)
    (evil-normal-state)))


;; ——— Safe editing fns ————————————————————————————————————————————————————————

(defun evil-lispy--check-unbalanced-op-p (&rest arg)
  "Given an Evil operator arg, check the range to see if this operation will
result in an unbalanced buffer.  Used with :before-while advice to block
the operation entirely if this returns nil."
  (let ((start (car arg))
        (end (cadr arg)))
    (cond
     ((evil-lispy--looking-at-empty-p) arg)
     ((evil-lispy--balanced-p start end) arg)
     (t nil))))

(defun evil-lispy--delete-additional (&rest arg)
  "Given an Evil operator ARG, modify the range to include the empty list if
we are deleting an empty list."
  (let* ((arg (car arg))
         (start (car arg))
         (end (cadr arg)))
    (cond
     ((evil-lispy--looking-at-empty-p)
      (progn
        (setcar (nthcdr 1 arg) (1+ end))
        arg))
     ((evil-lispy--inside-empty-p)
      (progn
        (setcar (nthcdr 0 arg) (1- start))
        arg))
     (t arg))))


;; ——— x key ———————————————————————————————————————————————————————————————————
(advice-add 'evil-delete-char :before-while #'evil-lispy--check-unbalanced-op-p)
(advice-add 'evil-delete-char :filter-args #'evil-lispy--delete-additional)

;; ——— X key ———————————————————————————————————————————————————————————————————
(advice-add 'evil-delete-backward-char :before-while #'evil-lispy--check-unbalanced-op-p)
(advice-add 'evil-delete-backward-char :filter-args #'evil-lispy--delete-additional)

;; ——— d key ———————————————————————————————————————————————————————————————————
(advice-add 'evil-delete :before-while #'evil-lispy--check-unbalanced-op-p)

;; ——— y key ———————————————————————————————————————————————————————————————————
(advice-add 'evil-yank :before-while #'evil-lispy--check-unbalanced-op-p)


;; ——— Text objects ————————————————————————————————————————————————————————————

(defun evil-lispy--bounds ()
  "Returns the current bounds."
  (or (lispy--bounds-comment)
      (lispy--bounds-string)
      (lispy--bounds-list)))

(evil-define-text-object evil-lispy--outer-form-object (&optional count beg end type)
  (let ((bounds (evil-lispy--bounds)))
    (when bounds
      (evil-range (car bounds) (cdr bounds)))))

(evil-define-text-object evil-lispy--inner-form-object (&optional count beg end type)
  (let ((bounds (evil-lispy--bounds)))
    (when bounds
      (evil-range (1+ (car bounds)) (1- (cdr bounds))))))


;; ——— Editing functions ———————————————————————————————————————————————————————

(defun evil-lispy--end-insert ()
  "Moves to the end of the form and enters insert mode."
  (interactive)
  (lispy-out-forward 1)
  (backward-char 1)
  (evil-insert-state))

(defun evil-lispy--beg-insert ()
  "Moves to the beginning of the form and enters insert mode.  Inserts
  an additional space."
  (interactive)
  (lispy-out-backward 1)
  (evil-forward-char 1 nil t)
  (insert-char ?\s)
  (evil-backward-char 1 nil t)
  (evil-insert-state))


;; ——— Non-special map —————————————————————————————————————————————————————————
(let ((map evil-lispy-mode-map))

  (define-key evil-inner-text-objects-map "f" 'evil-lispy--inner-form-object)
  (define-key evil-outer-text-objects-map "f" 'evil-lispy--outer-form-object)

  (local-unset-key (kbd ">"))
  (local-unset-key (kbd "<"))

  ;; ——— Entering Lispy state ——————————————————————————————————————————————————
  (evil-define-key 'normal map
    ")"  'evil-lispy--enter-state-right
    "("  'evil-lispy--enter-state-left
    "gv" 'evil-lispy--enter-state-with-symbol-marked
    "<<" 'evil-lispy--enter-state-at-beginning-of-defun
    ">>" 'evil-lispy--enter-state-at-end-of-defun)

  ;; ——— Insert mode maps ——————————————————————————————————————————————————————
  (evil-define-key 'insert map
    "(" 'lispy-parens
    ")" 'lispy-out-forward-nostring
    "[" 'lispy-brackets
    "]" 'lispy-out-forward-nostring
    "{" 'lispy-braces
    "}" 'lispy-out-forward-nostring
    "\"" 'lispy-quotes
    (kbd "DEL") 'lispy-delete-backward
    (kbd "C-1") 'lispy-describe-inline
    (kbd "C-2") 'lispy-arglist-inline)

  ;; ——— Normal mode maps ——————————————————————————————————————————————————————
  (evil-define-key 'normal map
    "C" (kbd "cif")
    "Y" (kbd "yaf")
    "D" 'lispy-kill
    ">i" 'evil-lispy--end-insert
    "<i" 'evil-lispy--beg-insert
    (kbd "C-1") 'lispy-describe-inline
    (kbd "C-2") 'lispy-arglist-inline))


;; ——— Special map —————————————————————————————————————————————————————————————
(setq evil-lispy-state-map (copy-keymap lispy-mode-map))
(define-key evil-lispy-state-map (kbd "<escape>") 'evil-lispy--exit-state)

(provide 'evil-lispy)

;;; evil-lispy.el ends here
