;;; clean-aindent.el

;; This is free and unencumbered software released into the public domain.
;; (http://unlicense.org)

;; Author: petar marinov <efravia@gmail.com>
;; Created: 2013-08-17
;; Last: 2014-03-07
;; Version: 1.1.0
;; License: C0 (public domain)
;; URL: https://github.com/pmarinov/clean-aindent
;; Doc URL: http://www.emacswiki.org/emacs/CleanAutoIndent
;; Keywords: indentation whitespace

;; This file is not part of GNU Emacs.

;;; Commentary:

;; 1. Extension of `newline-and-indent' that keeps track of the last
;; auto-indent operation and, if it is abandoned, would take care to
;; trim down the abandoned white space characters. It binds
;; `newline-and-indent' to RET.
;;
;; 2. Backspace Unindent. Extension of M-backspace.
;; When cursor is in the indentation space of a line, or at the first
;; character and you press M-backspace it will move the entire line to
;; be aligned to the line above or any other that is with indentation
;; smaller than the current.
;;

;;; Change Log:
;;
;; 2014-03-07, pmarinov, v1.1.0
;;     Added: Simple auto indent feature. Configurable via M-x customize.
;;
;; 2013-08-31, pmarinov, v1.0.0
;;     First implementation.
;;


;;
;; Implementation of Clean auto indent
;;

(defcustom clean-aindent_is-simple-indent nil
  "Indentation should use the smart language mode or simple mode"
  :tag "Clean auto indent is in simple mode"
  :group 'indent
  :type 'boolean)

(defun clean-aindent_get-indent-len()
  "Computes the length of the indentation at 'last-indent."
  (let ((len 0))
    (save-excursion
      (goto-char last-indent)
      (end-of-line)
      (set 'len (point))
      (beginning-of-line)
      ;; (message "len %d" (- len (point)))
      (set 'len (- len (point))))))

(defun clean-aindent_abandonedp()
  "Checks if last auto-indent position was abandoned.
Verifies if cursor moved away and that the indent was left
unaltered."
  (if (not last-indent)
    nil
    ;; (message "last-indent %d point %d" last-indent (point))
    (if (= last-indent (point))
      nil
      ;; Checking for indent length is to detect if something was not
      ;; typed to alter it. Altered indent shouldn't be trimmed.
      (if (not (= last-indent-len (clean-aindent_get-indent-len)))
        nil
        t))))

(defun clean-aindent_trim-last-point()
  "Deletes the whitespaces inserted at last indentation"
  (save-excursion
    (goto-char last-indent)
    ; Select the entire line
    (let ((s 0)
         (e 0))
      (beginning-of-line)
      (set 's (point))
      (end-of-line)
      (set 'e (point))
      (delete-trailing-whitespace s e)
      (end-of-line)
      (message "auto trimmed %d chars" (- e (point))))))

(defun clean-aindent_check-last-point()
  "Checks if last pos of auto-indent was abandoned and deletes it"
  (if (clean-aindent_abandonedp)
    (clean-aindent_trim-last-point))
  ;; Once we leave the position, clean the indent bookmark
  (if
    (and
      last-indent
      (not (= last-indent (point))))
    (set 'last-indent nil)))

(defun clean-aindent_find-indent()
  "Searches lines backward, finds first non-blank. Returns
indentation value"
  (save-excursion
    ;; Walk lines backward, until first non-blank
    (bsunindent_prev-line)
    ;; Return indentation of that line
    (current-indentation)))

(defun clean-aindent_simple-newline-and-indent()
  "Simple auto indent. Indentation is based only on previous line
indentation, regardless of language settings."
  ;; First remove any trailing spaces from the current line
  (save-excursion
    (let ((s 0)
         (e 0))
      (beginning-of-line)
      (set 's (point))
      (end-of-line)
      (set 'e (point))
      (delete-trailing-whitespace s e)
      (end-of-line)))
  ;; Insert a new line and indent
  (newline)
  (indent-to (clean-aindent_find-indent) 0))

(defun clean-aindent()
  "Invokes newline-and-indent().
Key `RET' is bound to clean-aindent. It does auto-indent via
newline-and-indent(). Keeps track of the last indent so that it can
be deleted in case it was abandoned"
  (interactive)
  (clean-aindent_check-last-point)  ; In case of consequtive aindent calls
  (if clean-aindent_is-simple-indent
    (clean-aindent_simple-newline-and-indent)
    (newline-and-indent))
  (set 'last-indent nil)
  (make-local-variable 'last-indent)
  (set 'last-indent (point))
  (set 'last-indent-len (clean-aindent_get-indent-len))
  (make-local-variable 'last-indent))


;;
;; Backspace-unindent implementation functions
;;

(defun bsunindent_get-line-len()
  "Computes length of current line"
  (save-excursion
    (beginning-of-line nil)
      (let ((pos (point)))
        (end-of-line nil)
        (- (point) pos))))

(defun bsunindent_line-emptyp()
  "Checks if line is empty"
  (save-excursion
    (beginning-of-line nil)
    (if (= (point) 1)
      nil
      (= (bsunindent_get-line-len) 0))))

(defun bsunindent_prev-line()
  "Move cursor to previous line, skip empty lines"
  (let ((c (point)))
    (while
      (and
        (= 0 (forward-line -1))
        (bsunindent_line-emptyp)))
    ;; return 't if we moved, nil if already beginning of buffer
    (not (= c (point)))))

(defun bsunindent_find-indent(start)
  "Searches lines backward, finds the one that is indented less
than certain indentation t"
  (save-excursion
    (let (c)
      (while
        (and
          (set 'c (current-indentation))
          (> c 0)
          ;; Find an indent smaller than _start_
          (<= start c)
          ;; Walk lines backward
          (bsunindent_prev-line)))
      ;; _c_ is the computed unindent size
      c)))

(defun bsunindent_inside-indentp()
  "Returns true if cursor is in the leading whitespace or first
non-blank character of a line"
  (save-excursion
    (let ((pos (point)))
      (beginning-of-line nil)
      (skip-chars-forward " \t")
      (if (<= pos (point))
        t
        nil))))

(defun bsunindent_line-point()
  "Get (point) at the beginning of the current line"
  (save-excursion
    (beginning-of-line)
    (point)))

(defun bsunindent(arg)
  "Unindents.
Bound to `M-backspace' key. Searches lines backward, finds the one that
is indented less than the current one. Unindents current line to
align with that smaller indentation"
  (interactive "p")
  (if (not (bsunindent_inside-indentp))
    (kill-word (- arg))  ;; Original "C-backspace" key function
    ;; Else, cursor inside indent space, do unindent
    (let*
        ((ln (bsunindent_line-point))
        (c (current-indentation))
        (n (bsunindent_find-indent c))  ;; compute new indent
        (s (+ ln n)))  ;; start of region to delete
      (if (not (= s c))
        (progn
          ;; (message "new unindent %d" n)
          ;; Delete characters between s to c
          (goto-char (+ ln c))
          (backward-delete-char-untabify (- c n)))))))


;;
;; Init/Done section
;;

(defun clean-aindent-init()
  "Init clean-indent()
Hook clean-indent() to 'return' key, and unindent to M-backspace"
  (interactive)
  (set 'last-indent nil)
  (set 'last-indent-length 0)
  (add-hook 'post-command-hook 'clean-aindent_check-last-point)
  (global-set-key (kbd "RET") 'clean-aindent)
  (global-set-key (kbd "M-DEL") 'bsunindent))

(defun clean-aindent-done()
  "Unhook clean-inindent()"
  (interactive)
  (set 'last-indent nil)
  (set 'last-indent-length 0)
  (remove-hook 'post-command-hook 'clean-aindent_trim-last-point)
  (global-set-key (kbd "RET") 'newline)
  (global-set-key (kbd "M-DEL") 'backward-kill-word))

(provide 'clean-aindent)
(clean-aindent-init)
