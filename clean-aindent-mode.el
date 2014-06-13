;;; clean-aindent-mode.el

;; This is free and unencumbered software released into the public domain.
;; (http://unlicense.org)

;; Author: peter marinov <efravia@gmail.com>
;; Created: 2013-08-17
;; Last: 2014-06-12
;; Version: 1.3.0
;; License: C0 (public domain)
;; URL: https://github.com/pmarinov/clean-aindent-mode
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
;; 2014-06-01, pmarinov, v1.3.0
;;     - Activate via a minor mode
;;     - Further cleanup of the name space
;;
;; 2014-05-27, pmarinov, v1.2.0
;;     Changed: Move all function under the same namespace (function prefix)
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

(defcustom clean-aindent--is-simple-indent nil
  "Indentation should use the smart language mode or simple mode"
  :tag "Clean auto indent is in simple mode"
  :group 'indent
  :type 'boolean)

(defun clean-aindent--get-indent-len()
  "Computes the length of the indentation at 'clean-aindent--last-indent."
  (let ((len 0))
    (save-excursion
      (goto-char clean-aindent--last-indent)
      (end-of-line)
      (set 'len (point))
      (beginning-of-line)
      ;; (message "len %d" (- len (point)))
      (set 'len (- len (point))))))

(defun clean-aindent--abandonedp()
  "Checks if last auto-indent position was abandoned.
Verifies if cursor moved away and that the indent was left
unaltered."
  (if (not clean-aindent--last-indent)
    nil
    ;; (message "clean-aindent--last-indent %d point %d" clean-aindent--last-indent (point))
    (if (= clean-aindent--last-indent (point))
      nil
      ;; Checking for indent length is to detect if something was not
      ;; typed to alter it. Altered indent shouldn't be trimmed.
      (if (not (= clean-aindent--last-indent-len (clean-aindent--get-indent-len)))
        nil
        t))))

(defun clean-aindent--trim-last-point()
  "Deletes the whitespaces inserted at last indentation"
  (save-excursion
    (goto-char clean-aindent--last-indent)
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

(defun clean-aindent--check-last-point()
  "Checks if last pos of auto-indent was abandoned and deletes it"
  (if (clean-aindent--abandonedp)
    (clean-aindent--trim-last-point))
  ;; Once we leave the position, clean the indent bookmark
  (if
    (and
      clean-aindent--last-indent
      (not (= clean-aindent--last-indent (point))))
    (set 'clean-aindent--last-indent nil)))

(defun clean-aindent--find-indent()
  "Searches lines backward, finds first non-blank. Returns
indentation value"
  (save-excursion
    ;; Walk lines backward, until first non-blank
    (clean-aindent--prev-line)
    ;; Return indentation of that line
    (current-indentation)))

(defun clean-aindent--simple-newline-and-indent()
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
  (indent-to (clean-aindent--find-indent) 0))

(defun clean-aindent()
  "Invokes newline-and-indent().
Key `RET' is bound to clean-aindent. It does auto-indent via
newline-and-indent(). Keeps track of the last indent so that it can
be deleted in case it was abandoned"
  (interactive)
  (if clean-aindent-mode
      (progn
      (clean-aindent--check-last-point)  ; In case of consequtive aindent calls
      (if clean-aindent--is-simple-indent
          (clean-aindent--simple-newline-and-indent)
        (newline-and-indent))
      (set 'clean-aindent--last-indent nil)
      (make-local-variable 'clean-aindent--last-indent)
      (set 'clean-aindent--last-indent (point))
      (set 'clean-aindent--last-indent-len (clean-aindent--get-indent-len))
      (make-local-variable 'clean-aindent--last-indent-len))
    ;; Mode is disabled, invoke default action
    (newline)))


;;
;; Backspace-unindent implementation functions
;;

(defun clean-aindent--get-line-len()
  "Computes length of current line"
  (save-excursion
    (beginning-of-line nil)
      (let ((pos (point)))
        (end-of-line nil)
        (- (point) pos))))

(defun clean-aindent--line-emptyp()
  "Checks if line is empty"
  (save-excursion
    (beginning-of-line nil)
    (if (= (point) 1)
      nil
      (= (clean-aindent--get-line-len) 0))))

(defun clean-aindent--prev-line()
  "Move cursor to previous line, skip empty lines"
  (let ((c (point)))
    (while
      (and
        (= 0 (forward-line -1))
        (clean-aindent--line-emptyp)))
    ;; return 't if we moved, nil if already beginning of buffer
    (not (= c (point)))))

(defun clean-aindent--find-u-indent(start)
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
          (clean-aindent--prev-line)))
      ;; _c_ is the computed unindent size
      c)))

(defun clean-aindent--inside-indentp()
  "Returns true if cursor is in the leading whitespace or first
non-blank character of a line"
  (save-excursion
    (let ((pos (point)))
      (beginning-of-line nil)
      (skip-chars-forward " \t")
      (if (<= pos (point))
        t
        nil))))

(defun clean-aindent--line-point()
  "Get (point) at the beginning of the current line"
  (save-excursion
    (beginning-of-line)
    (point)))

(defun clean-aindent--goto-column(col)
  "Moves the cursor to a certain column position.
Column position is different from char position because of TABs"
  (beginning-of-line nil)
  (while (< (current-column) col)
    (right-char)))

(defun clean-aindent--bsunindent(arg)
  "Unindents.
Bound to `M-backspace' key. Searches lines backward, finds the one that
is indented less than the current one. Unindents current line to
align with that smaller indentation"
  (interactive "p")
  (if clean-aindent-mode
      (progn
      (if (not (clean-aindent--inside-indentp))
        (kill-word (- arg))  ;; Original "C-backspace" key function
        ;; Else, cursor inside indent space, do unindent
        (let*
            ((ln (clean-aindent--line-point))
            (c (current-indentation))
            (n (clean-aindent--find-u-indent c))  ;; compute new indent
            (s (+ ln n)))  ;; start of region to delete
          (if (not (= s c))
            (progn
              ;; (message "new unindent %d" n)
              ;; Delete characters between s to c
              (clean-aindent--goto-column c)
              (backward-delete-char-untabify (- c n)))))))
    ;; Mode is disabled, invoke default action
    (backward-kill-word)))


;;
;; Initial setup
;;

(defvar clean-aindent--last-indent nil)
(defvar clean-aindent--last-indent-length 0)

(defun clean-aindent--post-command()
  "List of actions for `clean-aindent' at the end of each command."
  (if clean-aindent-mode
      (clean-aindent--check-last-point)))

;; The clean-aindent functions are hooked but dormant until
;; activated by a call to clean-aindent-mode
;;
;; Attching to "RET" via the usual minor-mode map assigns very high priority
;; that overrides desired functionality in, for example, minibuffer or dired.
(add-hook 'post-command-hook 'clean-aindent--post-command)
(global-set-key (kbd "RET") 'clean-aindent)
(global-set-key (kbd "M-DEL") 'clean-aindent--bsunindent)


;;;###autoload
(define-minor-mode clean-aindent-mode
  "Activates clean auto indent for key RET and
back-space unindent for M-DEL (meta-backspace).

clean-aindent mode is a global minor mode.

1. Extension of `newline-and-indent' that keeps track of the last
auto-indent operation and, if it is abandoned, would take care to
trim down the abandoned white space characters. It binds
`newline-and-indent' to RET.

2. Backspace Unindent. Extension of M-backspace.  When cursor is
in the indentation space of a line, or at the first character and
you press M-backspace it will move the entire line to be aligned
to the line above or any other that is with indentation smaller
than the current."
  ;; The modes sole purpose is to be activated/deactivated.
  :global t)

(provide 'clean-aindent-mode)
