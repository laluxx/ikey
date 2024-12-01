;;; ikey.el --- Enhanced key description with function name insertion -*- lexical-binding: t -*-

;; Copyright (C) 2024 Laluxx

;; Author: Laluxx
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, help
;; URL: https://github.com/laluxx/ikey

;;; Commentary:

;; Enhanced key description interface with documentation and insertion functionality.
;; It shows formatted key bindings in the minibuffer and can insert function names at point.
;; - Customize `ikey-always-insert' to control insertion behavior

;; TODO Handle lambdas better remove and add whitespaces where needed
;; (λ   (  if (use-region-p) (call-interactively (quote laluxx/generate-face-colors)) (insert "j")  ) )

;;; Code:

(require 'rainbow-delimiters nil t)

(defgroup ikey nil
  "Enhanced key description with function name insertion."
  :group 'help
  :prefix "ikey-")

(defcustom ikey-always-insert nil
  "If non-nil, always insert function names instead of showing them first."
  :type 'boolean
  :group 'ikey)

(defvar ikey--last-described-key nil
  "Store the last key described.")

(defvar ikey--last-described-function nil
  "Store the last function described.")

(defun ikey--format-namespace (name)
  "Format namespace part of NAME with proper face."
  (let ((parts (split-string name "/")))
    (if (= (length parts) 2)
        (concat
         (propertize (car parts) 'face 'default)
         (propertize "/" 'face 'default)
         (propertize (cadr parts) 'face 'font-lock-function-name-face))
      (propertize name 'face 'font-lock-function-name-face))))

(defun ikey--format-string (str)
  "Format STR as a string with proper face."
  (propertize (format "\"%s\"" str) 'face 'font-lock-string-face))

(defun ikey--format-symbol (sym)
  "Format SYM with proper face based on type."
  (let ((name (symbol-name sym)))
    (cond
     ((string= name "nil") (propertize name 'face 'font-lock-comment-face))
     ((string= name "if") (propertize name 'face 'font-lock-keyword-face))
     ((string= name "interactive") (propertize name 'face 'font-lock-keyword-face))
     ((string-match-p "/" name) (ikey--format-namespace name))
     (t (propertize name 'face 'font-lock-function-name-face)))))

(defun ikey--format-sexp (sexp indent)
  "Format SEXP with proper faces and INDENT level."
  (cond
   ((stringp sexp) (ikey--format-string sexp))
   ((symbolp sexp) (ikey--format-symbol sexp))
   ((listp sexp)
    (let ((op (propertize "(" 'face 'rainbow-delimiters-depth-2-face))
          (cp (propertize ")" 'face 'rainbow-delimiters-depth-2-face)))
      (if (null sexp)
          (propertize "nil" 'face 'font-lock-comment-face)
        (let* ((items (mapcar (lambda (item) (ikey--format-sexp item (+ indent 2))) sexp))
               (items-str (string-join items " ")))
          (if (< (length items-str) 60)
              (concat op items-str cp)
            (concat op "\n"
                    (make-string (1+ indent) ? ) items-str "\n"
                    (make-string indent ? ) cp))))))
   (t (format "%s" sexp))))

(defun ikey--format-lambda (fn)
  "Format lambda function FN with proper syntax highlighting."
  (let ((lambda-str (prin1-to-string fn)))
    (when (string-match "#\\[nil \\((.*)\\)" lambda-str)
      (let* ((body-str (match-string 1 lambda-str))
             (body-sexp (read body-str))
             ;; Delete all newlines and extra spaces
             (formatted-body (replace-regexp-in-string
                              "[\n\r]\\|[[:blank:]]+" " "
                              (substring (ikey--format-sexp body-sexp 0) 1))))
        (concat
         (propertize "(" 'face 'rainbow-delimiters-depth-2-face)
         (propertize "λ" 'face 'font-lock-keyword-face) " "
         formatted-body)))))

(defun ikey--format-key-modifiers (key)
  "Format KEY with bold modifiers (C, M, S)."
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "\\(C\\|M\\|S\\)-"
     (lambda (match)
       (propertize match 'face '(:inherit font-lock-constant-face :weight bold)))
     key)))

(defun ikey--format-key-description (key)
  "Format KEY description with font-lock-constant-face and bold modifiers."
  (let ((formatted-key (ikey--format-key-modifiers key)))
    (propertize formatted-key 'face 'font-lock-constant-face)))

(defun ikey--format-arrow ()
  "Return a beautifully formatted arrow."
  (propertize " ⟶ " 'face '(:inherit font-lock-comment-face :weight bold)))

(defun ikey--format-function-name (fn)
  "Format function name FN with appropriate face and decoration."
  (cond
   ((and (functionp fn) (string-match-p "^#\\[" (prin1-to-string fn)))
    (ikey--format-lambda fn))
   ((symbolp fn)
    (let ((open-paren (propertize "(" 'face 'rainbow-delimiters-depth-2-face))
          (close-paren (propertize ")" 'face 'rainbow-delimiters-depth-2-face)))
      (concat open-paren (ikey--format-symbol fn) close-paren)))
   (t (format "%s" fn))))

;;;###autoload
(defun ikey-describe-key (key)
  "Show enhanced description of key binding.
If `ikey-always-insert' is non-nil, always insert the function name.
Otherwise, show description first and insert on second press of the same key."
  (interactive "kPress key: ")
  (let* ((function (key-binding key))
         (message-log-max nil)
         (current-key-sequence (key-description key)))
    (if (or ikey-always-insert
            (and (equal current-key-sequence ikey--last-described-key)
                 ikey--last-described-function))
        (progn
          (if (and (functionp function) (string-match-p "^#\\[" (prin1-to-string function)))
              (insert (ikey--format-lambda function))
            (if (symbolp function)
                (insert (symbol-name function))
              (insert (prin1-to-string function))))
          (setq ikey--last-described-key nil
                ikey--last-described-function nil))
      (progn
        (message "%s%s%s"
                 (ikey--format-key-description current-key-sequence)
                 (ikey--format-arrow)
                 (ikey--format-function-name function))
        (setq ikey--last-described-key current-key-sequence
              ikey--last-described-function function)))))

;;;###autoload
(define-minor-mode ikey-mode
  "Toggle ikey mode.
When enabled, replaces the default C-h c binding with enhanced key description."
  :global t
  :lighter " ikey"
  (if ikey-mode
      (global-set-key (kbd "C-h c") #'ikey-describe-key)
    (global-set-key (kbd "C-h c") #'describe-key-briefly)))

(provide 'ikey)

;;; ikey.el ends here
