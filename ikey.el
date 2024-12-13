;;; ikey.el --- Enhanced key description with function name insertion -*- lexical-binding: t -*-

;; Copyright (C) 2024 Laluxx

;; Author: Laluxx
;; Version: 3.2.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, help
;; URL: https://github.com/laluxx/ikey

;;; Commentary:

;; [x] DONE Remember the last state of ?
;; [ ] TODO Simple theme options (do it in `ilib' so every "iprogram" obey)
;; [ ] TODO Fall back on `help' if `helpful' is not installed
;; [ ] TODO Apropos integretion
;; [ ] TODO Make the `ilib' package to make programs
;;     with a similar interface to this one

;; Enhanced key description interface with documentation and insertion functionality.
;; Shows formatted key bindings in the minibuffer and can insert function names at point.
;; Handles lambda functions gracefully both in display and insertion.

;;; Code:

(defgroup ikey nil
  "Enhanced key description with function name insertion."
  :group 'help
  :prefix "ikey-")

(defcustom ikey-always-insert nil
  "If non-nil, always insert function names instead of showing them first."
  :type 'boolean
  :group 'ikey)

(defcustom ikey-max-minibuffer-width 120
  "Maximum width for ikey messages in the minibuffer."
  :type 'integer
  :group 'ikey)

(defvar ikey--help-active nil
  "Whether the help view is currently active.")

(defvar ikey--actions
  '((?i . ("insert" . ikey--insert))
    (?d . ("describe" . describe-function))
    (?g . ("goto definition" . ikey--goto-definition))
    (?s . ("view source" . ikey--view-source)))
  "Mapping of keys to actions and their descriptions.")

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
  "Return a formatted arrow."
  (propertize " ⟶ " 'face '(:inherit font-lock-comment-face :weight bold)))

(defun ikey--parse-lambda-body (lambda-str)
  "Parse the lambda function body from LAMBDA-STR."
  (with-temp-buffer
    (insert lambda-str)
    (goto-char (point-min))
    (when (re-search-forward "#\\[nil \\((.*?)\\)" nil t)
      (let ((body (match-string 1)))
        (condition-case nil
            (read (current-buffer))
          (error body))))))

(defun ikey--format-lambda-for-display (fn)
  "Format lambda function FN for display in minibuffer."
  (let* ((lambda-str (prin1-to-string fn))
         (body (ikey--parse-lambda-body lambda-str)))
    (when body
      (let ((formatted-str
             (format "(λ %s)"
                     (replace-regexp-in-string
                      "\\s-+" " "
                      (prin1-to-string body)))))
        (propertize formatted-str 'face 'font-lock-function-name-face)))))

(defun ikey--format-lambda-for-insertion (fn)
  "Format lambda function FN for insertion at point."
  (let* ((lambda-str (prin1-to-string fn))
         (body (ikey--parse-lambda-body lambda-str)))
    (if body
        (format "(lambda () (interactive) %s)"
                (replace-regexp-in-string
                 "\\s-+" " "
                 (prin1-to-string body)))
      lambda-str)))

(defun ikey--format-function-name (fn &optional for-insertion)
  "Format function name FN with appropriate face.
If FOR-INSERTION is non-nil, format for insertion rather than display."
  (cond
   ((and (functionp fn) (not (symbolp fn)))
    (if for-insertion
        (ikey--format-lambda-for-insertion fn)
      (ikey--format-lambda-for-display fn)))
   ((symbolp fn)
    (if for-insertion
        (symbol-name fn)
      (propertize (symbol-name fn) 'face 'font-lock-function-name-face)))
   (t (format "%s" fn))))

(defun ikey--goto-definition (fn)
  "Go to the definition of FN, with special handling for lambdas."
  (if (and (functionp fn) (not (symbolp fn)))
      (message "Cannot navigate to anonymous lambda definition")
    (condition-case nil
        (find-function fn)
      (error (message "Cannot find function definition")))))

(defun ikey--view-source (fn)
  "View source of FN in another window."
  (if (and (functionp fn) (not (symbolp fn)))
      (message "Cannot view source of anonymous lambda definition")
    (condition-case nil
        (find-function-other-window fn)
      (error (message "Cannot find function source")))))

(defun ikey--truncate-message (message)
  "Truncate MESSAGE to fit in the minibuffer."
  (if (> (length message) ikey-max-minibuffer-width)
      (concat (substring message 0 (- ikey-max-minibuffer-width 3)) "...")
    message))

(defun ikey--format-action-prompt ()
  "Format the action prompt based on the current help view state."
  (let* ((actions (mapcar #'car ikey--actions))
         (help-key (propertize "?" 'face (if ikey--help-active
                                             'font-lock-keyword-face
                                           'font-lock-comment-face)))
         (action-string
          (if ikey--help-active
              (mapconcat
               (lambda (action)
                 (let ((key (car action))
                       (desc (cadr action)))
                   (concat
                    (propertize (char-to-string key) 'face 'font-lock-constant-face)
                    ":"
                    (propertize desc 'face 'font-lock-function-name-face))))
               ikey--actions "  ")
            (concat (propertize (mapconcat #'char-to-string actions "") 
                                'face 'font-lock-constant-face)))))
    (concat " [" action-string "] " help-key)))

(defun ikey--insert (fn)
  "Insert function FN at point with proper formatting."
  (insert (ikey--format-function-name fn t)))

(defun ikey--toggle-help ()
  "Toggle the expanded help view."
  (setq ikey--help-active (not ikey--help-active)))

;;;###autoload
(defun ikey-describe-key (key)
  "Show enhanced description of key binding."
  (interactive "kPress key: ")
  (let* ((function (key-binding key))
         (message-log-max nil)
         (current-key-sequence (key-description key))
         (key-desc (ikey--format-key-description current-key-sequence))
         (fn-desc (ikey--format-function-name function))
         (arrow (ikey--format-arrow))
         (prompt (ikey--format-action-prompt))
         (message (concat key-desc arrow fn-desc prompt)))
    
    (message "%s" (ikey--truncate-message message))
    
    (let ((action (read-char-exclusive)))
      (cond
       ((eq action ??)
        (ikey--toggle-help)
        (ikey-describe-key key))
       ((alist-get action ikey--actions)
        (funcall (cdr (alist-get action ikey--actions)) function))
       (t 
        ;; Fall through - execute the original key binding
        (setq unread-command-events (list (cons t action))))))))

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
