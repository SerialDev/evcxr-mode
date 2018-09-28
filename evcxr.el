;;; evcxr.el --- Evcxr minor mode for Rust Repl support

;; Copyright (C) 2018 Andres Mariscal

;; Author: Andres Mariscal <carlos.mariscal.melgar@gmail.com>
;; Created: 26 Sep 2018
;; Version: 0.0.1
;; Keywords: rust languages repl
;; URL: https://github.com/serialdev/evcxr-mode
;; Package-Requires: ((emacs "24.3", ))
;;; Commentary:
;; Rust Repl support through evcxr repl

(require 'comint)

(defun evcxr-is-running? ()
  "Return non-nil if evcxr is running."
  (comint-check-proc evcxr-shell-buffer-name))
(defalias 'evcxr-is-running-p #'evcxr-is-running?)


;;;###autoload
(defun evcxr (&optional arg)
  "Run evcxr.
Unless ARG is non-nil, switch to the buffer."
  (interactive "P")
  (let ((buffer (get-buffer-create evcxr-shell-buffer-name)))
    (unless arg
      (pop-to-buffer buffer))
    (unless (evcxr-is-running?)
      (with-current-buffer buffer
        (evcxr-startup)
        ;; (evcxr-mode))
        (inferior-evcxr-mode))
      (pop-to-buffer buffer))
    buffer))


;;;###autoload
(defalias 'run-rust #'evcxr)
;;;###autoload
(defalias 'inferior-rust #'evcxr)

(defun evcxr-startup ()
  "Start evcxr."
  (comint-exec evcxr-shell-buffer-name
               "Evcxr"
               evcxr-program
               (when (file-exists-p evcxr-startup-file)
                 evcxr-startup-file)
               evcxr-args))


;; (setq comint-prompt-regexp "\\^?\\[?\\[?[[:space:]]?\n?>?>[[:space:]]?")
;; (setq comint-prompt-regexp "Welcome to evcxr. For help, type :help\n>+ ")
;; (set (make-local-variable 'paragraph-start) comint-prompt-regexp)

(defun evcxr-eval-region (begin end)
  "Evaluate region between BEGIN and END."
  (interactive "r")
  (evcxr t)
  (comint-send-string evcxr-shell-buffer-name
    (replace-regexp-in-string "\n[[:space:]]?" " "(buffer-substring-no-properties begin end)))
  (comint-send-string evcxr-shell-buffer-name "\n"))

(defun evcxr-type-check ()
  (interactive)
  ;; (comint-send-string evcxr-shell-buffer-name (message "%s" (concat "let evcxrmodetype: () = " (thing-at-point 'symbol) ";")))
  (comint-send-string evcxr-shell-buffer-name (concat "let evcxrmodetype: () = " (thing-at-point 'symbol) ";"))
  (comint-send-string evcxr-shell-buffer-name "\n")
  )

(defun evcxr-eval-buffer ()
  "Evaluate complete buffer."
  (interactive)
  (evcxr-eval-region (point-min) (point-max)))

(defun evcxr-eval-line (&optional arg)
  "Evaluate current line.
If ARG is a positive prefix then evaluate ARG number of lines starting with the
current one."
  (interactive "P")
  (unless arg
    (setq arg 1))
  (when (> arg 0)
    (evcxr-eval-region
     (line-beginning-position)
     (line-end-position arg))))

(defvar evcxr-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ; (define-key map "\C-x\C-e" #'evcxr-eval-last-sexp)
    (define-key map "\C-c\C-c" #'evcxr-eval-buffer)
    (define-key map "\C-c\C-r" #'evcxr-eval-region)
    (define-key map "\C-c\C-l" #'evcxr-eval-line)
    (define-key map "\C-c\C-t" #'evcxr-type-check)
    (define-key map "\C-c\C-p" #'evcxr)
    map)
  "Mode map for `evcxr-minor-mode'.")

(defcustom evcxr-minor-mode-lighter " Evcxr"
  "Text displayed in the mode line (Lighter) if `evcxr-minor-mode' is active."
  :group 'evcxr
  :type 'string)

(easy-menu-define evcxr-minor-mode evcxr-minor-mode-map
  "Menu for Evcxr Minor Mode."
  '("Evcxr"
    ["Eval Buffer" evcxr-eval-buffer :help "Evaluate buffer with Evcxr"]
    ["Eval Region" evcxr-eval-region :help "Evaluate selected region with Evcxr"]
    ["Eval Line" evcxr-eval-line :help "Evaluate current line with Evcxr"]))


;;;###autoload
(define-minor-mode evcxr-minor-mode
  "Add keys and a menu to provide easy access to `evcxr' support.
Usage:
  (add-hook 'rust-mode-hook #'evcxr-minor-mode)"
  :group 'evcxr
  :lighter evcxr-minor-mode-lighter
  :keymap evcxr-minor-mode-map)

;;; Shell integration

(defcustom evcxr-shell-buffer-name "*Evcxr*"
  "Name of buffer for evcxr."
  :group 'evcxr
  :type 'string)

(defcustom evcxr-shell-interpreter "evcxr"
  "default repl for shell"
  :type 'string
  :group 'evcxr)

(defcustom evcxr-shell-internal-buffer-name "Evcxr Internal"
  "Default buffer name for the internal process"
  :type 'string
  :group 'python
  :safe 'stringp)


(defcustom evcxr-shell-prompt-regexp "\\^?\\[?\\[?[[:space:]]?\n?>?>[[:space:]]?"
  "Regexp to match prompts for evcxr.
   Matchint top\-level input prompt"
  :group 'evcxr
  :type 'regexp
  :safe 'stringp)

(defcustom evcxr-shell-prompt-block-regexp " "
  "Regular expression matching block input prompt"
  :type 'string
  :group 'evcxr
  :safe 'stringp)

(defcustom evcxr-shell-prompt-output-regexp ""
  "Regular Expression matching output prompt of evxcr"
  :type 'string
  :group 'evcxr
  :safe 'stringp)

(defcustom evcxr-shell-enable-font-lock t
  "Should syntax highlighting be enabled in the evcxr shell buffer?"
  :type 'boolean
  :group 'evcxr
  :safe 'booleanp)

;; (defcustom evcxr-shell-compilation-regexp-alist (("[[:space:]]\\^+?"))
;;   "Compilation regexp alist for inferior evcxr"
;;   :type '(alist string))

(defgroup evcxr nil
  "Rust interactive mode"
  :link '(url-link "https://github.com/serialdev/evcxr-mode")
  :prefix "evcxr"
  :group 'languages)

(defcustom evcxr-program (executable-find "evcxr")
  "Program invoked by `evcxr'."
  :group 'evcxr
  :type 'file)


(defcustom evcxr-args nil
  "Command line arguments for `evcxr-program'."
  :group 'evcxr
  :type '(repeat string))

(defcustom evcxr-startup-file (locate-user-emacs-file "init_evcxr.rs"
                                                      ".emacs-evcxr.rs")
  "Startup file for `evcxr'."
  :group 'evcxr
  :type 'file)


(defcustom evcxr-prompt-read-only t
  "Make the prompt read only.
See `comint-prompt-read-only' for details."
  :group 'evcxr
  :type 'boolean)

(defun evcxr-comint-output-filter-function (output)
  "Hook run after content is put into comint buffer.
   OUTPUT is a string with the contents of the buffer"
  (ansi-color-filter-apply output))

(define-derived-mode inferior-evcxr-mode comint-mode "Evcxr"
  (setq comint-prompt-regexp (format "^\\(?:%s\\|%s\\)"
				     evcxr-shell-prompt-regexp
				     evcxr-shell-prompt-block-regexp))
  (setq mode-line-process '(":%s"))
  (make-local-variable 'comint-output-filter-functions)
  (add-hook 'comint-output-filter-functions
	    'evcxr-comint-output-filter-function)
  ;; (set (make-local-variable 'compilation-error-regexp-alist)
  ;;      evcxr-shell-compilation-regexp-alist)
  (when evcxr-shell-enable-font-lock
    (set-syntax-table rust-mode-syntax-table)
    (set (make-local-variable 'font-lock-defaults)
	 '(rust-mode-font-lock-keywords nil nil nil nil))
    (set (make-local-variable 'syntax-propertize-function)
    	 (eval
    	  "Unfortunately eval is needed to make use of the dynamic value of comint-prompt-regexp"
    	  '(syntax-propertize-rules
    	    '(comint-prompt-regexp
    	       (0 (ignore
    		   (put-text-property
    		    comint-last-input-start end 'syntax-table
    		    python-shell-output-syntax-table)
    		   (font-lock-unfontify--region comint-last-input-start end))))
    	    )))
    (compilation-shell-minor-mode 1)))
  

;; (define-derived-mode evcxr-mode comint-mode "Evcxr"
;;   "Major mode for evcxr."
;;   (setq comint-process-echoes t)
;;   (setq comint-prompt-regexp "\\^?\\[?\\[?[[:space:]]?\n?>?>[[:space:]]?")
;;   (setq comint-prompt-regexp evcxr-shell-prompt-regexp)
;;   (setq comint-use-prompt-regexp t)
;;   (setq-local comment-start "[[:space:]]\\^+?")
;;   (setq-local comment-end "")
;;   (setq-local comint-prompt-read-only evcxr-prompt-read-only)
;;   )


(provide 'evcxr)

;;; evcxr.el ends here
