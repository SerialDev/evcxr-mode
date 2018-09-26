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

(defgroup evcxr nil
  "Rust interactive mode"
  :link '(url-link "https://github.com/serialdev/evcxr-mode")
  :prefix "evcxr-"
  :group 'languages)

(defcustom evcxr-program (executable-find "evcxr")
  "Program invoked by `evcxr'."
  :group 'evcxr
  :type 'file)

(defcustom evcxr-args nil
  "Command line arguments for `evcxr-program'."
  :group 'evcxr
  :type '(repeat string))

(defcustom evcxr-buffer "*Evcxr*"
  "Name of buffer for evcxr."
  :group 'evcxr
  :type 'string)

(defcustom evcxr-startup-file (locate-user-emacs-file "init_evcxr.rs"
                                                      ".emacs-evcxr.rs")
  "Startup file for `evcxr'."
  :group 'evcxr
  :type 'file)

(defcustom evcxr-prompt-regexp "^# "
  "Regexp to match prompts for evcxr."
  :group 'evcxr
  :type 'regexp)

(defcustom evcxr-prompt-regexp "^ "
  "Regexp to match prompts for evcxr."
  :group 'evcxr
  :type 'regexp)

(defcustom evcxr-prompt-read-only t
  "Make the prompt read only.
See `comint-prompt-read-only' for details."
  :group 'evcxr
  :type 'boolean)

(defun evcxr-is-running? ()
  "Return non-nil if evcxr is running."
  (comint-check-proc evcxr-buffer))
(defalias 'evcxr-is-running-p #'evcxr-is-running?)


;;;###autoload
(defun evcxr (&optional arg)
  "Run evcxr.
Unless ARG is non-nil, switch to the buffer."
  (interactive "P")
  (let ((buffer (get-buffer-create evcxr-buffer)))
    (unless arg
      (pop-to-buffer buffer))
    (unless (evcxr-is-running?)
      (with-current-buffer buffer
        (evcxr-startup)
        (evcxr-mode))
      (pop-to-buffer buffer))
    buffer))


;;;###autoload
(defalias 'run-rust #'evcxr)
;;;###autoload
(defalias 'inferior-rust #'evcxr)

(defun evcxr-startup ()
  "Start evcxr."
  (comint-exec evcxr-buffer
               "Evcxr"
               evcxr-program
               (when (file-exists-p evcxr-startup-file)
                 evcxr-startup-file)
               evcxr-args))


(define-derived-mode evcxr-mode comint-mode "Evcxr"
  "Major mode for evcxr."
  (setq comint-prompt-regexp evcxr-prompt-regexp
        comint-use-prompt-regexp t)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comint-prompt-read-only evcxr-prompt-read-only)
  )



(defun evcxr-eval-region (begin end)
  "Evaluate region between BEGIN and END."
  (interactive "r")
  (evcxr t)
  (comint-send-string evcxr-buffer
    ;; (replace-regexp-in-string "\n\s\+" " "(buffer-substring-no-properties begin end)))
    (replace-regexp-in-string "[]\n?[[:space:]]+" " "(buffer-substring-no-properties begin end)))
  (comint-send-string evcxr-buffer "\n"))


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


(provide 'evcxr)

;;; evcxr.el ends here
