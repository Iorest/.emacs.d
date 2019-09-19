;; init-custom.el --- custom.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; custom-setting.
;;

;;; Code:

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *os-is-mac* (eq system-type 'darwin))
(defconst *os-is-gnu* (eq system-type 'gnu/linux))
(defconst *os-is-win* (eq system-type 'windows-nt))

(defcustom iorest-proxy "127.0.0.1:8087"
  "Set network proxy."
  :type 'string)

(defcustom iorest-lsp nil
  "Set language server."
  :type '(choice
          (const :tag "LSP Mode" 'lsp-mode)
          (const :tag "eglot" 'eglot)
          nil))

(defcustom iorest-package-archives 'melpa
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Melpa Mirror" melpa-mirror)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Netease" netease)
          (const :tag "Tuna" tuna)))


;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init-custom)

;;; init-custom.el ends here
