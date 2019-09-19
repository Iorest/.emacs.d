;; init-sh.el --- sh-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; sh-setting.
;;

;;; Code:


(use-package multi-term
  :ensure t
  :init
  :bind (("C-x t" . multi-term))
  :config (setq multi-term-program "/bin/zsh"
                multi-term-buffer-name "MTerm"
                multi-term-scroll-show-maximum-output 0
                term-unbind-key-list '("C-h" "M-x" "C-x" "C-z")
                term-term-name "xterm-256color"))

(use-package eshell-git-prompt
  :ensure t
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))


(use-package vterm
  :ensure t
  :defer t)

(setq system-uses-terminfo nil)


(provide 'init-sh)
;;; init-sh.el ends here
