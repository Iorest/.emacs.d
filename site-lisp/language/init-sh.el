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
  :config (setq multi-term-program "/bin/bash"
                multi-term-buffer-name "MTerm"
                multi-term-scroll-show-maximum-output 0
                term-unbind-key-list '("C-h" "M-x" "C-x" "C-z")
                term-term-name "xterm-256color"))
(use-package emamux
  :ensure t
  :config)

(use-package eshell-git-prompt
  :ensure t
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))


(use-package vterm
  :ensure t
  :defer t)

(defun open-dir-in-iterm ()
  "Open the current directory of the buffer in iTerm."
  (interactive)
  (let* ((iterm-app-path "/Applications/iTerm.app")
         (iterm-path (if (file-directory-p iterm-app-path)
                         iterm-app-path)))
    (shell-command (concat "open -a " iterm-path " ."))))
(global-set-key (kbd "C-x C-w") 'open-dir-in-iterm)

(setq system-uses-terminfo nil)


(provide 'init-sh)
;;; init-sh.el ends here
