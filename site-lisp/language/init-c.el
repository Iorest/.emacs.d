;; init-c.el --- c-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; c-setting.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package cc-mode
  :ensure t
  :defer t
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :hook (c-mode-common . (lambda ()
                           (c-set-style "bsd")
                           (setq tab-width 4)
                           (setq c-basic-offset 4)))
  :config)

(unless iorest-lsp
  (use-package irony
    :ensure t
    :defines (irony-mode-map irony-server-w32-pipe-buffer-size)
    :defer t
    :diminish irony-mode "Iron"
    :hook ((c-mode . irony-mode)
           (c++-mode . irony-mode)
           (objc-mode . irony-mode)
           (irony-mode . irony-cdb-autosetup-compile-options))
    :config
    ;; Windows performance tweaks
    (when (boundp 'w32-pipe-read-delay)
      (setq w32-pipe-read-delay 0))
    ;; Set the buffer size to 64K on Windows (from the original 4K)
    (when (boundp 'w32-pipe-buffer-size)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
    (use-package company-irony
      :ensure t
      :config)
    (use-package company-irony-c-headers
      :ensure t
      :config
      (add-hook 'irony-mode-hook
                (lambda () (local-push-company-backend '(company-irony-c-headers company-irony)))))

    (use-package company-c-headers
      :ensure t
      :config
      (add-hook 'irony-mode-hook
                (lambda () (local-push-company-backend 'company-c-headers))))

    (use-package irony-eldoc
      :ensure t
      :hook (irony-mode . irony-eldoc))
    ))

(use-package rmsbolt
  :ensure t
  :defer t)

(use-package nasm-mode
  :ensure t
  :defer t)

(use-package mips-mode
  :ensure t
  :defer t)

(use-package riscv-mode
  :ensure t
  :defer t)

(provide 'init-c)
;;; init-c.el ends here
