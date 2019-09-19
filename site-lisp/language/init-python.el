;; init-python.el --- python-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; python-setting.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package python
  :ensure t
  :defer t
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (setq python-shell-interpreter "python3")

  ;; Anaconda mode
  (unless iorest-lsp
    (use-package anaconda-mode
      :ensure t
      :init
      :diminish anaconda-mode "CoDA"
      :config
      (add-hook 'python-mode-hook 'anaconda-mode)
      (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
      (use-package company-anaconda
        :ensure t
        :init
        :config (add-hook 'python-mode-hook
                          (lambda () (local-push-company-backend 'company-anaconda)))))))

(provide 'init-python)
;;; init-python.el ends here
