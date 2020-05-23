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
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  (use-package live-py-mode
    :ensure t)

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
