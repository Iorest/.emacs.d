;; init-elixir.el --- elixir-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; elixir-setting.
;;

;;; Code:

(use-package elixir-mode
  :ensure t
  :defer t
  :hook (elixir-mode . alchemist-mode)
  :config
  ;; (use-package flycheck-mix
  ;;   :ensure t
  ;;   :hook (elixir-mode . flycheck-mix-setup))
  ;; Alchemist offers integration with the Mix tool.
  (use-package alchemist
    :ensure t
    :diminish alchemist-mode "Aei"
    :commands alchemist-mode
    :init (setq alchemist-key-command-prefix (kbd "C-c C-a"))
    :config
    ;; Bind some Alchemist commands to more commonly used keys.
    (bind-keys :map alchemist-mode-map
               ("C-c C-c" . (lambda () (interactive)
                              (save-buffer)
                              (alchemist-iex-compile-this-buffer-and-go))))
    ))



(provide 'init-elixir)
;;; init-elixir.el ends here
