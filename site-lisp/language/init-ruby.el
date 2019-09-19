;; init-ruby.el --- ruby-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; ruby-setting.
;;

;;; Code:
;; Gem packages:
;; sudo gem install robe-server
;; sudo gem install pry
;; sudo gem install pry-doc

(eval-when-compile
  (require 'init-custom))

(use-package ruby-mode
  :ensure t
  :defer t
  :config
  (unless iorest-lsp
    (progn
      ;; Code navigation, documentation lookup and completion for Ruby
      (use-package robe
        :ensure t
        :diminish robe-mode "Robe"
        :config
        (add-hook 'ruby-mode-hook 'robe-mode)
        (add-hook 'ruby-mode-hook
                  (lambda () (local-push-company-backend 'company-robe))))))

  ;; Ruby refactoring helpers
  (use-package ruby-refactor
    :ensure t
    :diminish ruby-refactor-mode
    :hook (ruby-mode . ruby-refactor-mode-launch))

  ;; Run a Ruby process in a buffer
  (use-package inf-ruby
    :ensure t
    :hook ((ruby-mode . inf-ruby-minor-mode)
           (compilation-filter . inf-ruby-auto-enter)))

  ;; Rubocop
  ;; Install: gem install rubocop
  (use-package rubocop
    :ensure t
    :diminish rubocop-mode
    :hook (ruby-mode . rubocop-mode))

  (use-package bundler
    :ensure t
    :defer t)

  ;; RSpec
  (use-package rspec-mode
    :ensure t
    :diminish rspec-mode
    :commands rspec-install-snippets
    :hook (dired-mode . rspec-dired-mode)
    :config (with-eval-after-load 'yasnippet
              (rspec-install-snippets)))

  ;; Coverage for SimpleCov
  (use-package coverage
    :ensure t
    :defer t)

  ;; Yet Another RI interface for Emacs
  (use-package yari
    :ensure t
    :bind (:map ruby-mode-map ([f1] . yari)))

  ;; Ruby YARD comments
  (use-package yard-mode
    :ensure t
    :diminish yard-mode
    :hook (ruby-mode . yard-mode)))

(provide 'init-ruby)
;;; init-ruby.el ends here
