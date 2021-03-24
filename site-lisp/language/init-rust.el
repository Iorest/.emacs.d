;; init-rust.el --- rust-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; rust-setting.
;;

;;; Code:
;;
;; rustup toolchain add nightly
;; cargo +nightly install racer
;; rustup component add rustfmt
;; rustup component add rust-src

(eval-when-compile
  (require 'init-custom))

(use-package rust-mode
  :ensure t
  :defer t
  :init
  :config
  (use-package cargo
    :ensure t
    :diminish cargo-minor-mode
    :init
    :hook (rust-mode . cargo-minor-mode)
    :config
    (setq compilation-filter-hook
          (append compilation-filter-hook '(cargo-process--add-errno-buttons))))
  (use-package rust-playground
    :ensure t
    :defer t)
  (unless iorest-lsp
    (use-package racer
      :ensure t
      :diminish racer-mode "Race"
      :init
      (setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))
      ;; (setq racer-rust-src-path (concat (getenv "HOME") "/.rust/src"))
      :hook
      (rust-mode . racer-mode)
      (racer-mode . eldoc-mode))
    (use-package flycheck-rust
      :ensure t
      :after racer
      :hook (racer-mode . flycheck-rust-setup))))

(provide 'init-rust)
;;; init-rust.el ends here
