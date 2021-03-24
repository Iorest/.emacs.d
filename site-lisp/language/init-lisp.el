;; init-lisp.el --- lisp-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; lisp-setting.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package slime
  :ensure t
  :defer t
  :diminish
  :hook (lisp-mode . slime-mode)
  :config
  (setq inferior-lisp-program "sbcl")

  (use-package slime-company
    :ensure t
    :config
    (slime-setup '(slime-fancy slime-company))
    )
  )
(set-variable (quote scheme-program-name) "racket")
(use-package elisp-slime-nav
  :ensure t
  :diminish
  :hook ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode))

(use-package lispy
  :ensure t
  :defer t)

(use-package crux
  :ensure t
  :defer t)

(use-package suggest
  :ensure t
  :defer t)

(use-package s
  :ensure t
  :defer t)

(use-package f
  :ensure t
  :defer t)

(use-package dash
  :ensure t
  :defer t)

(use-package macrostep
  :ensure t
  :defer t
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              :map lisp-interaction-mode-map
              ("C-c e" . macrostep-expand)))

(use-package elmacro
  :ensure t
  :defer t)

(use-package cask-mode
  :ensure t
  :defer t)


(provide 'init-lisp)
;;; init-lisp.el ends here
