;; init-web.el --- web-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; web-setting.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package css-mode
  :ensure t
  :defer t
  :init (setq css-indent-offset 2))

;; SCSS mode
(use-package scss-mode
  :ensure t
  :defer t
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))

;; New `less-cs-mde' in Emacs 26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode :ensure t))

;; CSS eldoc
(use-package css-eldoc
  :ensure t
  :config
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

;; JSON mode
(use-package json-mode
  :ensure t
  :defer t)


;; Improved JavaScript editing mode
(use-package js2-mode
  :ensure t
  :defer t
  :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :config
  (with-eval-after-load 'flycheck
    (if (or (executable-find "eslint_d")
            (executable-find "eslint")
            (executable-find "jshint"))
        (setq js2-mode-show-strict-warnings nil))
    (if (executable-find "eslint_d")
        ;; https://github.com/mantoni/eslint_d.js
        ;; npm -i -g eslint_d
        (setq flycheck-javascript-eslint-executable "eslint_d")))

  (use-package js2-refactor
    :ensure t
    :diminish js2-refactor-mode
    :hook (js2-mode . js2-refactor-mode)
    :config (js2r-add-keybindings-with-prefix "C-c C-m")))

(unless iorest-lsp
  (use-package tide
    :ensure t
    :diminish tide-mode "Tide"
    :defines (company-backends tide-format-options)
    :functions (tide-setup tide-hl-identifier-mode)
    :preface
    (defun setup-tide-mode ()
      "Setup tide mode."
      (interactive)
      (tide-setup)
      (eldoc-mode 1)
      (tide-hl-identifier-mode 1))
    :hook (((typescript-mode js2-mode) . setup-tide-mode))
    :config
    (setq tide-format-options
          '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
            t
            :placeOpenBraceOnNewLineForFunctions
            nil))
    (add-hook 'tide-mode-hook
              (lambda () (local-push-company-backend 'company-tide)))))

;; (use-package polymode
;;   :ensure t
;;   :defer t
;;   :hook ((html-mode sgml-mode xml-mode nxml-mode) . polymode-minor-mode)
;;   :init (setq polymode-prefix-key "\C-cn")
;;   :config
;;   (use-package poly-markdown
;;     :ensure t
;;     :after polymode markdown-mode))

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :ensure t
  :defer t
  :diminish skewer-mode
  :hook ((js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (html-mode . skewer-html-mode))
  :init
  (with-eval-after-load 'skewer-css
    (diminish 'skewer-css-mode))
  (with-eval-after-load 'skewer-html
    (diminish 'skewer-html-mode)))



(provide 'init-web)
;;; init-web.el ends here
