;; init-go.el --- golang-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; golang-setting.
;;

;;; Code:
;;
;; Go packages:
;; go get -u github.com/mdempsky/gocode # github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u golang.org/x/tools/cmd/gotype
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/derekparker/delve/cmd/dlv
;; go get -u github.com/josharian/impl
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/fatih/gomodifytags
;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct

(use-package go-mode
  :ensure t
  :defer t
  :bind (:map go-mode-map
	      ([remap xref-find-definitions] . godef-jump)
	      ("C-c C-r" . go-remove-unused-imports)
	      ("C-c C-f" . gofmt)
	      ("C-c C-k" . godoc)
              ("C-c C-c" . go-run))
  :mode ("\\.go\\'" . go-mode)
  :config
  (setq gofmt-command "goimports")
  (add-to-list 'exec-path "~/go/bin")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (use-package go-dlv
    :ensure t)
  (use-package golint
    :ensure t)
  (use-package flycheck-golangci-lint
    :ensure t
    :hook (go-mode . flycheck-golangci-lint-setup))

  (with-eval-after-load 'projectile
    (use-package go-projectile
      :ensure t
      :commands (go-projectile-mode go-projectile-switch-project)
      :hook ((go-mode . go-projectile-mode)
	     (projectile-after-switch-project . go-projectile-switch-project))))

  (use-package go-eldoc
    :ensure t
    :hook (go-mode . go-eldoc-setup))

  (use-package gotest
    :ensure t
    :bind (:map go-mode-map
                ("C-c a" . go-test-current-project)
                ("C-c m" . go-test-current-file)
                ("C-c ." . go-test-current-test)))
  (use-package go-guru
    :ensure t
    :bind (:map go-mode-map
		;; ([remap xref-find-definitions] . go-guru-definition)
		([remap xref-find-references] . go-guru-referrers)))

  (with-eval-after-load 'company
    (use-package company-go
      :ensure t
      :defines company-backends
      :config  (add-hook 'go-mode-hook
			 (lambda () (local-push-company-backend 'company-go))))))

;; Local Golang playground for short snippets
(use-package go-playground
  :ensure t
  :defer t
  :diminish go-playground-mode
  :commands go-playground-mode)

(provide 'init-go)
;;; init-go.el ends here
