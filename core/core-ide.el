;; core-ide.el --- ide-feature-support.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Leaforest




;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; ide-features-surpports.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package exec-path-from-shell
  :ensure t
  :init (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-arguments '("-l"))
  :config (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "GOROOT" "GOPATH" "EDITOR"))
            (add-to-list 'exec-path-from-shell-variables var))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(when (fboundp 'prettify-symbols-mode)
  (add-hook 'prog-mode-hook 'prettify-symbols-mode))

;; (setq-default prettify-symbols-alist
;;               '(("lambda" . ?λ)
;;                 ("<-" . ?←)
;;                 ("->" . ?→)
;;                 ("->>" . ?↠)
;;                 ("=>" . ?⇒)
;;                 ("map" . ?↦)
;;                 ("/=" . ?≠)
;;                 ("!=" . ?≠)
;;                 ("==" . ?≡)
;;                 ("<=" . ?≤)
;;                 (">=" . ?≥)
;;                 ("=<<" . (?= (Br . Bl) ?≪))
;;                 (">>=" . (?≫ (Br . Bl) ?=))
;;                 ("<=<" . ?↢)
;;                 (">=>" . ?↣)
;;                 ("&&" . ?∧)
;;                 ("||" . ?∨)
;;                 ("not" . ?¬)))
(setq prettify-symbols-unprettify-at-point 'right-edge)

(use-package editorconfig
  :ensure t
  :defer t
  :diminish editorconfig-mode "EC"
  :hook (prog-mode . editorconfig-mode))

(use-package format-all
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (use-package avy-flycheck
    :ensure t
    :hook (global-flycheck-mode . avy-flycheck-setup)))

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :bind (("M-/" . company-other-backend)
         ("M-C-/"  . company-yasnippet)
         ("C-x /"  . company-dict)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("C-s" . company-search-candidates)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :config
  ;; (company-tng-configure-default)
  ;; (company-quickhelp-mode)
  (make-variable-buffer-local 'company-frontends)
  (setq-default company-frontends
                '(
                  ;; company-tng-frontend
                  company-pseudo-tooltip-unless-just-one-frontend
                  company-echo-metadata-frontend))

  (make-variable-buffer-local 'company-backends)
  (setq-default company-backends
                '(company-tabnine company-capf company-files
                                  (company-keywords company-gtags company-etags company-dabbrev-code)
                                  (company-dabbrev company-abbrev)))

  (setq company-show-numbers t)
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-dabbrev-other-buffers 't)
  (setq company-idle-delay 1.5)
  (setq company-tooltip-align-annotations t)

  (defun local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (push backend company-backends))

  (use-package company-dict
    :ensure t
    :init (setq company-dict-dir (concat user-emacs-directory "dict/"))
    :config
    (defun my-relevant-dicts ()
      (append  (gethash major-mode company-dict-table)
               (gethash 'all company-dict-table)
               (cl-loop for mode in company-dict-minor-mode-list
                        if (and (boundp mode)
                                (symbol-value mode))
                        nconc (gethash mode company-dict-table))
               nil))
    (advice-add #'company-dict--relevant-dicts :override #'my-relevant-dicts))

  (use-package company-tabnine
    :ensure t
    :config)

  (when (display-graphic-p)
    (use-package company-quickhelp
      :ensure t
      :bind (:map company-active-map
                  ("M-h" . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :config (setq company-quickhelp-delay 0.8)))

  :diminish company-mode)

(use-package ggtags
  :ensure t
  :defer t
  :config
  (add-to-list 'xref-backend-functions 'ggtags--xref-backend))

(use-package counsel-gtags
  :ensure t
  :defer t
  :init
  :config)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init (setq-default projectile-mode-line-prefix " Proj")
  (setq projectile-use-git-grep t)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              :map projectile-command-map
              ("%" . projectile-replace-regexp))
  :hook (after-init . projectile-mode)
  :config
  (use-package ibuffer-projectile
    :ensure t
    :functions ibuffer-do-sort-by-alphabetic
    :hook ((ibuffer . (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic))))))
  ;; (projectile-update-mode-line)
  (let ((command
         (cond
          ((executable-find "rg")
           (let ((rg-cmd ""))
             (dolist (dir projectile-globally-ignored-directories)
               (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
             (concat "rg -0 --files --color=never --hidden" rg-cmd)))
          ((executable-find "pt")
           (if *os-is-win*
               (concat "pt /0 /l /nocolor /hidden ."
                       (mapconcat #'identity
                                  (cons "" projectile-globally-ignored-directories)
                                  " /ignore:"))
             (concat "pt -0 -l --nocolor --hidden ."
                     (mapconcat #'identity
                                (cons "" projectile-globally-ignored-directories)
                                " --ignore="))))
          ((executable-find "ag")
           (concat "ag -0 -l --nocolor --hidden"
                   (mapconcat #'identity
                              (cons "" projectile-globally-ignored-directories)
                              " --ignore-dir="))))))
    (setq projectile-generic-command command))
  (when *os-is-win*
    (when (or (executable-find "rg") (executable-find "pt") (executable-find "ag"))
      (setq projectile-indexing-method 'alien)
      (setq projectile-enable-caching nil))
    (setq projectile-git-submodule-command "")))

(use-package ag
  :ensure t
  :defer t
  :init
  :config)

(use-package deadgrep
  :ensure t
  :defer t)

;; rg now is the best to search tool,fast and powerful
;; pt is other
;; rg supports GBK and other Chinese encoding,while grep/ag/ack does not support
;; For UTF-8, when regular expressions. rg and grep support Chinese characters, ag/ack does not support
(use-package rg
  :ensure t
  :hook (after-init . rg-enable-default-bindings)
  :config
  (setq rg-group-result t)
  (setq rg-show-columns t)
  (add-to-list
   'rg-custom-type-aliases '("none" . "*"))

  (with-eval-after-load 'projectile
    (defalias 'projectile-ripgrep 'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map))

  (bind-key "m" #'rg-menu rg-global-map)

  (when (fboundp 'ag)
    (bind-key "a" #'ag rg-global-map))

  (with-eval-after-load 'counsel
    (bind-keys :map rg-global-map
               ("c r" . counsel-rg)
               ("c s" . counsel-ag)
               ("c p" . counsel-pt)
               ("c f" . counsel-fzf))))

(use-package dumb-jump
  :ensure t
  :config
  ;; curl -LO https://github.com/BurntSushi/ripgrep/releases/download/0.10.0/ripgrep_0.10.0_amd64.deb
  ;; sudo dpkg -i ripgrep_0.10.0_amd64.deb
  (setq dumb-jump-prefer-searcher 'rg)
  (with-eval-after-load 'ivy
    (setq dumb-jump-selector 'ivy)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package ivy-yasnippet
  :ensure t
  :defer t
  :bind (("C-x y" . ivy-yasnippet)))
          ;; Select from xref candidates with Ivy
(use-package ivy-xref
  :ensure t
  :after ivy
  :init
  (when (boundp 'xref-show-definitions-function)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Correcting words with flyspell via Ivy
(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell ivy
  :bind (:map flyspell-mode-map
              ([remap flyspell-correct-word-before-point] . flyspell-correct-previous-word-generic)))
(use-package tiny
  :ensure t
  :defer t
  :config (tiny-setup-default)
  :bind (("C-M-j"  . tiny-expand)))

(use-package auto-yasnippet
  :ensure t
  :defer t)

(use-package neotree
  :ensure t
  :defer t
  :config
  (setq neo-theme 'arrow)
  (setq neo-window-width 35))

(use-package magit
  :commands transient-insert-suffix
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (setq auto-revert-check-vc-info t)
  (when *os-is-win*
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  ;; Add switch: --tags
  (transient-append-suffix 'magit-fetch
    "-p" '("-t" "Fetch all tags" ("-t" "--tags"))))

;; Access Git forges from Magit
(if (executable-find "cc")
    (use-package forge
      :ensure t
      :after magit
      :demand))

(use-package magit-todos
  :ensure t
  :after magit)

(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode t)
  :config
  :diminish git-gutter-mode "GT")

(use-package git-timemachine
  :ensure t
  :init
  :bind (:map vc-prefix-map
              ("t" . git-timemachine))
  :config
  :diminish git-timemachine-mode "TM")

;;; Pop up last commit information of current line
(use-package git-messenger
  :ensure t
  :commands git-messenger:copy-message
  :bind (:map vc-prefix-map
              ("p" . git-messenger:popup-message)
              :map git-messenger-map
              ("m" . git-messenger:copy-message))
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t))

(use-package gist
  :ensure t
  :defer t
  :config
  :bind (("C-c g" . gist-list)
         :map gist-list-menu-mode-map
         ("u" . gist-list-user)
         ("s" . gist-list-starred)))


(use-package gitattributes-mode
  :ensure t
  :defer t
  :init
  :config
  :bind)


(use-package gitignore-mode
  :ensure t
  :defer t
  :init
  :config
  :bind)

(use-package gitconfig-mode
  :ensure t
  :defer t
  :init
  :config
  :bind)

(use-package treemacs
  :ensure t
  :defer t)

(pcase iorest-lsp
  ('eglot
   (use-package eglot
     :ensure t
     :hook (prog-mode . eglot-ensure)))

  ('lsp-mode
   ;; Emacs client for the Language Server Protocol
   ;; https://github.com/emacs-lsp/lsp-mode/blob/master/README-NEXT.md#supported-languages
   (use-package lsp-mode
     :ensure t
     :hook (prog-mode . lsp)
     :bind (:map lsp-mode-map
                 ("C-c C-d" . lsp-describe-thing-at-point))
     :init
     (setq lsp-auto-guess-root t)       ; Detect project root
     (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
     :config
     (defun restart-lsp-server ()
       "Restart LSP server."
       (interactive)
       (lsp-restart-workspace)
       (revert-buffer t t)
       (message "LSP server restarted."))
     (require 'lsp-clients))

   (use-package lsp-ui
     :ensure t
     :bind (:map lsp-ui-mode-map
                 ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                 ([remap xref-find-references] . lsp-ui-peek-find-references)
                 ("C-c u" . lsp-ui-imenu))
     :hook (lsp-mode . lsp-ui-mode))

   (use-package company-lsp
     :ensure t
     :after company
     :defines company-backends
     :init
     (setq company-lsp-cache-candidates 'auto)
     (push 'company-lsp company-backends))

   ;; C/C++/Objective-C support
   ;; Install: brew tap twlz0ne/homebrew-ccls && brew install ccls
   ;;          refer to  https://github.com/MaskRay/ccls/wiki/Getting-started
   (use-package ccls
     :ensure t
     :defines projectile-project-root-files-top-down-recurring
     :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda ()
                                                      (require 'ccls)
                                                      (lsp)))
     :config
     (with-eval-after-load 'projectile
       (setq projectile-project-root-files-top-down-recurring
             (append '("compile_commands.json"
                       ".ccls")
                     projectile-project-root-files-top-down-recurring))))

   ;; ;; Java support
   ;; ;; Install: wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
   ;; ;;          tar jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/
   (use-package lsp-java
     :ensure t
     :defer t)


   (use-package dap-mode
     :hook ((after-init . dap-mode)
            (dap-mode . dap-ui-mode)

            (python-mode . (lambda () (require 'dap-python)))
            (go-mode . (lambda () (require 'dap-go)))
            (java-mode . (lambda () (require 'dap-java)))
            (rust-mode . (lambda () (require 'dap-rust)))
            ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))
            (php-mode . (lambda () (require 'dap-php)))))

   ))
(when iorest-lsp
  ;; Enable LSP in org babel
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enbale (lang)
    "Support LANG in org source code block."
    (cl-check-type lang stringp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((filename (or (->> info caddr (alist-get :file))
                               buffer-file-name)))
             (setq buffer-file-name filename)
             (pcase iorest-lsp
               ('eglot
                (and (fboundp 'eglot) (eglot)))
               ('lsp-mode
                (and (fboundp 'lsp)
                     ;; `lsp-auto-guess-root' MUST be non-nil.
                     (setq lsp-buffer-uri (lsp--path-to-uri filename))
                     (lsp))))))
         (put ',intern-pre 'function-documentation
              (format "Enable `%s' in the buffer of org source block (%s)."
                      iorest-lsp (upcase ,lang)))

         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (defvar org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))
  (add-to-list 'org-babel-lang-list (if (>= emacs-major-version 26) "shell" "sh"))
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enbale ,lang))))

;;language-support
(require 'init-lisp)
(require 'init-python)
(require 'init-ruby)
(require 'init-rust)
(require 'init-elixir)
(require 'init-go)
(require 'init-latex)
(require 'init-c)
(require 'init-sh)
(require 'init-org)
(require 'init-web)
(require 'init-verilog)
(require 'init-prodigy)
(require 'init-others)


(use-package quickrun
  :ensure t
  :defer t
  :config
  (quickrun-add-command "python"
    '((:command . "python3")
      (:compile-only . "pyflakes %s")
      (:description . "Run Python3 script"))
    :override t)
  :bind (("C-x C-z" . quickrun)))

(use-package reformatter
  :ensure t
  :defer t
  :commands (reformatter-define)
  :init
  (reformatter-define prettier-js
    :program "prettier"
    :args '("--parser" "babel"))
  (reformatter-define prettier-json
    :program "prettier"
    :args '("--parser" "json"))
  (reformatter-define prettier-html
    :program "prettier"
    :args '("--parser" "html"))
  (reformatter-define prettier-css
    :program "prettier"
    :args '("--parser" "css"))
  (reformatter-define prettier-md
    :program "prettier"
    :args '("--parser" "markdown")))

(use-package tldr
  :ensure t
  :defer t)

(use-package cheat-sh
  :ensure t
  :defer t)

(provide 'core-ide)
;;; core-ide.el ends here
