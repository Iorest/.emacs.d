;; core-ide.el --- base package setting.	-*- lexical-binding: t -*-

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
;; base package setting.
;;

;;; Code:


(require 'init-custom)


(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

(iorest-set-package-archives iorest-package-archives)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; (unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
;;   (setq package-enable-at-startup nil)          ; To prevent initializing twice
;;   (when (version< emacs-version "27.0") (package-initialize)))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
;; (setq use-package-always-ensure t)
;; (setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

(require 'use-package)

;; Disable lazy loading in daemon mode
(if (daemonp)
    (setq use-package-always-demand t))

(use-package hydra
  :ensure t
  :config)

(use-package ivy-hydra
  :ensure t
  :after ivy)

(use-package pretty-hydra
  :ensure t
  :bind ("<f6>" . toggles-hydra/body)
  :config
  ;; Global toggles
  (with-no-warnings
    (pretty-hydra-define toggles-hydra
      (:title "Toggles"
              :color amaranth :quit-key "q")
      ("Basic"
       (("n" (if (fboundp 'display-line-numbers-mode)
                 (display-line-numbers-mode (if display-line-numbers-mode -1 1))
               (global-linum-mode (if global-linum-mode -1 1)))
         "line number"
         :toggle (or (bound-and-true-p display-line-numbers-mode) global-linum-mode))
        ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
        ("d" global-hungry-delete-mode "hungry delete" :toggle t)
        ("e" electric-pair-mode "electric pair" :toggle t)
        ("c" flyspell-mode "spell check" :toggle t)
        ("s" prettify-symbols-mode "pretty symbol" :toggle t)
        ("l" global-page-break-lines-mode "page break lines" :toggle t)
        ("b" display-battery-mode "battery" :toggle t)
        ("T" display-time-mode "time" :toggle t))
       "Highlight"
       (("h l" global-hl-line-mode "line" :toggle t)
        ("h p" show-paren-mode "paren" :toggle t)
        ("h s" symbol-overlay-mode "symbol" :toggle t)
        ("h r" rainbow-mode "rainbow" :toggle t)
        ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
         "whitespace" :toggle show-trailing-whitespace)
        ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
        ("h i" highlight-indent-guides-mode "indent" :toggle t)
        ("h t" global-hl-todo-mode "todo" :toggle t))
       "Coding"
       (("f" flycheck-mode "flycheck" :toggle t)
        ("F" flymake-mode "flymake" :toggle t)
        ("o" origami-mode "folding" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("u" subword-mode "subword" :toggle t)
        ("W" which-function-mode "which function" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
       "Version Control"
       (("v" global-diff-hl-mode "gutter" :toggle t)
        ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
        ("M" diff-hl-margin-mode "margin gutter" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))
       "Package Archive"
       (("p m" (iorest-set-package-archives 'melpa t)
         "melpa" :toggle (eq iorest-package-archives 'melpa) :exit t)
        ("p i" (iorest-set-package-archives 'melpa-mirror t)
         "melpa mirror" :toggle (eq iorest-package-archives 'melpa-mirror) :exit t)
        ("p c" (iorest-set-package-archives 'emacs-china t)
         "emacs china" :toggle (eq iorest-package-archives 'emacs-china) :exit t)
        ("p n" (iorest-set-package-archives 'netease t)
         "netease" :toggle (eq iorest-package-archives 'netease) :exit t)
        ("p s" (iorest-set-package-archives 'ustc t)
         "ustc" :toggle (eq iorest-package-archives 'ustc) :exit t)
        ("p t" (iorest-set-package-archives 'tencent t)
         "tencent" :toggle (eq iorest-package-archives 'tencent) :exit t)
        ("p u" (iorest-set-package-archives 'tuna t)
         "tuna" :toggle (eq iorest-package-archives 'tuna) :exit t)
        ("p T" (iorest-test-package-archives) "speed test" :exit t))))))

;; TODO some error in pretty-hydra that it need to require
(require 'pretty-hydra)

(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode "ElD")
  (diminish 'abbrev-mode "Abr"))

(use-package quelpa
  :ensure t
  :config
  (use-package quelpa-use-package
    :ensure t))


(use-package try
  :ensure t
  :defer t)

(use-package paradox
  :ensure t
  :config (paradox-enable))

(use-package cask
  :ensure t)

(provide 'core-packages)
;;; core-packages.el ends here
