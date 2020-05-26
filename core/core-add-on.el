;; core-add-on.el --- plugins.	-*- lexical-binding: t -*-

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
;; plugins.
;;

;;; Code:


(autoload 'unicad "unicad" "Detect coding system smartly" t)
(add-to-list 'auto-mode-alist '("\\.elt\\'" . eltex-mode))
(autoload 'eltex-mode "eltex" "Write LaTeX in Emacs Lisp" t)
;(require 'smart-compile)
(require 'insert-translated-name)
(autoload 'toggle-company-english-helper "company-english-helper" "Help english company" t)
(autoload 'opencc-print-buffer "opencc" "opencc support." t)
(autoload 'punctuation "punctuation" "ligature support." t)
(autoload 'fira-code-symbol-mode "init-ligature" "ligature support." t)
;; (require 'init-ligature)
(require 'init-en-tools)
;; (require 'cal-china-x)
;; (cal-china-x-setup)
(require 'init-elfeed)
(require 'init-ghost)
(require 'init-emms)
(require 'init-tramp)

(use-package leetcode
  :ensure t
  :defer t)

(use-package tongbu
  :ensure t
  :defer t)

(use-package dashboard
  :ensure t
  :functions (all-the-icons-faicon
              all-the-icons-material
              winner-undo
              widget-forward)
  :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :init
  (dashboard-setup-startup-hook)
  :config
  ;; To customize which widgets are displayed, you can use the following snippet
  (setq dashboard-items '((recents  . 10)
                          (projects . 10)))
  ;; Set the banner
  (setq dashboard-startup-banner "~/.emacs.d/img/128x128@2x.png")
  ;; Set the title
  (setq dashboard-banner-logo-title "Hey, Iorest!")
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; To show navigator below the banner:
  (setq dashboard-set-navigator t)
  (setq dashboard-set-file-icons t))

(setq local-file (expand-file-name "init-local.el" user-emacs-directory))

(when (file-exists-p local-file)
  (load local-file))
(provide 'core-add-on)
;;; core-add-on.el ends here
