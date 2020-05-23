;; core-ide.el --- evil-support.	-*- lexical-binding: t -*-

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
;; evil-surpport.
;;

;;; Code:



(use-package evil
  :ensure t
  :init (setq-default
         evil-search-module 'isearch        ;可以用C-w yank word
         ;; evil-search-module 'evil-search        ;可以用gn 命令，需要取舍
         ;; gn 命令的用法 / search 之后，可以用dgn 或cgn 对search到的第一个内容进行处理，然后用.去重复之
         evil-ex-search-highlight-all nil
         evil-toggle-key "C-<tab>"                ;用不到了 绑定到一个不常用的键,在emacs与normal间切换
         evil-want-visual-char-semi-exclusive t ; 当v 选择到行尾时是否包含换行符
         evil-want-C-i-jump nil
         evil-cross-lines t
         evil-default-state 'emacs
         evil-want-fine-undo t                  ;undo更细化,否则从N->I->N 中所有的修改作为一个undo
         evil-symbol-word-search t              ;# search for symbol not word
         evil-flash-delay 0.5                   ;default 2
         evil-ex-search-case 'sensitive
         ;; C-e ,到行尾时,光标的位置是在最后一个字符后,还是在字符上
         evil-move-cursor-back nil)
  :config
  (evil-mode t)
  (evil-set-initial-state 'ivy-occur-grep-mode 'emacs)
  (evil-set-initial-state 'rg-mode 'emacs))

(use-package general
  :ensure t
  :after evil
  :config
  (general-evil-setup t)
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   ":"  'counsel-M-x
   "ff" 'find-file
   "bb" 'switch-to-buffer
   "w/" 'split-window-right
   "w-" 'split-window-below
   "wM" 'delete-other-windows
   "h"  'evil-mark-whole-buffer
   )

  (general-define-key
   ;; :states '(normal motion insert emacs)
   :states '(normal visual insert emacs)
   ;; :non-normal-prefix "C-SPC"
   "C-x C-k C-s" 'kmacro-save-macro
   "C-M-<backspace>" 'backward-kill-sexp
   "C-<backspace>" 'kill-back-to-indentation
   "C-:" 'shell-command
   "M-!" 'eshell-command
   "M-u" 'upcase-dwim
   "M-l" 'downcase-dwim
   "M-c" 'capitalize-dwim
   "C-s" 'swiper-isearch
   "C-r" 'swiper-isearch-backward
   "C-h u" 'counsel-unicode-char
   )

  (general-define-key
   ;; :states '(normal motion insert emacs)
   :states '(normal visual insert emacs)
   ;; :non-normal-prefix "C-SPC"
   :prefix "M-i"
   "m" 'hydra-mark/body
   "g" 'hydra-move/body
   "e" 'hydra-edit/body
   "n" 'hydra-mc/body
   "t" 'hydra-transpose/body
   "o" 'hydra-paredit/body
   "h" 'hydra-help/body
   "f" 'hydra-function/body
   "k" 'counsel-gtags-create-tags
   "a" 'embrace-add
   "c" 'embrace-change
   "d" 'embrace-delete
   )

  (general-define-key
   ;; :states '(normal motion insert emacs)
   :states '(normal visual insert emacs)
   ;; :non-normal-prefix "C-SPC"
   :prefix "M-s"
   "k" 'counsel-gtags-dwim
   "b" 'counsel-gtags-go-backward
   "d" 'counsel-gtags-find-definition
   "j" 'counsel-gtags-find-reference
   "a" 'counsel-ag
   "f" 'counsel-fzf
   "g" 'counsel-grep
   "l" 'counsel-locate
   "r" 'counsel-rg
   "v" 'deadgrep
   "s" 'swiper
   ;; "o" 'occur
   "n" 'counsel-org
   "p" 'counsel-org-goto-all
   "h" 'swiper-all
   "m" 'counsel-mark-ring
   "t" 'counsel-outline
   )

  (general-define-key
   ;; :states '(normal motion insert emacs)
   :states '(normal visual insert emacs)
   ;; :non-normal-prefix "C-SPC"
   :prefix "M-g"
   "l" 'goto-last-change
   "<SPC>" 'avy-goto-char
   "m" 'counsel-imenu
   "k" 'flush-lines
   "f" 'format-all-buffer
   "," 'dumb-jump-back
   "i" 'dumb-jump-go-prompt
   "." 'dumb-jump-go-other-window
   "x" 'dumb-jump-go-prefer-external
   "z" 'dumb-jump-go-prefer-external-other-window
   "u" 'browse-url-at-point
   "v" 'vdiff-current-file
   "d" 'ediff-current-file
   "M" 'ivy-imenu-anywhere
   "j" 'evilmi-jump-items
   )
  )


(use-package evil-surround
  :ensure t
  :after evil
  :config  (global-evil-surround-mode))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode))

(use-package evil-matchit
  :ensure t
  :defer t
  :config  (global-evil-matchit-mode 1))

(use-package evil-numbers
  :ensure t
  :defer t)

(provide 'core-evil)
;;; core-evil.el ends here
