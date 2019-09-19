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

(use-package evil-leader
  :ensure t
  :after evil
  :config  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    ":"  'counsel-M-x
    "ff" 'find-file
    "bb" 'switch-to-buffer
    "w/" 'split-window-right
    "w-" 'split-window-below
    "wM" 'delete-other-windows
    "h"  'evil-mark-whole-buffer))

(use-package evil-surround
  :ensure t
  :after evil
  :config  (global-evil-surround-mode))

(use-package evil-matchit
  :ensure t
  :defer t
  :bind (("M-g j" . evilmi-jump-items))
  :config  (global-evil-matchit-mode 1))

(use-package evil-numbers
  :ensure t
  :defer t)

(provide 'core-evil)
;;; core-evil.el ends here
