;; core-ide.el --- edit-setting.	-*- lexical-binding: t -*-

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
;; edit-setting.
;;

;;; Code:


(setq default-directory "~/Workspace/")
(setq visible-bell 0)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq display-line-numbers-width-start t)
(setq display-line-numbers-width 3)
(global-display-line-numbers-mode)


(require 'dired)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

(use-package desktop
  :ensure t
  :init
  (setq desktop-path (list user-emacs-directory)
        desktop-auto-save-timeout 600)
  (desktop-save-mode 1))

(use-package savehist
  :ensure t
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables
              '(mark-ring
                global-mark-ring
                search-ring
                kill-ring
                regexp-search-ring
                extended-command-history)
              savehist-autosave-interval 300))


(use-package bookmark
  :ensure t
  :config
  (setq  bookmark-save-flag 1
         bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)))

(setq-default
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 abbrev-mode t
 save-abbrevs t
 set-mark-command-repeat-pop t
 indent-tabs-mode nil
 make-backup-files nil
 truncate-lines nil
 truncate-partial-width-windows nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 blink-cursor-interval 0.4
 buffers-menu-max-size 30
 fill-column 80
 tooltip-delay 1.5
 system-time-locale "C"
 cursor-type 'hbar
 scroll-preserve-screen-position 'always
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil
 )


(use-package flyspell
  :ensure t
  :init
  (setq ispell-silently-savep t)
  (cond
   ((executable-find "hunspell")
    (setenv
     "DICPATH"
     "/usr/share/hunspell")
    ;; Tell ispell-mode to use hunspell.
    (setq ispell-program-name "hunspell")
    (setq ispell-local-dictionary "en_US")
    (setq ispell-hunspell-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US,en_MED") nil utf-8))))
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))
   (t
    (message "Neither aspell nor hunspell found")))
  :hook (((text-mode outline-mode) . flyspell-mode)))


(use-package multiple-cursors
  :ensure t
  :config
  :bind
  (("C-<"      . mc/mark-previous-like-this)
   ("C->"      . mc/mark-next-like-this)
   ("C-+"      . mc/mark-more-like-this-extended)
   ("C-c C-<"  . mc/mark-all-like-this)
   ;; From active region to multiple cursors:
   ("C-c m r"  . set-rectangular-region-anchor)
   ("C-c m c"  . mc/edit-lines)
   ("C-c m e"  . mc/edit-ends-of-lines)
   ("C-c m a"  . mc/edit-beginnings-of-lines)
   ("C-S-<mouse-1>"   . mc/add-cursor-on-click)
   ))


(use-package smex
  :ensure t
  :init
  :config  (setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))

(use-package avy
  :ensure t
  :init
  :config
  (avy-setup-default)
  (setq avy-background t))

(use-package ace-pinyin
  :ensure t
  :diminish ace-pinyin-mode
  :hook (after-init . ace-pinyin-global-mode))

(use-package ivy
  :ensure t
  :init (add-hook 'after-init-hook 'ivy-mode)
  :diminish ivy-mode
  :config (setq-default
           ivy-use-virtual-buffers t
           ivy-virtual-abbreviate 'fullpath
           ivy-count-format ""
           ivy-format-function 'ivy-format-function-arrow
           projectile-completion-system 'ivy
           ivy-magic-tilde nil)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (dolist (k '("C-j" "C-RET"))
    (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))
  (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
  (use-package fuz
    :ensure t
    :config
    ;; (unless (require 'fuz-core nil t)
    ;; (fuz-build-and-load-dymod))
    )
  (use-package pinyinlib
    :ensure t
    :config
    (defun re-builder-extended-pattern (str)
      (let* ((len (length str)))
        (cond
         ;; do nothing
         ((<= (length str) 0))

         ;; If the first charater of input in ivy is ":",
         ;; remaining input is converted into Chinese pinyin regex.
         ;; For example, input "/ic" match "isController" or "isCollapsed"
         ((string= (substring str 0 1) ":")
          (setq str (pinyinlib-build-regexp-string (substring str 1 len) t)))

         ;; If the first charater of input in ivy is "/",
         ;; remaining input is converted to pattern to search camel case word
         ((string= (substring str 0 1) "/")
          (let* ((rlt "")
                 (i 0)
                 (subs (substring str 1 len))
                 c)
            (when (> len 2)
              (setq subs (upcase subs))
              (while (< i (length subs))
                (setq c (elt subs i))
                (setq rlt (concat rlt (cond
                                       ((and (< c ?a) (> c ?z) (< c ?A) (> c ?Z))
                                        (format "%c" c))
                                       (t
                                        (concat (if (= i 0) (format "[%c%c]" (+ c 32) c)
                                                  (format "%c" c))
                                                "[a-z]+")))))
                (setq i (1+ i))))
            (setq str rlt))))
        (ivy--regex-plus str)))
    ;;; 这是我自定义的匹配策略
    (defun ivy--regex-pinyin (str)
      "The regex builder wrapper to support pinyin."
      (or (re-builder-extended-pattern str)
          (ivy-fuz-regex-fuzzy str)
          (ivy--regex-plus str)
          (ivy--regex-ignore-order str)))
    (dolist (fn '(swiper
                  swiper-isearch
                  swiper-all
                  counsel-ag
                  counsel-rg
                  counsel-pt
                  counsel-grep
                  counsel-yank-pop))
      (setf (alist-get fn ivy-re-builders-alist) #'ivy--regex-pinyin))
    ))


(use-package counsel
  :ensure t
  :init
  :diminish counsel-mode
  :hook (after-init . counsel-mode)
  :config
  (setq-default counsel-mode-override-describe-bindings t)
  (setq swiper-action-recenter t)
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")
  (let ((cmd (cond ((executable-find "rg")
                    "rg -S --no-heading --line-number --color never '%s' %s")
                   ((executable-find "ag")
                    "ag -S --noheading --nocolor --nofilename --numbers %s %s")
                   (t counsel-grep-base-command))))
    (setq counsel-grep-base-command cmd))
  ;; Pre-fill search keywords
  ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
  (defvar my-ivy-fly-commands
    '(query-replace-regexp
      flush-lines
      keep-lines
      ivy-read
      swiper
      swiper-backward
      swiper-all
      swiper-isearch
      swiper-isearch-backward
      counsel-grep-or-swiper
      counsel-grep-or-swiper-backward
      counsel-grep
      counsel-ag
      counsel-rg
      counsel-pt))

  (defun my-ivy-fly-back-to-present ()
    ;; (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)
    (cond ((and (memq last-command my-ivy-fly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
           ;; repeat one time to get straight to the first history item
           (setq unread-command-events
                 (append unread-command-events
                         (listify-key-sequence (kbd "M-p")))))
          ((memq this-command '(self-insert-command
                                yank
                                ivy-yank-word
                                counsel-yank-pop))
           (equal (this-command-keys-vector) (kbd "M-n"))
           (delete-region (point)
                          (point-max)))))

  (defun my-ivy-fly-time-travel ()
    (when (memq this-command my-ivy-fly-commands)
      (let* ((kbd (kbd "M-n"))
             (cmd (key-binding kbd))
             (future (and cmd
                          (with-temp-buffer
                            (when (ignore-errors
                                    (call-interactively cmd) t)
                              (buffer-string))))))
        (when future
          (save-excursion
            (insert (propertize (replace-regexp-in-string
                                 "\\\\_<" ""
                                 (replace-regexp-in-string
                                  "\\\\_>" ""
                                  future))
                                'face 'shadow)))
          (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

  (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
  (defun my-swiper-toggle-counsel-rg ()
    "Toggle `counsel-rg' with current swiper input."
    (interactive)
    (let ((text (replace-regexp-in-string
                 "\n" ""
                 (replace-regexp-in-string
                  "\\\\_<" ""
                  (replace-regexp-in-string
                   "\\\\_>" ""
                   (replace-regexp-in-string "^.*Swiper: " ""
                                             (thing-at-point 'line t)))))))
      (ivy-quit-and-run
        (counsel-rg text default-directory))))
  (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)

  (with-eval-after-load 'rg
    (defun my-swiper-toggle-rg-dwim ()
      "Toggle `rg-dwim' with current swiper input."
      (interactive)
      (ivy-quit-and-run (rg-dwim default-directory)))
    (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
    (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim ivy-minibuffer-map))
  (use-package helpful
    :ensure t
    :config
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (global-set-key (kbd "C-h o") #'helpful-symbol))

  :bind (:map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         ;; Search at point
         ;; "M-j": word-at-point
         ;; "M-n"/"C-w": symbol-at-point
         ;; Refer to https://www.emacswiki.org/emacs/SearchAtPoint#toc8
         ;; and https://github.com/abo-abo/swiper/wiki/FAQ
         ;; ("C-w" . (lambda ()
         ;;            (interactive)
         ;;            (insert (format "%s" (with-ivy-window (ivy-thing-at-point))))))

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-%" . swiper-query-replace)))

(use-package imenu-anywhere
  :ensure t
  :defer t)

(use-package autorevert
  :ensure t
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

(use-package undo-tree
  :ensure t
  :init (add-hook 'after-init-hook 'global-undo-tree-mode)
  :config
  :diminish undo-tree-mode)

(use-package hideshow
  :ensure t
  :defer t
  :diminish hs-minor-mode)

(use-package suggest
  :ensure t
  :defer t)

(use-package expand-region
  :ensure t
  :init (require 'expand-region)
  :bind ("C-=" . er/expand-region))


(use-package ediff
  :ensure t
  :defer t
  :hook(;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-make-buffers-readonly-at-startup t)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package vdiff
  :ensure t
  :defer t
  :after hydra
  :hook((vdiff-quit . winner-undo))
  :config
  (define-key vdiff-mode-map (kbd "M-i v") vdiff-mode-prefix-map))

(use-package smooth-scrolling
  :ensure t
  :init
  :config (setq smooth-scroll-margin 3)
  (smooth-scrolling-mode))

(use-package switch-window
  :ensure t
  :init
  :config (setq-default switch-window-shortcut-style 'alphabet)
  :bind ("C-x o"  . switch-window))

(use-package subword
  :ensure t
  :diminish subword-mode
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

(use-package paredit
  :ensure t
  :diminish paredit-mode "Par"
  :config
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-j") nil)
  (define-key paredit-mode-map (kbd "C-j") nil)
  (define-key paredit-mode-map (kbd "M-S") nil)
  (define-key paredit-mode-map (kbd "M-J") nil)
  (define-key paredit-mode-map (kbd "M-r") nil)
  :hook ((lisp-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)))

(use-package embrace
  :ensure t
  :hook ((LaTeX-mode . embrace-LaTeX-mode-hook)
         (org-mode . embrace-org-mode-hook)
         (ruby-mode . embrace-ruby-mode-hook))
  :config )

(use-package whitespace
  :ensure t
  :diminish
  :hook ((prog-mode outline-mode conf-mode text-mode) . whitespace-mode)
  :config
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))

  (with-eval-after-load 'popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)
    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing autocomplete box."
      (if whitespace-mode
          (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))

    (defadvice popup-delete (after my-restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting autocomplete box."
      (if my-prev-whitespace-mode
          (whitespace-mode 1)))))

(use-package aggressive-indent
  :ensure t
  ;; :hook (prog-mode . aggressive-indent-mode)
  :diminish aggressive-indent-mode "AI")

(use-package anzu
  :ensure t
  :hook (after-init . global-anzu-mode)
  :config
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :diminish anzu-mode)

(use-package browse-url
  :ensure t
  :defer t)

(use-package vlf
  :ensure t
  :defer t
  :config
  (require 'vlf-setup))

(use-package crux
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

(use-package delsel
  :ensure t
  :hook (after-init . delete-selection-mode))

(use-package wc-mode
  :ensure t
  :defer t)

(use-package winner
  :ensure t
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers
              '("*Completions*"
                "*Compile-Log*"
                "*inferior-lisp*"
                "*Fuzzy Completions*"
                "*Apropos*"
                "*Help*"
                "*cvs*"
                "*Buffer List*"
                "*Ibuffer*"
                "*esh command on file*")))
(require 'init-bindings)
(provide 'core-edit)
;;; core-edit.el ends here
