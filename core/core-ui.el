;; core-ui.el --- ui-setting.	-*- lexical-binding: t -*-

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
;; ui-setting.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq inhibit-startup-echo-area-message t)
(fset 'yes-or-no-p 'y-or-n-p)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(defvar alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))

(defun loop-alpha ()
  "Change the transparency."
  (interactive)
  (let ((h (car alpha-list)))                ;; head value will set to
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))
    ))

(global-set-key [(f9)] 'loop-alpha)

(use-package ample-theme
  :ensure t
  :defer t
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample-flat)))

(use-package nyan-mode
  :ensure t
  :config (nyan-mode))

(use-package telephone-line
  :ensure t
  :init (progn (setq telephone-line-height 24
                     telephone-line-evil-use-short-tag t)
               (setq telephone-line-primary-left-separator 'telephone-line-gradient
                     telephone-line-primary-right-separator 'telephone-line-gradient)
               (setq telephone-line-lhs
                     '((evil   . (telephone-line-evil-tag-segment))
                       (accent . (telephone-line-vc-segment
                                  telephone-line-buffer-segment
                                  telephone-line-filesize-segment
                                  telephone-line-erc-modified-channels-segment
                                  telephone-line-process-segment))
                       (nil    . (telephone-line-projectile-segment
                                  telephone-line-flycheck-segment
                                  telephone-line-minor-mode-segment
                                  telephone-line-nyan-segment))))
               (setq telephone-line-rhs
                     '((nil    . (telephone-line-misc-info-segment))
                       (accent . (telephone-line-major-mode-segment))
                       (evil   . (telephone-line-airline-position-segment)))))
  :config (telephone-line-mode 1))


;; More friendly display transformer for Ivy
(use-package ivy-rich
  :ensure t
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package which-key
  :ensure t
  :bind ("C-h C-k" . which-key-show-top-level)
  :hook (after-init . which-key-mode))

(use-package discover-my-major
  :ensure t
  :defer t
  :commands (discover-my-major discover-my-mode)
  :bind ("C-h C-m" . discover-my-major))

(use-package symbol-overlay
  :ensure t
  :hook (prog-mode  . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              ("M-n"  . symbol-overlay-jump-next)
              ("M-p"  . symbol-overlay-jump-prev)))

(use-package highlight-indent-guides
  :ensure t
  :init (setq highlight-indent-guides-method 'column)
  :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :diminish highlight-indent-guides-mode)

(use-package beacon
  :ensure t
  :diminish
  :config (beacon-mode t))

(use-package rainbow-mode
  :ensure t
  :defer t
  :diminish
  :hook ((emacs-lisp-mode html-mode css-mode) . rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package darkroom
  :ensure t
  :defer t)

(use-package paren
  :ensure t
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

(use-package hl-line
  :ensure t
  :config (set-face-background 'hl-line "#3e4446")
  :hook (after-init . global-hl-line-mode))

(use-package hl-todo
  :ensure t
  :custom-face (hl-todo ((t (:box t :inherit 'hl-todo))))
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

(use-package diff-hl
  :ensure t
  :defines desktop-minor-mode-table
  :commands diff-hl-magit-post-refresh
  :custom-face
  (diff-hl-change ((t (:background "DeepSkyBlue"))))
  (diff-hl-delete ((t (:background "OrangeRed"))))
  (diff-hl-insert ((t (:background "YellowGreen"))))
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq diff-hl-draw-borders nil)
  (setq fringes-outside-margins t)
  (set-fringe-mode '(4 . 8))

  (unless (display-graphic-p)
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; Enforce rules for popups
(use-package shackle
  :ensure t
  :defer t
  :hook (after-init . shackle-mode)
  :config
  (defun view-last-popup-buffer ()
    "View last popup buffer."
    (interactive)
    (ignore-errors
      (display-buffer shackle-last-buffer)))
  (bind-key "C-h z" #'view-last-popup-buffer)

  ;; Add keyword: `autoclose'
  (defvar shackle--popup-window-list nil) ; all popup windows
  (defvar-local shackle--current-popup-window nil) ; current popup window
  (put 'shackle--current-popup-window 'permanent-local t)

  (defun shackle-display-buffer-hack (fn buffer alist plist)
    (let ((window (funcall fn buffer alist plist)))
      (setq shackle--current-popup-window window)

      (when (plist-get plist :autoclose)
        (push (cons window buffer) shackle--popup-window-list))
      window))

  (defun shackle-close-popup-window-hack (&rest _)
    "Close current popup window via `C-g'."
    (setq shackle--popup-window-list
          (cl-loop for (window . buffer) in shackle--popup-window-list
                   if (and (window-live-p window)
                           (equal (window-buffer window) buffer))
                   collect (cons window buffer)))
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p)))
      (let (window buffer)
        (if (one-window-p)
            (progn
              (setq window (selected-window))
              (when (equal (buffer-local-value 'shackle--current-popup-window
                                               (window-buffer window))
                           window)
                (winner-undo)))
          (setq window (caar shackle--popup-window-list))
          (setq buffer (cdar shackle--popup-window-list))
          (when (and (window-live-p window)
                     (equal (window-buffer window) buffer))
            (delete-window window)

            (pop shackle--popup-window-list))))))

  (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
  (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack)

  ;; rules
  (setq shackle-default-size 0.4
        shackle-default-alignment 'below)
  (setq shackle-rules
        '(("*Help*" :select t :align 'below :autoclose t)
          ("*Apropos*" :select t :size 0.3 :align 'below :autoclose t)
          ("*compilation*" :select t :size 0.3 :align 'below :autoclose t)
          ("*Compile-Log*" :select t :size 0.3 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
          ("*ert*" :align 'below :autoclose t)
          ("*Backtrace*" :select t :size 15 :align 'below)
          ("*Warnings*" :size 0.3 :align 'below :autoclose t)
          ("*Messages*" :size 0.3 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
          (("*shell*" "*eshell*" "*ielm*") :popup t :align 'below)
          ("*Calendar*" :select t :size 0.3 :align 'below)
          ("*Occur*" :size 0.4 :select t :align 'below)
          ("\\*ivy-occur .*\\*" :regexp t :size 0.4 :select t :align 'below)
          (" *undo-tree*" :select t)
          ("*Paradox Report*" :size 0.3 :align 'below :autoclose t)
          ("*quickrun*" :select t :size 15 :align 'below)
          ("*tldr*" :align 'below :autoclose t)
          ("*Youdao Dictionary*" :size 0.3 :align 'below :autoclose t)
          ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)
          ("^\\*elfeed-entry" :regexp t :size 0.7 :align 'below :autoclose t)
          ("*lsp-help*" :size 0.3 :align 'below :autoclose t)
          ("*lsp session*" :size 0.4 :align 'below :autoclose t)
          (" *Org todo*" :select t :size 4 :align 'below :autoclose t)
          ("*Org Dashboard*" :select t :size 0.4 :align 'below :autoclose t)
          ("^\\*macro expansion\\**" :regexp t :size 0.4 :align 'below)

          (ag-mode :select t :align 'below)
          (grep-mode :select t :align 'below)
          (ivy-occur-grep-mode :select t :align 'below)
          (pt-mode :select t :align 'below)
          (rg-mode :select t :align 'below)

          (flycheck-error-list-mode :select t :size 0.3 :align 'below :autoclose t)
          (flymake-diagnostics-buffer-mode :select t :size 0.3 :align 'below :autoclose t)

          (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
          (comint-mode :align 'below)
          (helpful-mode :select t :size 0.4 :align 'below :autoclose t)
          (process-menu-mode :select t :size 0.3 :align 'below :autoclose t)
          (cargo-process-mode :select t :size 0.3 :align 'below :autoclose t)
          (list-environment-mode :select t :size 0.3 :align 'below :autoclose t)
          (profiler-report-mode :select t :size 0.5 :align 'below)
          (tabulated-list-mode :align 'below))))

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("<C-mouse-4>" . centaur-tabs-backward)
  ("<C-mouse-5>" . centaur-tabs-forward))

(defun iorest-advice-remove-button (function)
  "Add a button to remove advice in FUNCTION."
  (when (get-buffer "*Help*")
    (with-current-buffer "*Help*"
      (save-excursion
        (goto-char (point-min))
        ;; :around advice: ‘shell-command--shell-command-with-editor-mode’
        (while (re-search-forward "^:[-a-z]+ advice: [‘'`]\\(.+\\)[’'']$" nil t)
          (let ((advice (intern-soft (match-string 1))))
            (when (and advice (fboundp advice))
              (let ((inhibit-read-only t))
                (insert " » ")
                (insert-text-button
                 "Remove"
                 'action
                 ;; In case lexical-binding is off
                 `(lambda (_)
                    (message "Removing %s of advice from %s" ',function ',advice)
                    (advice-remove ',function #',advice)
                    (revert-buffer nil t))
                 'follow-link t)))))))))

(advice-add 'describe-function-1 :after #'iorest-advice-remove-button)

(defvar en-font "Fira Code Retina")
(defvar en-font-size
  (cond (*os-is-gnu* 12)
        (*os-is-mac* 12)
        (*os-is-win* 12)))
(defvar cn-font
  (cond (*os-is-gnu* "WenQuanYi Micro Hei Mono")
        (*os-is-mac* "Microsoft Yahei")
        (*os-is-win* "Microsoft Yahei")))

(defvar cn-font-size
  (cond (*os-is-gnu* 16)
        (*os-is-mac* 16)
        (*os-is-win* 16)))


(defun my-set-font (en-font en-size cn-font cn-size scale)
  "Set up font for english and chinese use EN-FONT EN-SIZE CN-FONT CN-SIZE SCALE."
  (set-face-attribute
   'default nil :font (concat en-font " " (number-to-string (floor (* en-size scale)))))
  ;; Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family cn-font :size (* cn-size scale)))))

(defun my-scale-font (scale)
  "SCALE font for english and chinese."
  (interactive "sScale:")
  ;; Font
  (or (numberp scale)
      (setq scale (string-to-number scale)))
  (my-set-font en-font en-font-size cn-font cn-font-size scale))

(defun scale-cn-font (scale)
  "SCALE."
  (interactive "sScale:")
  (setq cn-font-size (* cn-font-size scale))
  (my-set-font en-font en-font-size cn-font cn-font-size 1))

(defun cn-font-increse ()
  "Increase cn font."
  (interactive)
  (scale-cn-font 1.1))

(defun cn-font-decrese ()
  "Decrease cn font."
  (interactive)
  (scale-cn-font 0.9))

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (if window-system
                   (my-set-font en-font en-font-size cn-font cn-font-size 1))))
(if window-system
    (my-set-font en-font en-font-size cn-font cn-font-size 1))

(provide 'core-ui)
;;; core-ui.el ends here
