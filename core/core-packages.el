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


(eval-when-compile
  (require 'init-custom))

(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package archives (ELPA).
REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern (completing-read "Select package archives: "
                             (mapcar #'car iorest-package-archives-alist)))))
  ;; Set option
  (set-variable 'iorest-package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))
(defalias 'iorest-set-package-archives #'set-package-archives)

;; Refer to https://emacs-china.org/t/elpa/11192
(defun iorest-test-package-archives (&optional no-chart)
  "Test connection speed of all package archives and display on chart.
Not displaying the chart if NO-CHART is non-nil.
Return the fastest package archive."
  (interactive)

  (let* ((urls (mapcar
                (lambda (url)
                  (concat url "archive-contents"))
                (mapcar #'cdr
                        (mapcar #'cadr
                                (mapcar #'cdr
                                        iorest-package-archives-alist)))))
         (durations (mapcar
                     (lambda (url)
                       (let ((start (current-time)))
                         (message "Fetching %s..." url)
                         (cond ((executable-find "curl")
                                (call-process "curl" nil nil nil "--max-time" "10" url))
                               ((executable-find "wget")
                                (call-process "wget" nil nil nil "--timeout=10" url))
                               (t (user-error "curl or wget is not found")))
                         (float-time (time-subtract (current-time) start))))
                     urls))
         (fastest (car (nth (cl-position (apply #'min durations) durations)
                            iorest-package-archives-alist))))

    ;; Display on chart
    (when (and (not no-chart)
               (require 'chart nil t)
               (require 'url nil t))
      (chart-bar-quickie
       'horizontal
       "Speed test for the ELPA mirrors"
       (mapcar (lambda (url) (url-host (url-generic-parse-url url))) urls) "ELPA"
       (mapcar (lambda (d) (* 1e3 d)) durations) "ms"))

    (message "%s" urls)
    (message "%s" durations)
    (message "%s is the fastest package archive" fastest)

    ;; Return the fastest
    fastest))


(iorest-set-package-archives iorest-package-archives)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (when (version< emacs-version "27.0") (package-initialize)))

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
