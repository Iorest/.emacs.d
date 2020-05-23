;; init-en-tool.el --- some english tools.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; some english tools.
;;

;;; Code:


(use-package youdao-dictionary
  :ensure t
  :defer t
  :bind ("C-x j"  . youdao-dictionary-search-at-point))

(use-package pangu-spacing
  :ensure t
  :defer t
  :config
  (setq pangu-spacing-chinese-before-english-regexp "\\(?1:\\cC\\)\\(?2:[0-9A-Za-z$]\\)")
  (setq pangu-spacing-chinese-after-english-regexp "\\(?1:[0-9A-Za-z$]\\)\\(?2:\\cC\\)")
  (setq pangu-spacing-real-insert-separtor t))

(provide 'init-en-tools)
;;; init-en-tools.el ends here
