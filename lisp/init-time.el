;; init-time.el --- times.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; utils.
;;

;;; Code:

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


(use-package benchmark-init
  :ensure t
  :commands (benchmark-init/activate)
  :hook
  (after-init . benchmark-init/deactivate)
  :init (benchmark-init/activate)
  :config
  (with-eval-after-load 'swiper
    (add-to-list 'swiper-font-lock-exclude 'benchmark-init/tree-mode)))

(use-package esup
  :ensure t
  :defer t)

(provide 'init-time)

;;; init-time.el ends here
