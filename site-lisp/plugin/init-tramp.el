;; init-tramp.el --- tramp-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; tramp-setting.
;;

;;; Code:


(use-package tramp
  :ensure t
  :defer t
  :init
  :config
  (use-package counsel-tramp
    :ensure t)
  (cond
   (*os-is-win*
    (setq tramp-default-method "plink"
          tramp-password-end-of-line "\r\n"))
   (*os-is-gnu*
    (setq tramp-default-method "sshx"))))



(provide 'init-tramp)
;;; init-tramp.el ends here
