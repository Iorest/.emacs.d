;; init-ghost.el --- ghost-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; ghost-setting.(need to install ghosttext plugin in chrome)
;;

;;; Code:


(use-package atomic-chrome
  :ensure t
  :defer t
  :bind (:map atomic-chrome-edit-mode-map
         ("C-c C-c" . atomic-chrome-close-current-buffer))
  :config
  (setq atomic-chrome-server-ghost-text-port 4001)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("redmine" . textile-mode))))

(provide 'init-ghost)
;;; init-ghost.el ends here
