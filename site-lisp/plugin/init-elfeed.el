;; init-elfeed.el --- elfeed-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; elfeed-setting.
;;

;;; Code:

(use-package elfeed
  :ensure t
  :defer t
  :bind (("C-x w" . elfeed)
         :map elfeed-search-mode-map
         ("U" . elfeed-update))
  :config
  (setq elfeed-feeds
        '("http://planet.emacsen.org/atom.xml"
          "http://www.masteringemacs.org/feed/"
          "https://oremacs.com/atom.xml"
          "https://pinecast.com/feed/emacscast")))


(provide 'init-elfeed)
;;; init-elfeed.el ends here
