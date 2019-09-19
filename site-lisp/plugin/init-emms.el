;; init-emms.el --- emms-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; emms-setting.
;;

;;; Code:

(use-package emms
  :ensure t
  :defer t
  :config
  (when (fboundp 'emms-cache)
    (emms-cache 1))
  (use-package emms-setup
    :init
    (add-hook 'emms-player-started-hook 'emms-show)
    (setq emms-playlist-default-major-mode 'emms-playlist-mode)
    (setq emms-show-format "Playing: %s")
    :config
    (emms-all)
    (emms-default-players)
    (defun emms-mode-line-playlist-current ()
      "Format the currently playing song."
      (format emms-mode-line-format
              (propertize "Emms:"
                          'help-echo
                          (emms-track-description
                           (emms-playlist-current-selected-track)))))
    (setq emms-source-file-default-directory "~/音乐/music/")
    (setq emms-repeat-playlist t))
  (use-package emms-lyrics
    :init
    :config
    (setq emms-lyrics-display-on-minibuffer nil)
    (setq emms-lyrics-display-buffer t)
    (setq emms-lyrics-display-on-modeline nil)
    (setq emms-lyrics-dir "~/音乐/lyrics/")
    (emms-lyrics 1))
  ;; (use-package emms-mode-line-cycle
  ;;   :ensure t
  ;;   :init
  ;;   :config
  ;;   (emms-mode-line-cycle 1))
  :bind (:map emms-playlist-mode-map
              ("<SPC>" . emms-pause)))


(provide 'init-emms)
;;; init-emms.el ends here
