;; init-prodigy.el --- external services.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Manage external services from within Emacs.
;;

;;; Code:

(use-package prodigy
  :ensure t
  :defer t
  :bind (("C-x p" . prodigy))
  :config
  (add-hook 'prodigy-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))
  (setq prodigy-service-file (expand-file-name "service-prodigy.el" user-emacs-directory))
  (defun prodigy-new-service ()
    "Add new service."
    (interactive )
    (find-file prodigy-service-file)
    (goto-char (point-max))
    (newline)
    (insert "(prodigy-define-service\n")
    (indent-for-tab-command)
    (insert ":name\n")
    (indent-for-tab-command)
    (insert ":command\n")
    (indent-for-tab-command)
    (insert ":args\n")
    (indent-for-tab-command)
    (insert ":cwd\n")
    (indent-for-tab-command)
    (insert ":port\n")
    (indent-for-tab-command)
    (insert ":tags\n")
    (indent-for-tab-command)
    (insert ":stop-signal 'kill\n")
    (indent-for-tab-command)
    (insert ")"))


  (when (file-exists-p prodigy-service-file)
    (load prodigy-service-file))
  )


(provide 'init-prodigy)

;;; init-prodigy.el ends here
