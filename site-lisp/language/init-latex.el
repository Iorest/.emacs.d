;; init-latex.el --- latex-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; latex-setting.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-show-compilation t)
  (setq TeX-save-query nil)
  (TeX-fold-mode 1)
  (setq-default TeX-master t)
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
  (add-hook 'LaTeX-mode-hook
            (lambda ()   (setq TeX-command-default "XeLaTeX")))
  (setq TeX-source-correlate-start-server t)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  ;; (setq TeX-tree-roots '("/var/lib/miktex-texmf/"
  ;;                        "/var/cache/miktex-texmf/"
  ;;                        "/usr/local/share/miktex-texmf/"
  ;;                        "~/.miktex/texmfs/config/"
  ;;                        "~/.miktex/texmfs/data/"
  ;;                        "~/.miktex/texmfs/install/"))
  (cond
   (*os-is-win*
    (add-to-list 'TeX-view-program-selection '(output-pdf "SumatraPDF")))
   (*os-is-gnu*
    (add-to-list 'TeX-view-program-selection '(output-pdf "Atril"))))
  (add-to-list 'TeX-view-program-selection '(output-pdf "pdf-tools"))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (use-package company-auctex
    :ensure t
    :init
    :config (add-hook 'LaTeX-mode-hook
                      (lambda () (local-push-company-backend '(company-auctex-macros company-auctex-symbols company-auctex-environments company-auctex-bibs company-auctex-labels))))))

(use-package cdlatex
  :ensure t
  :defer t
  :hook (latex-mode . turn-on-cdlatex)
  :config)

(use-package reftex
  :ensure t
  :defer t
  :hook (latex-mode . reftex-mode)
  :bind
  (:map reftex-mode-map
        ("C-c (" . reftex-citation))
  :config
  (setq reftex-default-bibliography '("~/Org/org-roam/Academic/Bib/mybib.bib"))
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

  (setq reftex-cite-format
      '(
        (?\C-m . "\\cite[]{%l}")
        (?t . "\\textcite{%l}")
        (?a . "\\autocite[]{%l}")
        (?p . "\\parencite{%l}")
        (?f . "\\footcite[][]{%l}")
        (?F . "\\fullcite[]{%l}")
        (?x . "[]{%l}")
        (?X . "{%l}")
        ))

(setq font-latex-match-reference-keywords
      '(("cite" "[{")
        ("cites" "[{}]")
        ("autocite" "[{")
        ("footcite" "[{")
        ("footcites" "[{")
        ("parencite" "[{")
        ("textcite" "[{")
        ("fullcite" "[{")
        ("citetitle" "[{")
        ("citetitles" "[{")
        ("headlessfullcite" "[{")))
  (setq reftex-enable-partial-scans t)
  (setq reftex-save-parse-info t)
  (setq reftex-use-multiple-selection-buffers t)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-toc-split-windows-horizontally t)
  (setq reftex-toc-split-windows-fraction 0.2)
  (setq reftex-cite-cleanup-optional-args t)
  (setq reftex-cite-prompt-optional-args t)); Prompt for empty optional arguments in cite

;; PDF reader
(when (display-graphic-p)
  (use-package pdf-view
    :ensure pdf-tools
    :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
    :defines pdf-annot-activate-created-annotations
    :functions (my-pdf-view-set-midnight-colors my-pdf-view-set-dark-theme)
    :commands pdf-view-midnight-minor-mode
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :bind (:map pdf-view-mode-map
           ("C-s" . isearch-forward))
    :init (setq pdf-annot-activate-created-annotations t)
    :config
    ;; WORKAROUND: Fix compilation errors on macOS.
    ;; @see https://github.com/politza/pdf-tools/issues/480
    (when *os-is-mac*
      (setenv "PKG_CONFIG_PATH"
              "/usr/local/lib/pkgconfig:/usr/local/opt/libffi/lib/pkgconfig"))
    (pdf-tools-install t nil t t)

    ;; Set dark theme
    (defun my-pdf-view-set-midnight-colors ()
      "Set pdf-view midnight colors."
      (setq pdf-view-midnight-colors
            `(,(face-foreground 'default) . ,(face-background 'default))))

    (defun my-pdf-view-set-dark-theme ()
      "Set pdf-view midnight theme as color theme."
      (my-pdf-view-set-midnight-colors)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (eq major-mode 'pdf-view-mode)
            (pdf-view-midnight-minor-mode (if pdf-view-midnight-minor-mode 1 -1))))))

    (my-pdf-view-set-midnight-colors)
    (add-hook 'after-load-theme-hook #'my-pdf-view-set-dark-theme)

    ;; FIXME: Support retina
    ;; @see https://emacs-china.org/t/pdf-tools-mac-retina-display/10243/
    ;; and https://github.com/politza/pdf-tools/pull/501/
    (setq pdf-view-use-scaling t
          pdf-view-use-imagemagick nil)
    (with-no-warnings
      (defun pdf-view-use-scaling-p ()
        "Return t if scaling should be used."
        (and (or (and (eq system-type 'darwin) (string-equal emacs-version "27.0.50"))
                 (memq (pdf-view-image-type)
                       '(imagemagick image-io)))
             pdf-view-use-scaling))
      (defun pdf-view-create-page (page &optional window)
        "Create an image of PAGE for display on WINDOW."
        (let* ((size (pdf-view-desired-image-size page window))
               (width (if (not (pdf-view-use-scaling-p))
                          (car size)
                        (* 2 (car size))))
               (data (pdf-cache-renderpage
                      page width width))
               (hotspots (pdf-view-apply-hotspot-functions
                          window page size)))
          (pdf-view-create-image data
            :width width
            :scale (if (pdf-view-use-scaling-p) 0.5 1)
            :map hotspots
            :pointer 'arrow))))

    ;; Recover last viewed position
    (when (>= emacs-major-version 26)
      (use-package pdf-view-restore
        :hook (pdf-view-mode . pdf-view-restore-mode)
        :init (setq pdf-view-restore-filename
                    (locate-user-emacs-file ".pdf-view-restore"))))))

(use-package ivy-bibtex
  :ensure t
  :defer t
  :after ivy
  :config
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "xdg-open" nil 0 nil fpath)))
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-default)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))
  (setq bibtex-completion-browser-function 'browse-url-default-browser)
  (setq bibtex-completion-bibliography
        '("~/Org/Academic/Bib/mybib.bib"))
  (setq bibtex-completion-notes-path "~/Org/Academic/Bib/")
  (setq bibtex-completion-library-path '("~/Org/Academic/Bib/src/")))

(use-package ebib
  :ensure t
  :config
  (setq ebib-common-optional-fields
        '(translator keywords origlanguage url file location
                     partinfo subtitle edition abstract note annotator
                     crossref urldate address subtitle language))

  (setq ebib-uniquify-keys t)
  (setq ebib-autogenerate-keys t)
  (setq ebib-index-window-size 20)
  (setq ebib-print-multiline t)

                                        ;index view
  (setq ebib-index-display-fields (quote (year author)))
  (setq ebib-sort-order (quote ((year) (author) )))

  )
(use-package academic-phrases
  :ensure t
  :defer t)

(provide 'init-latex)
;;; init-latex.el ends here
