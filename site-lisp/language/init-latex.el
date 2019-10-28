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
            (lambda ()   (setq TeX-command-default "XeLaTeX")) )

  (setq TeX-source-correlate-start-server t)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-tree-roots '("/var/lib/miktex-texmf/"
                         "/var/cache/miktex-texmf/"
                         "/usr/local/share/miktex-texmf/"
                         "~/.miktex/texmfs/config/"
                         "~/.miktex/texmfs/data/"
                         "~/.miktex/texmfs/install/"))
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

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-toc-split-windows-horizontally t)
  (setq reftex-toc-split-windows-fraction 0.2)
  (setq reftex-cite-prompt-optional-args t)); Prompt for empty optional arguments in cite

(use-package pdf-tools
  :ensure t
  :defer t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :init (setq pdf-annot-activate-created-annotations t)
  :config (pdf-tools-install t nil t nil))

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
        '("~/Org/Bib/mybib.bib"))
  (setq bibtex-completion-notes-path "~/Org/Bib/")
  (setq bibtex-completion-library-path '("~/Org/Bib/src/")))

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
