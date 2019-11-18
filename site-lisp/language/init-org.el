;; init-org.el --- org-mode-setting.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; org-mode-setting.
;;

;;; Code:


(use-package org
  :ensure t
  :defer t
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :hook (org-indent-mode . (lambda()
                             (diminish 'org-indent-mode)
                             ;; WORKAROUND: Prevent text moving around while using brackets
                             ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                             (make-variable-buffer-local 'show-paren-mode)
                             (setq show-paren-mode nil)))
  :config
  (defun turn-on-org-show-all-inline-images ()
    (org-display-inline-images t t))

  (with-eval-after-load 'f
    (defun paste-markdown-to-org (&optional arg)
      "Yank markdown to org."
      (interactive "*P")
      (f-write-text
       (replace-regexp-in-string
        "\\(?1:`\\)" "\\1"
        (current-kill
         (cond ((listp arg) 0)
               ((eq arg '-) -2)
               (t (1- arg)))))
       'utf-8' "/tmp/pastetmp496")
      (insert
       (shell-command-to-string "pandoc -f markdown -t org /tmp/pastetmp496"))
      (f-delete "/tmp/pastetmp496" t))
    (defun paste-html-to-org (&optional arg)
      "Yank html to org."
      (interactive "*P")
      (f-write-text
       (current-kill
        (cond ((listp arg) 0)
              ((eq arg '-) -2)
              (t (1- arg))))
       'utf-8' "/tmp/pastetmp496")
      (insert
       (shell-command-to-string "pandoc -f html -t org /tmp/pastetmp496"))
      (f-delete "/tmp/pastetmp496" t))
    )
  (defun turn-off-truncate-lines ()
    (setq truncate-lines nil))
  (add-hook 'org-mode-hook 'turn-on-org-show-all-inline-images)
  (add-hook 'org-mode-hook 'turn-off-truncate-lines)
  (setq org-agenda-files '("~/Org/Agenda" "~/Org/Capture"))
  (setq org-capture-files '("~/Org/Capture"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
          (sequence "TODO(t)" "STARTED(s)" "DOING(i)" "HOLD(h)" "|" "DONE(d!/!)" "CANCEL(c@/!)")
          (sequence "PROJECT(p)" "WAITING(w@/!)" "HOLD(h)" "DELEGATED(e!)" "|" "TOPAY(l!/!)" "DONE(d!/!)" "CANCEL(c@/!)")
          (sequence "REMIND(r)" "|" "CANCEL(c@/!)")
          (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
        org-todo-repeat-to-state "NEXT")

  (setq org-highest-priority ?A
        org-lowest-priority ?D
        org-default-priority ?C
        org-priority-faces '((?A . (:foreground "DarkOrange"))
                             (?B . (:foreground "WhiteSmoke"))
                             (?C . (:foreground "DarkViolet"))
                             (?D . (:foreground "LightGreen"))))

  (setq org-tag-alist '(("WORK" . ?w) ("PROJECT" . ?p) ("DAILY" . ?d)))

  (setq org-stuck-projects
        '("+PROJECT/-MAYBE-DONE" ("NEXT" "TODO" "STARTED" "DOING" "DELEGATED") nil nil))

  (setq org-agenda-custom-commands
        '(
          ("x" "Reminder" todo "REMIND")
          ("i" "正在做" todo "DOING")
          ("w" . "任务安排")
          ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
          ("wb" "重要但不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
          ("wc" "紧急但不重要的任务" tags-todo "+PRIORITY=\"C\"")
          ("p" . "项目安排")
          ("pa" "未完成工作" tags-todo "+WORK-DONE")
          ("pb" "待交付工作" tags-todo "WORK&PROJECT&TOPAY")
          ("pc" "未完成项目" tags-todo "+PROJECT-DONE-DELEGATED-CANCEL")
          ("pd" "待交付项目" tags-todo "PROJECT&TOPAY")
          ("W" "Weekly Review"
           ((stuck "") (tags-todo "PROJECT") (todo "TOPAY") (todo "DONE")))
          ))

  (setq org-capture-templates
        `(("t" "Todo" entry (file+headline "~/Org/Capture/gtd.org" "Tasks")
           "* TODO [#B] %?\n%U\n"
           :clock-resume t
           :empty-lines 1)
          ("n" "Note" entry (file "~/Org/Capture/note.org")
           "* %? :NOTE:\n%U\n%a\n"
           :clock-resume t
           :empty-lines 1)
          ("s" "Code Snippet" entry (file "~/Org/Capture/snippet.org")
           "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
          ("j" "Journal" entry (file+datetree "~/Org/Capture/journal.org")
           "* %?"
           :empty-lines 1)
          ))

  (setq org-log-done 'time)
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-support-shift-select 'always)

  (use-package ox-gfm
    :ensure t
    :after org
    :init)

  ;; (add-to-list 'org-export-backends 'md)

  ;; More fancy UI
  (use-package org-bullets
    :ensure t
    :hook (org-mode . org-bullets-mode)
    :init
    (setq
     org-bullets-bullet-list
     '(;;; Large
       "●"
       "◉"
       "○"
       "✸"
       ;; "✿"
       ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;;; Small
       ;; ► • ★ ▸
       )))

  (use-package htmlize
    :ensure t
    :init)

  (use-package org-preview-html
    :ensure t
    :after org
    :diminish org-preview-html-mode)

  (use-package org-pomodoro
    :ensure t
    :after org-agenda
    :bind (:map org-agenda-mode-map
                ("P" . org-pomodoro)))


  ;; Babel Settings
  ;; 按下C-c ’ 后进入 Org-src block 下写代码
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (defvar load-language-list '((plantuml . t)
                               (dot . t)
                               (ditaa . t)
                               (gnuplot . t)
                               (shell . t)
                               (awk . t)
                               (sed . t)
                               (sqlite . t)
                               (org . t)
                               (latex . t)
                               (emacs-lisp . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (js . t)
                               (python . t)
                               (ruby . t)))
  (setq org-babel-python-command "python3")
  ;;ob-plantuml
  (let ((jar-name "plantuml.jar")
        (url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    (setq org-plantuml-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
    (unless (file-exists-p org-plantuml-jar-path)
      (url-copy-file url org-plantuml-jar-path)))
  ;;ob-ditaa
  (defun iorest/grab-ditaa (url jar-name)
    "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
    ;; TODO: handle errors
    (message "Grabbing %s for org." jar-name)
    (let ((zip-temp (make-temp-name "emacs-ditaa")))
      (unwind-protect
          (progn
            (when (executable-find "unzip")
              (url-copy-file url zip-temp)
              (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                     " " (shell-quote-argument jar-name) " > "
                                     (shell-quote-argument org-ditaa-jar-path)))))
        (when (file-exists-p zip-temp)
          (delete-file zip-temp)))))

  (unless (and (boundp 'org-ditaa-jar-path)
               (file-exists-p org-ditaa-jar-path))
    (let ((jar-name "ditaa0_9.jar")
          (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
      (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
      (unless (file-exists-p org-ditaa-jar-path)
        (iorest/grab-ditaa url jar-name))))

  (use-package org-download
    :ensure t
    :after org
    :commands (org-download-enable
               org-download-yank
               org-download-screenshot)
    :config
    (setq-default org-download-image-dir "./img")
    (defun org-download-image-at-point ()
      (interactive)
      (let* ((link-info (assoc :link (org-context)))
             (text (when link-info
                     (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                     (or (caddr link-info) (point-max))))))
        (if (not text)
            (error "Not in org link")

          (string-match org-bracket-link-regexp text)
          (beginning-of-line)
          (org-download-image (substring text (match-beginning 1) (match-end 1)))
          (delete-region (point) (line-end-position))
          )))
    :hook ((org-mode dired-mode) . org-download-enable))
  ;;ob-go
  (use-package ob-go
    :ensure t
    :after org
    :init
    (if (executable-find "go")
        (push '(go . t) load-language-list)))
  ;;ob-rust
  (use-package ob-rust
    :ensure t
    :after org
    :init
    (if (executable-find "rustc")
        (push '(rust . t) load-language-list)))
  ;;ob-ipython
  (use-package ob-ipython
    :ensure t
    :after org
    :init
    (if (executable-find "jupyter")
        (push '(ipython . t) load-language-list)))
  ;;ob-elixir
  (use-package ob-elixir
    :ensure t
    :after org
    :init
    (if (executable-find "elixir")
        (push '(elixir . t) load-language-list)))
  ;; ob-tmux
  (use-package ob-tmux
    :ensure t
    :after org
    :init
    (if (executable-find "tmux")
        (push '(tmux . t) load-language-list))
    :custom
    (org-babel-default-header-args:tmux
     '((:session . "default")      ; The default tmux session to send code to
       (:socket  . nil)            ; The default tmux socket to communicate with
       (:terminal . "guake")))
    (org-babel-tmux-session-prefix "ob-")
    (org-babel-tmux-location "/usr/bin/tmux"))
  ;;ob-http
  (use-package ob-http
    :ensure t
    :after org
    :init
    (if (executable-find "curl")
        (push '(http . t) load-language-list)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Export Settings
  (use-package ox-pandoc
    :ensure t
    :init
    :after ox
    :config
    (setq org-pandoc-options '((standalone . t)))
    (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex") (variable . "CJKmainfont:WenQuanYi Micro Hei")))
    (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex") (variable . "CJKmainfont:WenQuanYi Micro Hei")))
    (setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))
    (setq org-pandoc-options-for-revealjs '((variable . "revealjs-url:https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.8.0/")))
    (setq org-pandoc-options-for-slideous '((variable . "slideous-url:http://goessner.net/download/prj/slideous/")))
    (setq org-pandoc-options-for-slidy '((variable . "slidy-url:http://www.w3.org/Talks/Tools/Slidy2/"))))

  (use-package ox-reveal
    :ensure t
    :after ox
    :config
    (setq org-reveal-root "https://cdn.bootcss.com/reveal.js/3.8.0/"))

  (require 'ox-publish)
  (org-link-set-parameters
   "cite"
   :follow (lambda (path) (message "Citation to Export."))
   :export (lambda (path desc format)
             (cond
              ((eq format 'html)
               (format "(<cite>%s</cite>)" path))
              ((eq format 'latex)
               (if (or (not desc) (equal 0 (search "cite:" desc)))
                   (format "\\cite{%s}" path)
                 (format "\\cite[%s]{%s}" desc path)))))
   :face '(:foreground "red")
   :help-echo "Insert a Citation.")

  (setq org-publish-project-alist
        '(("org-notes"
           :base-directory "~/Org/Notes/"
           :base-extension "org"
           :publishing-directory "~/Org/Publish/Notes/"
           :recursive t
           :publishing-function org-html-publish-to-html
                                        ;:auto-sitemap t
                                        ;:sitemap-filename "sitemap.org"
                                        ;:sitemap-title "Sitemap"
                                        ;:sitemap-sort-folders last
           :headline-levels 4)
          ("org-static"
           :base-directory "~/Org/Notes/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/Org/Publish/Notes/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("org-site" :components ("org-notes" "org-static"))
          ("org-books"
           :base-directory "~/Org/Notes/"
           :base-extension "org"
           :publishing-directory "~/Org/Publish/Books/"
           :recursive t
           :publishing-function org-latex-publish-to-pdf
           :headline-levels 4)))

  (with-eval-after-load 'org
    (require 'ox-beamer))

  (add-to-list
   'org-latex-classes
   '("myart"
     "
\\documentclass{article}
[NO-DEFAULT-PACKAGES]
\\usepackage[UTF8]{ctex}
\\usepackage{titlesec}
\\usepackage{minted}
\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage{babel}
\\usepackage{indentfirst}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage[figuresright]{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{amssymb}
\\usepackage{tabularx}
\\usepackage{booktabs}
\\usepackage{makeidx}
\\usepackage[bookmarks=true,colorlinks,linkcolor=red,anchorcolor=black,citecolor=blue]{hyperref}
\\usepackage{listings}
\\usepackage[a4paper]{geometry}
\\usepackage{xcolor}
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\lfoot{}
\\cfoot{\\thepage}
\\rfoot{}
%如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
\\defaultfontfeatures[\\rmfamily]{Mapping=tex-text}
% 中文断行
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt
%\\setmainfont{Times New Roman} % 英文衬线字体
%\\setsansfont{DejaVu Serif} % 英文无衬线字体
\\setmonofont{Fira Code Retina}
%\\setCJKmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
%\\setCJKsansfont{WenQuanYi Micro Hei}
%\\setCJKmonofont{WenQuanYi Micro Hei Mono}
%让 verbatim 自动换行
\\makeatletter
\\def\\@xobeysp{\\mbox{}\\space}
\\def\\verbatim@font{\\normalfont\\ttfamily\\raggedright\\leftskip\\@totalleftmargin}
\\makeatother
[EXTRA]
"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


  (add-to-list
   'org-latex-classes
   '("mybook"
     "
\\documentclass{book}
[NO-DEFAULT-PACKAGES]
\\usepackage[UTF8]{ctex}
\\usepackage{titlesec}
\\usepackage{minted}
\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage{babel}
\\usepackage{indentfirst}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage[figuresright]{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{amssymb}
\\usepackage{tabularx}
\\usepackage{booktabs}
\\usepackage{makeidx}
\\usepackage[bookmarks=true,colorlinks,linkcolor=red,anchorcolor=black,citecolor=blue]{hyperref}
\\usepackage{listings}
\\usepackage[a4paper]{geometry}
\\usepackage{xcolor}
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\lfoot{}
\\cfoot{\\thepage}
\\rfoot{}
%如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
\\defaultfontfeatures[\\rmfamily]{Mapping=tex-text}
% 中文断行
\\XeTeXlinebreaklocale \"zh\"
\\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt
%\\setmainfont{Times New Roman} % 英文衬线字体
%\\setsansfont{DejaVu Serif} % 英文无衬线字体
\\setmonofont{Fira Code Retina}
%\\setCJKmainfont{WenQuanYi Micro Hei} % 设置缺省中文字体
%\\setCJKsansfont{WenQuanYi Micro Hei}
%\\setCJKmonofont{WenQuanYi Micro Hei Mono}
%让 verbatim 自动换行
\\makeatletter
\\def\\@xobeysp{\\mbox{}\\space}
\\def\\verbatim@font{\\normalfont\\ttfamily\\raggedright\\leftskip\\@totalleftmargin}
\\makeatother
[EXTRA]
"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list
   'org-latex-classes
   '("mybeamer"
     "
\\documentclass{beamer}
[NO-DEFAULT-PACKAGES]
% beamer set
\\usepackage{minted}
\\usepackage[none]{hyphenat}
\\usepackage[abs]{overpic}
\\usepackage{titlesec}
\\usepackage{minted}
\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage{babel}
\\usepackage{indentfirst}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage[figuresright]{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{amssymb}
\\usepackage{tabularx}
\\usepackage{booktabs}
\\usepackage{makeidx}
\\usepackage[UTF8]{ctex}
\\usepackage[bookmarks=true,colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
\\usepackage{listings}
\\usepackage[a4paper]{geometry}
\\usepackage{xcolor}
%\\setmainfont{Times New Roman} % 英文衬线字体
%\\setsansfont{DejaVu Serif} % 英文无衬线字体
\\setmonofont{Fira Code Retina}
%\\setCJKmainfont{WenQuanYi Micro Hei}
%\\setCJKsansfont{WenQuanYi Micro Hei}
%\\setCJKmonofont{WenQuanYi Micro Hei Mono}
%让 verbatim 自动换行
\\makeatletter
\\def\\@xobeysp{\\mbox{}\\space}
\\def\\verbatim@font{\\normalfont\\ttfamily\\raggedright\\leftskip\\@totalleftmargin}
\\makeatother
[EXTRA]
"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-default-class "myart")
  (setq org-latex-default-table-environment "longtable")
  (setq org-export-with-smart-quotes t)
  (setq org-latex-pdf-process
        '(
          "xelatex -interaction nonstopmode --shell-escape -output-directory %o %f"
          "bibtex %b"
          "xelatex -interaction nonstopmode --shell-escape -output-directory %o %f"
          "xelatex -interaction nonstopmode --shell-escape -output-directory %o %f"
          "rm -fr %b.out %b.log %b.tex %b.bbl _minted-* auto"))
  (setq org-latex-listings-options
        '(("numbers" "left")
          ("breaklines" "true")
          ("postbreak" "\\mbox{{$\\hookrightarrow$}\\space}")
          ("frame" "shadowbox")
          ("upquote" "true")
          ("rulesepcolor" "\\color{red!20!green!20!blue!20}")
          ;; ("backgroundcolor" "\\color[rgb]{1,1,0.76}")
          ("backgroundcolor" "\\color[RGB]{245,245,244}")
          ("keywordstyle" "\\bf\\color{blue}")
          ("showstringspaces" "false")
          ("basicstyle" "\\ttfamily")
          ("identifierstyle" "\\bf")
          ("numberstyle" "\\color[RGB]{0,192,192}")
          ("commentstyle" "\\it\\color[RGB]{0,96,96}")
          ("stringstyle" "\\ttfamily\\slshape\\color[RGB]{128,0,0}")))
  (setq org-latex-listings t))

(use-package deft
  :ensure t
  :defer t
  :config
  (setq deft-extensions '("org" "txt" "text" "md" "markdown")
        deft-default-extension "org"
        deft-use-filename-as-title t
        deft-auto-save-interval 0
        deft-recursive t
        deft-directory "~/Org/Notes")
  )

(use-package org-web-tools
  :ensure t
  :defer t)

(use-package yankpad
  :ensure t
  :defer t
  :init
  (setq yankpad-file "~/Org/yankpad.org"))

(provide 'init-org)
;;; init-org.el ends here
