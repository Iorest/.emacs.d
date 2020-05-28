;; init-utils.el --- utils.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; utils.
;;

;;; Code:


(eval-when-compile
  (require 'init-custom)
  (require 'cl))

(defun iorest/add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (remove-if-not
              (lambda (dir) (file-directory-p dir))
              (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
             load-path)))))

(iorest/add-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))

;; coding system

(defun iorest/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun iorest/locale-is-utf8-p ()
  "Return t if the \"locale\" command or environment variables prefer UTF-8."
  (or (iorest/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (iorest/utf8-locale-p (getenv "LC_ALL"))
      (iorest/utf8-locale-p (getenv "LC_CTYPE"))
      (iorest/utf8-locale-p (getenv "LANG"))))

(when (or window-system (iorest/locale-is-utf8-p))
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if *os-is-win* 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))


(defun iorest/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun iorest/align-to-colon (begin end)
  "Align region to colon (:) signs."
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) ":") 1 1 ))

(defun iorest/align-to-comma (begin end)
  "Align region to comma signs."
  (interactive "r")
  (align-regexp begin end
                (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 ))


(defun iorest/align-to-equals (begin end)
  "Align region to equal signs."
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) "=") 1 1 ))

(defun iorest/align-to-hash (begin end)
  "Align region to hash ( => ) signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) "=>") 1 1 ))

;; work with this
(defun iorest/align-to-comma-before (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) ",") 1 1 ))

(defun iorest/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (iorest/indent-buffer)
        (message "Indented buffer.")))))

(defun iorest/insert-date (format)
  "Wrapper around `format-time-string`."
  (interactive "MFormat: ")
  (insert (format-time-string format)))

(defun iorest/insert-buffer-hash ()
  "Insert hash string of buffer."
  (interactive)
  (insert (buffer-hash)))

(defun iorest/sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun iorest/sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun iorest/reverse-words (beg end)
  "Reverse the order of words in region between BEG and END."
  (interactive "*r")
  (apply
   'insert
   (reverse
    (split-string
     (delete-and-extract-region beg end) "\\b"))))

(defun iorest/randomize-region (beg end)
  "Randomize the order of words in region buffer BEG and END."
  (interactive "*r")
  (let ((all (mapcar
              (lambda (w) (if (string-match "\\w" w)
                         ;; Randomize words,
                         (cons (random) w)
                       ;; keep everything else in order.
                       (cons -1 w)))
              (split-string
               (delete-and-extract-region beg end) "\\b")))
        words sorted)
    (mapc (lambda (x)
            ;; Words are numbers >= 0.
            (unless (> 0 (car x))
              (setq words (cons x words))))
          all)
    ;; Random sort!
    (setq sorted (sort words
                       (lambda (a b) (< (car a) (car b)))))
    (mapc
     'insert
     ;; Insert using original list, `all',
     ;; but pull *words* from randomly-sorted list, `sorted'.
     (mapcar (lambda (x)
               (if (> 0 (car x))
                   (cdr x)
                 (prog1 (cdar sorted)
                   (setq sorted (cdr sorted)))))
             all))))

(defun iorest/randomize-comma-separated-list (start end)
  "Randomize order of the comma separated list in the region between START and END."
  (interactive "r")
  (let ((str (mapconcat 'identity
                        (sort (split-string (replace-regexp-in-string
                                             "\\s-+" " "
                                             (buffer-substring start end))
                                            " ?, ?" t)
                              (lambda (a b) (= 1 (random 2))))
                        ", ")))
    (delete-region start end)
    (insert str)))

(defun iorest/randomize-string (string)
  "Randomize a STRING. Equivalent to glibc's strfry()."
  (interactive)
  (let ((i 0)
        (char " ")
        (size (string-width string)))
    (while (< i size)
      (let ((j (random size)))
        (store-substring char 0 (substring string i (+ 1 i)))
        (store-substring string i (substring string j (+ 1 j)))
        (store-substring string j char)
        (setq i (+ 1 i))))
    string))

(defun iorest/html-decode-percent-encoded-url ()
  "Decode percent encoded URI of URI under cursor or selection.

Example:
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_%28D%C3%BCrer%29
becomes
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_(Dürer)

Example:
    http://zh.wikipedia.org/wiki/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8
becomes
    http://zh.wikipedia.org/wiki/文本编辑器"
  (interactive)
  (let (-boundaries -p1 -p2 -input-str)
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (progn
        (setq -boundaries (bounds-of-thing-at-point 'url))
        (setq -p1 (car -boundaries))
        (setq -p2 (cdr -boundaries))))
    (setq -input-str (buffer-substring-no-properties -p1 -p2))
    (require 'url-util)
    (delete-region -p1 -p2)
    (insert (decode-coding-string (url-unhex-string -input-str) 'utf-8))))

(defun iorest/html-encode-percent-encoded-url ()
  "Percent encode URL under cursor or selection.

Example:
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_(Dürer)
becomes
    http://en.wikipedia.org/wiki/Saint_Jerome_in_His_Study_(D%C3%BCrer)

Example:
    http://zh.wikipedia.org/wiki/文本编辑器
becomes
    http://zh.wikipedia.org/wiki/%E6%96%87%E6%9C%AC%E7%BC%96%E8%BE%91%E5%99%A8"
  (interactive)
  (let (-boundaries -p1 -p2 -input-str)
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (progn
        (setq -boundaries (bounds-of-thing-at-point 'url))
        (setq -p1 (car -boundaries))
        (setq -p2 (cdr -boundaries))))
    (setq -input-str (buffer-substring-no-properties -p1 -p2))
    (require 'url-util)
    (delete-region -p1 -p2)
    (insert (url-encode-url -input-str))))

(setq kmacro-file (expand-file-name "kmacro.el" user-emacs-directory))
(defun kmacro-save-macro (name)
  "NAME and save macro to file."
  (interactive "SName of the macro: ")
  (kmacro-name-last-macro name)
  (find-file kmacro-file)
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline)
  (save-buffer)
  (kill-buffer nil))

(when (file-exists-p kmacro-file)
  (load kmacro-file))


(defun markdown-export-to-org ()
  "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (format "pandoc -f markdown -t org |sed -E '/^[[:space:]]+:/ d'> %s"
                                   (concat (file-name-sans-extension (buffer-file-name)) ".org")))
  )

(defun switch-to-scratch-buffer ()
  "Toggle between *scratch* buffer and the current buffer.
     If the *scratch* buffer does not exist, create it."
  (interactive)
  (let ((scratch-buffer-name  "*scratch*")
        (prev-major-mode major-mode)
        )
    (if (equal (buffer-name (current-buffer)) scratch-buffer-name)
        (switch-to-buffer (other-buffer))
      (with-current-buffer
          (switch-to-buffer  scratch-buffer-name)
        (when (functionp prev-major-mode) (funcall prev-major-mode ))
        (when (equal major-mode 'fundamental-mode )(emacs-lisp-mode))
        (goto-char (point-max))))))

(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is \"%s\"" iorest-proxy)
    (message "No HTTP proxy")))



(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services `(("http" . ,iorest-proxy)
                             ("https" . ,iorest-proxy)
                             ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (proxy-http-disable)
    (proxy-http-enable)))

(defvar socks-noproxy)
(defvar socks-server)
(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if socks-noproxy
      (message "Current SOCKS%d proxy is %s:%d"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'socks)
  (setq socks-noproxy '("localhost"))
  (setq socks-server '("Default server" "127.0.0.1" 10800 5))
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native)
  (setq socks-noproxy nil)
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if socks-noproxy
      (proxy-socks-disable)
    (proxy-socks-enable)))

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defun kill-to-begin()
  "Kill from point to begin of buffer."
  (interactive)
  (let ((prev-pos (point)))
    (goto-char (point-min))
    (delete-region (point) prev-pos)))

(defun kill-to-end()
  "Kill from point to end of buffer."
  (interactive)
  (let ((prev-pos (point)))
    (goto-char (point-max))
    (delete-region (point) prev-pos)))

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; Revert buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (text-scale-increase 0)
    (widen)
    (if (and (fboundp 'fancy-narrow-active-p)
             (fancy-narrow-active-p))
        (fancy-widen))
    (revert-buffer t t)
    (message "Reverted this buffer.")))
(bind-key "<f5>" #'revert-this-buffer)
(if *os-is-mac*
    (bind-key "s-r" #'revert-this-buffer))

(defun upload-notes ()
  "Update Org files to the latest version."
  (interactive)
  (let ((dir (expand-file-name "~/Org/Notes/")))
    (if (file-exists-p dir)
        (progn
          (message "Upload Org Notes...")
          (cd dir)
          (shell-command "git add .")
          (shell-command "git commit -m 'update note'")
          (shell-command "git push origin master")
          (message "Update finished."))
      (message "Update error"))))

;; Open custom file
(defun open-custom-file()
  "Open custom.el if exists, otherwise create it."
  (interactive)
  (let ((custom-example (expand-file-name "custom-example.el" user-emacs-directory)))
    (if (not (file-exists-p custom-file))
        (if (file-exists-p custom-example)
            (copy-file custom-file)
          (error "Unable to find custom-example.el")))
    (find-file custom-file)))

;; Create a new scratch buffer
(defalias 'create-scratch-buffer 'crux-create-scratch-buffer)

(provide 'init-utils)
;;; init-utils.el ends here.
