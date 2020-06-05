;; init.el --- emacs config file.	-*- lexical-binding: t -*-
;; Copyright (C) 2018 Leaforest




;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; ui-setting.
;;

;;; Code:

;; Enforce minimum Emacs version
(let ((min-version "25.3"))
  (when (version< emacs-version min-version)
    (error "Gnu Emacs %s or newer is required" min-version)))

;; name and mail address
(setq user-full-name "Iorest"
      user-mail-address "vincent.leaforest@gmail.com")

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp" "core"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)
(update-load-path)
(add-subdirs-to-load-path)

(require 'init-utils)
(require 'core-packages)
(require 'init-time)
(require 'server)

(defun my-server-shunt ()
  "Shunts to emacsclient."
  (let ((args (append '("emacsclient" "-a" "\"\"" "-n")
                      (cdr command-line-args))))
    (shell-command (substring (format "%S" args) 1 -1))
    (kill-emacs)))

;; Keep only one Emacs server instance
(if (server-running-p)
    (if (daemonp)
        (error "Another running Emacs server detected, abort")
      (my-server-shunt))
  (server-start))

(require 'init-bindings)
(require 'core-ide)
(require 'core-ui)
(require 'core-edit)
(require 'core-evil)
(require 'core-add-on)




;;; init.el ends here
