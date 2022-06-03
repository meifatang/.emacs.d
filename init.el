;;; init.el --- Felix's Emacs config -*- lexical-binding: t -*-

;; Author: Felix M. Tang
;; Maintainer: Felix M. Tang
;; Version: 0.0.5
;; Package-Requires:
;; Homepage: https://github.com/meifatang/.emacs.d/
;; Keywords: felix emacs init


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; My Emacs Config

;;; Code:
(defun add-folder-to-load-path (folder)
  "Add folder and subdirs to the `load-path'."
  (unless (member folder load-path)
    (add-to-list 'load-path folder))
  (dolist (f (directory-files folder))
    (let ((name (expand-file-name f folder)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-folder-to-load-path name)))))

;; (add-folder-to-load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-folder-to-load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq gc-cons-threshold 10000000000)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-visual-line-mode)
(midnight-mode)
(winner-mode)
(server-start)

(add-hook 'before-save-hook 'org-update-all-dblocks)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(require 'felix-package) ;; define which package to install
(require 'felix-theme) ;; define theme
(require 'felix-zero) ;; basic third party package config
(require 'felix-one) ;; all in enhancement
(require 'felix-auto-save)
(require 'felix-org) ;; all in org

(require 'ecloud) ;; try to contribute it
(require 'felix-geoip)

(require 'felix-keybindings)
(require 'felix-functions)

(when (file-exists-p (expand-file-name "init-private.el" user-emacs-directory))
  (load-file (expand-file-name "init-private.el" user-emacs-directory)))

(provide 'init)
;;; init.el ends here
