;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; load-path
(defun add-folder-to-load-path (folder)
  "Add folder and subdirs to the `load-path'."
  (unless (member folder load-path)
    (add-to-list 'load-path folder))
  (dolist (f (directory-files folder))
    (let ((name (expand-file-name f folder)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-folder-to-load-path name)))))

(add-folder-to-load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-folder-to-load-path (expand-file-name "lisp" user-emacs-directory))

;; custom custom.el file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; startup buffer and scratch
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; ui
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; warp
(global-visual-line-mode)

;; enable server and midnight mode
(server-start)
(midnight-mode)
;; fullscreen shortcut
(global-set-key (kbd "C-s-f") #'toggle-frame-fullscreen)
;; define gc
(setq gc-cons-threshold 1000000000) ;; 100M

;; Package management
(add-hook 'after-init-hook (lambda () (progn (require 'packages))))
(require 'felix-packages)
(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Require other config files
(require 'felix-init)

(require 'felix-ivy)

(require 'felix-org)
(require 'felix-publish)

;; init-private.el
(when (file-exists-p (expand-file-name "init-private.el" user-emacs-directory))
  (load-file (expand-file-name "init-private.el" user-emacs-directory)))

(provide 'init)
;;; init.el ends here
