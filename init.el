;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
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

(add-folder-to-load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-folder-to-load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq make-backup-files nil)
(setq auto-save-default nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(global-visual-line-mode)

(server-start)

(global-set-key (kbd "C-s-f") #'toggle-frame-fullscreen)

(setq gc-cons-threshold 1000000000) ;; 100M

;; For packages
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

(require 'felix)

(provide 'init)
;;; init.el ends here
