;;; felix-init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Custom functions
(defun open-with-vscode ()
  "Open current file with vscode."
  (interactive)
  (let ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column))))
    (apply 'call-process "code" nil nil nil (list (concat buffer-file-name ":" line ":" column) "--goto"))))

;; rainbow
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; which key
(add-hook 'after-init-hook 'which-key-mode)

;;; helpful
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

;;; smart-hungry-delete
(smart-hungry-delete-add-default-hooks)
(global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
(global-set-key (kbd "C-d") 'smart-hungry-delete-forward-char)

;;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;;; files
(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))

(add-hook 'csv-mode-hook 'csv-align-mode)

(add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;; yasnippet
(require 'yasnippet)
(require 'yasnippet-snippets)
(require 'yatemplate)

(setq yas-snippet-dirs
    '("~/.emacs.d/snippets"))

(add-hook 'after-init-hook 'yas-global-mode)

;;; lsp
(require 'lsp-mode)
(add-hook 'sh-mode-hook #'lsp)
(add-hook 'python-mode #'lsp)

;;; company
(add-hook 'after-init-hook 'global-company-mode)

;;; exec-path-from-shell
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; leetcode
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/leetcode")

(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)

;;; ob-ditaa
(setq ob-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar"
      org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar")

;;; python
(setq org-babel-python-command "python3")
(setq org-babel-clojure-backend 'cider)

;;; easy gnupg
(setq epa-pinentry-mode 'loopback)
(pinentry-start)

;;; hledger
(require 'hledger-mode)
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
(setq hledger-jfile "~/org/felix/finance/finance.journal")

;;; rime
(setq default-input-method "rime")
(setq rime-librime-root "~/.emacs.d/librime/dist")
(setq rime-user-data-dir "~/Library/Rime")

;;; lisp/*
(require 'color-rg)
(require 'sudo-edit)
(require 'thing-edit)
(require 'password-generator)
;; auto-save
(require 'auto-save)
(auto-save-enable)
(setq auto-save-slient t)
;; geoip
(require 'geoip)
;; ecloud
(require 'ecloud)

;;; keybindings
(global-set-key (kbd "C-x g") #'magit-status)

(provide 'felix-init)
;;; felix-init.el ends here
