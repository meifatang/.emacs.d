;;; felix-zero.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  ;;(setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(add-hook 'after-init-hook 'which-key-mode)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

(advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;;; smart-hungry-delete
;; (smart-hungry-delete-add-default-hooks)
;; (global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
;; (global-set-key (kbd "C-d") 'smart-hungry-delete-forward-char)

;;; expand-region
;;(global-set-key (kbd "C-=") 'er/expand-region)

;;; files
;;(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))

;; (add-hook 'csv-mode-hook 'csv-align-mode)

;; (add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

;; (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;(setq inferior-lisp-program "/usr/local/bin/sbcl")
;;(setq slime-contribs '(slime-fancy))

(require 'yasnippet)
(require 'yasnippet-snippets)
(require 'yatemplate)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(add-hook 'after-init-hook 'yas-global-mode)

;; (require 'lsp-mode)
;; (add-hook 'sh-mode-hook 'lsp)
;; (add-hook 'python-mode 'lsp)

(add-hook 'after-init-hook 'global-company-mode)

(setq leetcode-save-solutions t)
(setq leetcode-directory "~/leetcode")

(setq ob-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar"
      org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar")

(setq org-babel-python-command "python3")

;; (setq org-babel-clojure-backend 'cider)

(setq epa-pinentry-mode 'loopback)
(pinentry-start)

;; (require 'hledger-mode)
;; (add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
;; (setq hledger-jfile "~/org/felix/finance/finance.journal")

;; (setq default-input-method "rime")
;; (setq rime-librime-root "~/.emacs.d/librime/dist")
;; (setq rime-user-data-dir "~/Library/Rime")

;;(require 'ox-moderncv)

(provide 'felix-zero)
;;; felix-zero.el ends here
