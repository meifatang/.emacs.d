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

;;(setq save-interprogram-paste-before-kill t)

(setq gc-cons-threshold 1000000000) ;; 100M

(require 'packages)

(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'auto-dark)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(add-hook 'after-init-hook 'which-key-mode)

(ivy-mode)

(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
(setq ivy-initial-inputs-alist '())

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(setq search-default-mode #'char-fold-to-regexp)

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(require 'ivy-rich)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

(smart-hungry-delete-add-default-hooks)
(global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
(global-set-key (kbd "C-d") 'smart-hungry-delete-forward-char)

(global-set-key (kbd "C-=") 'er/expand-region)

(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))

(add-hook 'csv-mode-hook 'csv-align-mode)

(add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

(require 'yasnippet)
(require 'yasnippet-snippets)
(require 'yatemplate)

(setq yas-snippet-dirs
    '("~/.emacs.d/snippets"))

(add-hook 'after-init-hook 'yas-global-mode)

(require 'lsp-mode)
(add-hook 'sh-mode-hook #'lsp)
(add-hook 'python-mode #'lsp)

(add-hook 'after-init-hook 'global-company-mode)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq leetcode-save-solutions t)
(setq leetcode-directory "~/leetcode")

(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)

(setq ob-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar"
      org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar")

(setq org-babel-python-command "python3")
(setq org-babel-clojure-backend 'cider)

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (awk . t)
   (shell . t)
   (go . t)
   (lisp . t)
   (http . t)
   (clojure . t)
   (sql . t)
   (js . t)
   (gnuplot . t)
   (ditaa . t)
   (dot . t)
   (elixir . t)
   (julia . t)
   (R . t)
   (octave . t)
   (java . t)
   (lua . t)
   (haskell . t)
   (fortran . t)
   (ruby . t)
   (rust . t)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))

(setq org-log-done 'time)
(setq org-log-done 'note)

(setq org-capture-templates
      '(
        ;; GTD
        ("g" "Gettting Things Done" entry (file+headline "~/org/felix/gtd.org" "Tasks")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n  %i  %a\n")

        ;; Reading, Learning
        ("r" "Reading List" entry (file+headline "~/org/felix/gtd.org" "Reading")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n  %i  %a\n")
        ("l" "Learn" entry (file+headline "~/org/felix/gtd.org" "Learning")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n  %i  %a\n")

        ;; Inbox, Journal
        ("i" "Inbox" entry (file+olp+datetree "~/org/felix/inbox.org")
         "* %?\n  %i  %a"
         :tree-type week)
        ("j" "Journal" entry (file+olp+datetree "~/org/felix/journal.org")
         "* %?\nEntered on %U\n  %i  %a"
         :tree-type week)

        ;; Health, Fitness
        ("h" "Health")
        ("hd" "Diet" table-line (file+headline "~/org/felix/health.org" "Diet")
         "| %u | %^{Food} | %^{Type} | %^{Note} |")
        ("hw" "Weight" table-line (file+headline "~/org/felix/health.org" "Weight")
         "| %U | %^{Weight} | %^{Note} |")

        ("f" "Fitness")
        ("fr" "Running" table-line (file+headline "~/org/felix/health.org" "Fitness" "Running")
         "| %U | %^{Distance} | %^{Avg.Pace} | %^{Note} |")

        ;; Finance
        ("F" "Finance Record" plain
         (file "~/org/felix/Finance/finance.journal")
         "%(org-read-date) %^{Description}
    %^{Category|Expenses:Food:Groceries|Expenses:Food:Dining|Expenses:Transport|Expenses:Home|Expenses:Entertainment|Revenues:Salary|Revenues:Misc}    %^{Amount}
    %^{Asset/Liability Account|Assets:Bank:Checking|Assets:Cash|Liabilities:Bank:Credit Card}"
         :empty-lines 1)

        ;; Entertainments
        ("e" "Entertainment")
        ("em" "Movie" entry (file+olp+datetree "~/org/felix/entertainments.org" "Movie")
         "* %?\nEntered on $U\n  %i  %a"
         :tree-type month)
        ("et" "TV Show" entry (file+olp+datetree "~/org/felix/entertainments.org" "TV")
         "* %?\nEntered on $U\n  %i  %a"
         :tree-type month)

        ;; Job
        ("J" "Job" table-line (file+headline "~/org/felix/job.org" "Logging")
         "| %U | %^{Company} | %^{Type} | %^{Status} | %^{Note} |")
        ))

(global-set-key (kbd "C-c c") #'org-capture)

(setq org-agenda-files '("~/org/felix/gtd.org"
                         "~/org/20220321210142-the_calculus_lifesaver.org"
                         "~/org/20220322134315-c_c_dev.org"
                         "~/org/20211011152232-tangmeifa_com.org"
                         "~/org/felix/job.org"
                         ))

(global-set-key (kbd "C-c a") #'org-agenda)

(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f" "xelatex -interaction nonstopmode %f"))

(setq org-roam-directory "~/org")
(setq org-roam-v2-ack t)
(setq org-roam-completion-everywhere t)

(add-to-list 'display-buffer-alist
           '("\\*org-roam\\*"
             (display-buffer-in-direction)
             (direction . right)
             (window-width . 0.33)
             (window-height . fit-window-to-buffer)))

(org-roam-db-autosync-mode)
;;(add-hook 'org-roam-find-file-hook 'org-roam-buffer)

(global-set-key (kbd "C-x j r") #'org-roam-node-random)
(global-set-key (kbd "C-x j f") #'org-roam-node-find)
(global-set-key (kbd "C-x j i") #'org-roam-node-insert)
(global-set-key (kbd "C-x j t") #'org-roam-tag-add)

(setq newsticker-url-list
    '(("Emacs SE"
       "https://emacs.stackexchange.com/feeds")))

(setq epa-pinentry-mode 'loopback)
(pinentry-start)

(require 'hledger-mode)
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
(setq hledger-jfile "~/org/felix/finance/finance.journal")

(setq default-input-method "rime")
(setq rime-librime-root "~/.emacs.d/librime/dist")
(setq rime-user-data-dir "~/Library/Rime")

;; (setq telega-chat-show-avatars nil)
;; (setq telega-root-show-avatars nil)
;; (setq telega-user-show-avatars nil)
;; (setq telega-active-locations-show-avatars nil)
;; (setq telega-company-username-show-avatars nil)

;;(setq telega-proxies (list '(:server "127.0.0.1" :port 7890 :enable t
;;                                     :type (:@type "proxyTypeSocks5"))))

(midnight-mode)

(require 'color-rg)

(require 'sudo-edit)

(require 'thing-edit)

(require 'ecloud)

(require 'password-generator)

(require 'auto-save)
(auto-save-enable)
(setq auto-save-slient t)

(require 'geoip)

(require 'felix)

(when (file-exists-p (expand-file-name "init-private.el" user-emacs-directory))
  (load-file (expand-file-name "init-private.el" user-emacs-directory)))

(provide 'init)
;;; init.el ends here
