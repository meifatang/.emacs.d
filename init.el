(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
        ("http" . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))
(setq url-gateway-method 'socks)
(setq socks-server '("Default server" "127.0.0.1" 7890 5))

(setenv "http_proxy" "http://127.0.0.1:7890")
(setenv "https_proxy" "http://127.0.0.1:7890")
(setenv "all_proxy" "socks5://127.0.0.1:7890")

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

(setq user-full-name "Felix Tang")
(setq user-mail-address "me@tangmeifa.com")

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

(setq package-list '(google-this exec-path-from-shell rime pinentry auctex
                                 rainbow-delimiters sudo-edit which-key ivy counsel amx ivy-rich helpful
                                 csv-mode nginx-mode jenkinsfile-mode dockerfile-mode
                                 undo-tree smart-hungry-delete
                                 expand-region browse-kill-ring
                                 projectile
                                 magit forge
                                 toml-mode yaml-mode ansible csv-mode nginx-mode jenkinsfile-mode
                                 terraform-mode dockerfile-mode docker-compose-mode
                                 go-mode rust-mode julia-mode slime haskell-mode erlang elixir-mode applescript-mode
                                 yasnippet yasnippet-snippets yatemplate
                                 lsp-mode company flycheck
                                 ess
                                 emamux edbi pass logview restclient docker kubernetes jenkins
                                 0x0 elfeed leetcode
                                 ob-async ob-go ob-rust ob-browser ob-dart ob-graphql
                                 ob-http ob-ipython ob-mongo ob-tmux ob-uart ob-restclient ob-elixir
                                 org2ctex org-roam org-roam-ui org-ref
                                 org-noter org-pdftools org-noter-pdftools pdf-tools
                                 hledger-mode telega bongo slack vterm
                                 restart-emacs guix nix-mode nix-env-install nix-buffer pcache))

(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

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

(require 'undo-tree)
(global-undo-tree-mode)

(smart-hungry-delete-add-default-hooks)
(global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
(global-set-key (kbd "C-d") 'smart-hungry-delete-forward-char)

(require 'sudo-edit)

(global-set-key (kbd "C-=") 'er/expand-region)

(require 'thing-edit)

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

(require 'ecloud)

(require 'password-generator)

(setq elfeed-feeds
    '("http://nullprogram.com/feed/"
      "https://planet.emacslife.com/atom.xml"
      "https://www.lujun9972.win/rss.xml"
      ))

(setq leetcode-save-solutions t)
(setq leetcode-directory "~/leetcode")

(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)

(setq org-babel-python-command "python3")
;;(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (shell . t)
   (go . t)
   (lisp . t)
   (http . t)
   (clojure . t)
   (sql . t)
   (js . t)
   ;;(ts . t)
   (dot . t)
   (elixir . t)
   (julia . t)
   (R . t)
   (octave . t)
   (java . t)
   (haskell . t)
   (ruby . t)
   (rust . t)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))

(setq org-log-done 'time)
(setq org-log-done 'note)

;;(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("g" "Gettting Things Done" entry (file+headline "~/org/Self/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")

        ;; Inbox, Journal
        ("i" "Inbox" entry (file+olp+datetree "~/org/Self/inbox.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("j" "Journal" entry (file+olp+datetree "~/org/Self/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")

        ;; Health
        ("d" "Diet" entry (file+olp+datetree "~/org/Self/diet.org")
         "* %?\nEntered on $U\n  %i\n  %a")
        ("h" "Health" entry (file+olp+datetree "~/org/Self/health.org")
         "* %?\nEntered on $U\n  %i\n  %a")
        ("f" "Fitness" entry (file+olp+datetree "~/org/Self/fitness.org")
         "* %?\nEntered on $U\n  %i\n  %a")

        ;; Grow
        ("r" "Read" entry (file+olp+datetree "~/org/Self/read.org")
         "* %?\nEntered on $U\n  %i\n  %a")
        ("l" "Learn" entry (file+olp+datetree "~/org/Self/learn.org")
         "* %?\nEntered on $U\n  %i\n  %a")

        ;; Finance
        ("F" "Finance" plain
         (file "~/org/Self/Finance/finance.journal")
         "%(org-read-date) %^{Description}
    %^{Category|Expenses:Food:Groceries|Expenses:Food:Dining|Expenses:Transport|Expenses:Home|Expenses:Entertainment|Revenues:Salary|Revenues:Misc}    %^{Amount}
    %^{Asset/Liability Account|Assets:Bank:Checking|Assets:Cash|Liabilities:Bank:Credit Card}"
         :empty-lines 1)

        ("e" "Entertainment")
        ("em" "Movie" entry (file+olp+datetree "~/org/Self/entertainments.org" "Movie")
         "* %?\nEntered on $U\n  %i\n  %a")
        ("et" "TV Show" entry (file+olp+datetree "~/org/Self/entertainments.org" "TV")
         "* %?\nEntered on $U\n  %i\n  %a")

        ("3" "fzm")
        ("3t" "fzm's Todo" entry (file+headline "~/org/fzm/fzm-gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("3l" "fzm's Work Log" entry (file+olp+datetree "~/org/fzm/fzm-log.org")
         "* %?\nEntered on %U\n  %i\n  %a")

        ))

(setq org-agenda-files '("~/org/33cn/gtd.org" "~/org/gtd.org"))

;;(require 'org-superstar)
;;(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(require 'org2ctex)
(org2ctex-toggle t)

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

(setq epa-pinentry-mode 'loopback)
(pinentry-start)

(require 'hledger-mode)
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
(setq hledger-jfile "~/org/Self/finance/finance.journal")

(setq default-input-method "rime")
(setq rime-librime-root "~/.emacs.d/librime/dist")

;; (setq telega-chat-show-avatars nil)
;; (setq telega-root-show-avatars nil)
;; (setq telega-user-show-avatars nil)
;; (setq telega-active-locations-show-avatars nil)
;; (setq telega-company-username-show-avatars nil)

(setq telega-proxies (list '(:server "127.0.0.1" :port 7890 :enable t
                                     :type (:@type "proxyTypeSocks5"))))

(midnight-mode)

(require 'geoip)

(load-theme 'felix t)

(when (file-exists-p (expand-file-name "init-private.el" user-emacs-directory))
  (load-file (expand-file-name "init-private.el" user-emacs-directory)))
