;;; felix-org.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; babel
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

;; todo
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))

(setq org-log-done 'time)
;;(setq org-log-done 'note)

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

(setq org-roam-node-display-template
    (concat "${title:*} "
            (propertize "${tags:10}" 'face 'org-tag)))

(org-roam-db-autosync-mode)
;;(add-hook 'org-roam-find-file-hook 'org-roam-buffer)

(require 'org-roam-export)

(global-set-key (kbd "C-x j r") #'org-roam-node-random)
(global-set-key (kbd "C-x j f") #'org-roam-node-find)
(global-set-key (kbd "C-x j i") #'org-roam-node-insert)
(global-set-key (kbd "C-x j t") #'org-roam-tag-add)

(provide 'felix-org)
;;; felix-org ends here
