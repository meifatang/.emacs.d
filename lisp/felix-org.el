;;; felix-org.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)

(setq org-export-global-macros
      '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))

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
   (org . t)
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

(setq org-capture-templates
      '(("g" "Gettting Things Done" entry (file+headline "~/org/daily/gtd.org" "Tasks")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n  %i  %a\n")

        ("r" "Reading List" entry (file+headline "~/org/daily/gtd.org" "Reading")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n  %i  %a\n")
        ("l" "Learn" entry (file+headline "~/org/daily/gtd.org" "Learning")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n  %i  %a\n")

        ("i" "Inbox" entry (file+olp+datetree "~/org/daily/inbox.org")
         "* %?\n  %i  %a"
         :tree-type week)
        ("j" "Journal" entry (file+olp+datetree "~/org/daily/journal.org")
         "* %?\nEntered on %U\n  %i  %a"
         :tree-type week)

        ("h" "Health")
        ("hd" "Diet" table-line (file+headline "~/org/daily/health.org" "Diet")
         "| %u | %^{Food} | %^{Type} | %^{Note} |")
        ("hw" "Weight" table-line (file+headline "~/org/daily/health.org" "Weight")
         "| %U | %^{Weight} | %^{Note} |")

        ("f" "Fitness")
        ("fr" "Running" table-line (file+headline "~/org/daily/health.org" "Fitness" "Running")
         "| %U | %^{Distance} | %^{Avg.Pace} | %^{Note} |")

        ("F" "Finance Record" plain
         (file "~/org/daily/Finance/finance.journal")
         "%(org-read-date) %^{Description}
    %^{Category|Expenses:Food:Groceries|Expenses:Food:Dining|Expenses:Transport|Expenses:Home|Expenses:Entertainment|Revenues:Salary|Revenues:Misc}    %^{Amount}
    %^{Asset/Liability Account|Assets:Bank:Checking|Assets:Cash|Liabilities:Bank:Credit Card}"
         :empty-lines 1)

        ("e" "Entertainment")
        ("em" "Movie" entry (file+olp+datetree "~/org/daily/entertainments.org" "Movie")
         "* %?\nEntered on $U\n  %i  %a"
         :tree-type month)
        ("et" "TV Show" entry (file+olp+datetree "~/org/daily/entertainments.org" "TV")
         "* %?\nEntered on $U\n  %i  %a"
         :tree-type month)))

(add-to-list 'org-agenda-files "~/org/daily/gtd.org" "~/org/daily/gtd/")

(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f" "xelatex -interaction nonstopmode %f"))

(defun org-dblock-write:generate-blog-list (params)
  (let ((files (directory-files default-directory)))
    (insert "\n")
    (dolist (file files)
      (if (string-match "\\.org$" file)
	  (insert (concat "* " file "\n"))))))

(setq org-roam-directory "~/org")
(setq org-roam-v2-ack t)
(setq org-roam-completion-everywhere t)

(require 'org-roam-export)

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:10}" 'face 'org-tag)))

(setq org-roam-capture-templates
      '(("i" "Inbox" plain "%?"
	 :target (file+head "inbox/%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n")
	 :unnarrowed t)
	("a" "Article" plain "%?"
	 :target (file+head "article/%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n#+filetags: :article:\n")
	 :unnarrowed t)
	("b" "Book" plain "%?"
	 :target (file+head "book/%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n#+filetags: :book:\n")
	 :unnarrowed t)
	("p" "Project" plain "%?"
	 :target (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n#+filetags: :project:\n")
	 :unnarrowed t)
	("z" "Chinese Contents" plain "%?"
	 :target (file+head "zh/%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n#+filetags: :zh:\n")
	 :unnarrowed t)))

(setq org-roam-dailies-directory "daily/")
(setq org-roam-dailies-capture-templates
      '(("i" "idea" entry
         "* %?"
         :target (file+head+olp "idea/%<%Y-%m>.org"
				"#+title: %<%Y-%m>\n\#+filetags: :idea:\n"
				("%<%Y-%m-%d>")))
	("g" "Getting things done" entry
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n  %i  %a\n"
         :target (file+head+olp "gtd/%<%Y-%m>.org"
				"#+title: %<%Y-%m>\n#+filetags: :gtd:\n"
				("Tasks" "%<%Y-%m-%d>")))
	("j" "Journal" entry
         "* %?\nEntered on %U\n  %i  %a"
         :target (file+head "journal/%<%Y-%m>.org"
                            "#+title: %<%Y-%m>\n#+filetags: :journal:\n"
			    ("%<%Y-%m-%d>")))))

(org-roam-db-autosync-mode)

(defun roam-sitemap (title list)
  "Modified sitemap function for Org-roam"
  (concat "#+OPTIONS: ^:nil author:nil html-postamble:nil\n"
          "#+TITLE: " title "\n\n"
          (org-list-to-org list)))

(setq my-publish-time 0)
(defun roam-publication-wrapper (plist filename pubdir)
  "Modified function to publish Org-roam"
  (org-roam-graph)
  (org-html-publish-to-html plist filename pubdir)
  (setq my-publish-time (cadr (current-time))))

(defun felix/publish-rss (title list)
  "Modified sitemap function to generate rss.xml"
  ;; TODO
  )

(defun felix/sitemap-format-entry (entry style project)
  "Format ENTRY in org-publish PROJECT Sitemap format ENTRY ENTRY STYLE format that includes date."
  (let ((filename (org-publish-find-title entry project)))
    (if (= (length filename) 0)
        (format "*%s*" entry)
      (format "{{{timestamp(%s)}}} [[file:%s][%s]]"
	      (format-time-string "%Y.%m"
                                  (org-publish-find-date entry project))
	      entry
	      filename))))

(setq org-publish-project-alist
      '(("org"
         :base-directory "~/org/"
	 :recursive t
	 ;;:exclude "~/org/README.org"
         :publishing-function roam-publication-wrapper
         :publishing-directory "~/org/public_html/"
	 :auto-sitemap t
	 :sitemap-title "Sitemap"
	 :sitemap-function roam-sitemap
	 :sitemap-sort-files anti-chronologically
	 :with-toc nil
	 :html-postamble nil
	 :html-doctype "html5"
	 :html-head-include-default-style nil
	 :html-head "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\">"
	 :html-preamble "<header>
  <nav>
    <a href=\"index.html\">Home</a>
    <a href=\"about.html\">About</a>
    <a href=\"rss.xml\" id=\"rss\">RSS</a>
  </nav>
</header>"
	 :html-postamble "<footer>
  <div class=\"copyright-container\">
    <div class=\"copyright\">
      Copyright &copy; 2022 all right reserved
    </div>
  </div>
  <div class=\"generated\">
    Generated by %c</a>
  </div>
</footer>")

	("org-static"
	 :base-directory "~/org/static/"
         :base-extension "css\\|el"
         :publishing-directory "~/org/public_html"
         :publishing-function org-publish-attachment)

	("website" :components ("org" "org-static"))))

(provide 'felix-org)
;;; felix-org ends here
