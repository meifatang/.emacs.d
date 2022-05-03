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
      '(("g" "Gettting Things Done" entry (file+headline "~/org/life/gtd.org" "Tasks")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n  %i  %a\n")

        ("r" "Reading List" entry (file+headline "~/org/life/gtd.org" "Reading")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n  %i  %a\n")
        ("l" "Learn" entry (file+headline "~/org/life/gtd.org" "Learning")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n  %i  %a\n")

        ("i" "Inbox" entry (file+olp+datetree "~/org/life/inbox.org")
         "* %?\n  %i  %a"
         :tree-type week)
        ("j" "Journal" entry (file+olp+datetree "~/org/life/journal.org")
         "* %?\nEntered on %U\n  %i  %a"
         :tree-type week)

        ("h" "Health")
        ("hd" "Diet" table-line (file+headline "~/org/life/health.org" "Diet")
         "| %u | %^{Food} | %^{Type} | %^{Note} |")
        ("hw" "Weight" table-line (file+headline "~/org/life/health.org" "Weight")
         "| %U | %^{Weight} | %^{Note} |")

        ("f" "Fitness")
        ("fr" "Running" table-line (file+headline "~/org/life/health.org" "Fitness" "Running")
         "| %U | %^{Distance} | %^{Avg.Pace} | %^{Note} |")

        ("F" "Finance Record" plain
         (file "~/org/life/Finance/finance.journal")
         "%(org-read-date) %^{Description}
    %^{Category|Expenses:Food:Groceries|Expenses:Food:Dining|Expenses:Transport|Expenses:Home|Expenses:Entertainment|Revenues:Salary|Revenues:Misc}    %^{Amount}
    %^{Asset/Liability Account|Assets:Bank:Checking|Assets:Cash|Liabilities:Bank:Credit Card}"
         :empty-lines 1)

        ("e" "Entertainment")
        ("em" "Movie" entry (file+olp+datetree "~/org/life/entertainments.org" "Movie")
         "* %?\nEntered on $U\n  %i  %a"
         :tree-type month)
        ("et" "TV Show" entry (file+olp+datetree "~/org/life/entertainments.org" "TV")
         "* %?\nEntered on $U\n  %i  %a"
         :tree-type month)))

(setq org-agenda-files '("~/org/life/gtd.org"))

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

(require 'org-roam-export) ;; Modify export to suit org-roam

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

(defun org-dblock-write:generate-blog-list (params)
  (let ((files (directory-files default-directory)))
    (insert "\n")
    (dolist (file files)
      (if (string-match "\\.org$" file)
	  (insert (concat "* " file "\n"))))))

(setq org-publish-project-alist
      '(("org"
         :base-directory "~/org/"
         :publishing-function roam-publication-wrapper
         :publishing-directory "~/org/public_html"
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
	
        ("org-zh"
         :base-directory "~/org-zh/"
	 :publishing-function org-html-publish-to-html
         :publishing-directory "~/org-zh/public_html"
	 :auto-sitemap t
	 :sitemap-title "网站地图"
	 :sitemap-sort-files anti-chronologically
	 ;;:sitemap-format-entry felix/sitemap-format-entry
	 :with-toc nil
	 :html-postamble nil
	 :html-doctype "html5"
	 :language "zh-Hans"
	 :html-head-include-default-style nil
	 :html-head "<link rel=\"stylesheet\" media=\"all\" href=\"https://cdnjs.cloudflare.com/ajax/libs/Han/3.2.7/han.min.css\">
<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\">"
	 :html-preamble "<header>
  <nav>
    <a href=\"index.html\">主页</a>
    <a href=\"about.html\">关于</a>
    <a href=\"rss.xml\" id=\"rss\">订阅</a>
  </nav>
</header>"
	 :html-postamble "<script>
</script>
<footer>
  <div class=\"copyright-container\">
    <div class=\"copyright\">
      版权所有 &copy; 2022 汤美法，
      使用 %c 生成。
  </div>
</footer>")
	
	("org-zh-static"
         :base-directory "~/org-zh/static/"
         :base-extension "css\\|el"
         :publishing-directory "~/org-zh/public_html"
         :publishing-function org-publish-attachment)

	("c-c.dev" :components ("org" "org-static"))
	("tangmeifa.com" :components ("org-zh" "org-zh-static"))))

(provide 'felix-org)
;;; felix-org ends here
