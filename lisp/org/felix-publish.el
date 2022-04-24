;;; felix-publish.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Org-roam
(defun roam-sitemap (title list)
  (concat "#+OPTIONS: ^:nil author:nil html-postamble:nil\n"
          "#+TITLE: " title "\n\n"
          (org-list-to-org list) "\nfile:sitemap.svg"))

(setq my-publish-time 0)
(defun roam-publication-wrapper (plist filename pubdir)
  (org-roam-graph)
  (org-html-publish-to-html plist filename pubdir)
  (setq my-publish-time (cadr (current-time))))

(setq org-export-global-macros
      '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))

;;; Format sitemap entries
(defun org-sitemap-date-entry-format (entry style project)
  "Format ENTRY in org-publish PROJECT Sitemap format ENTRY ENTRY STYLE format that includes date."
  (let ((filename (org-publish-find-title entry project)))
    (if (= (length filename) 0)
        (format "*%s*" entry)
      (format "{{{timestamp(%s)}}} [[file:%s][%s]]"
              (format-time-string "%Y-%m-%d"
                                  (org-publish-find-date entry project))
              entry
              filename))))

;;; Define projects
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
	 :html-postamble "<footer>
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

(provide 'felix-publish)
