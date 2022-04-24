(defun roam-sitemap (title list)
  (concat "#+OPTIONS: ^:nil author:nil html-postamble:nil\n"
          "#+TITLE: " title "\n\n"
          (org-list-to-org list) "\nfile:sitemap.svg"))

(setq my-publish-time 0)
(defun roam-publication-wrapper (plist filename pubdir)
  (org-roam-graph)
  (org-html-publish-to-html plist filename pubdir)
  (setq my-publish-time (cadr (current-time)))
  )

(setq org-publish-project-alist
      '(("org"
         :base-directory "~/org/"
         :publishing-function roam-publication-wrapper
         :publishing-directory "~/org/public_html"
	 :auto-sitemap t
	 :sitemap-function roam-sitemap
	 :with-toc nil
	 :makeindex t
	 :html-postamble nil
	 :html-doctype "html5"
	 :html-head-include-default-style nil
	 :html-head "<link rel=\"stylesheet\" href=\"./style.css\" type=\"text/css\">")
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
	 :sitemap-title ""
	 :with-toc nil
	 :makeindex t
	 :html-postamble nil
	 :html-doctype "html5"
	 :language "zh-CN"
	 :html-head-include-default-style nil
	 :html-head "<link rel=\"stylesheet\" href=\"./style.css\" type=\"text/css\">"
	 :html-preamble "<header>
  <nav>
    <a href=\"./index.html\">Home</a>
    <a href=\"./theindex.html\">Articles</a>
    <a href=\"./about.html\">About</a>
  </nav>
</header>
<hr/>"
	 :html-postamble "<footer>
  <div class=\"copyright-container\">
    <div class=\"copyright\">
      版权所有 &copy; 2022 汤美法
    </div>
  </div>
  <div class=\"generated\">
    使用 %c 构建</a>
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
