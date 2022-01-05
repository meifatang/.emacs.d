(deftheme felix
  "Created 2021-12-05.")

(custom-theme-set-faces
 'felix

 ;; org custom
 '(org-level-1 ((t (:inherit outline-1 :extend nil :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :weight bold))))
 '(org-level-3 ((t (:inherit outline-3 :extend nil :weight bold))))
 '(org-level-4 ((t (:inherit outline-4 :extend nil :weight bold))))
 '(org-level-5 ((t (:inherit outline-5 :extend nil :weight bold))))
 '(org-level-6 ((t (:inherit outline-6 :extend nil :weight bold))))
 '(org-level-7 ((t (:inherit outline-7 :extend nil :weight bold))))
 '(org-level-8 ((t (:inherit outline-8 :extend nil :weight bold))))
 )

(provide-theme 'felix)
