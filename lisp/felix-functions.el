(defun org-roam-node-insert-immediate (arg &rest args)
  "insert roam note immediate"
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun felix/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun felix/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (felix/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun felix/org-roam-refresh-agenda-list ()
  (interactive)
  (add-to-list 'org-agenda-files (felix/org-roam-list-notes-by-tag "project")))

;;(felix/org-roam-refresh-agenda-list)

;; (defun meow-quit ()
;;   "Rewrite the original meow-quit
;; Quit current window or buffer."
;;   (interactive)
;;     (previous-buffer))

(provide 'felix-functions)
