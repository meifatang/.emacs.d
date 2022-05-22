;;; felix-keybindings

(global-set-key (kbd "C-s-f") #'toggle-frame-fullscreen)

(global-set-key (kbd "C-x g") #'magit-status)

(global-set-key (kbd "C-c a") #'org-agenda)

;;(global-set-key (kbd "C-c c") #'org-capture)

(global-set-key (kbd "C-c n r") #'org-roam-node-random)
(global-set-key (kbd "C-c n n") #'org-roam-node-random)
(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n I") #'org-roam-node-insert-immediate)
(global-set-key (kbd "C-c n t") #'org-roam-tag-add)
(global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)

(global-set-key (kbd "C-x /") #'counsel-rg)

(provide 'felix-keybindings)
;;; felix-keybindings ends here
