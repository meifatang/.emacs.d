(global-set-key (kbd "C-s-f") #'toggle-frame-fullscreen)

(global-set-key (kbd "C-x g") #'magit-status)

(global-set-key (kbd "C-c a") #'org-agenda)

(global-set-key (kbd "C-c c") #'org-capture)

(global-set-key (kbd "C-x j r") #'org-roam-node-random)
(global-set-key (kbd "C-x j f") #'org-roam-node-find)
(global-set-key (kbd "C-x j i") #'org-roam-node-insert)
(global-set-key (kbd "C-x j t") #'org-roam-tag-add)

(global-set-key (kbd "C-x /") #'counsel-rg)

(provide 'felix-keybindings)
