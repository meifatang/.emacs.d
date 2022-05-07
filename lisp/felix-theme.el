(setq modus-themes-mode-line '(borderless))
(setq modus-themes-org-blocks 'gray-background)

;; (setq modus-themes-headings
;;       '((1 . (1.1))
;;         (2 . (1.1))
;;         (3 . (1.1))
;; 	(t . (monochrome))))

;; (load-theme 'modus-operandi t)

(require 'auto-dark)
(setq auto-dark--light-theme 'modus-operandi)
(setq auto-dark--dark-theme 'modus-vivendi)

(provide 'felix-theme)
