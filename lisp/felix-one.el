;;; felix-one.el --- -*- lexical-binding: t -*-

;;; Code:
(vertico-mode t)
(vertico-grid-mode t)

(setq completion-styles '(orderless))

;;(marginalia-mode t)

(global-set-key (kbd "C-;") 'embark-act)
(setq prefix-help-command 'embark-prefix-help-command)

(global-set-key (kbd "C-s") 'consult-line)

(provide 'felix-one)
;;; felix-one.el ends here
