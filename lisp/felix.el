(defun open-with-vscode ()
  "Open current file with vscode."
  (interactive)
  (let ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column))))
    (apply 'call-process "code" nil nil nil (list (concat buffer-file-name ":" line ":" column) "--goto"))))

(defun org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/README.org"))
    (let ((org-config-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook #'org-babel-tangle-config)))

(defun add-tangle-header-and-footer ()
  (message "running in %s" (buffer-file-name))
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/init.el"))

    (goto-char (point-min))
    (insert ";;; init.el --- -*- lexical-binding: t -*-\n;;; Commentary:\n;;; Code:\n\n")
    (goto-char (point-max))
    (insert "\n\(provide 'init\)
;;; init.el ends here\n")
    (save-buffer)))

(add-hook 'org-babel-post-tangle-hook 'add-tangle-header-and-footer)

(provide 'felix)
