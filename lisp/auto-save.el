(defgroup auto-save nil
  "Auto save file when emacs idle."
  :group 'auto-save)

(defcustom auto-save-idle 1
  "The idle seconds to auto save file."
  :type 'integer
  :group 'auto-save)

(defcustom auto-save-silent nil
  "Nothing to ditry minbuffer if this option is non-nil."
  :type 'boolean
  :group 'auto-save)

(setq auto-save-default nil)

;;
(defun auto-save-buffers ()
  (interactive)
  (let ((autosave-buffer-list))
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if (and (buffer-file-name) (buffer-modified-p))
            (progn
              (push (buffer-name) autosave-buffer-list)
              (if auto-save-silent
                  (with-temp-message ""
                    (basic-save-buffer))
                (basic-save-buffer))
              )))
      (unless auto-save-silent
        (cond
         ((= (length autosave-buffer-list) 1)
          (message "# Saved %s" (car autosave-buffer-list)))
         ((> (length autosave-buffer-list) 1)
          (message "# Saved %d files: %s"
                   (length autosave-buffer-list)
                   (mapconcat 'identity autosave-buffer-list ", ")))))
      )))

(defun auto-save-enable ()
  (interactive)
  (run-with-idle-timer auto-save-idle t #'auto-save-buffers))

(provide 'auto-save)
