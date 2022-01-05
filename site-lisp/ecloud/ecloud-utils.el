;;; ecloud-utils.el --- Util functions for ecloud -*- lexical-binding: t -*-

;; Author: Ramanathan Sivagurunathan
;; Maintainer: Ramanathan Sivagurunathan
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (magit "2.13.0") (ht "2.2") (s "1.12.0") (pcache "0.4.2"))
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Contains general utility function for ecloud

;;; Code:
(require 'asoc)

(defun ecloud-display-buffer-fullframe (buffer)
  "Function to display `BUFFER 'in full frame."
  (let ((display-fn
         (lambda (buffer alist)
           (when-let (window (or (display-buffer-reuse-window buffer alist)
                                 (display-buffer-same-window buffer alist)
                                 (display-buffer-pop-up-window buffer alist)
                                 (display-buffer-use-some-window buffer alist)))
             (delete-other-windows window)
             window))))
    (display-buffer buffer (list display-fn)))
  )

(defun ecloud-display-buffer (buffer)
  "Function to display ecloud `BUFFER."
  (let ((window (funcall ecloud-display-buffer-function buffer)))
    (when ecloud-display-buffer-select
      (select-frame-set-input-focus
       (window-frame (select-window window)))))
  )

(defun ecloud-insert-kv-list (kvlist)
  "Function to insert key value list `KVLIST in a buffer."
  (-map (lambda (elem)
          (insert (propertize (format "%s: " (car elem))
                              'face 'magit-section-heading))
          (insert (format "%s\n" (cdr elem)))) kvlist))

(defun ecloud-class-to-cloud-and-rtype (class)
  "Function to convert `CLASS to cloud and rtype."
  (cons (nth 0 (split-string (format "%s" class) "-"))
        (string-join (cdr (split-string (format "%s" class) "-")) "-"))
  )

(defun ecloud-read-int (prompt &optional initial-input history default-value
                               inherit-input-method no-whitespace)
  "Read an integer from the minibuffer, prompting with string PROMPT.
`INITIAL-INPUT is the default input.  `HISTORY is the history of previous input.
* \": \" is appended to PROMPT, and
* an invalid DEFAULT-VALUE is silently ignored.
`INHERIT-INPUT-METHOD for inheriting the input method.
* empty input is not allowed
* whitespace is not allowed and leading and trailing whitespace is
  removed automatically if `NO-WHITESPACE is non-nil."
  (when default-value
    (when (consp default-value)
      (setq default-value (car default-value)))
    (unless (integerp default-value)
      (setq default-value 1)))
  (let* ((minibuffer-completion-table nil)
         (val (read-from-minibuffer
               (magit-prompt-with-default (concat prompt ": ") default-value)
               initial-input (and no-whitespace magit-minibuffer-local-ns-map)
               nil history default-value inherit-input-method))
         (trim (lambda (regexp string)
                 (save-match-data
                   (if (string-match regexp string)
                       (replace-match "" t t string)
                     string)))))
    (setq val (funcall trim "\\`\\(?:[ \t\n\r]+\\)"
                       (funcall trim "\\(?:[ \t\n\r]+\\)\\'" val)))
    (cond ((string= val "")
           (user-error "Need non-empty input"))
          ((not (string-match "\\`[0-9]*[1-9][0-9]*\\'" val))
           (user-error "Entered Input is not integer"))
          (t (string-to-number val)))))

(cl-defun ecloud-map-tags-to-kv (tags)
  "Convert AWS `TAGS to key value alist"
  (--map
   (cons (asoc-get it 'Key) (asoc-get it 'Value)) tags))

(provide 'ecloud-utils)

;;; ecloud-utils.el ends here
