;;; https://kitchingroup.cheme.cmu.edu/blog/2015/06/29/Getting-Emacs-to-read-to-me/

(defvar words-voice "Vicki"
  "Mac voice to use for speaking.")

(defun words-speak (&optional text)
  "Speak word at point or region. Mac only."
  (interactive)
  (unless text
    (setq text (if (use-region-p)
                   (buffer-substring
                    (region-beginning) (region-end))
                 (thing-at-point 'word))))
  ;; escape some special applescript chars
  (setq text (replace-regexp-in-string "\\\\" "\\\\\\\\" text))
  (setq text (replace-regexp-in-string "\"" "\\\\\"" text))
  (do-applescript
   (format
    "say \"%s\" using \"%s\""
    text
    words-voice)))

(setq sentence-end-double-space nil)

(defun mac-say-word (&optional arg)
  "Speak word at point. With ARG, go forward ARG words."
  (interactive "P")
  ;; arg can be (4), 4, "-", or -1. we handle these like this.
  (let ((newarg))
    (when arg
      (setq newarg (cond
                    ((listp arg)
                     (round (log (car arg) 4)))
                    ((and (stringp arg) (string= "-" arg))
                     ((< 0 arg) arg)
                     -1)
                    (t arg)))
      (forward-word newarg))
    (when (thing-at-point 'word)
      (words-speak (thing-at-point 'word)))))

(defun mac-say-sentence (&optional arg)
  "Speak sentence at point. With ARG, go forward ARG sentences."
  (interactive "P")
  ;; arg can be (4), 4, "-", or -1. we handle these like this.
  (let ((newarg))
    (when arg
      (setq newarg (cond
                    ((listp arg)
                     (round (log (car arg) 4)))
                    ((and (stringp arg) (string= "-" arg))
                     ((< 0 arg) arg)
                     -1)
                    (t arg)))
      (forward-sentence newarg)
      (when (< 0 newarg) (forward-word)))
    (when (thing-at-point 'sentence)
      (words-speak (thing-at-point 'sentence)))))

(defun mac-say-paragraph (&optional arg)
  "Speak paragraph at point. With ARG, go forward ARG paragraphs."
  (interactive "P")
  ;; arg can be (4), 4, "-", or -1. we handle these like this.
  (let ((newarg))
    (when arg
      (setq newarg (cond
                    ((listp arg)
                     (round (log (car arg) 4)))
                    ((and (stringp arg) (string= "-" arg))
                     ((< 0 arg) arg)
                     -1)
                    (t arg)))
      (forward-paragraph newarg)
      (when (< 0 newarg) (forward-word)))
    (when (thing-at-point 'paragraph)
      (words-speak (thing-at-point 'paragraph)))))
