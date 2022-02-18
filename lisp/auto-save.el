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

;; 前方高能核心代码， 请集中注意力
(defun auto-save-buffers ()
  ;; 所有你在 Alt + x 以后可以调用的函数都要手动加上 (interactive) ， 否则这段代码只能在 Elisp 解释器中执行， 但是不能直接被用户从 Alt + x 调用， 就想 interactive 这个单词的意思一样
  (interactive)
 ;; 创建 autosave-buffer-list 这个变量， 用于保存所有需要遍历的 buffer 列表
  (let ((autosave-buffer-list))
    ;; save-excursion 这个关键字的意思是， 所有在 save-excursion 里面的代码不管怎么折腾都不会对 save-excursion 之前的Emacs状态进行任何改变， 你可以理解为这个关键字的意思就是用于保护现场用的 ;)
    (save-excursion
      ;; dolist 的作用就和很多语言的 foreach 一个意思， 把 buffer-list 这个函数返回的所有 buffer 在循环内赋值给 buf 这个变量， 并在 dolist 的作用域中执行对 buf 影响的代码
      (dolist (buf (buffer-list))
        ;; 设置当前代码的 buffer 为 buf 变量值， 如果没有前面 save-excursion, 你会发现emacs会一直在快速的切换所有 buffer 的过程
        (set-buffer buf)
        ;; 如果当前 buffer 有一个相关联文件 (buffer-file-name), 同时当前 buffer 已经被用户修改了 (buffer-modified-p) 的情况下就执行自动保存
        (if (and (buffer-file-name) (buffer-modified-p))
            (progn
              ;; 把当前 buffer 的名字压进 autosave-buffer-list 列表， 用于后面的保存提示
              (push (buffer-name) autosave-buffer-list)
              (if auto-save-silent
                  ;; 如果 auto-save-silent 这个变量为 true, 就不显示任何保存信息， 因为 Emacs 的保存函数 (basic-save-buffer) 本身机会 blabla 的告诉你文件已经保存了， 所以我们用 with-temp-message 配合空字符串来禁止 with-temp-message 里面的代码在 minibuffer 显示任何内容
                  (with-temp-message ""
                    (basic-save-buffer))
                (basic-save-buffer))
              )))
      ;; unless 的意思是除非 auto-save-silent 为 false 就执行
      (unless auto-save-silent
        ;; cond 就是 elisp 版的 switch， 用于条件语句对比执行
        (cond
         ;; 如果 autosave-buffer-list 列表里面没有任何一个文件需要保存， 我们就不要去烦用户了， 默默打酱油路过就好了

         ;; 如果有一个文件需要保存， 我们就说 Saved ...
         ((= (length autosave-buffer-list) 1)
          (message "# Saved %s" (car autosave-buffer-list)))
         ;; 如果有多个文件需要保存， 就说 Saved ... files
         ((> (length autosave-buffer-list) 1)
          (message "# Saved %d files: %s"
                   (length autosave-buffer-list)
                   (mapconcat 'identity autosave-buffer-list ", ")))))
      )))

(defun auto-save-enable ()
  (interactive)
  (run-with-idle-timer auto-save-idle t #'auto-save-buffers))

(provide 'auto-save)
