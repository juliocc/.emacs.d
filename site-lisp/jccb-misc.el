(defun chmod+x-this ()
  "Add executable permissions to the current file."
  (interactive)
  (if buffer-file-name
      (let ((new-mode (logior #o111 (file-modes buffer-file-name))))
        (set-file-modes buffer-file-name new-mode))
    (message "No such file to make executable.")))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on paranthesis. Else go to the
   opening paranthesis one level up."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))))))))

(defun jccb/doctor ()
  (dolist (exe `("rg" "ag" ,jccb/fd-command "git" "terraform" "aspell"
                 "grip" "markdown" "yapf" "isort" "zstd"))
    (unless (executable-find exe)
      (display-warning 'jc/doctor
                       (format "Can't find %s" exe)))))

(defun jccb/project-save-some-buffers ()
  (interactive)
  (let ((save-some-buffers-default-predicate 'save-some-buffers-root))
    (save-some-buffers)))

(provide 'jccb-misc)
