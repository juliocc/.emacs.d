(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

;; (global-set-key "\C-x|" 'split-window-horizontally-instead)
;; (global-set-key "\C-x_" 'split-window-vertically-instead)

;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))


; When splitting window, show (other-buffer) in the new window
(defun split-window-func-with-other-buffer (split-function)
  `(lambda ()
     (interactive)
     (,split-function)
     (set-window-buffer (next-window) (other-buffer))))

(defun split-window-horizontally-with-other-buffer ()
  (interactive)
  (funcall (split-window-func-with-other-buffer 'split-window-horizontally)))

(defun split-window-vertically-with-other-buffer ()
  (interactive)
  (funcall (split-window-func-with-other-buffer 'split-window-vertically)))


;;(fset 'quick-switch-buffer [?\C-x ?b return])
;; (defun quick-switch-buffer ()
;;   "Switch to most recent buffer. Repeated calls toggle back and forth between the most recent two buffers."
;;   (interactive)
;;   (switch-to-buffer (other-buffer (current-buffer) 1)))

(provide 'jccb-windows)
