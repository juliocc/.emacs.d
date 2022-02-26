;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)

(setq initial-frame-alist '((width . 250)
                            (height . 100)
                            (tool-bar-lines . 0)
                            (left-fringe . 0)
                            (right-fringe . 0)
                            (bottom-divider-width . 0)
                            (right-divider-width . 1))
      default-frame-alist initial-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      x-gtk-resize-child-frames 'resize-mode)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)
