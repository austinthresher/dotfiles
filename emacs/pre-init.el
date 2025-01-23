;;; -*- no-byte-compile: t; lexical-binding: t; -*-

;; Hacky way to skip my post-init.el while still loading minimal-emacs.d
;; Launch with `emacs --name Q` and post-init.el will not be loaded. This
;; allows testing stuff with packages available and the correct paths set,
;; but without anything else in my config that could be breaking stuff.
(when (and (selected-frame) (string= "Q" (frame-parameter nil 'name)))
  ;; Turn this into a no-op
  (defun minimal-emacs-load-user-init (&rest _)
    nil)
  (message "skipping post-init.el"))
