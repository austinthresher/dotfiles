;;; -*- no-byte-compile: t; lexical-binding: t; -*-
(setq minimal-emacs-ui-features '(context-menu tooltips dialogs))
(setq use-package-enable-imenu-support t)
(push '(background-color . "#000") default-frame-alist)
(push '(foreground-color . "#CCC") default-frame-alist)
(push '(width . 190) default-frame-alist)
(push '(height . 50) default-frame-alist)
(push '(left-fringe . 1) default-frame-alist)
(push '(right-fringe . 1) default-frame-alist)
(push '(line-spacing . 2) default-frame-alist)
(push '(tab-bar-lines . 1) default-frame-alist)
(push '(line-spacing . 2) default-frame-alist)

(push '(tab-bar-lines . 1) initial-frame-alist)
