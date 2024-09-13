;; -*- lexical-binding: t -*-

;;;; TODO:
;;;; =========================================================================
;; - Figure out how to show corfu help pop-up even when there is only
;;   one match
;; - Make tabs insert spaces in comments / non-syntax areas
;; - Make shift-tab unindent too
;; - Use keymap-global-set instead of global-set-key
;; - Try adding a preview to string-insert-rectangle
;;    OR checkout CUA mode's rectangle stuff, that might be better
;; - Use advice :override instead of redefining functions
;;   (currently doing this to customize doom-modeline and corfu, among others)


;;;; Early / system settings
;;;; =========================================================================

(setopt display-time-default-load-average nil)
;; (setq debug-on-error t)

(defvar after-load-theme-hook nil)
(defadvice load-theme (after run-after-load-theme-hook activate)
  (run-hooks 'after-load-theme-hook))

(defun my/windows-p () (eq system-type 'windows-nt))
(defun my/font-available-p (name) (member name (font-family-list)))

(defvar font-mono (face-attribute 'default :family))
(defvar font-fixed-serif (face-attribute 'fixed-pitch-serif :family))
(defvar font-variable-pitch (face-attribute 'variable-pitch :family))

(defun my/setup-fonts ()
  (when (display-graphic-p)
    (cond
     ((my/windows-p)
      (cond ((my/font-available-p "Iosevka NF")
             (setq font-mono "Iosevka NF")
             (setq font-fixed-serif font-mono))
            ((my/font-available-p "JetBrainsMono NF")
             (setq font-mono "JetBrainsMono NF")
             (setq font-fixed-serif font-mono)))
      (when (my/font-available-p "Segoe UI")
        (setq font-variable-pitch "Segoe UI"))
      (when (my/font-available-p "IosevkaTermSlab NF")
        (setq font-fixed-serif "IosevkaTermSlab NF")))
     (t
      (cond ((my/font-available-p "Iosevka Nerd Font Propo")
             (setq font-mono "Iosevka Nerd Font Propo")
             (setq font-fixed-serif font-mono))
            ((my/font-available-p "JetBrainsMono Nerd Font")
             (setq font-mono "JetBrainsMono Nerd Font")
             (setq font-fixed-serif font-mono)))
      (when (my/font-available-p "IosevkaTermSlab Nerd Font Propo")
        (setq font-fixed-serif "IosevkaTermSlab Nerd Font Propo"))
      (cond
       ((my/font-available-p "Asap SemiCondensed")
        (setq font-variable-pitch "Asap SemiCondensed"))
       ((my/font-available-p "Roboto") (setq font-variable-pitch "Roboto")))))

    (set-face-attribute 'default nil :family font-mono :height 120)
    (set-face-attribute 'fixed-pitch nil :family font-mono :height 120)
    (set-face-attribute 'fixed-pitch-serif nil :family font-fixed-serif :height 120)
    (set-face-attribute 'variable-pitch nil :family font-variable-pitch :height 120
                        :weight 'medium)))

(add-hook 'server-after-make-frame-hook #'my/setup-fonts -90)
(add-hook 'window-setup-hook #'my/setup-fonts -90)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t)

(setq frame-background-mode 'dark)

(defface my/term-bg
  '((default :inherit default))
  "term background color")
(defface my/modeline-highlight
    '((default :box '(:line-width (-2 . -2) :style flat-button)))
  "mouseover highlight for mode line items")
(defface my/treemacs-bg
  '((default :inherit default))
  "treemacs background color")
(defface my/read-only
    '((default :inherit default))
  "face used for read-only buffers like *Messages*")
(defface my/treemacs-face
  '((default :inherit variable-pitch :height 0.9))
  "treemacs files")
(defface my/treemacs-big-face
  '((default :inherit variable-pitch
             :height 1.1))
  "treemacs projects")
(defface my/ibuffer-face
    '((default :inherit variable-pitch :height 0.9))
  "ibuffer sidebar face")
(defface my/ibuffer-group
    '((default :inherit variable-pitch :height 0.9))
  "ibuffer filter group")
(defface my/tab-bar-separator
    '((default :width ultra-condensed :height 50))
  "controls distance between tabs")
(defface my/help-bg
  '((default :inherit default))
  "help background color")
(defface my/minibuffer-bg
  '((default :inherit default))
  "minibuffer background color")
(defface my/hl-line
    '((default))
  "hl-line background color")

;;;; Packages
;;;; =========================================================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(setopt package-native-compile t)
;; Uncomment to profile startup
;; (setopt use-package-compute-statistics t)
(setopt use-package-enable-imenu-support t)
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)

(use-package no-littering
  :ensure t
  :config (no-littering-theme-backups))

(use-package savehist
  :ensure nil
  :config (savehist-mode t))

(use-package tab-bar
  :ensure nil
  :config
  (setopt tab-bar-format
          '(tab-bar-format-menu-bar
            tab-bar-format-tabs
            tab-bar-separator
            tab-bar-format-add-tab))
  (setopt tab-bar-separator " ")
  (setopt tab-bar-auto-width-max '(320 100))
  (setopt tab-bar-close-button-show nil)
  (setopt tab-bar-tab-name-truncated-max 64)
  (defun my/tab-bar-name-padded (tab i)
    (let ((txt (tab-bar-tab-name-format-default tab i)))
      (propertize (format " %-64s " txt)
                  'face (funcall tab-bar-tab-face-function tab))))
  (setopt tab-bar-tab-name-format-function #'my/tab-bar-name-padded)
  (keymap-global-set "C-S-t" 'tab-bar-new-tab-to)
  (keymap-global-set "M-1" (lambda () (interactive) (tab-bar-select-tab 1)))
  (keymap-global-set "M-2" (lambda () (interactive) (tab-bar-select-tab 2)))
  (keymap-global-set "M-3" (lambda () (interactive) (tab-bar-select-tab 3)))
  (keymap-global-set "M-4" (lambda () (interactive) (tab-bar-select-tab 4)))
  (keymap-global-set "M-5" (lambda () (interactive) (tab-bar-select-tab 5)))
  (keymap-global-set "M-6" (lambda () (interactive) (tab-bar-select-tab 6)))
  (keymap-global-set "M-7" (lambda () (interactive) (tab-bar-select-tab 7)))
  (keymap-global-set "M-8" (lambda () (interactive) (tab-bar-select-tab 8)))
  (keymap-global-set "M-9" (lambda () (interactive) (tab-bar-select-tab 9)))
  (tab-bar-mode t)
  (setq tab-bar-menu-bar-button (propertize " 󰍜 " 'face 'my/minibuffer-bg)))

(use-package whitespace
  :ensure nil
  :hook (prog-mode . whitespace-mode)
  :config
  (setopt whitespace-style '(tab-mark))
  (setq-default indent-tabs-mode nil))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setopt rainbow-delimiters-outermost-only-face-count 1))

(use-package memoize
    :ensure t
    :demand t
    :config
    ;; defmemoize will complain if we use it to redefine existing functions
    (dolist (fn '(my/darken my/lighten my/desaturate my/saturate))
      (setplist fn nil))
    (defun my/normalize-color (color) ; memoizing this broke stuff
      (if (not (string-prefix-p "#" color))
          color
        (if (= (length color) 13)
            (let ((r (substring color 1 3))
                  (g (substring color 5 7))
                  (b (substring color 9 11)))
              (concat "#" r g b))
          color (message "after: %s" color))))
    (defmemoize my/darken (color amt):
      (my/normalize-color (color-darken-name color amt)))
    (defmemoize my/lighten (color amt)
      (my/normalize-color (color-lighten-name color amt)))
    (defmemoize my/desaturate (color amt)
      (my/normalize-color (color-desaturate-name color amt)))
    (defmemoize my/saturate (color amt)
      (my/normalize-color (color-saturate-name color amt)))
    (defun my/adjust-fg (color amt)
      (pcase frame-background-mode
        ('light (my/darken color amt))
        ('dark (my/lighten color amt))))
    (defun my/adjust-bg (color amt)
      (pcase frame-background-mode
        ('light (my/lighten color amt))
        ('dark (my/darken color amt)))))

(use-package catppuccin-theme
  :ensure t
  :config
  (setopt catppuccin-flavor 'frappe)
  (setopt catppuccin-italic-comments t)
  (defun my/customize-catppuccin ()
    (let ((bg-color (my/lighten (catppuccin-color 'base) 4)))
      (set-face-attribute 'default nil :background bg-color)
      (set-face-attribute 'fringe nil :background bg-color))
    (set-face-attribute 'cursor nil
                        :background (my/adjust-fg (catppuccin-color 'text) 20))
    (set-face-attribute 'mode-line-active nil
                        :background (my/adjust-bg (catppuccin-color 'base) 25)
                        :box (list :line-width '(-1 . 1) :style 'flat-button
                                   :color (catppuccin-color 'overlay2)))
    (set-face-attribute 'mode-line-inactive nil
                        :foreground (catppuccin-color 'surface2)
                        :background (my/darken (catppuccin-color 'base) 5)
                        :box (list :line-width '(-1 . 1) :style 'flat-button
                                   :color (catppuccin-color 'surface1)))
    ;; Default rainbow colors are too easy to mix up when side-by-side
    (dolist (pair '((rainbow-delimiters-depth-1-face . text)
                    (rainbow-delimiters-depth-2-face . blue)
                    (rainbow-delimiters-depth-3-face . yellow)
                    (rainbow-delimiters-depth-4-face . green)
                    (rainbow-delimiters-depth-5-face . peach)
                    (rainbow-delimiters-depth-6-face . teal)
                    (rainbow-delimiters-depth-7-face . mauve)
                    (rainbow-delimiters-depth-8-face . rosewater)
                    (rainbow-delimiters-depth-9-face . green)))
      (face-spec-set (car pair)
                     `((t (:foreground ,(catppuccin-color (cdr pair)))))))
    ;; Make unmatched delimiters much more noticable
    (face-spec-set 'rainbow-delimiters-unmatched-face
                   `((t (:background ,(catppuccin-color 'red)
                                     :foreground unspecified
                                     :weight bold))))
    (face-spec-set 'window-divider-last-pixel
                   `((t (:foreground ,(catppuccin-color 'base)))))
    (face-spec-set 'window-divider-first-pixel
                   `((t (:foreground ,(catppuccin-color 'base)))))
    (let ((dark-border (my/darken (catppuccin-color 'crust)
                                  (pcase catppuccin-flavor
                                    ('latte 20)
                                    (_ 40)))))
      (face-spec-set 'window-divider
                     `((t (:foreground ,dark-border))))
      (set-face-attribute 'show-paren-match nil
                          :background (catppuccin-color 'crust)
                          :foreground 'unspecified
                          :inverse-video nil
                          :box `(:line-width (-2 . -2) :color ,dark-border)
                          :weight 'bold))
    (set-face-attribute 'tab-bar-tab nil
                        :background (catppuccin-color 'base)
                        :family font-variable-pitch
                        :height 120
                        :weight 'bold
                        :box (list :line-width '(-1 . -1) :style 'released-button
                                   :color (catppuccin-color 'crust)))
    (set-face-attribute 'tab-bar nil
                        :background (my/darken (catppuccin-color 'crust) 5)
                        :height 20 :family font-mono) ; tiny size shrinks spacer
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :background (catppuccin-color 'crust)
                        :family font-variable-pitch
                        :height 120
                        :foreground (my/desaturate (my/adjust-bg (catppuccin-color 'text) 30) 60)
                        :box (list :line-width '(-1 . -1) :style 'released-button
                                   :color (my/adjust-bg (catppuccin-color 'crust) 20)))
    (set-face-attribute 'my/term-bg nil
                        :background
                        (pcase frame-background-mode
                          ('light (my/lighten (my/saturate (catppuccin-color 'base) 100) 2))
                          ('dark (my/saturate (my/darken (catppuccin-color 'base) 20) 12))))
    (set-face-attribute 'my/help-bg nil
                        :foreground
                        (my/desaturate (my/adjust-bg (catppuccin-color 'text) 8) 40)
                        :background
                        (pcase frame-background-mode
                          ('light (my/lighten (my/saturate (catppuccin-color 'base) 25) 2))
                          ('dark (my/darken (my/desaturate (catppuccin-color 'base) 0) 15))))
    (set-face-attribute 'my/read-only nil
                        :foreground (catppuccin-color 'overlay2)
                        :background (my/desaturate (catppuccin-color 'base) 6))
    (set-face-attribute 'my/minibuffer-bg nil
                        :background (my/darken (catppuccin-color 'crust) 5)
                        :height 120
                        :family font-mono)
    (let ((sidebar-color (pcase frame-background-mode
                           ('light (catppuccin-color 'base))
                           ('dark (my/darken (catppuccin-color 'crust) 15)))))
      (set-face-attribute 'my/treemacs-bg nil :background sidebar-color)
      (set-face-attribute 'my/treemacs-big-face nil :background sidebar-color)
      (set-face-attribute 'my/ibuffer-face nil :background sidebar-color))
    (set-face-attribute 'my/ibuffer-group nil
                        :foreground (catppuccin-color 'teal)
                        :weight 'normal)
    (set-face-attribute 'my/hl-line nil
                        :extend t
                        :background (catppuccin-color 'base))
    (setopt hl-line-face 'my/hl-line)
    (set-face-attribute 'widget-field nil
                        :background (catppuccin-color 'surface0)
                        :box (list :line-width '(1 . -1)
                                   :color (catppuccin-color 'overlay0)))
    )
  (add-hook 'after-load-theme-hook #'my/customize-catppuccin))

(use-package minions
    :ensure t
    :config
    (add-to-list 'minions-prominent-modes 'view-mode)
    (minions-mode t))

(use-package doom-modeline
    :ensure t
    :custom ((doom-modeline-icon t)
             (doom-modeline-minor-modes t)
             (doom-modeline-bar-width 1)
             (doom-modeline-height 24)
             (doom-modeline-buffer-file-name-style 'relative-from-project)
             (doom-modeline-buffer-encoding 'nondefault)
             (doom-modeline-workspace-name nil)
             (doom-modeline-irc nil)
             (doom-modeline-display-misc-in-all-mode-lines nil))
    :config
    (defun my/propertize-buffer-name (name tooltip)
      (or
       (ignore-errors
         (concat
          (doom-modeline-spc)
          (doom-modeline--buffer-mode-icon)
          (doom-modeline--buffer-state-icon)
          (propertize name
                      'help-echo tooltip
                      'face 'doom-modeline-buffer-file
                      'mouse-face 'my/modeline-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line mouse-1] 'mouse-buffer-menu)
                                   (define-key map [mode-line mouse-2] 'mouse-buffer-menu)
                                   (define-key map [mode-line mouse-3] 'mouse-buffer-menu)
                                   map)))) ""))
    ;; Overwrite the built-in buffer info segments
    (doom-modeline-def-segment buffer-info
        (my/propertize-buffer-name (doom-modeline--buffer-name)
         (or (buffer-file-name) "Buffer name")))
    (doom-modeline-def-segment buffer-info-simple
        (my/propertize-buffer-name (doom-modeline--buffer-simple-name)
         (or (buffer-file-name) "Buffer name")))
    (doom-modeline-mode t))

(use-package which-key
    :ensure t
    :defer t
    :config
    (setopt which-key-dont-use-unicode nil)
    (setopt which-key-add-column-padding 2)
    (setopt which-key-min-display-lines 8)
    (setopt which-key-max-description-length 100)
    (setopt which-key-max-display-columns (if (display-graphic-p) 1 nil))
    (setopt which-key-max-description-length 1.0)
    (setopt which-key-idle-delay 10000.0)
    (setopt which-key-show-early-on-C-h t)
    (setopt which-key-idle-secondary-delay 0.05)
    (setopt which-key-preserve-window-configuration t)
    (which-key-mode t))

(use-package nerd-icons :ensure t :defer)

;; TODO: Take a look at vertico-unobtrusive and vertico-flat
(use-package vertico
    :ensure t
    :defer t
    :bind (:map vertico-map ("TAB" . #'minibuffer-complete))
    :custom ((vertico-cycle t)
             (vertico-count 5))
    :init (vertico-mode))

(use-package vertico-reverse
    :ensure nil
    :after vertico
    :init (vertico-reverse-mode t))

(use-package corfu
    :ensure t
    :defer t
    :init (global-corfu-mode)
    :bind (:map corfu-map
                ("<tab>" . corfu-next)
                ("<backtab>" . corfu-previous))
    :config
    (setopt corfu-cycle t)
    (setopt corfu-preselect 'valid)
    (setopt corfu-preview-current 'insert)
    (setopt corfu-on-exact-match 'insert)
    (keymap-unset corfu-map "<remap> <move-beginning-of-line>")
    (keymap-unset corfu-map "<remap> <move-end-of-line>")
    ;; For some reason, Corfu is hard-coded to not show the preview for
    ;; the first match in the list. This overrides the predicate to remove
    ;; that check.
    (defun corfu--preview-current-p ()
      (and corfu-preview-current (>= corfu--index 0))))

(use-package corfu-history
    :ensure nil
    :after (corfu savehist)
    :config
    (add-to-list 'savehist-additional-variables 'corfu-history)
    (corfu-history-mode t))

(use-package corfu-popupinfo
    :ensure nil
    :after corfu
    :config
    (corfu-popupinfo-mode t)
    (setopt corfu-popupinfo-delay '(0.5 . 0.1)))

(use-package corfu-candidate-overlay
    :ensure t
    :after corfu
    :config
    (corfu-candidate-overlay-mode t)
    (global-set-key (kbd "C-S-<space>") 'completion-at-point))

(use-package cape
    :ensure t
    :defer t
    :init
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-file))

(use-package orderless
    :ensure t
    :custom
    (orderless-component-separator #'orderless-escapable-split-on-space)
    (completion-styles '(orderless flex basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles partial-completion)))))

(use-package dabbrev
    :ensure nil
    :bind (("M-/" . dabbrev-completion)
           ("C-M-/" . dabbrev-expand))
    :config
    (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
    (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package consult
    :ensure t
    :defer t
    :bind (("C-x b" . consult-buffer)
           ("M-y" . consult-yank-pop)
           ("M-s r" . consult-ripgrep)
           ("M-s s" . consult-line)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)
           ("M-s e" . consult-isearch-history)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi))
    :config (setq consult-narrow-key "<"))

(use-package embark-consult :ensure t :defer t)

(use-package embark
    :ensure t
    :defer t
    :bind (("C-c a" . embark-act)))

(defun my/customize-shell ()
      (face-remap-add-relative 'default 'my/term-bg)
      (face-remap-add-relative 'fringe 'my/term-bg)
      (setq cursor-type 'box)
      (setq cursor-in-non-selected-windows t))

(use-package eshell
    :ensure t
    :defer t
    :init (defun my/setup-eshell ()
            (keymap-set eshell-mode-map "C-r" 'consult-history)
            (my/customize-shell))
    :hook ((eshell-mode . my/setup-eshell)))

(use-package eat
    :ensure t
    :defer t
    :config
    (eat-eshell-mode)
    (eat-eshell-visual-command-mode)
    (setopt eat-term-name "xterm-256color")
    (setopt eat-default-cursor-type '(box 0.5 hollow))
    (add-hook 'eat-mode-hook #'my/customize-shell))

(use-package magit
    :ensure t
    :defer t
    :bind (("C-x g" . magit-status)
           ("C-x G" . magit-dispatch)
           :map transient-map ("<escape>" . transient-quit-one))
    :config (global-unset-key (kbd "C-x M-g")))

(use-package transpose-frame :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package json-mode :ensure t :defer t)
(use-package ca65-mode :ensure t :defer t)
(use-package cmake-mode :ensure t :defer t)
(use-package csv-mode :ensure t :defer t)
(use-package sml-mode :ensure t :defer t)
(use-package tmux-mode :ensure t :defer t)
(use-package nasm-mode :ensure t :defer t)
(use-package lua-mode :ensure t :defer t)
(use-package ini-mode :ensure t :defer t)
(use-package glsl-mode :ensure t :defer t)
(use-package markdown-mode :ensure t :defer t)
(use-package haskell-mode :ensure t :defer t)
(use-package pip-requirements :ensure t :defer t)

(use-package docker-compose-mode :ensure t :defer t)
(use-package dockerfile-mode :ensure t :defer t)
(use-package docker
    :ensure t
    :defer t
    :config
    (setopt docker-show-status nil)
    (setopt docker-compose-command "docker compose"))


(use-package projectile
    :ensure t
    :defer t
    :init (projectile-mode t)
    :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
    :config
    (projectile-register-project-type 'godot '("project.godot")
                                      :project-file "project.godot"))

(use-package ibuffer-sidebar
    :ensure t
    :demand
    :config
    ;; setq is used here because I get type errors if I use setopt
    (setq ibuffer-sidebar-mode-line-format nil)
    (setq ibuffer-sidebar-width 24)
    (setq ibuffer-sidebar-face 'my/ibuffer-face)
    (setq ibuffer-use-header-line nil)
    (setq ibuffer-sidebar-use-custom-font t)
    (setq ibuffer-default-sorting-mode 'alphabetic)
    (setq ibuffer-sidebar-display-alist
          '((side . left) (slot . -1) (window-height . 0.2)))
    (setq ibuffer-sidebar-special-refresh-commands
          '((kill-buffer . 0)
            (find-file . 0)
            (delete-file . 0)
            (kill-current-buffer . 0)))
    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-saved-filter-groups
          '(("Groups"
             ("Files" (visiting-file))
             ("Shell" (or (derived-mode . eat-mode)
                          (derived-mode . eshell-mode)))
             ("Process" (process))
             ("Docs" (or (derived-mode . help-mode)
                         (derived-mode . apropos-mode)
                         (derived-mode . Info-mode)
                         (derived-mode . helpful-mode)
                         (derived-mode . shortdoc-mode)
                         (derived-mode . eww-mode)
                         (derived-mode . doc-view-mode)))
             ("Customize" (derived-mode . Custom-mode))
             ("Magit" (name ."^magit"))
             ("Other" (name . "\\*"))
             )))
    (define-ibuffer-column icon (:name "Icon")
      (with-current-buffer buffer
        (nerd-icons-icon-for-buffer)))
    (setq ibuffer-sidebar-formats '((" " mark " " icon " " name)))
    (setq ibuffer-hidden-filter-groups '("Other"))
    (defun my/ibuffer-load-groups ()
      (ibuffer-switch-to-saved-filter-groups "Groups"))
    (add-hook 'ibuffer-mode-hook #'my/ibuffer-load-groups)
    (add-to-list 'ibuffer-sidebar-special-refresh-commands 'kill-current-buffer)
    (keymap-set ibuffer-name-map "<mouse-1>" #'ibuffer-mouse-visit-buffer)
    (defun my/ibuffer-kill-mouse-buffer (event)
      (interactive "e")
      (when-let ((win (ibuffer-sidebar-showing-sidebar-p)))
        (with-selected-window win
          (let ((pt (save-excursion
                      (mouse-set-point event)
                      (point))))
            (goto-char pt)
            (when-let ((buf (ignore-errors (ibuffer-current-buffer t))))
              (kill-buffer buf))))))
    (keymap-set ibuffer-name-map "<mouse-2>" #'my/ibuffer-kill-mouse-buffer)
    (keymap-set ibuffer-mode-filter-group-map "<mouse-1>" #'ibuffer-mouse-toggle-filter-group)
    (keymap-set ibuffer-mode-filter-group-map "<mouse-2>" #'ibuffer-mouse-toggle-mark)
    (defun my/customize-ibuffer ()
      (face-remap-add-relative 'fringe 'my/treemacs-bg)
      (face-remap-add-relative ibuffer-filter-group-name-face 'my/ibuffer-group)
      (set-fringe-mode 10))
    (add-hook 'ibuffer-sidebar-mode-hook #'my/customize-ibuffer)
    (defun my/ibuffer-remove-header (_)
      (save-excursion
        (when-let ((buf (ibuffer-sidebar-buffer)))
          (with-current-buffer buf
            (let ((ro buffer-read-only))
              (setq-local buffer-read-only nil)
              (unwind-protect
                   (progn
                     (goto-char 1)
                     (search-forward "-\n" nil t)
                     (delete-region 1 (point)))
                (setq-local buffer-read-only ro)))))))
    (advice-add 'ibuffer-update-title-and-summary :after #'my/ibuffer-remove-header))

(use-package treemacs
    :ensure t
    :demand
    :config
    (setopt treemacs-is-never-other-window t)
    (setopt treemacs-select-when-already-in-treemacs 'move-back)
    (setopt treemacs-width-is-initially-locked nil)
    (setopt treemacs-width 24)
    (setopt treemacs-move-files-by-mouse-dragging nil)
    (treemacs-follow-mode -1)
    (defun my/treemacs-hide-modeline (&rest _)
      (when (treemacs-is-treemacs-window-selected?)
        (text-scale-set -1)
        (setq mode-line-format nil)
        (set-window-fringes (selected-window) 10 1)))
    (defun my/customize-treemacs ()
      (keymap-set treemacs-mode-map "<mouse-1>" #'treemacs-single-click-expand-action)
      (face-remap-add-relative 'treemacs-root-face
                               'my/treemacs-big-face
                               :foreground (catppuccin-color 'teal))
      (face-remap-add-relative 'treemacs-root-remote-face
                               'my/treemacs-big-face
                               :foreground (catppuccin-color 'yellow))
      (face-remap-add-relative 'treemacs-root-remote-disconnected-face
                               'my/treemacs-big-face
                               :foreground (catppuccin-color 'mauve))
      (face-remap-add-relative 'treemacs-root-remote-unreadable-face
                               'my/treemacs-big-face
                               :foreground (catppuccin-color 'maroon))
      (dolist (face '(treemacs-directory-face treemacs-file-face
                      treemacs-git-unmodified-face treemacs-git-modified-face
                      treemacs-git-added-face treemacs-git-conflict-face
                      treemacs-git-renamed-face treemacs-git-untracked-face
                      treemacs-tags-face treemacs-term-node-face
                      treemacs-marked-file-face treemacs-git-commit-diff-face
                      treemacs-async-loading-face))
        (face-remap-add-relative face 'my/treemacs-face))
      (face-remap-add-relative 'treemacs-window-background-face 'my/treemacs-bg))
    (add-hook 'treemacs-select-functions #'my/treemacs-hide-modeline)
    (add-hook 'treemacs-mode-hook #'my/customize-treemacs))

(use-package treemacs-tab-bar
    :ensure t
    :after treemacs)

(use-package treemacs-projectile
    :ensure t
    :after (treemacs projectile))

(use-package treemacs-nerd-icons
    :ensure t
    :after treemacs
    :config (treemacs-load-theme "nerd-icons"))

(use-package hydra :ensure t :defer t)

(use-package flycheck
    :ensure t
    :defer t
    :init (add-hook 'prog-mode-hook #'flycheck-mode)
    :bind (("C-c e" . flycheck-next-error)
           ("C-c E" . flycheck-previous-error)
           ("C-c C-e" . flycheck-list-errors))
    :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package lsp-mode
    :ensure t
    :defer t
    :init
    (defun my/ls-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(flex)))
    :hook
    (lsp-mode . lsp-enable-which-key-integration)
    (lsp-completion-mode . my/lsp-mode-setup-completion)
    :config
    (setopt lsp-keymap-prefix "C-c l")
    (setopt lsp-completion-provider :none))

(use-package dap-mode
    :ensure t
    :defer t
    :after lsp-mode)

(use-package lsp-jedi
    :ensure t
    :defer t
    :after lsp-mode)

(use-package gdscript-mode
    :ensure t
    :after lsp-mode
    :defer t
    :if (eq system-type 'windows-nt)
    :bind (:map gdscript-mode-map ("<F5>" . gdscript-godot-run-project))
    :config
    (setq gdscript-use-tab-indents nil)
    (setq gdscript-godot-executable "C:/Programs/Executables/godot.exe"))

(use-package gdscript-hydra :ensure nil :after gdscript-mode :defer t)

;; The lsp client in lsp-gdscript doesn't know what to do with the
;; gdscript/capabilities notification. This ignores it.
(use-package lsp-gdscript
  :ensure nil
  :defer t
  :after gdscript-mode
  :defines lsp-register-client make-lsp-client lsp-gdscript-tcp-connect-to-port lsp-activate-on
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-gdscript-tcp-connect-to-port)
                    :activation-fn (lsp-activate-on "gdscript")
                    :server-id 'gdscript
                    :notification-handlers (ht ("gdscript/capabilities" 'ignore)))))

(use-package adaptive-wrap
  :ensure t
  :config
  (setopt adaptive-wrap-extra-indent 1)
  (add-hook 'prog-mode 'adaptive-wrap-prefix-mode)
  (set-fringe-bitmap-face 'right-curly-arrow 'ansi-color-magenta)
  (set-fringe-bitmap-face 'left-curly-arrow 'ansi-color-magenta))

(use-package helpful
    :ensure t
    :config
    (keymap-global-set "C-h f" #'helpful-callable)
    (keymap-global-set "C-h v" #'helpful-variable)
    (keymap-global-set "C-h k" #'helpful-key)
    (keymap-global-set "C-h x" #'helpful-command)
    (keymap-global-set "C-h C-h" #'helpful-at-point))

(use-package slime
    :ensure t
    :defer t
  :config (setopt inferior-lisp-program "sbcl"))

(use-package server
    :ensure nil
    :demand t
    :config (when (and (my/windows-p) (not (server-running-p)))
              (server-start)))

(use-package recentf
  :ensure nil
  :config
  (add-to-list 'recentf-exclude "/sudo:")
  (add-to-list 'recentf-exclude "/sudoedit:")
  (add-to-list 'recentf-exclude "/su:")
  (add-to-list 'recentf-exclude "/doas:"))

(use-package rect
  :ensure nil
  :config
  (keymap-set rectangle-mark-mode-map "C-i" 'string-insert-rectangle)
  (keymap-set rectangle-mark-mode-map "C-r" 'replace-rectangle))

(use-package browse-url
  :ensure nil
  :defer t
  :config
  (setopt browse-url-browser-function 'eww-browse-url))

(use-package apropos
  :ensure nil
  :defer t
  :bind (:map apropos-mode-map ("q" . kill-current-buffer))
  :config (setopt apropos-do-all t))


;;;; Customize built-in modes (use-package didn't work)
;;;; =========================================================================

(setopt custom-buffer-done-kill t)
(setopt help-window-select t)
(setopt help-downcase-arguments t)
(defun my/customize-help ()
  (face-remap-add-relative 'default 'my/help-bg)
  (face-remap-add-relative 'fringe 'my/help-bg))
(add-hook 'help-mode-hook #'my/customize-help)
(add-hook 'helpful-mode-hook #'my/customize-help)
(add-hook 'Custom-mode-hook #'my/customize-help)
(add-hook 'apropos-mode-hook #'my/customize-help)
(add-hook 'shortdoc-mode-hook #'my/customize-help)
(add-hook 'Info-mode-hook #'my/customize-help)

(defun my/customize-read-only ()
  (face-remap-add-relative 'default 'my/read-only)
  (face-remap-add-relative 'fringe 'my/read-only))

(add-hook 'view-mode-hook #'my/customize-read-only)
(add-hook 'messages-buffer-mode-hook #'my/customize-read-only)
(with-current-buffer (messages-buffer)
  (my/customize-read-only))

;; Even with helpful, the built-in help will get called from time to time.
;; Rename help buffers based on their topic.
(defun my/help-rename (&rest _)
  (when (string= (buffer-name) "*Help*")
    (let ((sym (plist-get help-mode--current-data :symbol)))
      (when sym
        (rename-buffer (concat "*Help* [" (symbol-name sym) "]"))))))
(advice-add 'help-make-xrefs :after #'my/help-rename)

(defun my/customize-minibuffer ()
  (face-remap-add-relative 'default 'my/minibuffer-bg)
  (face-remap-add-relative 'fringe 'my/minibuffer-bg))
(add-hook 'minibuffer-mode-hook #'my/customize-minibuffer)


;;;; Global minor modes and related options
;;;; =========================================================================

(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode t)

(tool-bar-mode -1)
(menu-bar-mode -1) ; shown in tab bar instead
(indent-tabs-mode -1) ; always use spaces
(context-menu-mode t)
(unless (display-graphic-p) (xterm-mouse-mode t))
(delete-selection-mode t)
(undelete-frame-mode t) ; allow restoring closed frames
(set-fringe-mode 20)
(minibuffer-depth-indicate-mode t)

(setopt window-divider-default-right-width 3)
(setopt window-divider-default-bottom-width 3)
(setopt window-divider-default-places t)
(window-divider-mode t)

(setopt pixel-scroll-precision-use-momentum t)
(setopt pixel-scroll-precision-interpolate-mice t)
(setopt pixel-scroll-precision-interpolate-page nil)
(setopt pixel-scroll-precision-large-scroll-height 1.0)
(setopt pixel-scroll-precision-interpolation-factor 2.0)
(setopt pixel-scroll-precision-interpolation-between-scroll (/ 1.0 60.0))
(setopt pixel-scroll-precision-interpolation-total-time 0.25)

(pixel-scroll-precision-mode t)

;;;; Options
;;;; =========================================================================

;; Fixes indentation of quoted lists
(setopt lisp-indent-function 'common-lisp-indent-function)
(setopt lisp-indent-defun-method '(2 &lambda &body))

;; Built-in completion options
(setopt enable-recursive-minibuffers t)
(setopt completion-cycle-threshold nil)
(setopt completions-detailed t)
(setopt tab-always-indent 'complete)
;; (setopt completion-styles '(basic flex))
(setopt completions-max-height 8)
(setopt completions-format 'one-column)
(setopt completions-header-format nil)
(setopt completion-on-separator-character t)
(setopt completion-show-help nil)

(setopt history-delete-duplicates t)

(setq minibuffer-beginning-of-buffer-movement t)
(setq minibuffer-default-prompt-format "")
(setq minibuffer-prompt-properties
        '(read-only t
          cursor-intangible t
          face minibuffer-prompt))

(add-to-list 'completion-ignored-extensions "__pycache__/")
(setopt read-extended-command-predicate
        #'command-completion-default-include-p)
(setopt sentence-end-double-space nil)
(setopt vc-follow-symlinks t)

(setopt mouse-wheel-progressive-speed t)
(setopt mouse-wheel-scroll-amount '(0.1
                                    ((shift) . 0.9)
                                    ((meta) . hscroll)
                                    ((control) . text-scale)
                                    ((control meta) . global-text-scale)))
(setopt mouse-buffer-menu-maxlen 64)
(setopt mouse-buffer-menu-mode-mult 999)
(setopt mouse-drag-and-drop-region-scroll-margin 2)
(setopt mouse-wheel-flip-direction t)
(setopt mouse-wheel-tilt-scroll t)

(setopt scroll-step 1) ; Allow scrolling line-by-line, mainly for terms
(setopt hscroll-step 1)
(setopt hscroll-margin 1)

(setopt x-underline-at-descent-line nil)
(setopt switch-to-buffer-obey-display-actions t)
(setopt column-number-mode t)
(setopt line-move-visual nil)
(setopt show-paren-delay 0.0)
(setopt show-paren-when-point-in-periphery t)
(setopt show-paren-when-point-inside-paren t)
(setopt cursor-in-non-selected-windows nil)
(setopt show-trailing-whitespace nil)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(setq-default buffer-file-coding-system 'utf-8-unix)

(setopt disabled-command-function 'ignore)
(setopt isearch-allow-scroll 'unlimited)
(setopt isearch-repeat-on-direction-change t)
(setopt isearch-wrap-pause 'no-ding)
(setopt search-default-mode t)

(setopt Man-width-max 120)

(setopt switch-to-buffer-in-dedicated-window 'pop)
(setopt switch-to-buffer-obey-display-actions t)

(setq display-buffer-alist
      `(((or (major-mode . Info-mode)
             (major-mode . help-mode)
             (major-mode . helpful-mode)
             (major-mode . apropos-mode)
             (major-mode . Custom-mode))
         (display-buffer-reuse-mode-window
          display-buffer-below-selected))
        ))

;; EXPERIMENTAL, not quite working yet
(when (my/windows-p)
  (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
  (setq shell-file-name explicit-shell-file-name)
  (add-to-list 'exec-path "C:/Program Files/Git/bin"))


;;;; Key Customization
;;;; =========================================================================

;; Change windows with Alt + Arrow Keys
(windmove-default-keybindings 'meta)

;; Allow ESC to quit prompts / etc, but customized to not close splits.
(defun my/keyboard-escape-quit-adv (fun)
  "Around advice for `keyboard-escape-quit` FUN.
Preserve window configuration when pressing ESC."
  (let ((old-buffer-quit-function buffer-quit-function))
    (setq buffer-quit-function (or buffer-quit-function #'keyboard-quit))
    (funcall fun)
    (setq buffer-quit-function old-buffer-quit-function)))
(advice-add #'keyboard-escape-quit :around #'my/keyboard-escape-quit-adv)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; I keep accidentally hitting this when trying to exit which-key
(global-unset-key (kbd "C-x ESC"))

(when (display-graphic-p) ; fix awful default GUI behavior
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

;;; Style note: not including the my/ prefix for interactive commands.

(defun theme-dark ()
  (interactive)
  (setq frame-background-mode 'dark)
  (mapc 'frame-set-background-mode (frame-list))
  (setopt catppuccin-flavor 'frappe)
  (load-theme 'catppuccin t))

(defun theme-light ()
  (interactive)
  (setq frame-background-mode 'light)
  (mapc 'frame-set-background-mode (frame-list))
  (setopt catppuccin-flavor 'latte)
  (load-theme 'catppuccin t))

;; Cycle file buffers, skipping others
(defun previous-file-buffer ()
  (interactive)
  (let ((old-switch-to-prev-buffer-skip switch-to-prev-buffer-skip))
    (setq switch-to-prev-buffer-skip
          (lambda (_ buf _) (eq nil (buffer-file-name buf))))
    (previous-buffer)
    (setq switch-to-prev-buffer-skip old-switch-to-prev-buffer-skip)))

(defun next-file-buffer ()
  (interactive)
  (let ((old-switch-to-prev-buffer-skip switch-to-prev-buffer-skip))
    (setq switch-to-prev-buffer-skip
          (lambda (_ buf _) (eq nil (buffer-file-name buf))))
    (next-buffer)
    (setq switch-to-prev-buffer-skip old-switch-to-prev-buffer-skip)))

(defun my/get-window-under-mouse ()
  (let ((mpos (mouse-position)))
    (or (window-at (cadr mpos) (cddr mpos) (car mpos))
        (selected-window))))

(defun my/call-in-window-under-mouse (fn &rest args)
  (let ((selected-window (selected-window))
        (target-window (my/get-window-under-mouse)))
    (unless (eq selected-window target-window)
      (select-window target-window 'mark-for-redisplay))
    (ignore-errors (apply fn args))
    (unless (eq selected-window target-window)
      (select-window selected-window t))))

(defmacro my/in-window-under-mouse (&rest body)
  `(my/call-in-window-under-mouse (lambda () ,@body)))

(defun back-or-previous-buffer ()
  (interactive)
  (my/in-window-under-mouse
   (or (and (eq major-mode 'help-mode)
            (ignore-errors (help-go-back) t))
       (and (eq major-mode 'Info-mode)
            (ignore-errors (Info-history-back) t))
       (previous-buffer))))

(defun forward-or-next-buffer ()
  (interactive)
  (my/in-window-under-mouse
   (or (and (eq major-mode 'help-mode)
            (ignore-errors (help-go-forward) t))
       (and (eq major-mode 'Info-mode)
            (ignore-erors (Info-history-forward ) t))
       (next-buffer))))

(keymap-global-set "M-[" 'previous-file-buffer)
(keymap-global-set "M-]" 'next-file-buffer)
;; Ctrl+Alt allows cycling any buffers, not just ones with files
(keymap-global-set "C-M-[" 'previous-buffer)
(keymap-global-set "C-M-]" 'next-buffer)
(keymap-global-set "<mouse-8>" 'back-or-previous-buffer)
(keymap-global-set "<mouse-9>" 'forward-or-next-buffer)

;; Middle clicking should delete the window instead of right click
(keymap-global-set "<mode-line> <mouse-2>" 'mouse-delete-window)
;; Right click should _not_ delete the window, much too easy to do accidentally.
;; Come up with something clever here, for now just show buffer menu.
(keymap-global-set "<mode-line> <mouse-3>" 'mouse-buffer-menu)

;; my custom sidebar, treemacs + ibuffer-sidebar
(defun show-sidebar ()
  (interactive)
  (ignore-errors (treemacs-select-window))
  (redisplay)
  (ignore-errors
    (when (treemacs-is-treemacs-window-selected?)
      (treemacs-select-window))
    (redisplay)) ; somehow keeps both in sync, sometimes breaks without
  (ibuffer-sidebar-show-sidebar)
  (with-selected-window (ibuffer-sidebar-showing-sidebar-p)
    (setq-local window-size-fixed nil)))


(defun hide-sidebar ()
  (interactive)
  (when-let ((treemacs-window (treemacs-get-local-window)))
    (delete-window treemacs-window))
  ;; Documentation on this is wrong (it returns the window, not the buffer)
  (when-let ((ibuffer-window (ibuffer-sidebar-showing-sidebar-p)))
    (delete-window ibuffer-window)))

(defun toggle-sidebar ()
  (interactive)
  (if (and (treemacs-get-local-window)
           (ibuffer-sidebar-showing-sidebar-p))
      (hide-sidebar)
    (show-sidebar)))

(keymap-global-set "C-c C-d" 'toggle-sidebar)

(defvar my/one-time-setup nil)
(defun my/first-time-theme-setup ()
  (unless my/one-time-setup
    (setq my/one-time-setup t)
    (message "one time theme apply %s" (server-running-p))
    (theme-dark)
    (message "done")
    )
  (remove-hook 'window-setup-hook 'my/first-time-theme-setup)
  (remove-hook 'server-after-make-frame-hook 'my/first-time-theme-setup))

(unless (server-running-p)
  (add-hook 'window-setup-hook #'my/first-time-theme-setup)
  (add-hook 'window-setup-hook #'show-sidebar))

(add-hook 'server-after-make-frame-hook #'my/first-time-theme-setup)
(add-hook 'server-after-make-frame-hook #'show-sidebar)

;; Make the sidebar play nice with transpose-frame and related functions.
;; Lots of paranoid redisplays here to make sure window changes have applied.
(defun my/hide-and-restore-sidebar (fn &rest args)
  (let ((treemacs-window (treemacs-get-local-window))
        (ibuffer-window (ibuffer-sidebar-showing-sidebar-p)))
    (when treemacs-window (delete-window treemacs-window))
    (when ibuffer-window (delete-window ibuffer-window))
    (redisplay)
    (apply fn args)
    (let ((cur-win (selected-window)))
      (when treemacs-window (treemacs-select-window) (redisplay))
      (when ibuffer-window (ibuffer-sidebar-show-sidebar) (redisplay))
      (select-window cur-win))))

(dolist (fn '(transpose-frame flip-frame flop-frame rotate-frame
              rotate-frame-clockwise rotate-frame-anticlockwise))
  (advice-add fn :around #'my/hide-and-restore-sidebar))


;; Make C-x k kill the current buffer without prompting to select a buffer
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(keymap-global-set "C-x k" 'kill-current-buffer)

(keymap-global-set "C-S-s" 'isearch-backward)
(keymap-set isearch-mode-map "C-S-s" 'isearch-repeat-backward)
(keymap-set help-mode-map "q" 'kill-current-buffer)
;(keymap-set backtrace-mode-map "q" 'kill-current-buffer)
(keymap-set special-mode-map "q" 'kill-current-buffer)


;; Vim-style, join down instead of up
(defun vim-join-line ()
  (interactive)
  (delete-indentation t))
(keymap-global-set "C-S-j" 'vim-join-line)

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)


;;;; Language-specific configuration
;;;; =========================================================================

(setopt python-indent-guess-indent-offset nil)


;;;; Misc
;;;; =========================================================================

(defun hide-buffer (name)
  (interactive "bBuffer to hide: ")
  (let ((b (get-buffer name)))
    (unless (string-match-p "^ " (buffer-name b))
      (with-current-buffer b
        (rename-buffer (concat " " (buffer-name b)))))))

;; File-type mode detection special cases
(add-to-list 'auto-mode-alist '("bashrc" . sh-mode)) ; no leading '.'

;; Make emacs source files (from help links, etc) open in view mode
;; NOTE: This should be after use-package stuff but before any files
;;       that might need to be edited by hand are added to load-path
(dir-locals-set-class-variables
 'emacs
 '((nil . ((eval . (when buffer-file-name
                     (view-mode-enter nil #'kill-buffer)))))))
(dolist (path (cons source-directory load-path))
  (dir-locals-set-directory-class path 'emacs))
