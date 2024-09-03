;; -*- lexical-binding: t -*-

;;;; Early / system settings
;;;; =========================================================================
(setopt display-time-default-load-average nil)
(setq debug-on-error t)
(add-hook 'after-init-hook (lambda () (setq debug-on-error nil)))

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

    (set-face-attribute 'default nil :family font-mono :height 130)
    (set-face-attribute 'fixed-pitch nil :family font-mono :height 130)
    (set-face-attribute 'fixed-pitch-serif nil :family font-fixed-serif :height 130)
    (set-face-attribute 'variable-pitch nil :family font-variable-pitch :height 130)))

(add-hook 'server-after-make-frame-hook #'my/setup-fonts)
(add-hook 'window-setup-hook #'my/setup-fonts)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t)


;;;; Packages
;;;; =========================================================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(setopt package-native-compile t)
(unless (package-installed-p 'use-package) (package-install 'use-package))
(setopt use-package-enable-imenu-support t)

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
  (setopt tab-bar-separator "")
  (setopt tab-bar-auto-width-max '(320 100))
  (setopt tab-bar-close-button-show nil)
  (defun my/tab-bar-name-padded (tab i)
    (propertize (concat "  " (tab-bar-tab-name-format-default tab i) "  ")
                'face (funcall tab-bar-tab-face-function tab)))
  (setopt tab-bar-tab-name-format-function #'my/tab-bar-name-padded)
  (global-set-key (kbd "C-S-t") 'tab-bar-new-tab-to)
  (tab-bar-mode t))

(use-package whitespace
  :ensure nil
  :hook (prog-mode . whitespace-mode)
  :config
  (setopt whitespace-style '(tab-mark))
  (setq-default indent-tabs-mode nil))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package catppuccin-theme
  :ensure t
  :config
  (setopt catppuccin-flavor 'frappe)
  (setopt catppuccin-italic-comments t)
  (defun my/customize-catppuccin ()
    (set-face-attribute 'cursor nil :background (catppuccin-color 'surface0 'latte))
    (set-face-attribute 'mode-line-active nil :background "#151520")
    (set-face-attribute 'mode-line-inactive nil :background "#202030")
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
    (set-face-attribute 'show-paren-match nil
                        :background (catppuccin-color 'crust)
                        :weight 'bold)
    (set-face-attribute 'tab-bar nil
                        :background (catppuccin-color 'mantle))
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :background (catppuccin-color 'mantle)
                        :family font-variable-pitch)
    (set-face-attribute 'tab-bar-tab nil
                        :background (catppuccin-color 'base)
                        :family font-variable-pitch
                        :weight 'bold))
  (add-hook 'after-load-theme-hook #'my/customize-catppuccin)
  (load-theme 'catppuccin t))

(use-package doom-modeline
  :ensure t
  :custom ((doom-modeline-icon t))
  :config
  (defun my/buffer-info ()
    (or
     (ignore-errors
       (concat
        (doom-modeline-spc)
        (doom-modeline--buffer-mode-icon)
        (doom-modeline--buffer-state-icon)
        (propertize (doom-modeline--buffer-simple-name)
                    'help-echo "Buffer name"
                    'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [mode-line mouse-1] 'mouse-buffer-menu)
                                 (define-key map [mode-line mouse-2] 'mouse-buffer-menu)
                                 (define-key map [mode-line mouse-3] 'mouse-buffer-menu)
                                 map)))) ""))
  ;; Overwrite the built-in buffer info segments
  (doom-modeline-def-segment buffer-info (my/buffer-info))
  (doom-modeline-def-segment buffer-info-simple (my/buffer-info))
  (doom-modeline-mode t))

(use-package which-key
  :ensure t
  :config
  (setq which-key-dont-use-unicode nil)
  (setq which-key-add-column-padding 2)
  (setq which-key-min-display-lines 8)
  (setq which-key-max-description-length 100)
  (setq which-key-max-display-columns (if (display-graphic-p) 1 nil))
  (setq which-key-max-description-length 1.0)
  (setq which-key-idle-delay 10000)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-secondary-delay 0.05)
  (keymap-global-set "C-h h" 'which-key-show-major-mode)
  (keymap-global-set "C-h H" 'which-key-show-top-level)
  (which-key-mode t))

(use-package nerd-icons :ensure t)

(use-package which-key-posframe
  :ensure t

  :after which-key
  :if (display-graphic-p)
  :config (setq which-key-posframe-poshandler
                'posframe-poshandler-frame-top-right-corner)
  :init (which-key-posframe-mode))

(use-package vertico
  :ensure t
  :bind (:map vertico-map ("TAB" . #'minibuffer-complete))
  :custom ((vertico-cycle t)
           (vertico-count 8))
  :init (vertico-mode))

(use-package vertico-reverse
  :ensure nil
  :after vertico
  :init (vertico-reverse-mode t))

(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :bind (:map corfu-map
         ("<tab>" . corfu-next)
         ("<backtab>" . corfu-previous))
  :config
  (setopt corfu-cycle t)
  (setopt corfu-preselect 'first))

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
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

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

(use-package embark-consult :ensure t)

(use-package embark
  :ensure t
  :bind (("C-c a" . embark-act)))

(use-package eshell
  :ensure t
  :defines eshell-mode-map
  :init (defun my/setup-eshell ()
          (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . my/setup-eshell)))

(use-package eat
  :ensure t
  :config
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode)
  (setopt eat-term-name "xterm-256color")
  (setopt eat-default-cursor-type '(box 0.5 hollow))
  (defun my/customize-eat ()
    (face-remap-add-relative 'default :background "#222228")
    (face-remap-add-relative 'fringe :background "#222228")
    (setq cursor-type 'box)
    (setq cursor-in-non-selected-windows t)
    (set-window-fringes (selected-window) 0))
  (add-hook 'eat-mode-hook #'my/customize-eat))

(use-package magit
  :ensure t
  :defer t
  :defines transient-map
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch)
         :map transient-map ("<escape>" . transient-quit-one))
  :config (global-unset-key (kbd "C-x M-g")))

(use-package transpose-frame :ensure t)
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
  :init (projectile-mode t)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :config
  (projectile-register-project-type 'godot '("project.godot")
                                    :project-file "project.godot"))

(use-package treemacs
  :ensure t
  :bind (("C-x C-d" . treemacs)
         ("C-x d" . treemacs-select-window)
         :map treemacs-mode-map
         ([mouse-1] . treemacs-single-click-expand-action))
  :init (add-hook 'window-setup-hook #'treemacs-start-on-boot)
  :config
  (setopt treemacs-width 24)
  (setopt treemacs-is-never-other-window t)
  (setopt imenu-auto-rescan t)
  (setopt treemacs-tag-follow-delay 2.0)
  (treemacs-tag-follow-mode t)
  (defun my/setup-treemacs-fonts (&rest _)
    (dolist (face '(treemacs-root-face treemacs-root-remote-face
                    treemacs-root-remote-disconnected-face
                    treemacs-root-remote-unreadable-face))
      (set-face-attribute face nil :inherit 'variable-pitch :height 1.25))
    (set-face-attribute 'treemacs-root-face nil :background 'unspecified)
    (dolist (face '(treemacs-directory-face treemacs-file-face  treemacs-git-unmodified-face))
      (set-face-attribute face nil :inherit 'variable-pitch :height 1.0))
    (set-face-attribute 'treemacs-window-background-face nil :background "#232634")
    (when (treemacs-is-treemacs-window? (selected-window))
      (setq mode-line-format nil)
      (set-window-fringes (selected-window) 8 1)))
  (advice-add 'treemacs :after #'my/setup-treemacs-fonts)
  (advice-add 'treemacs-select-window :after #'my/setup-treemacs-fonts)
  ;; Play nice with transpose-frame by forcing treemacs to hide
  (defun my/treemacs-ensure-hidden (&rest _)
    (let ((visible (progn (treemacs--select-visible-window)
                          (treemacs-is-treemacs-window? (selected-window)))))
      (when visible (treemacs))
      visible))
  (advice-add 'transpose-frame :before #'my/treemacs-ensure-hidden)
  (advice-add 'flip-frame :before #'my/treemacs-ensure-hidden)
  (advice-add 'flop-frame :before #'my/treemacs-ensure-hidden)
  (advice-add 'rotate-frame :before #'my/treemacs-ensure-hidden)
  (advice-add 'rotate-frame-clockwise :before #'my/treemacs-ensure-hidden)
  (advice-add 'rotate-frame-anticlockwise :before #'my/treemacs-ensure-hidden)
  (add-hook 'after-load-theme-hook #'my/treemacs-ensure-hidden))

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
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  :bind (("C-c e" . flycheck-next-error)
         ("C-c E" . flycheck-previous-error)
         ("C-c C-e" . flycheck-list-errors))
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package lsp-mode
  :ensure t
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

(use-package dap-mode :ensure t :defer t)

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
  :after gdscript-mode
  :defines lsp-register-client make-lsp-client lsp-gdscript-tcp-connect-to-port lsp-activate-on
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-gdscript-tcp-connect-to-port)
                    :activation-fn (lsp-activate-on "gdscript")
                    :server-id 'gdscript
                    :notification-handlers (ht ("gdscript/capabilities" 'ignore)))))

(use-package server
  :ensure nil
  :defines server-running-p
  :if (eq system-type 'windows-nt)
  :config (unless (server-running-p) (server-start)))


(use-package recentf
  :ensure nil
  :config
  (add-to-list 'recentf-exclude "/sudo:")
  (add-to-list 'recentf-exclude "/sudoedit:")
  (add-to-list 'recentf-exclude "/su:")
  (add-to-list 'recentf-exclude "/doas:"))

(defun my/customize-help ()
  (face-remap-add-relative 'default :background "#343442")
  (face-remap-add-relative 'fringe :background "#343442"))
(add-hook 'help-mode-hook #'my/customize-help)
(add-hook 'apropos-mode-hook #'my/customize-help)

(defun my/customize-minibuffer ()
  (face-remap-add-relative 'default :background "#2A2A32")
  (face-remap-add-relative 'fringe :background "#2A2A32"))
(add-hook 'minibuffer-mode-hook #'my/customize-minibuffer)


;;;; Global minor modes and related options
;;;; =========================================================================

(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(indent-tabs-mode -1) ; always use spaces
(context-menu-mode t)
(unless (display-graphic-p) (xterm-mouse-mode t))
(delete-selection-mode t)
(set-fringe-mode 8)


;;;; Options
;;;; =========================================================================

;; Built-in completion options
;(setopt enable-recursive-minibuffers t)
(setopt completion-cycle-threshold nil)
(setopt completions-detailed t)
(setopt tab-always-indent 'complete)
(setopt completion-styles '(basic substring flex))
(setopt completions-max-height 10)
(setopt completions-format 'one-column)
(add-to-list 'completion-ignored-extensions "__pycache__/")
(setopt read-extended-command-predicate #'command-completion-default-include-p)
(setopt sentence-end-double-space nil)
(setopt vc-follow-symlinks t)

(setopt mouse-wheel-progressive-speed nil)
(setopt mouse-wheel-scroll-amount '(0.2))
(setopt scroll-step 1) ; Allow scrolling line-by-line, particularly for terms

(setopt x-underline-at-descent-line nil)
(setopt switch-to-buffer-obey-display-actions t)
(setopt column-number-mode t)
(setopt line-move-visual nil)
(setopt cursor-in-non-selected-windows nil)
(setopt show-trailing-whitespace nil)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(setq-default buffer-file-coding-system 'utf-8-unix)


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

(global-set-key (kbd "M-[") 'previous-file-buffer)
(global-set-key (kbd "M-]") 'next-file-buffer)

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)


;;;; Language-specific configuration
;;;; =========================================================================

(setopt python-indent-guess-indent-offset nil)


;;;; Misc
;;;; =========================================================================

;; File-type mode detection special cases
(add-to-list 'auto-mode-alist '("bashrc" . sh-mode)) ; no leading '.'
