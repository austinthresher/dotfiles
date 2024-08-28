;; -*- lexical-binding: t -*-

;;;; Early / system settings
;;;; =========================================================================
(setq gc-cons-threshold 10000000)
(setq read-process-output-max (* 1024 1024))
(setq byte-compile-warnings '(not obsolete))
(setq inhibit-startup-message t)
;; This worked but gave an error when trying to save custom.el
;; (put 'inhibit-startup-echo-area-message 'saved-value
;; (setq inhibit-startup-echo-area-message (user-login-name)))
(setopt warning-suppress-log-types '((comp) (bytecomp)))
(setopt native-comp-async-report-warnings-errors 'silent)

;; Prevent blinding startup window
(set-foreground-color "#CCCCCC")
(set-background-color "#111111")
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(cursor-type . bar))

(defun windows-p () (eq system-type 'windows-nt))
(defun font-available-p (name) (member name (font-family-list)))

(defvar font-mono (face-attribute 'default :family))
(defvar font-fixed-serif (face-attribute 'fixed-pitch-serif :family))
(defvar font-variable-pitch (face-attribute 'variable-pitch :family))

(defun setup-fonts ()
  (when (display-graphic-p)
    (cond
     ((windows-p)
      (cond ((font-available-p "Iosevka NF")
	     (setq font-mono "Iosevka NF")
	     (setq font-fixed-serif font-mono))
	    ((font-available-p "JetBrainsMono NF")
	     (setq font-mono "JetBrainsMono NF")
	     (setq font-fixed-serif font-mono)))
      (when (font-available-p "Segoe UI")
	(setq font-variable-pitch "Segoe UI"))
      (when (font-available-p "IosevkaTermSlab NF")
	(setq font-fixed-serif "IosevkaTermSlab NF")))
     (t
      (cond ((font-available-p "Iosevka Nerd Font Propo")
	     (setq font-mono "Iosevka Nerd Font Propo")
	     (setq font-fixed-serif font-mono))
	    ((font-available-p "JetBrainsMono Nerd Font")
	     (setq font-mono "JetBrainsMono Nerd Font")
	     (setq font-fixed-serif font-mono)))
      (when (font-available-p "IosevkaTermSlab Nerd Font Propo")
	(setq font-fixed-serif "IosevkaTermSlab Nerd Font Propo"))
      (cond
       ((font-available-p "Asap SemiCondensed")
	(setq font-variable-pitch "Asap SemiCondensed"))
       ((font-available-p "Roboto") (setq font-variable-pitch "Roboto")))))

    (set-face-attribute 'default nil :family font-mono :height 130)
    (set-face-attribute 'fixed-pitch nil :family font-mono :height 130)
    (set-face-attribute 'fixed-pitch-serif nil :family font-fixed-serif :height 130)
    (set-face-attribute 'variable-pitch nil :family font-variable-pitch :height 130)))

(add-hook 'server-after-make-frame-hook #'setup-fonts)
(add-hook 'window-setup-hook #'setup-fonts)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t)

(setopt display-time-default-load-average nil)

;; Taken directly from emacs-bedrock.
;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun get-backup-file-name (fpath)
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath ))
         (backupFilePath (replace-regexp-in-string
			  "//" "/" (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath)
		    (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'get-backup-file-name)
(add-to-list 'completion-ignored-extensions "__pycache__/")


;;;; Global minor modes and related options
;;;; =========================================================================

(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)
(savehist-mode)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(indent-tabs-mode -1) ; always use spaces
(context-menu-mode t)
(unless (display-graphic-p) (xterm-mouse-mode t))
(delete-selection-mode t)


;;;; Packages
;;;; =========================================================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))

(use-package rainbow-delimiters
  :ensure t
  :after catppuccin-theme
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package catppuccin-theme
  :ensure t
  :config
  (setopt catppuccin-flavor 'frappe)
  (setopt catppuccin-italic-comments t)
  (load-theme 'catppuccin t)
  ;; Default colors are too easy to mix up
  (custom-set-faces
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,(catppuccin-color 'text)))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,(catppuccin-color 'blue)))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,(catppuccin-color 'yellow)))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,(catppuccin-color 'green)))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,(catppuccin-color 'peach)))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,(catppuccin-color 'teal)))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,(catppuccin-color 'mauve)))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,(catppuccin-color 'rosewater)))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,(catppuccin-color 'green)))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,(catppuccin-color 'red)
                                            :background ,(catppuccin-color 'crust)))))
   `(show-paren-match ((t (:foreground ,(catppuccin-color 'pink)
                           :background ,(catppuccin-color 'surface1)
                           :weight bold))))))


(use-package doom-modeline
  :ensure t
  :custom ((doom-modeline-icon t))
  :config
  (defun custom-buffer-info ()
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
  ;; Overwrite the built-in buffer info segments with my
  ;; customized version.
  (doom-modeline-def-segment buffer-info (custom-buffer-info))
  (doom-modeline-def-segment buffer-info-simple (custom-buffer-info))
  (doom-modeline-mode t))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode)
  :config (progn
	    (setq which-key-dont-use-unicode nil)
	    (setq which-key-add-column-padding 2)
	    (setq which-key-min-display-lines 8)
	    (setq which-key-max-description-length 100)
	    (setq which-key-max-display-columns (if (display-graphic-p) 1 nil))
	    (setq which-key-max-description-length 1.0)
	    (setq which-key-idle-delay 0.5)
	    (keymap-global-set "C-h h" 'which-key-show-major-mode)
    	    (keymap-global-set "C-h H" 'which-key-show-top-level)))

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

(use-package nerd-icons :ensure t)

(use-package company
  :ensure t
  :defines company-in-string-or-comment
  :bind (("C-S-SPC" . company-complete)
	 :map company-active-map ("ESC" . company-abort))
  :config
  (global-company-mode t)
  (company-tng-mode t)
  (setopt company-idle-delay
          (lambda () (if (company-in-string-or-comment) nil 0.2)))
  ;; (setopt company-global-modes '(not <add modes here>))
  (setopt company-selection-wrap-around t)
  (setopt company-tooltip-align-annotations t))

(use-package company-quickhelp
  :ensure t
  :after company
  :config (company-quickhelp-mode t))

(use-package company-quickhelp-terminal
  :ensure t
  :after company-quickhelp
  :init (add-hook 'after-make-frame-functions
		  (lambda ()
		    (unless (window-system)
		      (company-quickhelp-terminal-mode t)))))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
	 ("M-y" . consult-yank-pop)
	 ("M-s r" . consult-ripgrep)
	 ("C-S-s" . consult-line)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)
	 ("M-s e" . consult-isearch-history)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi))
  :config (setq consult-narrow-key "<"))

(use-package embark-consult :ensure t)

(use-package embark
  :ensure t
  :demand t
  :bind (("C-c a" . embark-act)))

(use-package eshell
  :ensure t
  :defines eshell-mode-map
  :init (defun setup-eshell ()
          (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . setup-eshell)))

(use-package eat
  :ensure t
  :config
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode)
  (setopt eat-term-name "xterm-256color")
  (setopt eat-default-cursor-type '(box 0.5 hollow))
  (add-hook 'eat-mode-hook
	    (lambda () (face-remap-add-relative 'default :background "#222228"))))

(use-package magit
  :ensure t
  :defines transient-map
  :bind (("C-x g" . magit-status)
	 ("C-x G" . magit-dispatch)
	 :map transient-map ("<escape>" . transient-quit-one))
  :config (global-unset-key (kbd "C-x M-g")))

(use-package yaml-mode :ensure t)
(use-package json-mode :ensure t)
(use-package transpose-frame :ensure t)

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
  :init (add-hook 'window-setup-hook 'treemacs-start-on-boot)
  :config
  (setopt treemacs-width 24)
  (setopt treemacs-is-never-other-window t)
  (setopt imenu-auto-rescan t)
  (setopt treemacs-tag-follow-delay 1.0)
  (treemacs-tag-follow-mode t)
  (defun setup-treemacs-fonts (&rest _)
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
      (set-fringe-mode 1)))
  (advice-add 'treemacs :after #'setup-treemacs-fonts)
  (advice-add 'treemacs-select-window :after #'setup-treemacs-fonts)
  ;; Play nice with transpose-frame by forcing treemacs to hide
  (defun treemacs-ensure-hidden (&rest _)
    (let ((visible (progn (treemacs--select-visible-window)
			  (treemacs-is-treemacs-window? (selected-window)))))
      (when visible (treemacs))))
  (advice-add 'transpose-frame :before #'treemacs-ensure-hidden)
  (advice-add 'flip-frame :before #'treemacs-ensure-hidden)
  (advice-add 'flop-frame :before #'treemacs-ensure-hidden)
  (advice-add 'rotate-frame :before #'treemacs-ensure-hidden)
  (advice-add 'rotate-frame-clockwise :before #'treemacs-ensure-hidden)
  (advice-add 'rotate-frame-anticlockwise :before #'treemacs-ensure-hidden))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package treemacs-nerd-icons
  :ensure t
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))

(use-package hydra :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (("C-c e" . flycheck-next-error)
	 ("C-c E" . flycheck-previous-error)
	 ("C-c C-e" . flycheck-list-errors))
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (lsp-mode . lsp-enable-which-key-integration))

(use-package dap-mode :ensure t)

(use-package gdscript-mode
  :ensure t
  :after lsp-mode
  :if (eq system-type 'windows-nt)
  :bind (:map gdscript-mode-map ("<F5>" . gdscript-godot-run-project))
  :config
  (setq gdscript-use-tab-indents nil)
  (setq gdscript-godot-executable "C:/Programs/Executables/godot.exe"))

(use-package gdscript-hydra :ensure nil :after gdscript-mode)

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

;;;; Options
;;;; =========================================================================

;; Built-in completion options (corfu doesn't support terminal mode)
;(setopt enable-recursive-minibuffers t)
(setopt completion-cycle-threshold 1)
(setopt completions-detailed t)
(setopt tab-always-indent 'complete)
(setopt completion-styles '(basic initials substring))
(setopt completions-max-height 10)
(setopt completions-format 'one-column)

(setopt sentence-end-double-space nil)
(setopt vc-follow-symlinks t)

(setopt mouse-wheel-progressive-speed nil)
(setopt mouse-wheel-scroll-amount '(0.25))

(setopt x-underline-at-descent-line nil)
(setopt switch-to-buffer-obey-display-actions t)
(setopt column-number-mode t)
(setopt line-move-visual nil)
(setopt cursor-in-non-selected-windows nil)
(setopt show-trailing-whitespace nil)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(setq-default buffer-file-coding-system 'utf-8-unix)

;;;; Key Customization
;;;; =========================================================================

;; Change windows with Alt + Arrow Keys
(windmove-default-keybindings 'meta)

;; TODO: Rewrite to support lexical binding
;; Allow ESC to quit prompts / etc, but customized to not close splits.
;; (defun +keyboard-escape-quit-adv (fun)
;;   "Around advice for `keyboard-escape-quit` FUN.
;; Preserve window configuration when pressing ESC."
;;   (cond (company-mode
;; 	 (let ((buffer-quit-function (or buffer-quit-function #'company-abort)))
;; 	   (funcall fun)))
;; 	(t (let ((buffer-quit-function (or buffer-quit-function #'keyboard-quit)))
;; 	     (funcall fun)))))
;; (advice-add #'keyboard-escape-quit :around #'+keyboard-escape-quit-adv)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; I keep accidentally hitting this when trying to exit which-key
(global-unset-key (kbd "C-x ESC"))

(when (display-graphic-p) ; fix awful default GUI behavior
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

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

(global-set-key (kbd "C-<tab>") 'previous-file-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'next-file-buffer)

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)


;;;; Language-specific configuration
;;;; =========================================================================

(setopt python-indent-guess-indent-offset nil)


;;;; Misc
;;;; =========================================================================

;; File-type mode detection special cases
(add-to-list 'auto-mode-alist '("bashrc" . sh-mode)) ; no leading '.'
