;;;; Early / system settings
;;;; =========================================================================
(setq gc-cons-threshold 10000000)
(setq read-process-output-max (* 1024 1024))
(setq byte-compile-warnings '(not obsolete))
(setq inhibit-startup-message t)
(put 'inhibit-startup-echo-area-message 'saved-value
     (setq inhibit-startup-echo-area-message (user-login-name)))
(setopt warning-suppress-log-types '((comp) (bytecomp)))
(setopt native-comp-async-report-warnings-errors 'silent)

;; Prevent blinding startup window
(set-foreground-color "#CCCCCC")
(set-background-color "#000000")
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(cursor-type . bar))

(defun windows-p () (eq system-type 'windows-nt))
(defun font-available-p (name) (member name (font-family-list)))

(defvar font-mono (face-attribute 'default :family))
(defvar font-variable-pitch (face-attribute 'variable-pitch :family))

(defun setup-fonts ()
  (when (display-graphic-p)
    (cond
     ((windows-p)
      (when (font-available-p "JetBrainsMonoNL NFM")
	(setq font-mono "JetBrainsMonoNL NFM"))
      (when (font-available-p "Segoe UI")
	(setq font-variable-pitch "Segoe UI")))
     (t
      (cond ((font-available-p "Iosevka Nerd Font")
	     (setq font-mono "Iosevka Nerd Font"))
	    ((font-available-p "JetBrainsMono Nerd Font")
	     (setq font-mono "JetBrainsMono Nerd Font")))
      (cond ((font-available-p "Roboto")
	     (setq font-variable-pitch "Roboto")))))

    (set-face-attribute 'default nil :family font-mono)
    (set-face-attribute 'fixed-pitch nil :family font-mono)
    (set-face-attribute 'variable-pitch nil :family font-variable-pitch)
    (when (facep 'treemacs-root-face)
      (set-face-attribute 'treemacs-root-face nil :font font-variable-pitch :height 1.2)
      (set-face-attribute 'treemacs-directory-face nil :font font-variable-pitch)
      (set-face-attribute 'treemacs-file-face nil :font font-variable-pitch))))

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
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons :ensure t)


(use-package modus-themes
  :ensure t
  :config
  (setopt modus-themes-italic-constructs t)
  (setopt modus-themes-bold-constructs t)
  (setopt modus-themes-variable-pitch-ui nil)
  (setopt modus-themes-mixed-fonts t)
  (setopt modus-themes-common-palette-overrides
	  '((bg-main "#202028")
	    ))
  (load-theme 'modus-vivendi t))

;; (use-package doom-modeline
;;   :ensure t
;;   :disabled
;;   :init (doom-modeline-mode t)
;;   :custom ((doom-modeline-icon t)))

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

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper))
  :config (progn
	    (setq ivy-use-virtual-buffers t)
 	    (setq ivy-use-selectable-prompt t)))

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
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s o" . consult-outline)
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
  :custom (eat-term-name "xterm-256color")
  :config (progn (eat-eshell-mode)
		 (eat-eshell-visual-command-mode)))

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
  :defines treemacs-follow-mode
  :config
;  (treemacs-resize-icons 16)
  (treemacs-follow-mode t)
  (setopt treemacs-width 24)
  (setopt treemacs-is-never-other-window t)
  (set-face-attribute 'treemacs-window-background-face nil :background "#0A0A0A")
  (add-hook 'treemacs-mode-hook (lambda ()
				  (setq-local mode-line-format nil)
				  (set-fringe-mode 1)))
  ;; Play nice with transpose-frame by forcing treemacs to hide
  (defun treemacs-ensure-hidden (&rest unused)
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
(setopt enable-recursive-minibuffers t)
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

;; Allow ESC to quit prompts / etc, but customized to not close splits.
(defun +keyboard-escape-quit-adv (fun)
  "Around advice for `keyboard-escape-quit` FUN.
Preserve window configuration when pressing ESC."
  (let ((buffer-quit-function (or buffer-quit-function #'keyboard-quit)))
    (funcall fun)))
(advice-add #'keyboard-escape-quit :around #'+keyboard-escape-quit-adv)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; I keep accidentally hitting this when trying to exit which-key
(global-unset-key (kbd "C-x ESC"))

(when (display-graphic-p) ; fix awful default GUI behavior
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)


;;;; Language-specific configuration
;;;; =========================================================================

(setopt python-indent-guess-indent-offset nil)


;;;; Misc
;;;; =========================================================================

;; File-type mode detection special cases
(add-to-list 'auto-mode-alist '("bashrc" . sh-mode)) ; no leading '.'
