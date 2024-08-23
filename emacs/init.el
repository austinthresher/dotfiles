;;;; Early / system settings
;;;; =========================================================================
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-echo-area-message (user-login-name))
(setq frame-resize-pixelwise t)

;; Prevent blinding startup window
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(cursor-type . bar))
(set-foreground-color "#CCCCCC")
(set-background-color "#000000")

(add-hook 'server-after-make-frame-hook
	  (lambda ()
	    (set-frame-size (selected-frame) 120 35)
	    (set-face-attribute 'default nil :font "JetBrainsMono NF 12")
	    (set-face-attribute 'mode-line nil :font "JetBrainsMono NF 10")
	    (set-face-attribute 'mode-line-inactive nil :font "JetBrainsMono NF 10")
	    (set-face-attribute 'variable-pitch nil :font "Roboto 12")))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setopt inhibit-startup-message t)
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
(pixel-scroll-precision-mode)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(indent-tabs-mode -1) ; always use spaces

(context-menu-mode t)
(unless (display-graphic-p)
  (xterm-mouse-mode t))


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

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t)
  :custom ((doom-modeline-height 36)
	   (doom-modeline-icon t)))

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

(use-package solaire-mode
  :ensure t
  :config (solaire-global-mode t))

;; Close runner-up, looks pretty nice
;(use-package nano-theme
;  :ensure t
;  :config (progn
;	    (setq nano-theme-light/dark 'dark)
;	    (load-theme 'nano t)))

(use-package modus-themes
  :ensure t
  :config (progn
	    (setq modus-themes-italic-constructs t)
	    (setq modus-themes-bold-constructs t)
	    (load-theme 'modus-vivendi-tinted t)))

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

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle)
         :map completion-list-mode-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package all-the-icons-completion
  :ensure t
  :after marginalia
  :if (display-graphic-p)
  :config (progn
	    (all-the-icons-completion-mode)
	    (add-hook 'marginalia-mode-hook
                      #'all-the-icons-completion-marginalia-setup)))

(use-package corfu
  :ensure t
  :custom ((corfu-cycle t)
	   (corfu-quit-no-match t))
  :init (global-corfu-mode)
  :config (progn
	    (global-set-key (kbd "C-S-SPC") 'completion-at-point)
	    (keymap-set corfu-map
		        "RET" `(menu-item "" nil :filter
					  ,(lambda (&optional _)
					     (and (derived-mode-p 'eshell-mode
								  'comint-mode)
						  #'corfu-send))))))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :if (display-graphic-p)
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom ((corfu-popupinfo-delay '(0.25 . 0.1))
	   (corfu-popupinfo-hide nil))
  :config (corfu-popupinfo-mode))

(use-package corfu-candidate-overlay
  :ensure t
  :after corfu
  :init (defun complete-corfu-or-tab (&optional arg)
	  (interactive "P")
	  (let ((str (condition-case nil
	 (corfu-candidate-overlay--get-overlay-property 'after-string)
		       (error ""))))
	    (if (string= "" str)
      		(indent-for-tab-command arg)
	      (corfu-candidate-overlay-complete-at-point))))
  :config (progn
	    (corfu-candidate-overlay-mode t)
	    (global-set-key "\t" 'complete-corfu-or-tab)))

(use-package corfu-terminal
  :ensure t
  :if (not (display-graphic-p))
  :after corfu
  :init (corfu-terminal-mode t))

(use-package kind-icon
  :ensure t
  :after corfu
  :config (progn
	    (add-to-list 'corfu-margin-formatters
                         #'kind-icon-margin-formatter)
	    (let ((k (assoc 'keyword kind-icon-mapping)))
	      (setcdr k '("kw" :icon "rhombus-medium"
                          :face font-lock-keyword-face)))))

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

(use-package embark-consult
  :ensure t)

(use-package embark
  :ensure t
  :demand t
  :bind (("C-c a" . embark-act)))

(use-package eshell
  :ensure t
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
  :bind (("C-x g" . magit-status)
	 ("C-x G" . magit-dispatch)
	 :map transient-map ("<escape>" . transient-quit-one))
  :config (global-unset-key (kbd "C-x M-g")))

(use-package yaml-mode :ensure t)
(use-package json-mode :ensure t)
(use-package transpose-frame :ensure t)

(use-package dired-sidebar
  :ensure t
  :bind (("C-x C-d" . dired-sidebar-toggle-sidebar)
	 ("C-x d" . dired-sidebar-jump-to-sidebar))
  :custom-face
  (dired-sidebar-custom-face ((t (:font "JetBrainsMono NF 10"))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setopt dired-sidebar-display-remote-icons t)
  (setopt dired-sidebar-follow-file-at-point-on-toggle-open t)
  (setopt dired-sidebar-follow-file-idle-delay 1.0)
  (setopt dired-sidebar-should-follow-file t)
  (setopt dired-sidebar-theme 'nerd)
  (setopt dired-sidebar-resize-on-open nil)
  (setopt dired-sidebar-pop-to-sidebar-on-toggle-open nil)
  (setopt dired-sidebar-use-one-instance t)
  (setopt dired-sidebar-window-fixed nil)
  (setopt dired-sidebar-use-custom-font t)
  (setopt dired-sidebar-face 'dired-sidebar-custom-face))

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
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)

(setopt sentence-end-double-space nil)
(setopt vc-follow-symlinks t)

(setopt x-underline-at-descent-line nil)
(setopt switch-to-buffer-obey-display-actions t)
(setopt column-number-mode t)
(setopt line-move-visual nil)
(setopt cursor-in-non-selected-windows nil)
(setopt show-trailing-whitespace nil)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))


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
