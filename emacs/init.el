(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-echo-area-message (user-login-name))
(when (window-system)
  (set-frame-size (selected-frame) 120 35))
(setq frame-resize-pixelwise t)

;; Prevent blinding startup window
(add-to-list 'default-frame-alist '(foreground-color . "#CCCCCC"))
(add-to-list 'default-frame-alist '(background-color . "#2E3440"))
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 35))
(set-foreground-color "#CCCCCC")
(set-background-color "#2E3440")

(set-face-attribute 'default nil :font "JetBrainsMono NF 12")
(set-face-attribute 'mode-line nil :font "JetBrainsMono NF 10")
(set-face-attribute 'mode-line-inactive nil :font "JetBrainsMono NF 10")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq inhibit-startup-message t)
(setopt display-time-default-load-average nil)

(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

(savehist-mode)

(windmove-default-keybindings 'meta)
(setopt sentence-end-double-space nil)
(setopt resize-mini-windows nil)
(setopt vc-follow-symlinks t)
;; Without leading dots, to match the versions in my dotfiles repo
(add-to-list 'auto-mode-alist '("bashrc" . sh-mode))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(indent-tabs-mode -1) ; always use spaces

(when (display-graphic-p) (context-menu-mode t))

;; Taken directly from emacs-bedrock.
;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath ))
         (backupFilePath (replace-regexp-in-string
			  "//" "/" (concat backupRootDir filePath "~"))))
    (make-directory (file-name-directory backupFilePath)
		    (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

(add-to-list 'completion-ignored-extensions "__pycache__/")

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
	    (setq which-key-max-display-columns 1)
	    (setq which-key-max-description-length 1.0)
	    (setq which-key-idle-delay 0.5)
	    (keymap-global-set "C-h h" 'which-key-show-major-mode)
    	    (keymap-global-set "C-h H" 'which-key-show-top-level)
	    ))

(use-package which-key-posframe
  :ensure t
  :after which-key
  :config (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-right-corner)
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
  :custom ((vertico-resize t)
	   (vertico-cycle t)
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
  :config (progn
	    (all-the-icons-completion-mode)
	    (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)))

(use-package corfu
  :ensure t
  :custom ((corfu-cycle t)
	   ;(corfu-auto t)
	   (corfu-quit-no-match t)
	   )
  :init (global-corfu-mode)
  :config (progn
	    (global-set-key (kbd "C-S-SPC") 'set-mark-command)
	    (global-set-key (kbd "C-SPC") 'completion-at-point)
	    (keymap-set corfu-map
		      "RET" `(menu-item "" nil :filter
					,(lambda (&optional _)
					   (and (derived-mode-p 'eshell-mode
								'comint-mode)
						#'corfu-send))))))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
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

(use-package kind-icon
  :ensure t
  :after corfu
  :config (progn
	    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
	    (let ((k (assoc 'keyword kind-icon-mapping)))
	      (setcdr k '("kw" :icon "rhombus-medium" :face font-lock-keyword-face))
	    )))

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
  :init (defun setup-eshell () (keymap-set eshell-mode-map "C-r" 'consult-history))
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
  :custom (global-unset-key (kbd "C-x M-g")))


;; Built-in completion options. I don't think these are active with
;; vertico and corfu, but just in case.
(setopt enable-recursive-minibuffers t)
(setopt completion-cycle-threshold 1)
(setopt completions-detailed t)
(setopt tab-always-indent 'complete)
(setopt completion-styles '(basic initials substring))
(setopt completion-auto-help 'always)
(setopt completions-max-height 10)
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

(setopt x-underline-at-descent-line nil)
(setopt switch-to-buffer-obey-display-actions t)
(setopt column-number-mode t)

(setq show-trailing-whitespace t)
(add-hook 'eat-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(setopt show-trailing-whitespace t)
(pixel-scroll-precision-mode)

