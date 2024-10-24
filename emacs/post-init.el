;;; FILENAME.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(load-theme 'modus-operandi t)

(set-frame-font "Iosevka 15" t t)

(setq truncate-lines nil)
(setq fast-but-imprecise-scrolling nil) ; check if this fixes vertico-mouse
(setq idle-update-delay 0.1)
(setq show-paren-delay 0)
(setq mouse-1-click-follows-link t)
(setq mouse-wheel-progressive-speed nil)
(setq shell-kill-buffer-on-exit t)
(setq eshell-kill-on-exit t)
(setq eshell-scroll-to-bottom-on-input 'this)
(setq c-ts-mode-indent-style 'k&r)
(setq c-ts-mode-indent-offset 4)
(setq c-default-style '((c-mode . "stroustrup")
                        (c++-mode . "stroustrup")
                        (java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))

(setq set-message-functions '(inhibit-message set-minibuffer-message))
(add-to-list 'inhibit-message-regexps "Cleaning up the recentf")

(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)
(add-hook 'after-init-hook #'pixel-scroll-precision-mode)


(setq evil-want-keybinding nil)
(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-disable-insert-state-bindings t)
  (evil-shift-round nil)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (define-key evil-window-map (kbd "o") 'evil-window-mru)
  (define-key evil-normal-state-map (kbd "<tab>") ">>")
  (define-key evil-normal-state-map (kbd "<backtab>") "<<")
  (define-key evil-visual-state-map (kbd "<tab>") ">")
  (define-key evil-visual-state-map (kbd "<backtab>") "<")
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

(with-eval-after-load "evil-collection"
  (defun clear-highlight-and-recenter (&optional arg)
    (interactive "P")
    (evil-ex-nohighlight)
    (recenter-top-bottom arg))
  (keymap-global-set "C-l" 'clear-highlight-and-recenter)
  ;; Make == execute vip=
  (defun indent-paragraph-or-evil-indent (fn beg end)
    ;; Condition from original evil-indent
    (if (and (= beg (line-beginning-position))
             (= end (line-beginning-position 2)))
        (save-excursion
          (execute-kbd-macro (read-kbd-macro "vip=")))
      (funcall fn beg end)))
  (advice-add 'evil-indent :around 'indent-paragraph-or-evil-indent)
  )

;; Fix mouse clicks in Customize buffers
(with-eval-after-load "evil"
  (with-eval-after-load "custom"
    (evil-make-overriding-map custom-mode-map)))

(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :custom
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000)))

(use-package undo-fu-session
  :ensure t
  :config (undo-fu-session-global-mode))

(use-package goto-chg :ensure t)

(use-package evil-visualstar
  :after evil
  :ensure t
  :defer t
  :commands global-evil-visualstar-mode
  :hook (after-init . global-evil-visualstar-mode))

(use-package evil-surround
  :after evil
  :ensure t
  :defer t
  :commands global-evil-surround-mode
  :hook (after-init . global-evil-surround-mode))

(with-eval-after-load "evil"
  (evil-define-operator my/evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my/evil-comment-or-uncomment))

(use-package vterm
  :ensure t
  :defer t
  :commands vterm
  :config (setq vterm-timer-delay 0.01))

(use-package devdocs
  :ensure t)
 
(use-package vertico
  :ensure t
  :defer t
  :commands (vertico-mode vertico-reverse-mode vertico-mouse-mode)
  :bind (:map vertico-map
              ("<prior>" . vertico-scroll-up)
              ("<next>" . vertico-scroll-down))
  :custom
  (vertico-cycle t)
  (vertico-scroll-margin 1)
  (vertico-resize t)
  :hook
  (after-init . vertico-mode)
  (after-init . vertico-reverse-mode)
  (after-init . vertico-mouse-mode))

(with-eval-after-load "vertico"
  (defun my/vertico-inverse (args) (list (- (car args))))
  (advice-add 'vertico-mouse--scroll-up :filter-args 'my/vertico-inverse)
  (defun my/crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add 'completing-read-multiple :filter-args 'my/crm-indicator))

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  :ensure t
  :defer t
  :bind (("C-." . embark-act)
         ("C-," . embark-dwim)
         ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;;(add-to-list 'display-buffer-alist
  ;;             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
  ;;               nil
  ;;               (window-parameters (mode-line-format . none))))
  )

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))


(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode corfu-popupinfo-mode)
  :hook ((after-init . global-corfu-mode)
         (after-init . corfu-popupinfo-mode))
  :bind (:map corfu-map
              ("<prior>" . corfu-scroll-down)
              ("<next>" . corfu-scroll-up)
              ("<tab>" . corfu-expand)
              ("<return>" . corfu-send))
  :custom
  (corfu-cycle t)
  (corfu-preselect 'valid)
  (corfu-preview-current nil)
  (corfu-on-exact-match 'show)
  (corfu-popupinfo-delay 0.1)
  (corfu-popupinfo-hide nil)
  (corfu-quit-no-match 'separator)
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package form-feed-st
  :ensure t
  :hook (after-init . global-form-feed-st-mode))

(show-paren-mode t)

(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets :ensure t)

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package pdf-tools
  :ensure t
  :defer t
  :hook (after-init . pdf-loader-install))

(use-package simple-modeline
  :ensure t
  :hook (after-init . simple-modeline-mode)
  :custom (simple-modeline-word-count-modes nil))

(use-package org
  :ensure nil
  :hook (org-mode . org-indent-mode))

(use-package minions
  :ensure t
  :custom
  (minions-mode-line-delimiters '(" " . ""))
  :hook (after-init . minions-mode))

(use-package winner-mode
  :ensure nil
  :bind (:map evil-window-map
              ("u" . winner-undo)
              ("C-r" . winner-redo))
  :custom (winner-dont-bind-my-keys t)
  :hook (after-init . winner-mode))

(defun my/colorize (text fg bg)
  (propertize text 'face `(:foreground ,fg :background ,bg)))

(defun my/vim-color ()
  (if (mode-line-window-selected-p)
      (pcase (symbol-name evil-state)
        ("normal"  "#a4d5f9")
        ("insert"  "#8adf80")
        ("visual"  "#fff576")
        ("replace" "#ff8f88")
        ("emacs"   "#ffddff")
        (_ 'unspecified))
    'unspecified))

(defun my/vim-state ()
  (if (mode-line-window-selected-p)
    (let ((mode-text (concat " " (upcase (symbol-name evil-state)) "▕")))
      (my/colorize mode-text "black" (my/vim-color)))
    ""))

(with-eval-after-load "minions"
  (with-eval-after-load "simple-modeline"
    (with-eval-after-load "evil"
      (defun my/evil-state () '(:eval (my/vim-state)))
      (defun my/modeline-modes ()
        (if (mode-line-window-selected-p)
            minions-mode-line-modes
          (format-mode-line mode-name)))
      (defun my/modeline-position ()
        (if (mode-line-window-selected-p)
            `(:propertize ("▏%3l : %2C") face (:background ,(my/vim-color)))
          ""))
      (defun my/modeline-extend (result)
        (concat result (my/colorize " " "black" (my/vim-color))))
      (advice-add 'simple-modeline--format :filter-return 'my/modeline-extend)
      (setopt simple-modeline-segments
              '((my/evil-state
                 simple-modeline-segment-modified
                 simple-modeline-segment-buffer-name)
                (simple-modeline-segment-misc-info
                 simple-modeline-segment-vc
                 my/modeline-modes
                 my/modeline-position)))
      (set-face-attribute 'fringe nil :background "white")
      )))

(keymap-global-set "C-x k" 'kill-current-buffer)
(keymap-global-set "<mode-line> <mouse-2>" 'mouse-delete-window)
(keymap-global-set "<mode-line> <mouse-3>" 'mouse-buffer-menu)

(when (treesit-available-p)
  (setq treesit-font-lock-level 4)
  (when (treesit-language-available-p 'cmake)
    (add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))))

(defun recompile-or-prompt ()
  (interactive)
  (if (string= compile-command "make -k ")
      ;; Subtle change from the default to make this not trigger a second time
      (progn (setq compile-command "make -k")
             (call-interactively 'compile))
    (call-interactively 'recompile)))

(keymap-global-set "<f5>" 'recompile-or-prompt)
(keymap-global-set "<f6>" 'compile)

;;(display-buffer-base-action '(display-buffer-same-window))
(setq switch-to-prev-buffer-skip-regexp (rx (seq bos (or "*" " "))))
(setq window-sides-slots '(1 0 0 2))
(setq fit-window-to-buffer-horizontally t)
(setq
 display-buffer-alist
 `(
   (,(rx (seq bos (or "*" " *")
             (or "Help" "Customize" "info" "eldoc" "Occur" "grep")))
    display-buffer-in-side-window
    (side . bottom) (slot . -1) (preserve-size . (nil . t)))
   (,(rx (seq bos (or "*" " *")
             (or "compilation" "shell" "eshell" "terminal" "vterm")))
    display-buffer-in-side-window
    (side . bottom) (slot . 1) (preserve-size . (nil . t)))
   ((mode comint-mode)
    display-buffer-in-side-window
    (side . bottom) (slot . 1) (preserve-size . (nil . t)))
   (,(rx (seq bos (or "*" " *")))
    (display-buffer-in-side-window display-buffer-no-window)
    (side . bottom))
   ))
