;; -*- lexical-binding: t -*-

(my/user-load "bootstrap-elpaca.el")
(add-hook 'elpaca-after-init-hook (apply-partially #'my/user-load "custom.el"))

;; TODO: Move theme override stuff to theme-init.el
(elpaca sublime-themes
  (load-theme 'mccarthy t)
  (set-face-attribute 'default nil :foreground "#444")
  (set-face-attribute 'font-lock-builtin-face nil :foreground "#111")
  (set-face-attribute 'font-lock-string-face nil :foreground "#2c6415")
  (set-face-attribute 'font-lock-comment-face nil :foreground "#888")
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#888")
  (set-face-attribute 'font-lock-doc-face nil :weight 'light)
  (set-face-attribute 'show-paren-match nil
                      :box '(:line-width (-1 . -1))
                      :weight 'black
                      :background "white")
  (set-face-attribute 'show-paren-mismatch nil
                      :foreground "white"
                      :background "IndianRed4"
                      :inverse-video nil)
  (set-face-attribute 'trailing-whitespace nil
                      :background "gainsboro")
  (set-face-attribute 'isearch-fail nil :underline "red"
                      :foreground "red3" :background 'unspecified)
  (set-face-attribute 'highlight nil :background "LightBlue1")
  (set-face-attribute 'region nil :background "LightBlue1")
  (set-face-attribute 'mode-line nil :inherit '(variable-pitch))
  (set-face-attribute 'mode-line-inactive nil :inherit '(variable-pitch))
  (face-spec-set 'whitespace-tab '((t (:foreground "gray80" :background unspecified))))
  (let ((bg "#f6f7f8")
        (inactive-bg "#c6c7c8")
        (line-bg "#e6e7e8")
        (h 110))
    (face-spec-set 'window-divider-first-pixel `((t (:foreground ,bg))))
    (face-spec-set 'window-divider-last-pixel `((t (:foreground ,bg))))
    (set-face-attribute 'tab-line-tab nil :background bg :box 'unspecified :height h
                        :underline `(:color ,bg :position 0))
    (set-face-attribute 'tab-line-tab-current nil :foreground "black" :background bg
                        :box 'unspecified :height h :underline `(:color ,bg :position 0))
    (set-face-attribute 'tab-line-highlight nil :box 'unspecified :underline t :height h
                        :background 'unspecified :foreground 'unspecified :inherit nil)
    (set-face-attribute 'tab-line-tab-modified nil :weight 'bold :foreground 'unspecified :height h)
    (set-face-attribute 'tab-line-tab-inactive nil :background inactive-bg :height h
                        :underline `(:color ,inactive-bg :position 0))
    (set-face-attribute 'tab-line nil :background line-bg :underline `(:color ,inactive-bg :position 0))))

(elpaca doom-modeline
  (column-number-mode)
  (line-number-mode)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-icon nil
        doom-modeline-highlight-modified-buffer-name t
        doom-modeline-minor-modes t)
  (doom-modeline-mode)
  (dolist (face (list 'doom-modeline-buffer-file 'doom-modeline-project-dir
                      'doom-modeline-project-root-dir 'doom-modeline-project-parent-dir))
    (set-face-attribute face nil :inherit '(variable-pitch)))
  (set-face-attribute 'doom-modeline-bar nil
                      :inherit '(mode-line-active)
                      :foreground 'unspecified :background 'unspecified)
  (set-face-attribute 'doom-modeline-bar-inactive nil
                      :inherit '(mode-line-inactive)
                      :foreground 'unspecified :background 'unspecified)
  (set-face-attribute 'doom-modeline-buffer-minor-mode nil :foreground "grey95")
  (set-face-attribute 'doom-modeline-buffer-modified nil
                      :foreground 'unspecified ;"DarkOrange"
                      :weight 'bold
                      :inherit '(variable-pitch)))

(elpaca minions
  (minions-mode)
  (add-to-list 'minions-prominent-modes 'view-mode))

(elpaca embark
  (keymap-global-set "C-." 'embark-act)
  (keymap-global-set "C-," 'embark-dwim)
  (keymap-global-set "C-h B" 'embark-bindings)
  (setq prefix-help-command 'embark-prefix-help-command))

(elpaca consult
  (keymap-global-set "C-M-x" 'consult-mode-command)
  (keymap-global-set "C-x b" 'consult-buffer)
  (keymap-global-set "C-M-:" 'consult-complex-command)
  (keymap-global-set "C-x r b" 'consult-bookmark)
  (keymap-global-set "C-x p b" 'consult-project-buffer)
  (keymap-global-set "M-y" 'consult-yank-pop)
  (keymap-global-set "M-g e" 'consult-compile-error)
  (keymap-global-set "M-g f" 'consult-flymake)
  (keymap-global-set "M-g g" 'consult-goto-line)
  (keymap-global-set "M-g M-g" 'consult-goto-line)
  (keymap-global-set "M-g o" 'consult-outline)
  (keymap-global-set "M-g M" 'consult-global-mark)
  (keymap-global-set "M-g m" 'consult-mark)
  (keymap-global-set "M-g i" 'consult-imenu)
  (keymap-global-set "M-g I" 'consult-imenu-multi)
  (keymap-global-set "M-s d" 'consult-fd)
  (keymap-global-set "M-s g" 'consult-grep)
  (keymap-global-set "M-s G" 'consult-git-grep)
  (keymap-global-set "M-s r" 'consult-ripgrep)
  (keymap-global-set "M-s l" 'consult-line)
  (keymap-global-set "M-s L" 'consult-line-multi)
  (keymap-global-set "M-s k" 'consult-keep-lines)
  (keymap-global-set "M-s f" 'consult-focus-lines)
  (keymap-global-set "M-s h" 'consult-isearch-history)
  (keymap-global-set "M-s e" 'consult-eglot-symbols)
  (keymap-set isearch-mode-map "M-h" 'consult-isearch-history)
  (keymap-set isearch-mode-map "M-s h" 'consuilt-isearch-history)
  (keymap-set isearch-mode-map "M-s l" 'consult-line)
  (keymap-set isearch-mode-map "M-s L" 'consult-line-multi)
  (keymap-set minibuffer-local-map "M-s" 'consult-history)
  (keymap-set minibuffer-local-map "M-r" 'consult-history)
  (setq consult-narrow-key "C--")
  (setq register-preview-delay 0.25
        register-preview-function #'consult-register-format)
  (advice-add 'register-preview :override 'consult-register-window)
  (setq xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref)
  (require 'consult)
  (consult-customize consult-theme :preview-key '(:debounce 0.2 any)
                     consult-ripgrep consult-git-grep consult-grep
                     consult-bookmark consult-recent-file consult-xref
                     consult--source-file-register consult--source-recent-file
                     consult--source-bookmark consult--source-project-recent-file
                     :preview-key '(:debounce 0.4 any)))

(elpaca vertico
  (setq vertico-cycle t
        vertico-scroll-margin 1
        vertico-resize t)
  (vertico-mode)
  (vertico-reverse-mode)
  ;; Not actually using now, but keeping for possible future use
  ;; (vertico-multiform-mode)
  ;; (setq vertico-multiform-commands '()
  ;;       vertico-multiform-categories '())
  (keymap-set vertico-map "TAB" 'minibuffer-complete)
  (keymap-set vertico-map "M-P" 'previous-line-x8)
  (keymap-set vertico-map "M-N" 'next-line-x8)
  (keymap-set vertico-map "<prior>" 'vertico-scroll-up)
  (keymap-set vertico-map "<next>" 'vertico-scroll-down)
  (defun my/crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add 'completing-read-multiple :filter-args 'my/crm-indicator))

(elpaca corfu
  (setq corfu-cycle t
        corfu-popupinfo-delay '(0.25 . 0.1)
        corfu-popupinfo-hide nil
        corfu-preview-current nil
        corfu-on-exact-match nil
        corfu-quit-no-match 'separator
        corfu-preselect 'valid
        corfu-quit-at-boundary 'separator)
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (keymap-unset corfu-map "<remap> <move-beginning-of-line>")
  (keymap-unset corfu-map "<remap> <move-end-of-line>")
  (keymap-set corfu-map "<prior>" 'corfu-scroll-down)
  (keymap-set corfu-map "<next>" 'corfu-scroll-up)
  (keymap-set corfu-map "<tab>" 'corfu-expand)
  (keymap-set corfu-map "RET" 'corfu-send)
  (corfu-history-mode)
  (require 'savehist)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (defun corfu--preview-current-p ()
    (and corfu-preview-current (>= corfu--index 0)
         corfu--input
         (not (seq-contains-p (car corfu--input) corfu-separator)))))

(elpaca cape
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(elpaca embark-consult
  (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))

(elpaca marginalia
  (marginalia-mode)
  (keymap-set minibuffer-local-map "M-M" 'marginalia-cycle))

(elpaca orderless
  (setq orderless-component-separator 'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

(elpaca consult-eglot)
(elpaca consult-eglot-embark (consult-eglot-embark-mode))

;; Not sure I want to keep this
;; (elpaca yasnippet)
;; (elpaca yasnippet-snippets)
;; (elpaca consult-yasnippet
;;   (setq consult-yasnippet-use-thing-at-point t))

;; TODO: https://bard.github.io/emacs-run-command/quickstart
;; (elpaca run-command)

(elpaca ace-window
  (face-spec-set 'aw-leading-char-face
                 '((t (:height 360 :foreground "red"
                       :inherit (variable-pitch)))))
  (keymap-global-set "M-o" 'ace-window))

;;;; Misc small QoL packages

(elpaca form-feed-st
  (global-form-feed-st-mode)
  (set-face-attribute 'form-feed-st-line nil :foreground "grey75"))

(elpaca '(adaptive-wrap :host github :repo "emacsmirror/adaptive-wrap" :branch "master")
  (setq adaptive-wrap-extra-indent 1)
  (add-hook 'prog-mode-hook 'adaptive-wrap-prefix-mode))

(elpaca treesit-auto
  (require 'treesit-auto)
  (setq treesit-auto-install 'prompt)
  ;; The package author doesn't seem to update often. Patch or remove broken recipes.
  (defconst broken-treesit-auto '(markdown latex))
  (setq treesit-auto-langs
        (seq-difference (mapcar #'treesit-auto-recipe-lang treesit-auto-recipe-list)
                        broken-treesit-auto))
  (defun my/find-treesit-auto-recipe (lang)
    (car (seq-filter (lambda (x) (eq (treesit-auto-recipe-lang x) lang))
                                       treesit-auto-recipe-list)))
  ;; Janet has the wrong name
  (when-let ((janet (my/find-treesit-auto-recipe 'janet)))
    (setf (treesit-auto-recipe-lang janet) 'janet-simple))
  (global-treesit-auto-mode))

(defun my/elisp-imenu ()
  (add-to-list 'imenu-generic-expression '(nil "^(elpaca \\([^ )]*\\)" 1)))
(add-hook 'emacs-lisp-mode-hook 'my/elisp-imenu)
