;;; -*- no-byte-compile: t; lexical-binding: t; -*-

;; TODOs that probably require writing elisp:
;; - Make keybinds for the mouse back/forward buttons that call appropriate
;;   functions in the window under the mouse, based on the major mode of that
;;   window's buffer. pdf-history-backward, help-go-back, etc.
;; - Try to figure out triggering sp-comment when the comment string is typed
;;   for the current language's mode.


;;;; Utility macros
;;;; ======================================================================

(defmacro add-to-list* (ls &rest vals)
  "Add multiple items to the same list. Expands to multiple add-to-list calls."
  (let ((exps))
    (dolist (v vals)
      (push `(add-to-list ,ls ,v) exps))
    (cons 'progn (nreverse exps))))


;;;; Theme and font
;;;; ======================================================================
;; (setq modus-themes-headings
;;       (quote ((t . (variable-pitch medium 1.1)))))
(setq modus-themes-mixed-fonts t)
(load-theme 'modus-operandi t)

(set-face-attribute 'default nil :family "Iosevka" :height 140)
(set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 130)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Slab" :height 140)


;;;; General settings
;;;; ======================================================================

(setq truncate-lines nil)
(setq fast-but-imprecise-scrolling nil)
(setq idle-update-delay 0.1)
(setq mouse-1-click-follows-link t)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(0.2
                                  ((shift) . 0.9)
                                  ((control meta) . global-text-scale)
                                  ((control) . text-scale)
                                  ((meta) . hscroll)))
(setq next-screen-context-lines 1)
(setq shell-kill-buffer-on-exit t)
(setq eshell-kill-on-exit t)
(setq eshell-scroll-to-bottom-on-input 'this)
(setq compilation-scroll-output t)
(setq c-ts-mode-indent-style 'k&r)
(setq c-ts-mode-indent-offset 4)
(setq c-default-style '((c-mode . "stroustrup")
                        (c++-mode . "stroustrup")
                        (java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))
(setq tab-width 8)
(set-face-attribute 'window-divider-first-pixel nil :foreground "white")
(set-face-attribute 'window-divider-last-pixel nil :foreground "white")
(setopt window-divider-default-right-width 3)


;;;; Cursor customization
;;;; ======================================================================

(setq blink-cursor-blinks 0)
(setq blink-cursor-delay 0.2)
(setq blink-cursor-interval 0.1)
(setq-default cursor-type 'box)
(setq blink-cursor-alist '((box . box) (hollow . hollow) (bar . bar)
                           (t . box) (nil . box)
                           ((box . 1) . box)
                           ((hbar . 2) . (hbar . 2))
                           ((hbar . 4) . (hbar . 4))
                           ((hbar . 6) . (hbar . 6))
                           ((hbar . 16) . (hbar . 16))
                           ((bar . 1) . (bar . 1))
                           ((bar . 2) . (bar . 2))
                           ((bar . 3) . (bar . 3))))
(setq cursor-in-non-selected-windows nil)

(defvar my/cursor-color-idx 0)
(defun my/get-cursor-color ()
  (nth my/cursor-color-idx
       (if (and (boundp 'evil-state) (eq evil-state 'emacs))
           '("#FF00FF" "#FF33FF" "#FF77FF" "#FFBBFF"
             "#FFEEFF" "#FFBBFF" "#FF77FF" "#FF33FF")
         '("#000000" "#333333" "#777777" "#BBBBBB"
           "#EEEEEE" "#BBBBBB" "#777777" "#333333"))))

(defvar my/underline-ov nil)

(defun my/update-underline-color ()
  (when (overlayp my/underline-ov)
    (overlay-put my/underline-ov 'face
                 `(:underline (:color ,(my/get-cursor-color) :position 0)))))

(defun my/update-cursor-overlay (&rest _)
  (or (ignore-errors
        (if (not (eq evil-state 'normal))
            (when (overlayp my/underline-ov) (delete-overlay my/underline-ov))
          (unless (overlayp my/underline-ov)
            (setq my/underline-ov (make-overlay (point) (point))))
          (my/update-underline-color)
          (let ((start (point)))
            (save-excursion
              (forward-char)
              (move-overlay my/underline-ov start (point) (current-buffer)))))
        t)
      ;; If we had any errors, just delete the overlay entirely to try again
      ;; next time.
      (progn (when (overlayp my/underline-ov) (delete-overlay my/underline-ov))
             (setq my/underline-ov nil))))

(add-hook 'post-command-hook 'my/update-cursor-overlay)

(defun my/cursor-color-reset (&rest _)
  (setq my/cursor-color-idx 0)
  (set-face-attribute 'cursor nil :background (my/get-cursor-color))
  (my/update-underline-color))

(defun my/cursor-color-advance (&rest _)
  (setq my/cursor-color-idx (% (+ 1 my/cursor-color-idx) 8))
  (set-face-attribute 'cursor nil :background (my/get-cursor-color))
  (my/update-underline-color))

(advice-add 'blink-cursor-start :before 'my/cursor-color-reset)
(advice-add 'blink-cursor-end :before 'my/cursor-color-reset)
(advice-add 'blink-cursor-timer-function :before 'my/cursor-color-advance)
(blink-cursor-mode t)


;;;; Less noisy minibuffer
;;;; ======================================================================

(setq set-message-functions '(inhibit-message set-minibuffer-message))
(add-to-list* 'inhibit-message-regexps
              "Cleaning up the recentf"
              "Mark saved")


;;;; Initial built-in minor modes
;;;; ======================================================================

(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)
(global-prettify-symbols-mode t)
(show-paren-mode -1)


;;;; File type associations
;;;; ======================================================================

(add-to-list* 'auto-mode-alist
              '("bashrc" . sh-mode) ; matches bashrc and bashrc_local (no dot)
              '("profile\\'" . sh-mode) ; matches .profile and .bash_profile
              )

(when (treesit-available-p)
  (setq treesit-font-lock-level 4)
  (when (treesit-language-available-p 'cmake)
    (add-to-list* 'auto-mode-alist
                  '("CMakeLists.txt" . cmake-ts-mode)
                  '("\\.cmake\\'" . cmake-ts-mode))))


;;;; Customization functions that can be used with hooks or advice
;;;; ======================================================================

(defun my/no-mode-line (&rest _)
  "Add this as a hook to modes that should not have a modeline"
  (setq-local mode-line-format nil))

(defun my/no-fringes (&rest _)
  "Add this as a hook to buffers that should not show fringes"
  (setq-local left-fringe-width 1
              right-fringe-width 1))

(defun my/no-fringes-redisplay (&rest _)
  "Performs the redisplay that is necessary for the fringes change to appear."
  (my/no-fringes)
  (set-window-buffer (selected-window) (current-buffer)))

(defun my/margins (&rest _)
  "Add this as a hook to buffers that should have extra margins"
  (setq-local left-margin-width 1
              right-margin-width 1))

(defun my/margins-redisplay (&rest _)
  "Performs the redisplay that is necessary for the margin change to appear."
  (my/margins)
  (set-window-buffer (selected-window) (current-buffer)))

(defun my/no-blink-cursor (&rest _)
  "Add this as a hook to buffers that should not blink the cursor"
  (setq-local blink-cursor-mode nil))

(defun my/word-wrap (&rest _)
  "Add this as a hook to force word wrap in a buffer"
  (setq-local truncate-lines nil)
  (word-wrap-whitespace-mode t))

(defun my/smaller-fonts (&rest _)
  "Add this as a hook to have smaller fonts in a buffer"
  (face-remap-add-relative 'default '(:height 120))
  (face-remap-add-relative 'variable-pitch '(:height 110))
  (face-remap-add-relative 'fixed-pitch '(:height 120))
  (face-remap-add-relative 'header-line '(:height 120)))

;;;; External Packages
;;;; ======================================================================

;; Enable extra use-package keywords
(use-package general :ensure t :demand t)

;; Fix elisp indentation of property lists
(use-package fuco1-redef-lisp-indent :ensure t :demand t
  :vc (:url "https://git.sr.ht/~razzi/fuco1-redef-lisp-indent.el"))

(use-package evil :ensure t :demand t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-shift-round nil)
  (evil-want-empty-ex-last-command nil)
  (evil-echo-state nil)
  (evil-ex-search-persistent-highlight t)
  (evil-move-beyond-eol t)
  (evil-move-cursor-back nil)
  (evil-split-window-below t)
  :config
  (setq evil-lookup-func 'help-follow-symbol)
  (setq evil-default-cursor '((bar . 2)))
  (setq evil-emacs-state-cursor '((bar . 2)))
  (setq evil-insert-state-cursor '((bar . 1)))
  (setq evil-normal-state-cursor evil-default-cursor)
  (setq evil-motion-state-cursor '((hbar . 2)))
  (setq evil-visual-state-cursor 'box)
  (setq evil-operator-state-cursor 'hollow)
  (add-hook 'evil-normal-state-entry-hook 'my/cursor-color-reset)
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; Evil doesn't give an option to hide the previous search term.
  ;; Always show a blank prompt when performing a search.
  (defun my/hide-prev-search (args)
    (if (string-prefix-p "evil-ex-search" (symbol-name this-command))
        `(,(car args) "" ,@(cddr args))
      args))
  (advice-add 'read-string :filter-args 'my/hide-prev-search)
  (defun my/auto-clear-anzu (&rest _) (anzu--reset-status))
  (advice-add 'evil-force-normal-state :after 'evil-ex-nohighlight)
  (advice-add 'evil-force-normal-state :after 'deactivate-mark)
  (advice-add 'evil-next-line :after 'my/auto-clear-anzu)
  (advice-add 'evil-previous-line :after 'my/auto-clear-anzu)
  (advice-add 'evil-forward-char :after 'my/auto-clear-anzu)
  (advice-add 'evil-backward-char :after 'my/auto-clear-anzu)
  (advice-add 'windmove-do-window-select :after 'my/auto-clear-anzu)
  (advice-add 'switch-to-buffer :after 'my/auto-clear-anzu)
  (evil-mode t)
  (defun my/set-hollow-cursor ()
    (setq-local evil-motion-state-cursor 'hollow
                evil-normal-state-cursor 'hollow
                evil-insert-state-cursor 'hollow
                evil-visual-state-cursor 'hollow
                evil-emacs-state-cursor 'hollow))
  ;; Fix mouse clicks in Customize buffers, also cursor apperance over icons
  (with-eval-after-load "custom"
    (evil-make-overriding-map custom-mode-map)
    (add-hook 'Custom-mode-hook 'my/set-hollow-cursor))
  (with-eval-after-load "yasnippet"
    (evil-make-overriding-map yas-minor-mode-map))
  (evil-define-operator my/evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  ;; Set C-S-w to evil-window-map everywhere, then swap it so that C-S-w
  ;; is mapped to the default C-w, while C-w is now evil-window-map everywhere.
  (general-def '(insert emacs) "C-S-w" 'evil-window-map)
  (general-swap-key nil '(insert emacs) "C-S-w" "C-w")
  :general-config
  ('(normal visual) 'prog-mode-map "gc" 'my/evil-comment-or-uncomment)
  ('evil-window-map "o" 'evil-window-mru)
  ('insert 'prog-mode-map "<tab>" 'indent-for-tab-command)
  ('visual "<tab>" 'evil-shift-right)
  ('visual "<backtab>" 'evil-shift-left)
  ('normal "<tab>" 'evil-shift-right-line)
  ('normal "<backtab>" 'evil-shift-left-line))

(use-package evil-collection :ensure t
  :after evil
  :config
  (evil-collection-init)
  ;; Make insert mode act like Emacs
  (setopt evil-disable-insert-state-bindings t)
  ;; Make '==' execute 'vip='. I couldn't figure this out as a keybind.
  (defun my/indent-paragraph-or-evil-indent (fn beg end)
    ;; Condition from original evil-indent
    (if (and (= beg (line-beginning-position))
             (= end (line-beginning-position 2)))
        (save-excursion (execute-kbd-macro (read-kbd-macro "vip=")))
      (funcall fn beg end)))
  (advice-add 'evil-indent :around 'my/indent-paragraph-or-evil-indent))

(use-package evil-surround :ensure t
  :config (global-evil-surround-mode))

(use-package evil-anzu :ensure t
  :custom (anzu-cons-mode-line-p nil)
  :config
  (require 'evil-anzu) ; Somehow this is necessary
  (global-anzu-mode))

(use-package undo-fu :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :custom
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000)))

(use-package undo-fu-session :ensure t
  :config (undo-fu-session-global-mode))

(use-package goto-chg :ensure t)

(use-package evil-visualstar :ensure t
  :after evil
  :commands global-evil-visualstar-mode
  :hook (after-init . global-evil-visualstar-mode))

(use-package magit :ensure t :defer t
  :commands magit)

;; NOTE: C-q will quote the next input, so you can send ESC with C-q ESC
(use-package eat :ensure t
  :commands eat
  :custom (eat-kill-buffer-on-exit t)
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode))
  :general-config
  ('(normal insert) eat-mode-map C-c P 'eat-send-password))

(use-package devdocs :ensure t
  :commands devdocs-lookup
  :bind ("C-h D" . devdocs-lookup)
  :config
  (general-add-hook 'devdocs-mode-hook (list 'my/smaller-fonts
                                             'my/word-wrap
                                             'my/no-mode-line
                                             'my/no-fringes-redisplay)))

(use-package vertico :ensure t :defer t
  :commands (vertico-mode vertico-reverse-mode vertico-mouse-mode)
  :bind (:map minibuffer-mode-map
         ("<tab>" . completion-at-point)
         ("C-S-k" . kill-line)
         :map vertico-map
         ("<next>" . vertico-scroll-up)
         ("<prior>" . vertico-scroll-down))
  :custom
  (vertico-cycle t)
  (vertico-scroll-margin 1)
  (vertico-resize t)
  :hook
  (after-init . vertico-mode)
  (after-init . vertico-mouse-mode)
  :config
  (cl-defmethod vertico--display-candidates (lines)
    "Put the vertico prompt at the bottom without reversing the entire display"
    (move-overlay vertico--candidates-ov (point-min) (point-min))
    (let ((string (apply #'concat lines)))
      (add-face-text-property 0 (length string) 'default 'append string)
      (overlay-put vertico--candidates-ov 'before-string string)
      (overlay-put vertico--candidates-ov 'after-string nil))
    (vertico--resize-window (length lines)))
  (defun my/crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add 'completing-read-multiple :filter-args 'my/crm-indicator))

(use-package orderless :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia :ensure t :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark :ensure t :defer t
  :general ("<f8>" 'embark-act
            "S-<f8>" 'embark-dwim
            "C-h B" 'embark-bindings)
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult :ensure t :demand t
  :bind (("C-c M-x" . consult-mode-command)
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
         ("M-s f" . consult-focus-lines)
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
  (add-to-list* 'consult-buffer-filter
                "\\`\\*Compile-Log\\*\\'"
                "\\`\\*Async-native-compile-log\\*\\'")
  (defvar my/consult--source-buffer-no-star
    `(:name "Buffer"
      :narrow ?b
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :default t
      :items
      ,(lambda ()
         (consult--buffer-query :sort 'visibility
                                :as #'consult--buffer-pair
                                :exclude
                                `("\\`\\*" ,@consult-buffer-filter)))))
  (defun consult-buffer-only (&optional arg)
    "`consult-buffer` that only shows buffers. With prefix, show * buffers."
    (interactive "P")
    (if arg
        (progn
          (message "prefix")
          (consult-buffer '(consult--source-buffer)))
      (message "no prefix")
      (consult-buffer '(my/consult--source-buffer-no-star))))
      
  (keymap-global-set "C-x B" 'consult-buffer-only)

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package corfu :ensure t :defer t
  :commands (corfu-mode global-corfu-mode corfu-popupinfo-mode)
  :hook ((after-init . global-corfu-mode)
         (after-init . corfu-popupinfo-mode))
  :bind (("C-SPC" . completion-at-point) ; for when tab isn't usable
         ;; :map evil-insert-state-map
         ;; ("<tab>" . indent-for-tab-command)
         :map corfu-map
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

(use-package cape :ensure t :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package form-feed-st :ensure t
  :hook (after-init . global-form-feed-st-mode))

(use-package highlight-parentheses :ensure t
  :hook (minibuffer-setup . highlight-parentheses-minibuffer-setup)
  :custom
  (highlight-parentheses-colors
   '("#005500" "#0000AA" "#550099" "#550000" "#333300"))
  (highlight-parentheses-background-colors
   '("#BBFFDD" "#BBDDFF" "#FFCCFF" "#FFDDDD" "#FFEECC"))
  :config (global-highlight-parentheses-mode))

(use-package yasnippet :ensure t
  :hook (after-init . yas-global-mode)
  :custom (yas-alias-to-yas/prefix-p nil)
  :bind (:map yas-minor-mode-map
         ("C-i" . yas-expand)
         ("C-S-i" . yas-insert-snippet)
         :map yas-keymap
         ("C-n" . yas-next-field)
         ("C-p" . yas-prev-field)
         ("S-<return>" . newline)
         ("<return>" . yas-next-field)
         ("<tab>" . yas-next-field))
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "S-TAB") nil)
  (define-key yas-keymap (kbd "TAB") nil)
  (defun my/ensure-insert ()
    (unless (eq evil-state 'insert)
      (evil-insert 1)))
  (add-hook 'yas-after-exit-snippet-hook 'evil-normal-state)
  (add-hook 'yas-before-expand-snippet-hook 'my/ensure-insert))

(use-package yasnippet-snippets :ensure t)

(use-package lua-mode :ensure t
  :mode "\\.lua\\'")

(use-package fennel-mode :ensure t
  :mode "\\.fnl\\'"
  :config
  (put 'when-let 'fennel-indent-function 1))

(use-package pdf-tools :ensure t :defer t
  :hook
  (after-init . pdf-loader-install)
  (pdf-view-mode . my/no-blink-cursor))

(use-package simple-modeline :ensure t :demand t
  :hook (after-init . simple-modeline-mode)
  :custom (simple-modeline-word-count-modes nil))

(use-package org-superstar :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom (org-superstar-special-todo-items t))

(use-package org-variable-pitch :ensure t
  :hook (after-init . org-variable-pitch-setup))

(use-package cycbuf :ensure t
  :bind (:map evil-motion-state-map
         ("C-j" . cycbuf-switch-to-next-buffer)
         ("C-k" . cycbuf-switch-to-previous-buffer)
         :map evil-normal-state-map
         ("C-j" . cycbuf-switch-to-next-buffer)
         ("C-k" . cycbuf-switch-to-previous-buffer))
  :config
  (add-to-list 'cycbuf-dont-show-regexp "\\`\\*")
  (setq cycbuf-max-window-height 5)
  (setq cycbuf-file-name-replacements '(("/home/athr[^/]*/" "~/")))
  (defconst cycbuf-header-lines-length 0)
  (advice-add 'cycbuf-show-header :override #'ignore)
  (defun my/cycbuf-set-window-height ()
    (unless (one-window-p t)
      (shrink-window (- (window-height)
                        (+ (min (length cycbuf-current-list)
                                cycbuf-max-window-height))))))
  (defun my/goto-line (line)
    (goto-char (point-min))
    (forward-line (1- line)))
  (defun my/cycbuf-layout-status-line (fn win buf)
    ;; Prevent the original recentering logic.
    ;; Also replace uses of goto-line because it sets the mark.
    (cl-letf (((symbol-function #'recenter) #'ignore)
              ((symbol-function #'goto-line) #'my/goto-line))
      (funcall fn win buf))
    (with-selected-window win
      (let* ((max-scroll (- (length cycbuf-buffer-list) (window-height)))
             (cur-line (line-number-at-pos))
             (ideal-target (- cur-line (/ (+ 1 (window-height)) 2)))
             (actual-target (max 0 (min max-scroll ideal-target))))
        (set-window-vscroll nil actual-target))))
  (advice-add 'cycbuf-set-window-height :override 'my/cycbuf-set-window-height)
  (advice-add 'cycbuf-layout-status-line :around 'my/cycbuf-layout-status-line)
  (add-hook 'cycbuf-mode-hook 'my/no-mode-line)
  (add-hook 'cycbuf-mode-hook 'my/no-fringes-redisplay)
  (setq cycbuf-attributes-list
        '(("Buffer"     cycbuf-get-name-length left  cycbuf-get-name)
          (""           2                      left  "  ")
          ("M"          1                      left  cycbuf-get-modified-string)
          ("R"          2                      left  cycbuf-get-readonly-string)
          (""           1                      left  " ")
          ("Mode"      12                      left  cycbuf-get-mode-name)
          (""           2                      left  "  ")
          ("Directory"  cycbuf-get-file-length left cycbuf-get-file-name))))

;; NOTE: Remember you can "fix" pairs with replace without toggling strict mode
(use-package smartparens :ensure t
  :hook
  (prog-mode . smartparens-mode)
  (smartparens-mode . smartparens-strict-mode)
  :custom-face
  (sp-pair-overlay-face ((t (:background "#F0F0F0" :inherit unspecified))))
  :custom (sp-delete-blank-sexps t)
  :config
  (require 'smartparens-config)
  ;;(sp-with-modes '(css-base-mode js-base-mode)
  ;;  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
  (require 'aggressive-indent)
  (defun my/wrap-quotes ()
    (interactive "*")
    (sp-wrap-with-pair "\""))
  (defun my/wrap-round-indent ()
    (interactive "*")
    (call-interactively #'sp-wrap-round)
    (aggressive-indent-indent-defun))
  (defun my/wrap-curly-indent ()
    (interactive "*")
    (call-interactively #'sp-wrap-curly)
    (aggressive-indent-indent-defun))
  (defun my/wrap-square-indent ()
    (interactive "*")
    (call-interactively #'sp-wrap-square)
    (aggressive-indent-indent-defun))
  :general-config
  ('(normal insert)
   "M-j" 'sp-join-sexp
   "C-M-j" 'sp-split-sexp
   "M-;" 'sp-comment
   "M-\"" 'my/wrap-quotes)
  ('visual
   "M-(" 'my/wrap-round-indent
   "M-{" 'my/wrap-curly-indent
   "M-[" 'my/wrap-square-indent))

;; TODO: Set up movement keybinds that don't conflict with Vim muscle memory
(use-package evil-cleverparens :ensure t
  :custom
  (evil-cleverparens-use-additional-bindings nil)
  (evil-cleverparens-use-additional-movement-keys nil)
  (evil-cleverparens-use-regular-insert t)
  :hook (smartparens-mode . evil-cleverparens-mode)
  :config
  (defun my/wrap-quotes-selected (beg end)
    (interactive "r")
    (evil-cp--wrap-region-with-pair "\"" beg end))
  :general-config
  ('(normal insert)
   ;; Mnemonic: Holding Ctrl moves left paren, holding Alt moves the
   ;; right paren (Ctrl is left of Alt when using right hand for <>).
   "C->" 'sp-backward-barf-sexp
   "C-<" 'sp-backward-slurp-sexp
   "M->" 'sp-forward-slurp-sexp
   "M-<" 'sp-forward-barf-sexp)
  ('insert
   "M-(" 'evil-cp-wrap-next-round
   "M-)" 'evil-cp-wrap-previous-round
   "M-{" 'evil-cp-wrap-next-curly
   "M-}" 'evil-cp-wrap-previous-curly
   "M-[" 'evil-cp-wrap-next-square
   "M-]" 'evil-cp-wrap-previous-square)
  ('visual
   "M-\"" 'my/wrap-quotes-selected))

  

(use-package aggressive-indent :ensure t
  :config
  ;; Aggressive indent doesn't respond well to the way elisp indents ; and ;;
  ;; comments differently. This is modified from the existing comment logic in
  ;; aggressive-indent.el.
  (add-to-list 'aggressive-indent-dont-indent-if
               '(let ((line (thing-at-point 'line)))
                  (and (stringp line)
                       (stringp comment-start)
                       (let ((c (substring comment-start 0 1)))
                         ;; Whitespace, followed by any amount of the comment
                         ;; starting character.
                         (string-match (concat "\\`[[:blank:]]*" c "*") line)))))
  (defun my/indent-defun (&rest _) (aggressive-indent-indent-defun))
  (advice-add 'insert-parentheses :after 'my/indent-defun)
  (global-aggressive-indent-mode))

(use-package flycheck :ensure t :defer t
  :config
  (defun my/flycheck-mouse-next (event)
    (interactive "e")
    (with-selected-window (posn-window (event-start event))
      (flycheck-next-error)))
  (defun my/flycheck-mouse-prev (event)
    (interactive "e")
    (with-selected-window (posn-window (event-start event))
      (flycheck-previous-error))))

(use-package minions :ensure t
  :custom (minions-mode-line-delimiters '(" " . ""))
  :hook (after-init . minions-mode))

(use-package markdown-mode :ensure t
  :custom
  (markdown-display-remote-images t)
  (markdown-enable-highlighting-syntax t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-markup t)
  (markdown-enable-wiki-links t)
  (markdown-list-item-bullets '("•" "‣" "‧" "╴"))
  :config
  (general-add-hook (list 'markdown-mode-hook 'markdown-view-mode-hook
                          'gfm-mode-hook 'gfm-view-mode-hook)
                    (list 'my/word-wrap 'my/no-fringes 'my/margins 'variable-pitch-mode)))


(use-package treesit-auto :ensure t
  :custom (treesit-auto-install 'prompt)
  :config
  ;; The package author doesn't seem to update often. Patch or remove broken recipes.
  (defconst broken-treesit-auto '(markdown latex c-sharp lua))
  (setq treesit-auto-langs
        (seq-difference (mapcar #'treesit-auto-recipe-lang treesit-auto-recipe-list)
                        broken-treesit-auto))
  (defun my/find-treesit-auto-recipe (lang)
    (car (seq-filter (lambda (x) (eq (treesit-auto-recipe-lang x) lang))
                                       treesit-auto-recipe-list)))
  (defun my/symcat (a b) (intern (concat (symbol-name a) (symbol-name b))))
  (defun my/patch-treesit-auto-recipe (lang field val)
    (when-let ((recipe (my/find-treesit-auto-recipe lang)))
      (let ((fn-sym (my/symcat 'treesit-auto-recipe- field)))
        (eval `(setf (,fn-sym ,recipe) ,val)))))
  ;; Janet has the wrong name
  (my/patch-treesit-auto-recipe 'janet 'lang (quote 'janet-simple))
  (message "janet: %s" (my/find-treesit-auto-recipe 'janet))
  (message "janet-simple: %s" (my/find-treesit-auto-recipe 'janet-simple))

  ;; (when-let ((janet (my/find-treesit-auto-recipe 'janet)))
  ;;  (setf (treesit-auto-recipe-lang janet) 'janet-simple))

  ;; C++ is broken unless we get this specific revision
  (my/patch-treesit-auto-recipe 'cpp 'revision "v0.22.0")
  (global-treesit-auto-mode))

;; This would be an external package, but the actual package is stale and
;; hasn't merged any fixes in a while.
(when (minimal-emacs-load-user-init "treesit-auto.el")
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))



;;;; Internal Packages
;;;; ======================================================================

(use-package eshell :ensure nil
  :custom (eshell-destroy-buffer-when-process-dies t)
  :config (when (require 'eat nil :noerror)
            (setq eshell-visual-commands '()))
  :general-config ('insert eshell-mode-map "<tab>" 'completion-at-point))

(use-package winner-mode :ensure nil
  :general
  (:keymaps 'evil-window-map ; C-w prefix
   "u" 'winner-undo
   "C-r" 'winner-redo)
  :custom (winner-dont-bind-my-keys t)
  :hook (after-init . winner-mode))

(use-package flymake :ensure nil
  :config
  (defun my/flymake-mouse-next (event)
    (interactive "e")
    (with-selected-window (posn-window (event-start event))
      (flymake-goto-next-error)))
  (defun my/flymake-mouse-prev (event)
    (interactive "e")
    (with-selected-window (posn-window (event-start event))
      (flymake-goto-prev-error))))

(use-package isearch :ensure nil
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "[%s/%s] "))

(use-package eldoc :ensure nil
  :init (defvar my/eldoc-help-message "")
  :config 
  (defun my/eldoc-minibuffer-message (fn fmt-str &rest args)
    (if (or (bound-and-true-p edebug-mode) (minibufferp))
        (progn
          (if (stringp fmt-str)
              (setq my/eldoc-help-message (apply #'format-message fmt-str args))
            (setq my/eldoc-help-message ""))
          (force-mode-line-update t))
      (apply fn fmt-str args)))
  (advice-add 'eldoc-minibuffer-message :around 'my/eldoc-minibuffer-message)
  (defun my/clear-eldoc-help-message ()
    (setq my/eldoc-help-message ""))
  (add-hook 'minibuffer-exit-hook 'my/clear-eldoc-help-message))

(use-package compile :ensure nil
  :bind (:map compilation-mode-map
         ("C-j" . cycbuf-switch-to-next-buffer)
         ("C-k" . cycbuf-switch-to-previous-buffer)
         ("<normal-state> C-j" . cycbuf-switch-to-next-buffer)
         ("<normal-state> C-k" . cycbuf-switch-to-previous-buffer))
  :config
  (add-hook 'compilation-mode-hook 'my/smaller-fonts))

(use-package eglot :ensure nil
  :custom (eglot-ignored-server-capabilities '(:inlayHintProvider)))

(use-package org :ensure nil
  :hook (org-mode . auto-fill-mode)
  :bind (("C-c C-o c" . org-capture)
         :map org-mode-map
         ("<backtab>" . org-shifttab)
         ("<normal-state> <backtab>" . org-shifttab)
         ("C-j" . cycbuf-switch-to-next-buffer)
         ("C-k" . cycbuf-switch-to-previous-buffer)
         ("<normal-state> C-j" . cycbuf-switch-to-next-buffer)
         ("<normal-state> C-k" . cycbuf-switch-to-previous-buffer))
  :custom
  (org-pretty-entitites t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-adapt-indentation t)
  (org-ellipsis " ⤵")
  (org-cycle-separator-lines 1)
  :config
  (unless (file-exists-p org-directory) (make-directory org-directory t))
  (setq org-default-notes-file
        (concat (file-name-as-directory org-directory) "notes")))

(use-package comint :ensure nil
  :config
  (add-hook 'comint-mode-hook 'my/smaller-fonts))

(use-package uniquify :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package dired :ensure nil
  :custom
  (dired-free-space nil)
  :config
  (setq dired-x-hands-off-my-keys t)
  (require 'dired-x)
  (setopt dired-omit-files "\\`\\.")
  ;; Always open directories in the same window, files in another window
  (defun my/dired-mouse-find-file-smart (event)
    (interactive "e" dired-mode)
    (dired-mouse-find-file event
                           'find-file-other-window
                           'find-alternate-file))
  (advice-add 'dired-mouse-find-file-other-window :override
              'my/dired-mouse-find-file-smart)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'dired-omit-mode))


;;;; Customized Mode Line
;;;; ======================================================================

(defun my/colorize (text fg bg)
  (propertize text 'face `(:foreground ,fg :background ,bg)))

(defun my/vim-color ()
  (if (mode-line-window-selected-p)
      (pcase (symbol-name evil-state)
        ("normal"   "#a4d5f9")
        ("insert"   "#8adf80")
        ("visual"   "#fff576")
        ("replace"  "#ff8f88")
        ("operator" "#d5a4f9")
        ("emacs"    "#ffddff")
        (_ 'unspecified))
    'unspecified))

(defun my/vim-state ()
  (if (mode-line-window-selected-p)
      (let ((mode-text (concat " " (upcase (symbol-name evil-state)) "▕")))
        (my/colorize mode-text "black" (my/vim-color)))
    ""))

(defun my/make-check-text (status errors warnings info map)
  (if (or (null status) (string= status ""))
      (let* ((e (format "%s " errors))
             (w (format "%s " warnings))
             (i (format "%s" info)))
        `((:propertize ,e mouse-face mode-line-highlight keymap ,map face error)
          (:propertize ,w mouse-face mode-line-highlight keymap ,map face warning)
          (:propertize ,i mouse-face mode-line-highlight keymap ,map face success)))
    `(:propertize ,status mouse-face mode-line-highlight keymap ,map face warning)))

(defun my/flycheck-status ()
  (if (bound-and-true-p flycheck-mode)
      (let ((map (make-sparse-keymap)))
        (define-key map [mode-line down-mouse-1] flycheck-mode-menu-map)
        (define-key map [mode-line wheel-down] 'my/flycheck-mouse-next)
        (define-key map [mode-line wheel-up] 'my/flycheck-mouse-prev)
        (if (eq flycheck-last-status-change 'finished)
            (let* ((all (flycheck-count-errors flycheck-current-errors))
                   (errors (cdr (or (assoc 'error all) '(nil . 0))))
                   (warnings (cdr (or (assoc 'warning all) '(nil . 0))))
                   (info (cdr (or (assoc 'info all) '(nil . 0)))))
              (my/make-check-text "" errors warnings info map))
          (my/make-check-text (symbol-name flycheck-last-status-change) 0 0 0 map)))
    ""))

(defun my/flymake-status ()
  (if (bound-and-true-p flymake-mode)
      (let ((map (make-sparse-keymap))
            (status (flymake--mode-line-exception)))
        (define-key map [mode-line down-mouse-1] 'flymake-menu)
        (define-key map [mode-line wheel-down] 'my/flymake-mouse-next)
        (define-key map [mode-line wheel-up] 'my/flymake-mouse-prev)
        (if status
            (my/make-check-text status 0 0 0 map)
          (let* ((e (cadadr (flymake--mode-line-counter :error)))
                 (w (cadadr (flymake--mode-line-counter :warning)))
                 (i (cadadr (flymake--mode-line-counter :note))))
            (my/make-check-text "" e w i map))))
    ""))

(defun my/evil-state () '(:eval (my/vim-state)))
(defun my/modeline-modes ()
  (if (mode-line-window-selected-p) 
      minions-mode-line-modes
    (format-mode-line mode-name)))
(defun my/modeline-position-default ()
  `(:propertize ("▏ %3l : %2C ") face (:background ,(my/vim-color))))
(defun my/modeline-position-pdf ()
  (require 'pdf-view)
  (require 'pdf-info)
  (let ((str (format "▏ Page %d/%d "
                     (pdf-view-current-page)
                     (pdf-info-number-of-pages))))
    `(:propertize ,str face (:background ,(my/vim-color)))))
(defun my/modeline-position ()
  (if (mode-line-window-selected-p)
      (cond ((eq major-mode 'pdf-view-mode) (my/modeline-position-pdf))
            (t (my/modeline-position-default)))
    ""))
(defun my/modeline-search ()
  (if (mode-line-window-selected-p)
      (let ((search-info (anzu--update-mode-line)))
        (if (null search-info)
            ""
          (concat "  ▏" search-info)))
    ""))
(defun my/modeline-eldoc ()
  (or 
   (unless (string= "" my/eldoc-help-message)
     (when (active-minibuffer-window)
       (let ((bot-win (or (window-in-direction 'above (minibuffer-window))
                          (minibuffer-selected-window)
                          (get-largest-window))))
         (when (eq (selected-window) bot-win)
           (concat " ▏" my/eldoc-help-message " ")))))
   ""))
(defun my/modeline-fly ()
  (if (mode-line-window-selected-p)
      (cond ((bound-and-true-p flymake-mode) '("" (:eval (my/flymake-status)) "▕ "))
            ((bound-and-true-p flycheck-mode) '("" (:eval (my/flycheck-status)) "▕ "))
            (t ""))
    ""))
(defun my/modeline-vc ()
  (if (mode-line-window-selected-p)
      '(vc-mode ("" vc-mode " ▕ "))
    ""))
(defun my/modeline-extend (result)
  (concat result (my/colorize " " "black" (my/vim-color))))
(advice-add 'simple-modeline--format :filter-return 'my/modeline-extend)
(setopt simple-modeline-segments
        '((my/evil-state
           simple-modeline-segment-modified
           simple-modeline-segment-buffer-name
           my/modeline-search
           my/modeline-eldoc)
          (simple-modeline-segment-misc-info
           my/modeline-fly
           my/modeline-vc
           my/modeline-modes
           my/modeline-position)))
(set-face-attribute 'fringe nil :background "white")

;;;; Other keybinds
;;;; ======================================================================

;; TODO: Use general.el

(keymap-global-set "C-x k" 'kill-current-buffer)
(keymap-global-set "<mode-line> <mouse-2>" 'mouse-delete-window)
(keymap-global-set "<mode-line> <mouse-3>" 'mouse-buffer-menu)

(defun recompile-or-prompt ()
  (interactive)
  (if (string= compile-command "make -k ")
      ;; Subtle change from the default to make this not trigger a second time
      (progn (setq compile-command "make -k")
             (call-interactively 'compile))
    (call-interactively 'recompile)))

(general-def
  "<f5>" 'recompile-or-prompt
  "<S-f5>" 'compile)

(keymap-global-set "C-x C-m" 'pp-macroexpand-last-sexp)

(keymap-global-unset "C-h t")
(keymap-global-unset "<f1> t")

;; I really want escape to do what it says
(keymap-global-set "C-h ESC" 'keyboard-escape-quit)
(keymap-global-set "C-M-g" 'keyboard-escape-quit)
(keymap-global-unset "M-ESC :") ; the only default keybind prefixed by M-ESC
(keymap-set minibuffer-mode-map "<escape>" 'abort-minibuffers)

;; Prevents ESC from messing with splits
(defun my/keyboard-escape-quit-advice (fn)
  (let ((buffer-quit-function (or buffer-quit-function #'keyboard-quit)))
    (funcall fn)))
(advice-add 'keyboard-escape-quit :around 'my/keyboard-escape-quit-advice)

;; Having the box cursor for the minibuffer is weird when it indicates
;; normal mode everywhere else
(defun my/local-bar-cursor () (setq-local cursor-type 'bar))
(add-hook 'minibuffer-mode-hook 'my/local-bar-cursor)

(with-eval-after-load "rect"
  (keymap-set rectangle-mark-mode-map "C-i" 'string-insert-rectangle)
  (keymap-set rectangle-mark-mode-map "C-r" 'replace-rectangle))


;;;; Window layout and display-buffer-alist
;;;; ======================================================================

(setq switch-to-buffer-obey-display-actions nil)
(setq switch-to-prev-buffer-skip-regexp (rx (seq bos (or "*" " "))))
(setq window-sides-slots '(1 0 0 2))
(setq fit-window-to-buffer-horizontally t)
(setq
 display-buffer-alist
 `(
   ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
    nil
    (window-parameters (mode-line-format . none)))
   ("\\`[ ]?\\*\\(Help\\|Customize\\|info\\|eldoc\\|Occur\\|grep\\|devdocs\\|Pp\\)"
    display-buffer-in-side-window
    (side . bottom) (slot . -1) (preserve-size . (nil . t)))
   ((major-mode . dired-mode)
    display-buffer-in-side-window
    (side . bottom) (slot . -1) (preserve-size . (nil . t)))
   ("\\`[ ]?\\*\\(compilation\\|[e]?shell\\|[v]?term\\|.*REPL\\)"
    display-buffer-in-side-window
    (side . bottom) (slot . 1) (preserve-size . (nil . t)))
   ((mode comint-mode)
    display-buffer-in-side-window
    (side . bottom) (slot . 1) (preserve-size . (nil . t)))
   (,(rx (seq bos (or "*" " *")))
    (display-buffer-in-side-window display-buffer-no-window)
    (side . bottom))))

   
