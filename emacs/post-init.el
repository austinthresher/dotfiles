;;; -*- no-byte-compile: t; lexical-binding: t; -*-

;;;; Packages to look into:
;;;; auto-yasnippet

;;;; Cool packages that don't fit my current workflow:
;;;; popup-switcher

;;;; TODOs that probably require writing elisp:
;;;; - Make keybinds for the mouse back/forward buttons that call appropriate
;;;;   functions in the window under the mouse, based on the major mode of that
;;;;   window's buffer. pdf-history-backward, help-go-back, etc.

;;;; TODO: Fix mashing escape in Emacs State closing windows

(setq modus-themes-headings
      (quote ((t . (variable-pitch medium 1.1)))))
(load-theme 'modus-operandi t)

(set-face-attribute 'default nil :family "Iosevka" :height 130)
(set-face-attribute 'variable-pitch nil :family "Roboto Condensed")

(setq truncate-lines nil)
(setq fast-but-imprecise-scrolling nil)
(setq idle-update-delay 0.1)
(setq show-paren-delay 0)
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
(setq blink-cursor-blinks 0)
(setq blink-cursor-delay 0.2)
(setq blink-cursor-interval 0.1)
(setq blink-cursor-alist '((box . box) (hollow . hollow) (bar . bar)
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

(global-prettify-symbols-mode t)

(setq set-message-functions '(inhibit-message set-minibuffer-message))
(add-to-list 'inhibit-message-regexps "Cleaning up the recentf")
(add-to-list 'inhibit-message-regexps "Mark saved")

(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)
(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

(add-to-list 'auto-mode-alist '("bashrc" . sh-mode))

(defun my/no-mode-line (&rest _)
  "Add this as a hook to modes that should not have a modeline"
  (setq-local mode-line-format nil))

(defun my/no-fringes (&rest _)
  "Add this as a hook to buffers that should not show fringes"
  (setq-local left-fringe-width 1
              right-fringe-width 1)
  (set-window-buffer (selected-window) (current-buffer)))

(defun my/no-blink-cursor (&rest _)
  "Add this as a hook to buffers that should not blink the cursor"
  (setq-local blink-cursor-mode nil))

(setq evil-want-keybinding nil)
(use-package evil
  :ensure t
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
  (define-key evil-window-map (kbd "o") 'evil-window-mru)
  ;; These don't work with evil-cleverparens.
  ;; TODO: Add a hook to include these in non-lisp modes
  ;; (define-key evil-normal-state-map (kbd "<tab>") ">>")
  ;; (define-key evil-normal-state-map (kbd "<backtab>") "<<")
  ;; (define-key evil-visual-state-map (kbd "<tab>") ">")
  ;; (define-key evil-visual-state-map (kbd "<backtab>") "<")
  ;; Evil doesn't give an option to hide the previous search term.
  ;; Always show a blank prompt when performing a search.
  (defun my/hide-prev-search (args)
    (if (string-prefix-p "evil-ex-search" (symbol-name this-command))
        `(,(car args) "" ,@(cddr args))
      args))
  (advice-add 'read-string :filter-args 'my/hide-prev-search)
  (defun my/auto-clear-anzu (&rest _) (anzu--reset-status))
  (advice-add 'evil-force-normal-state :after 'evil-ex-nohighlight)
  (advice-add 'evil-next-line :after 'my/auto-clear-anzu)
  (advice-add 'evil-previous-line :after 'my/auto-clear-anzu)
  (advice-add 'evil-forward-char :after 'my/auto-clear-anzu)
  (advice-add 'evil-backward-char :after 'my/auto-clear-anzu)
  (advice-add 'windmove-do-window-select :after 'my/auto-clear-anzu)
  (advice-add 'switch-to-buffer :after 'my/auto-clear-anzu)

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

(with-eval-after-load "evil-collection"
  (setopt evil-disable-insert-state-bindings t)
  ;; Make == execute vip=
  (defun indent-paragraph-or-evil-indent (fn beg end)
    ;; Condition from original evil-indent
    (if (and (= beg (line-beginning-position))
             (= end (line-beginning-position 2)))
        (save-excursion
          (execute-kbd-macro (read-kbd-macro "vip=")))
      (funcall fn beg end)))
  (advice-add 'evil-indent :around 'indent-paragraph-or-evil-indent))
  

(use-package evil-anzu
  :ensure t
  :custom (anzu-cons-mode-line-p nil)
  :config
  (require 'evil-anzu) ; Somehow this is necessary
  (global-anzu-mode))


;; (with-eval-after-load "evil"
;;   ;; Fix mouse clicks in Customize buffers
;;   (with-eval-after-load "custom"
;;     (evil-make-overriding-map custom-mode-map))
;;   (with-eval-after-load "yasnippet"
;;     (evil-make-overriding-map yas-minor-mode-map)))


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

(use-package magit
  :ensure t
  :defer t
  :commands magit)

(use-package vterm
  :ensure t
  :defer t
  :commands vterm
  :config (setq vterm-timer-delay 0.01))

(use-package devdocs
  :ensure t
  :commands devdocs-lookup
  :bind ("C-h D" . devdocs-lookup))

(use-package vertico
  :ensure t
  :defer t
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
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :demand t
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
  (add-to-list 'consult-buffer-filter "\\`\\*Compile-Log\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Async-native-compile-log\\*\\'")
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


(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode corfu-popupinfo-mode)
  :hook ((after-init . global-corfu-mode)
         (after-init . corfu-popupinfo-mode))
  :bind (;;("<tab>" . indent-for-tab-command)
         ("C-SPC" . completion-at-point) ; for when tab isn't usable
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

(show-paren-mode -1)
(use-package highlight-parentheses
  :ensure t
  :hook (minibuffer-setup . highlight-parentheses-minibuffer-setup)
  :custom
  (highlight-parentheses-colors
   '("#005500" "#0000AA" "#550099" "#550000" "#333300"))
  (highlight-parentheses-background-colors
   '("#BBFFDD" "#BBDDFF" "#FFCCFF" "#FFDDDD" "#FFEECC"))
  :config (global-highlight-parentheses-mode))

(use-package yasnippet
  :ensure t
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

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package fennel-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))
  (put 'when-let 'fennel-indent-function 1))

(use-package pdf-tools
  :ensure t
  :defer t
  :hook
  (after-init . pdf-loader-install)
  (pdf-view-mode . my/no-blink-cursor))

(use-package simple-modeline
  :ensure t
  :hook (after-init . simple-modeline-mode)
  :custom (simple-modeline-word-count-modes nil))

(use-package org
  :ensure nil
  :hook
  (org-mode . auto-fill-mode)
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

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom (org-superstar-special-todo-items t))

(use-package org-variable-pitch
  :ensure t
  :hook (after-init . org-variable-pitch-setup))

(use-package compile
  :ensure nil
  :bind (:map compilation-mode-map
              ("C-j" . cycbuf-switch-to-next-buffer)
              ("C-k" . cycbuf-switch-to-previous-buffer)
              ("<normal-state> C-j" . cycbuf-switch-to-next-buffer)
              ("<normal-state> C-k" . cycbuf-switch-to-previous-buffer)))

(use-package eglot
  :ensure nil
  :custom (eglot-ignored-server-capabilities '(:inlayHintProvider)))

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "[%s/%s] "))


(defvar my/eldoc-help-message "")
(use-package eldoc
  :ensure nil
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

(use-package cycbuf
  :ensure t
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
  (add-hook 'cycbuf-mode-hook 'my/no-fringes)
  (setq cycbuf-attributes-list
        '(("Buffer"     cycbuf-get-name-length left  cycbuf-get-name)
          (""           2                      left  "  ")
          ("M"          1                      left  cycbuf-get-modified-string)
          ("R"          2                      left  cycbuf-get-readonly-string)
          (""           1                      left  " ")
          ("Mode"      12                      left  cycbuf-get-mode-name)
          (""           2                      left  "  ")
          ("Directory"  cycbuf-get-file-length left cycbuf-get-file-name))))

(use-package smartparens
  :ensure t
  :bind ("C-c C-s" . smartparens-strict-mode)
  :hook
  (prog-mode . smartparens-mode)
  (smartpares-mode . smartparens-strict-mode)
  :config (require 'smartparens-config))

;; TODO: Set up movement keybinds that don't conflict with Vim muscle memory
(use-package evil-cleverparens
  :ensure t
  :custom
  (evil-cleverparens-use-additional-bindings nil)
  (evil-cleverparens-use-additional-movement-keys nil)
  :hook (smartparens-mode . evil-cleverparens-mode))

(use-package aggressive-indent
  :ensure t
  :config (global-aggressive-indent-mode))

;; (use-package parinfer-rust-mode
;;   :ensure t
;;   :hook (emacs-lisp-mode . parinfer-rust-mode))

;; The actual package is stale and hasn't merged any fixes in a while
(when (minimal-emacs-load-user-init "treesit-auto.el")
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package minions
  :ensure t
  :custom (minions-mode-line-delimiters '(" " . ""))
  :hook (after-init . minions-mode))

(use-package winner-mode
  :ensure nil
  :bind (:map evil-window-map
              ("u" . winner-undo) ; "C-w u" to undo a window change
              ("C-r" . winner-redo))
  :custom (winner-dont-bind-my-keys t)
  :hook (after-init . winner-mode))

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

(with-eval-after-load "minions"
  (with-eval-after-load "simple-modeline"
    (with-eval-after-load "evil"
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
            (concat " " (anzu--update-mode-line))
          ""))
      (defun my/modeline-eldoc ()
        (or 
         (unless (string= "" my/eldoc-help-message)
           (when (active-minibuffer-window)
             (let ((bot-win (or (window-in-direction 'above (minibuffer-window))
                                (minibuffer-selected-window)
                                (get-largest-window))))
               (when (eq (selected-window) bot-win)
                 (concat " " my/eldoc-help-message " ")))))
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
                 simple-modeline-segment-vc
                 my/modeline-modes
                 my/modeline-position)))
      (set-face-attribute 'fringe nil :background "white"))))



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

(keymap-global-set "C-x C-m" 'pp-macroexpand-last-sexp)

(keymap-set minibuffer-mode-map "<escape>" 'abort-minibuffers)

;; Having the box cursor for the minibuffer is weird when it indicates
;; normal mode everywhere else
(defun my/local-bar-cursor () (setq-local cursor-type 'bar))
(add-hook 'minibuffer-mode-hook 'my/local-bar-cursor)

(setq switch-to-buffer-obey-display-actions nil)
;;(display-buffer-base-action '(display-buffer-same-window))
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
   ("\\`[ ]?\\*\\(compilation\\|[e]?shell\\|[v]?term\\|.*REPL\\)"
    display-buffer-in-side-window
    (side . bottom) (slot . 1) (preserve-size . (nil . t)))
   ((mode comint-mode)
    display-buffer-in-side-window
    (side . bottom) (slot . 1) (preserve-size . (nil . t)))
   (,(rx (seq bos (or "*" " *")))
    (display-buffer-in-side-window display-buffer-no-window)
    (side . bottom))))
   
