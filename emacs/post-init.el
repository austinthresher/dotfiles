;;; -*- no-byte-compile: t; lexical-binding: t; -*-

;; TODOs that probably require writing elisp:
;; - Make keybinds for the mouse back/forward buttons that call appropriate
;;   functions in the window under the mouse, based on the major mode of that
;;   window's buffer. pdf-history-backward, help-go-back, etc.
;; - Try to figure out triggering sp-comment when the comment string is typed
;;   for the current language's mode.


;;;; Utility macros and functions
;;;; ======================================================================

(defmacro add-to-list* (ls &rest vals)
  "Add multiple items to the same list. Expands to multiple add-to-list calls."
  (let ((exps))
    (dolist (v vals)
      (push `(add-to-list ,ls ,v) exps))
    (cons 'progn (nreverse exps))))

(defmacro remove-from-list (ls &rest vals)
  "Remove multiple items from a list. Comparisons are made with eq. ls must be
an unquoted variable name containing the list, as it will be evaluated
multiple times."
  (let ((val-sym (gensym)))
    `(let ((,val-sym (list ,@vals)))
       (setq ,ls (seq-remove (lambda (x) (memq x ,val-sym)) ,ls)))))

(defun my/close-other-tabs ()
  (remove-hook 'server-after-make-frame-hook 'my/close-other-tabs)
  (tab-bar-close-other-tabs))

(defun find-file-new-tab (filename &optional wildcards)
  "Like find-file-other-tab, but behaves when no frame yet exists."
  (let* ((buffer-names (mapcar 'buffer-name (buffer-list)))
         (is-initial-frame? (member " *server-dummy*" buffer-names)))
    (if is-initial-frame?
        (find-file filename wildcards)
      (find-file-other-tab filename wildcards)))
  (raise-frame))



;;;; Theme and font
;;;; ======================================================================

;;(setq modus-themes-mixed-fonts t)
;;(load-theme 'modus-operandi t)

(use-package doom-themes :ensure t :demand t
  :config (load-theme 'doom-tomorrow-day t))

(setq-default line-spacing nil)

;; Note- using color names like "white" will not give correct results for
;; terminal sessions
(set-face-attribute 'default nil :family "Iosevka" :height 140 :weight 'light
                    :background "#FFFFFF")
(set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 140 :weight 'normal)
(set-face-attribute 'variable-pitch-text nil :height 'unspecified)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Slab" :height 140 :weight 'light)
(set-face-attribute 'fixed-pitch-serif nil :family "Iosevka Slab" :height 140 :weight 'light)
(set-face-attribute 'fringe nil :background "#FFFFFF")
(set-face-attribute 'mode-line nil :family "Roboto" :height 140 :weight 'light)
(set-face-attribute 'mode-line-inactive nil :family "Roboto" :height 140 :weight 'light)
(set-face-attribute 'header-line nil :weight 'normal)


;;;; General settings
;;;; ======================================================================

(setq-default truncate-lines nil)
(setq large-file-warning-threshold nil)
(setq fast-but-imprecise-scrolling nil)
(setq disabled-command-function nil)
;; (setq scroll-conservatively 101)
(setq idle-update-delay 0.1)
(setq mouse-1-click-follows-link t)
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-progressive-speed nil)
(setq pixel-scroll-precision-interpolation-factor 1.0)
(setq mouse-wheel-scroll-amount '(0.1
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

(window-divider-mode -1)
;; (set-face-attribute 'window-divider-first-pixel nil :foreground "#FFFFFF")
;; (set-face-attribute 'window-divider-last-pixel nil :foreground "#FFFFFF")
;; (setopt window-divider-default-right-width 3)


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
                           ((bar . 4) . (bar . 4))))
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

(defun my/cursor-over-image? ()
  (let ((prop (get-text-property (point) 'display)))
    (eq 'image (car-safe prop))))

(defun my/cursor-should-underline? ()
  (and (not (my/cursor-over-image?))
       blink-cursor-mode))

(defun my/update-cursor-overlay (&rest _)
  ;; Diminished cursor, usually reading a document or something
  (unless blink-cursor-mode
    (set-face-attribute 'cursor nil :background
                        (if (and (boundp 'evil-state) (eq evil-state 'emacs))
                            "#FFBBFF" "#BBBBBB")))
  (or (ignore-errors
        (if (or (not (eq evil-state 'normal))
                (not (my/cursor-should-underline?)))
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
  (setq blink-cursor-blinks 0)
  (set-face-attribute 'cursor nil :background (my/get-cursor-color))
  (my/update-underline-color))

(defun my/cursor-start-blink (&rest _)
  ;; The animation looks really weird when the cursor is on an image
  (if (my/cursor-over-image?)
       (progn (when (overlayp my/underline-ov)
                (delete-overlay my/underline-ov))
              (setq blink-cursor-blinks 1)
              (when blink-cursor-timer (cancel-timer blink-cursor-timer))
              (setq blink-cursor-timer nil)
              (blink-cursor-end))
     (my/cursor-color-reset)))

(defun my/cursor-color-advance (&rest _)
   (setq my/cursor-color-idx (% (+ 1 my/cursor-color-idx) 8))
   (set-face-attribute 'cursor nil :background (my/get-cursor-color))
   (my/update-underline-color))

(advice-add 'blink-cursor-start :after 'my/cursor-start-blink)
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

(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'recentf-mode)
(add-hook 'after-init-hook 'save-place-mode)
(add-hook 'after-init-hook 'minibuffer-depth-indicate-mode)
;; This is causing more problems than it's worth
;; (add-hook 'after-init-hook 'pixel-scroll-precision-mode)
(add-hook 'after-init-hook 'delete-selection-mode)
(add-hook 'after-init-hook 'tab-bar-mode)
(add-hook 'after-init-hook 'undelete-frame-mode)
;; Adding this even though minimal-emacs.d adds it because (display-graphic-p)
;; will return false for the daemon, preventing it from being loaded.
(add-hook 'after-init-hook 'context-menu-mode)
;; TODO: remove some of these, like "or"
;; (add-hook 'prog-mode-hook 'global-prettify-symbols-mode)
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
  (setq-local line-spacing nil)
  (face-remap-add-relative 'default '(:height 120))
  (face-remap-add-relative 'variable-pitch '(:height 110))
  (face-remap-add-relative 'fixed-pitch '(:height 120))
  (face-remap-add-relative 'fixed-pitch-serif '(:height 120))
  (face-remap-add-relative 'header-line '(:height 120)))

(defun my/font-weight (&rest _)
  "Add this as a hook to buffers that should have slightly heavier default
font weight"
  (face-remap-add-relative 'default '(:weight normal)))

(defun my/line-spacing (&rest _)
  "Add this as a hook to buffers that should have extra line spacing"
  (setq-local line-spacing 0.2))

(defun my/prog-word-syntax (&rest _)
  "Make _ behave as part of a word, not punctuation."
  (modify-syntax-entry ?_ "w"))

(defun my/lisp-word-syntax (&rest _)
  "Make - behave as part of a word, not punctuation."
  (modify-syntax-entry ?- "w"))


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
  ;; Make insert mode act like Emacs
  (evil-disable-insert-state-bindings t)
  (evil-shift-round nil)
  (evil-want-empty-ex-last-command t)
  (evil-echo-state nil)
  (evil-ex-search-persistent-highlight t)
  (evil-move-beyond-eol t)
  (evil-move-cursor-back nil)
  (evil-split-window-below t)
  (evil-want-C-w-delete nil)
  (evil-cross-lines t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  :custom-face
  (evil-ex-substitute-matches ((t (:foreground "#888888"
                                   :strike-through "#FF0000"
                                   :background "#EE2222"
                                   :box (:line-width (1 . -13) :color "#FFFFFF")
                                   :inherit unspecified))))
  (evil-ex-substitute-replacement ((t (:weight normal
                                       :box (:line-width (1 . -1)
                                             :color "#90EE90")
                                       :inherit unspecified))))
  :config
  (setq evil-lookup-func 'help-follow-symbol)
  (setq evil-default-cursor '((bar . 2)))
  (setq evil-emacs-state-cursor '((bar . 2)))
  (setq evil-replace-state-cursor '((bar . 4)))
  (setq evil-insert-state-cursor '((bar . 1)))
  (setq evil-normal-state-cursor evil-default-cursor)
  (setq evil-motion-state-cursor '((hbar . 2)))
  (setq evil-visual-state-cursor 'box)
  (setq evil-operator-state-cursor 'hollow)
  (add-to-list* 'evil-emacs-state-modes
                'comint-mode 'eat-mode 'eshell-mode 'shell-mode 'term-mode
                'inferior-python-mode)
  (setq evil-insert-state-modes
        (seq-remove (lambda (x) (memq x evil-emacs-state-modes))
                    evil-insert-state-modes))
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
  ;; Fix mouse clicks in Customize buffers
  (with-eval-after-load "custom"
    (evil-make-overriding-map custom-mode-map))

  ;; I don't remember what this was fixing but it's breaking TAB,
  ;; commenting out for now. Using general.el would probably solve the
  ;; original issue, whatever it was.
  ;; (with-eval-after-load "yasnippet"
  ;;   (evil-make-overriding-map yas-minor-mode-map))

  (evil-define-operator my/evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  ;; Set C-S-w to evil-window-map everywhere, then swap it so that C-S-w
  ;; is mapped to the default C-w, while C-w is now evil-window-map everywhere.
  (general-def '(insert emacs) "C-w" 'evil-window-map)
  (general-def '(insert emacs) "C-S-w" 'evil-delete-backward-word)
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
  :custom (evil-collection-key-blacklist '("C-j" "C-k"))
  :config
  (remove-from-list evil-collection-mode-list
                    'eat)
  (evil-collection-init)
  ;; Make '==' execute 'vip='. I couldn't figure this out as a keybind.
  (defun my/indent-paragraph-or-evil-indent (fn beg end)
    ;; Condition from original evil-indent
    (if (and (= beg (line-beginning-position))
             (= end (line-beginning-position 2)))
        (save-excursion (execute-kbd-macro (read-kbd-macro "vip=")))
      (funcall fn beg end)))
  (advice-add 'evil-indent :around 'my/indent-paragraph-or-evil-indent))

(use-package evil-matchit :ensure t
  :hook (after-init . global-evil-matchit-mode))

(use-package evil-surround :ensure t
  :hook (evil-mode . global-evil-surround-mode))

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
  :commands global-evil-visualstar-mode
  :hook (after-init . global-evil-visualstar-mode))

;; Normal/visual gl and gL alignment operator. Pressing glip= would align all =
;; in the paragraph. Pressing 1glip" would align the first quote on each line.
;; L instead of l uses right alignment instead of left alignment.
(use-package evil-lion :ensure t
  :hook (after-init . evil-lion-mode))

  ;; NOTE: Keybinds are in evil-cleverparens because, yet again, it overwrites
  ;; a bunch of default evil keybinds without checking.
(use-package evil-surround :ensure t
  :hook (after-init . global-evil-surround-mode)
  :config
  (add-to-list* 'evil-surround-operator-alist
                '(evil-cp-change . change)
                '(evil-cp-delete . delete)))

(use-package magit :ensure t :defer t
  :commands magit)

;; NOTE: C-q will quote the next input, so you can send ESC with C-q ESC
(use-package eat :ensure t
  :commands eat
  :custom (eat-kill-buffer-on-exit t)
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode))
  :general-config
  ('(normal insert emacs) eat-mode-map "C-c P" 'eat-send-password)
  ('(normal insert emacs) eshell-mode-map "C-c P" 'eat-send-password)
  )

(use-package devdocs :ensure t
  :commands devdocs-lookup
  :bind ("C-h D" . devdocs-lookup)
  :config
  (general-add-hook 'devdocs-mode-hook (list 'my/smaller-fonts
                                             'my/word-wrap
                                             'my/no-mode-line
                                             'my/no-fringes)))

(use-package vertico :ensure t :defer t
  :commands (vertico-mode vertico-reverse-mode vertico-mouse-mode)
  :hook
  (after-init . vertico-mode)
  (after-init . vertico-mouse-mode)
  (server-after-make-frame . vertico-mode)
  (server-after-make-frame . vertico-mouse-mode)
  :custom
  (vertico-cycle t)
  (vertico-scroll-margin 1)
  (vertico-resize t)
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
  (advice-add 'completing-read-multiple :filter-args 'my/crm-indicator)
  :general-config
  ('minibuffer-mode-map
   "<tab>" 'completion-at-point
   "TAB" 'completion-at-point)
  ('vertico-map
   "<next>" 'vertico-scroll-up
   "<prior>" 'vertico-scroll-down
   ;; Complete up to prefix
   "<tab>" 'minibuffer-complete
   "TAB" 'minibuffer-complete
   "C-SPC" 'vertico-insert
   ;; Send typed input even if it doesn't match any candidates
   "C-<return>" 'vertico-exit-input
   "C-RET" 'vertico-exit-input
   ;; This one works in the terminal
   "C-c RET" 'vertico-exit-input)
  :custom-face
  (vertico-current ((t (:background "#D0F0FF"
                      :box (:line-width (1 . -1) :color "#DDD"))))))

(use-package orderless :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  :custom-face
  (orderless-match-face-0 ((t (:background unspecified :underline t))))
  (orderless-match-face-1 ((t (:background unspecified :underline t))))
  (orderless-match-face-2 ((t (:background unspecified :underline t))))
  (orderless-match-face-3 ((t (:background unspecified :underline t)))))

(use-package marginalia :ensure t :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (vertico-mode . marginalia-mode))

(use-package embark :ensure t :defer t
  :general ("<f7>" 'embark-select
            "<f8>" 'embark-act
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
  :hook
  (after-init . global-corfu-mode)
  (after-init . corfu-popupinfo-mode)
  (server-after-make-frame . global-corfu-mode)
  (server-after-make-frame . corfu-popupinfo-mode)
  :general
  ('insert
   "C-SPC" 'completion-at-point)        ; for when tab isn't usable
  ('corfu-map
   "<prior>" 'corfu-scroll-down
   "<next>" 'corfu-scroll-up
   "<tab>" 'corfu-expand
   "TAB" 'corfu-expand
   "<return>" 'corfu-send)
  :custom
  (corfu-cycle t)
  (corfu-preselect 'valid)
  (corfu-preview-current nil)
  (corfu-on-exact-match 'show)
  (corfu-popupinfo-delay 0.1)
  (corfu-popupinfo-hide nil)
  (corfu-quit-no-match 'separator)
  (corfu-left-margin-width 0.0)
  (corfu-right-margin-width 0.2)
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  :custom-face
  (corfu-default ((t (:background "#FFF"))))
  (corfu-current ((t (:background "#D0F0FF"
                      :box (:line-width (1 . -1) :color "#DDD")))))
  (corfu-popupinfo ((t (:background "#F8F8FD")))))

;; Note: this _might_ be conflicting with popupinfo in the GUI, needs testing
(use-package corfu-terminal :ensure t
  :config (corfu-terminal-mode))

(use-package cape :ensure t :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (general-add-hook 'completion-at-point-functions
                    (list 'cape-dabbrev 'cape-file))
  :config
  ;; TODO: Look at corfu wiki for example merging elisp cap with dabbrev

  ;; Fix the issue where completion doesn't show all of the candidates
  (advice-add 'eglot-completion-at-point :around 'cape-wrap-buster)
  )

(use-package form-feed-st :ensure t
  :hook
  (after-init . global-form-feed-st-mode)
  (server-after-make-frame-hook . global-form-feed-st-mode))

(use-package highlight-parentheses :ensure t
  :hook
  (minibuffer-setup . highlight-parentheses-minibuffer-setup)
  (prog-mode . highlight-parentheses-mode)
  :custom
  (highlight-parentheses-colors
   '("#005500" "#0000AA" "#550099" "#550000" "#333300"))
  (highlight-parentheses-background-colors
   '("#BBFFDD" "#BBDDFF" "#FFCCFF" "#FFDDDD" "#FFEECC")))

(use-package yasnippet :ensure t
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
  (yas-global-mode)
  (general-unbind 'insert '(yas-minor-mode-map yas-keymap)
    "<tab>" "<backtab>" "TAB" "S-TAB")
  (define-key yas-keymap (kbd "TAB") nil)
  (defun my/ensure-insert ()
    (unless (eq evil-state 'insert)
      (evil-insert 1)))
  (add-hook 'yas-after-exit-snippet-hook 'evil-normal-state)
  (add-hook 'yas-before-expand-snippet-hook 'my/ensure-insert))

(use-package yasnippet-snippets :ensure t)

(use-package expand-region :ensure t
  :custom (expand-region-contract-fast-key "V")
  :general-config
  ('visual "v" 'er/expand-region))

(use-package lua-mode :ensure t :mode "\\.lua\\'")
(use-package vimrc-mode :ensure t :mode "[._]?g?vim\\(rc\\)?")

(use-package fennel-mode :ensure t
  :mode "\\.fnl\\'"
  :config
  (add-hook 'fennel-mode-hook 'my/lisp-word-syntax)
  (put 'when-let 'fennel-indent-function 1))

(use-package pdf-tools :ensure t :defer t
  :commands pdf-view-mode
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :config
  (pdf-loader-install)
  (general-add-hook 'pdf-view-mode-hook (list 'my/no-blink-cursor
                                              'my/no-fringes)))

(use-package org-superstar :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom (org-superstar-special-todo-items t))

(use-package org-variable-pitch :ensure t
  :hook (org-mode . org-variable-pitch-setup))

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
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-delete-blank-sexps t)
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
    "Adds one to the end to match Vim-style visual selection, except newlines"
    (interactive "r")
    (if (eq (char-after (- end 1)) ?\n)
        (evil-cp--wrap-region-with-pair "\"" beg end)
      (evil-cp--wrap-region-with-pair "\"" beg (min (+ 1 end) (point-max)))))
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
  ('(insert emacs)
   "C-S-w" 'evil-cp-delete-backward-word)
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
  :hook
  (after-init . minions-mode)
  (server-after-make-frame . minions-mode)
  :custom
  (minions-mode-line-face 'fixed-pitch)
  (minions-mode-line-lighter " ≡")
  (minions-mode-line-delimiters '(" " . "")))

(use-package list-unicode-display :ensure t)

(use-package nov :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (general-add-hook 'nov-mode-hook (list 'my/no-blink-cursor)))

(use-package markdown-mode :ensure t
  :custom
  (markdown-display-remote-images t)
  (markdown-enable-highlighting-syntax t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-markup t)
  (markdown-enable-wiki-links t)
  (markdown-list-item-bullets '("•" "‣" "‧" "╴"))
  :custom-face
  (markdown-code-face ((t (:background unspecified
                           :weight normal
                           :inherit fixed-pitch))))
  :config
  (general-add-hook (list 'markdown-mode-hook 'markdown-view-mode-hook
                          'gfm-mode-hook 'gfm-view-mode-hook)
                    (list 'my/word-wrap 'my/no-fringes 'my/margins 'variable-pitch-mode)))


(use-package treesit-auto :ensure t
  :hook
  (after-init . global-treesit-auto-mode)
  (server-after-make-frame . global-treesit-auto-mode)
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
  ;; C++ is broken unless we get this specific revision
  (my/patch-treesit-auto-recipe 'cpp 'revision "v0.22.0")
  ;; Update list of languages with patched changes
  (setq treesit-auto-langs (seq-map #'treesit-auto-recipe-lang
                                    treesit-auto-recipe-list)))


;;;; Internal Packages
;;;; ======================================================================

(use-package eshell :ensure nil
  :custom (eshell-destroy-buffer-when-process-dies t)
  :config (when (require 'eat nil :noerror)
            (setq eshell-visual-commands '()))
  :general-config ('(insert emacs) eshell-mode-map "<tab>" 'completion-at-point))

(use-package winner-mode :ensure nil
  :hook
  (after-init . winner-mode)
  (server-after-make-frame-hook . winner-mode)
  :general
  (:keymaps 'evil-window-map            ; C-w prefix
   "u" 'winner-undo
   "C-r" 'winner-redo)
  :custom (winner-dont-bind-my-keys t))

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

(use-package doc-view :ensure nil :defer t
  :custom
  (doc-view-continuous t)
  :config
  (general-add-hook 'doc-view-mode-hook (list 'my/no-blink-cursor
                                              'my/no-fringes))
  :general
  ('normal 'doc-view-mode-map
           "C-j" 'cycbuf-switch-to-next-buffer
           "C-k" 'cycbuf-switch-to-previous-buffer))

(use-package isearch :ensure nil
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "[%s/%s] "))

(defvar read-the-docs-history nil)
(defvar wikipedia-history nil)

(use-package savehist :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (add-to-list* 'savehist-additional-variables
                'read-the-docs-history
                'wikipedia-history))

(use-package eww :ensure nil
  :commands eww
  :bind ("C-h W" . eww)
  :custom
  (eww-header-line-format "%u")
  (eww-readable-urls '((".*github\\.com\\/" . nil) ".*"))
  (shr-max-inline-image-size '(0.75 . 50.0))
  :init
  (defun my/prompt-for-url (prompt url-fmt history &optional replace-spaces)
    (when-let ((str (read-string prompt nil history))
               (str-replaced (if replace-spaces
                                 (string-replace " " replace-spaces str)
                               str))
               (url (format url-fmt str-replaced)))
      (eww url :newbuffer)))
  (defun read-the-docs ()
    (interactive)
    (my/prompt-for-url "readthedocs.io subdomain: "
                       "https://%s.readthedocs.io/en/latest/"
                       'read-the-docs-history))
  (defun wikipedia ()
    (interactive)
    (my/prompt-for-url "Wikipedia article: "
                       "https://en.wikipedia.org/wiki/%s"
                       'wikipedia-history
                       "_"))
  :config
  (setq browse-url-browser-function 'eww-browse-url)

  ;; TODO: Make this more generic with a url regex / function alist later
  (defun my/clean-github (document &optional point buffer)
    "Make Github repos slightly more readable in eww."
    (let ((url (alist-get 'href (cadr document))))
      (when (string-match "github" url)
        (let ((nodes (dom-by-class document
                                   (rx (and (= 0 "user-content-")
                                            (or "pagehead-actions"
                                                "HeaderMktg"
                                                "UnderlineNav-"
                                                "flash-warn"
                                                "TextInput-"
                                                "types__StyledButton"
                                                "form-control"
                                                "flash-full"
                                                "search"
                                                ))))))
          (dolist (child nodes)
            (dom-remove-node (dom-parent document child) child))))))
  (advice-add 'eww-display-document :before 'my/clean-github))


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
         ("<normal-state> C-k" . cycbuf-switch-to-previous-buffer)))

(use-package eglot :ensure nil
  :custom (eglot-ignored-server-capabilities '(:inlayHintProvider)))

(use-package org :ensure nil
  :hook (org-mode . auto-fill-mode)
  :commands org-capture
  :bind (("C-c C-o c" . org-capture))
  :custom
  (org-pretty-entitites t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-adapt-indentation t)
  (org-startup-with-inline-images t)
  (org-ellipsis " ⤵")
  (org-cycle-separator-lines -1)
  (org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
  :config
  (unless (file-exists-p org-directory) (make-directory org-directory t))
  (setq org-default-notes-file
        (concat (file-name-as-directory org-directory) "notes"))
  :general-config
  ('(normal insert) 'org-mode-map
   "<backtab>" 'org-shifttab)
  ('normal 'org-mode-map
           "C-j" 'cycbuf-switch-to-next-buffer
           "C-k" 'cycbuf-switch-to-previous-buffer))

(use-package shr :ensure nil :defer t
  :custom-face
  (shr-code ((t (:family "Iosevka Slab" :inherit unspecified :weight medium))))
  (shr-text ((t (:family "Noto Sans" :inherit unspecified))))
  (shr-h1 ((t (:height 1.50 :slant unspecified :weight bold :underline t))))
  (shr-h2 ((t (:height 1.45 :slant unspecified :weight bold))))
  (shr-h3 ((t (:height 1.30 :slant unspecified :weight bold))))
  (shr-h4 ((t (:height 1.20 :slant unspecified :weight medium))))
  (shr-h5 ((t (:height 1.15 :slant unspecified :weight normal))))
  (shr-h6 ((t (:height 1.10 :slant oblique :weight normal))))
  )

(use-package proced :ensure nil
  :custom
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-tree-flag t)
  (proced-enable-color-flag t))

(use-package uniquify :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package tab-bar :ensure nil
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-select-restore-windows nil)
  (tab-bar-show 1))

(use-package dabbrev :ensure nil
  :config (add-to-list* 'dabbrev-ignored-buffer-modes
                        'doc-view-mode 'pdf-view-mode
                        'tags-table-mode))

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
  (general-add-hook 'dired-mode-hook (list 'dired-hide-details-mode
                                           'dired-omit-mode)))

(general-add-hook 'prog-mode-hook 'my/prog-word-syntax)

(general-add-hook (list 'lisp-mode-hook
                        'lisp-data-mode-hook
                        'fennel-mode-hook)
                  'my/lisp-word-syntax)

(general-add-hook (list 'prog-mode-hook
                        'text-mode-hook)
                  'my/line-spacing)

(general-add-hook (list 'eww-mode-hook)
                  'my/word-wrap)

(general-add-hook (list 'eww-mode-hook)
                  'my/no-fringes)

(general-add-hook (list 'evil-collection-eldoc-doc-buffer-mode-hook
                        'eww-mode-hook)
                  'my/no-mode-line)

(general-add-hook (list 'help-mode-hook 'eww-mode-hook 'compilation-mode-hook
                        'comint-mode-hook 'apropos-mode-hook 'Info-mode-hook
                        'evil-collection-eldoc-doc-buffer-mode-hook
                        'package-menu-mode-hook 'cycbuf-mode-hook
                        'eat-mode-hook 'proced-mode-hook)
                  'my/smaller-fonts)


;;;; Customized Mode Line
;;;; ======================================================================

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

(defun my/inactive-left ()
  (if (mode-line-window-selected-p)
      ""
    '(:propertize "▌" face ((:height 150 :weight bold :foreground "#DDD") fixed-pitch))))

(defun my/vim-state ()
  (if (mode-line-window-selected-p)
      (let ((mode-text (concat " " (upcase (symbol-name evil-state)) " ")))
        (concat (propertize mode-text 'face
                            `(:foreground "#000000"
                              :background ,(my/vim-color)
                              :family "Iosevka"))
                (propertize "▏" 'face `((:height 150) fixed-pitch))))))

(defun my/make-check-text (status errors warnings info map)
  (if (or (null status) (string= status ""))
      (let* ((e (format "%s " (or errors 0)))
             (w (format "%s " (or warnings 0)))
             (i (format "%s" (or info 0))))
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

(defun my/modeline-buffer-name ()
  `(:propertize " %b" face
    ,(if (mode-line-window-selected-p)
         '((:weight normal)
           mode-line-buffer-id)
       '((:weight light)))))

(defun my/modeline-position-default ()
  `(;; Uncomment this to include the %
    ;;(-3 "%3o%")
    (:propertize "▕" face ((:height 150 :weight bold) fixed-pitch))
    (:propertize ( " %3l : %2C  ") face (:background ,(my/vim-color)
                                         :family "Iosevka"))))

(defun my/modeline-position-pdf ()
  (require 'pdf-view)
  (require 'pdf-info)
  (let ((str (format " Page %d/%d  "
                     (pdf-view-current-page)
                     (pdf-info-number-of-pages))))
    `((:propertize "▕" face ((:height 150) fixed-pitch))
      (:propertize ,str face (:background ,(my/vim-color)
                              :family "Iosevka")))))

(defun my/modeline-position-doc-view ()
  (let ((str (format " Page %d/%d  "
                     (doc-view-current-page)
                     (doc-view-last-page-number))))
    `((:propertize "▕" face ((:height 150) fixed-pitch))
      (:propertize ,str face (:background ,(my/vim-color)
                              :family "Iosevka")))))

(defun my/modeline-position ()
  (if (mode-line-window-selected-p)
      (cond ((eq major-mode 'pdf-view-mode) (my/modeline-position-pdf))
            ((eq major-mode 'doc-view-mode) (my/modeline-position-doc-view))
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

;; Not actually sure I want this, disabling for now
;; (defun my/modeline-vc ()
;;   (if (mode-line-window-selected-p)
;;       '(vc-mode ("" vc-mode " ▕ "))
;;     ""))

(defun my/modeline-modified ()
  "Based on simple-modeline. Show a modified indicator for non-special
buffers."
  (unless (string-match-p "\\`[ ]?\\*" (buffer-name))
    (let ((read-only (and buffer-read-only (buffer-file-name)))
          (modified (buffer-modified-p)))
      (propertize
       (if read-only "" (if modified  "●" "○"))
       'face `(:inherit
               ,(if modified 'error
                  (if read-only 'bold
                    'shadow)))))))

(setopt mode-line-right-align-edge 'window)
(setq-default mode-line-format
              '((:eval (my/inactive-left))
                (:eval (my/evil-state))
                (:eval (my/modeline-modified))
                (:eval (my/modeline-buffer-name))
                (:eval (my/modeline-search))
                (:eval (my/modeline-eldoc))
                mode-line-misc-info
                mode-line-format-right-align
                (:eval (my/modeline-fly))
                (:eval (my/modeline-modes))
                (:eval (my/modeline-position))
                (:eval (if (mode-line-window-selected-p) "" "   "))))

;; In daemon mode, the messages buffer is created too early to get the
;; mode line changes.
(add-hook 'messages-buffer-mode-hook 'my/smaller-fonts)
(with-current-buffer (messages-buffer)
  (setq-local mode-line-format nil)
  (my/smaller-fonts))
    

;;;; Other keybinds
;;;; ======================================================================

;; TODO: Use general.el

(general-define-key
 "C-x k" 'kill-current-buffer
 "<mode-line> <mouse-2>" 'mouse-delete-window
 "<mode-line> <mouse-3>" 'mouse-buffer-menu)

;; evil-cleverparens overwrites these at some point, try to ensure
;; that doesn't happen
(with-eval-after-load 'evil-cleverparens
  (general-def '(normal visual)
    ">" 'evil-shift-right
    "<" 'evil-shift-left)
  ;; More things evil-cp overwrites, plus my customization
  (with-eval-after-load 'evil-surround
    (general-def 'visual 'evil-surround-mode-map
      "s" #'evil-surround-region
      "S" #'evil-Surround-region)))

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
;; normal mode everywhere else.
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
    display-buffer-in-direction
    (direction . rightmost)
    (window-parameters (mode-line-format . none)))
   ("\\`[ ]?\\*\\(Help\\|Customize\\|info\\|eldoc\\|Occur\\|grep\\|devdocs\\|Pp\\|eww\\)"
    display-buffer-in-side-window
    (side . bottom) (slot . -1) (preserve-size . (nil . t)))
   ((or (derived-mode . dired-mode)
        (derived-mode . eww-mode))
    display-buffer-in-side-window
    (side . bottom) (slot . -1) (preserve-size . (nil . t)))
   ("\\`[ ]?\\*\\(compilation\\|[e]?shell\\|[v]?term\\|.*REPL\\)"
    display-buffer-in-side-window
    (side . bottom) (slot . 1) (preserve-size . (nil . t)))
   ((derived-mode . comint-mode)
    display-buffer-in-side-window
    (side . bottom) (slot . 1) (preserve-size . (nil . t)))
   (,(rx (seq bos (or "*" " *")))
    (display-buffer-in-side-window display-buffer-no-window)
    (side . bottom))))
