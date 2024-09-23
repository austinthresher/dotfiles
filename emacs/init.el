;; -*- lexical-binding: t -*-

;;;; TODO:
;;;; =========================================================================
;; - Check out lsp-booster, but it might not be faster than native json parsing
;; - Figure out how to make completion ignore characters after the cursor
;; - Try to make mouse scroll in corfu-popupinfo scroll the help popup
;; - Look at tags to potentially add a tag navigation bar
;; - Make right-click menu on modeline show options to split the window
;; - Figure out how to show corfu help pop-up even when there is only one match
;; - Make tabs insert spaces in comments / non-syntax areas
;; - Make shift-tab unindent too
;; - Use keymap-global-set instead of global-set-key
;; - Try adding a preview to string-insert-rectangle
;;    OR checkout CUA mode's rectangle stuff, that might be better
;; - Use advice :override instead of redefining functions
;;   (currently doing this to customize doom-modeline and corfu, among others)
;; - Look for hooks that would be better as buffer-local
;; - Figure out better window navigation than Alt+Arrows
;; - Look into ways to save common shell commands / etc. Commands to include:
;;     - Profile a Python script, then launch pyprof2calltree / kcachegrind
;; - Try to hook eww to remove junk from github pages
;; - Is it possible to hack tab-line to be per-window instead of per-buffer?
;; - Show number of matches / index of current match when using isearch
;; - Fix python-mode keymap conflict for C-c C-d
;; - Packages to check out:
;;     - disaster, eldoc-box, eldoc-overlay, flycheck-hl-todo,
;;       form-feed-st or page-break-lines, focus
;; - Indicate pop-up frames by disabling the tab bar on them. Maintain a list
;;   of them and reuse them for read-only buffers. Don't allow the sidebar to
;;   exist on these frames.
;; - Figure out what part of lsp-mode is responsible for the nice mouseover
;;   windows and remove scrollbar from them. Try to make them appear for cursor
;;   hover too instead of the eldoc window at the bottom.
;; - Try to make find/replace better. Can the default behavior be changed to
;;   search the entire file, not just from the cursor?


;;;; Early / system settings
;;;; =========================================================================

(setopt display-time-default-load-average nil)
;; (setq debug-on-error t)

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

    (set-face-attribute 'default nil :family font-mono :height 120)
    (set-face-attribute 'fixed-pitch nil :family font-mono :height 120)
    (set-face-attribute 'fixed-pitch-serif nil :family font-fixed-serif :height 120)
    (set-face-attribute 'variable-pitch nil :family font-variable-pitch :height 120
                        :weight 'medium)))

(add-hook 'server-after-make-frame-hook #'my/setup-fonts -90)
(add-hook 'window-setup-hook #'my/setup-fonts -90)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t)

(setq frame-background-mode 'dark)

(defface my/term-bg
  '((default :inherit default))
  "term background color")
(defface my/modeline-highlight
    '((default :box '(:line-width (-2 . -2) :style flat-button)))
  "mouseover highlight for mode line items")
(defface my/treemacs-bg
  '((default :inherit default))
  "treemacs background color")
(defface my/read-only
    '((default :inherit default))
  "face used for read-only buffers like *Messages*")
(defface my/treemacs-face
  '((default :inherit variable-pitch :height 1.0))
  "treemacs files")
(defface my/treemacs-big-face
  '((default :inherit variable-pitch
             :height 1.1))
  "treemacs projects")
(defface my/ibuffer-face
    '((default :inherit variable-pitch :height 120))
  "ibuffer sidebar face")
(defface my/ibuffer-group
    '((default :inherit variable-pitch :height 120))
  "ibuffer filter group")
(defface my/tab-bar-separator
    '((default :width ultra-condensed :height 50))
  "controls distance between tabs")
(defface my/current-tab
    '((default :weight bold))
  "currently selected tab")
(defface my/help-bg
  '((default))
  "help background color")
(defface my/minibuffer-bg
  '((default))
  "minibuffer background color")
(defface my/invisible-help
    '((default))
  "foreground, background, and underline are all the same")
(defface my/invisible-read-only
    '((default))
  "foreground, background, and underline are all the same")
(defface my/invisible-term
    '((default))
  "foreground, background, and underline are all the same")
(defface my/invisible
    '((default))
  "foreground, background, and underline are all the same")
(defface my/hl-line
    '((default))
  "hl-line background color")
(defface my/hl-line-box
    '((default))
  "hl-line for editor windows")


(defun my/get-window-under-mouse ()
  (let ((mpos (mouse-position)))
    (or (window-at (cadr mpos) (cddr mpos) (car mpos))
        (selected-window))))

(defun my/call-in-window-under-mouse (fn &rest args)
  (let ((selected-window (selected-window))
        (target-window (my/get-window-under-mouse)))
    (unless (eq selected-window target-window)
      (select-window target-window 'mark-for-redisplay))
    (ignore-errors (apply fn args))
    (unless (eq selected-window target-window)
      (select-window selected-window t))))

(defmacro my/in-window-under-mouse (&rest body)
  `(my/call-in-window-under-mouse (lambda () ,@body)))

;; If the window is scrolled past the end of the buffer, clamp scroll
;; to the end of the buffer.
(defun my/eob-recenter ()
  (when (and (eq (selected-window) (get-buffer-window))
             (pos-visible-in-window-p (point-max)))
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))

(defun my/is-sidebar-buffer (&optional buf)
  (let ((name (buffer-name (or buf (current-buffer)))))
    (or (string= "*:Buffers:*" name)
        (string-prefix-p " *Treemacs-" name))))

;; Fixes mouse scroll when using built-in scroll mode
(defun my/scroll-up (&optional amt)
  (interactive)
  (scroll-up amt)
  (my/eob-recenter))
(setq mwheel-scroll-up-function 'my/scroll-up)

(defun my/add-scroll-bar (&optional win)
  (let ((w (or win (selected-window))))
    (with-current-buffer (window-buffer w)
      (let ((lines (count-lines (buffer-end -1) (buffer-end 1)))
            (height (window-height w)))
        (if (> lines height)
            (set-window-scroll-bars w 10 'right)
          (set-window-scroll-bars w 0 nil))))))

(defun my/scroll-bar-advice (win &rest _)
  (unless (my/is-sidebar-buffer (window-buffer win))
    (my/add-scroll-bar win)))
(advice-add 'select-window :after #'my/scroll-bar-advice)

;; Functions to filter a list of buffers to only include buffers that are or are
;; not visiting a file. This breaks without the lambda around (buffer-file-name).
;; Never show the scratch buffer in either of these.
(defun my/only-file-buffers (bufs)
  (seq-filter (lambda (b) (buffer-file-name b)) bufs))
(defun my/no-file-buffers (bufs)
  (seq-remove (lambda (b) (or (buffer-file-name b)
                    (string= (buffer-name b) "*scratch*")))
              bufs))


;;;; Packages
;;;; =========================================================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(setopt package-native-compile t)
;; Uncomment to profile startup
;; (setopt use-package-compute-statistics t)
(setopt use-package-enable-imenu-support t)
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)

(use-package no-littering
    :ensure t
    :config (no-littering-theme-backups))

(use-package savehist
    :ensure nil
    :config (savehist-mode t))

(use-package doom-modeline
    :ensure t
    :custom ((doom-modeline-icon t)
             (doom-modeline-minor-modes t)
             (doom-modeline-bar-width 1)
             (doom-modeline-height 16)
             (doom-modeline-buffer-file-name-style 'relative-from-project)
             (doom-modeline-buffer-encoding 'nondefault)
             (doom-modeline-workspace-name nil)
             (doom-modeline-irc nil)
             (doom-modeline-display-misc-in-all-mode-lines nil)
             (doom-modeline-highlight-modified-buffer-name nil))
    :config
    ;; Wrap doom-modeline's formatting for buffer names to reuse elsewhere,
    ;; stripping text properties.
    (defun my/buffer-name (buf)
      (if (and (buffer-file-name buf)
               (not (buffer-local-value 'buffer-read-only buf)))
          (with-current-buffer buf
            (let ((name (doom-modeline-buffer-file-name)))
              (remove-list-of-text-properties
               0 (length name) '(mouse-face face help-echo local-map) name)
              name))
        (buffer-name buf)))
    (defun my/propertize-buffer-name (name tooltip)
      (or
       (ignore-errors
         (concat
          (doom-modeline-spc)
          (doom-modeline--buffer-mode-icon)
          (doom-modeline--buffer-state-icon)
          (propertize name
                      'help-echo tooltip
                      'mouse-face 'doom-modeline-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line mouse-1] 'mouse-buffer-menu)
                                   (define-key map [mode-line mouse-2] 'mouse-buffer-menu)
                                   (define-key map [mode-line mouse-3] 'mouse-buffer-menu)
                                   map)))) ""))
    ;; Overwrite the built-in buffer info segments
    (doom-modeline-def-segment buffer-info
        (my/propertize-buffer-name (doom-modeline--buffer-name)
         (or (buffer-file-name) "Buffer name")))
    (doom-modeline-def-segment buffer-info-simple
        (my/propertize-buffer-name (doom-modeline--buffer-simple-name)
         (or (buffer-file-name) "Buffer name")))
    (doom-modeline-mode t))

(use-package tab-bar
    :ensure nil
    :config
    (setopt tab-bar-format
            '(tab-bar-format-menu-bar
              tab-bar-format-tabs
              tab-bar-separator
              tab-bar-format-align-right))
    (setopt tab-bar-separator " ")
    (setopt tab-bar-auto-width-max '(320 100))
    (setopt tab-bar-close-button-show nil)
    (setopt tab-bar-tab-name-truncated-max 64)
    (defun my/tab-bar-name-padded (tab i)
      (let ((txt (tab-bar-tab-name-format-default tab i)))
        (propertize (format " %-64s " txt)
                    'face (funcall tab-bar-tab-face-function tab))))
    (setopt tab-bar-tab-name-format-function #'my/tab-bar-name-padded)
    (keymap-global-set "C-S-t" 'tab-bar-new-tab-to)
    (keymap-global-set "M-1" (lambda () (interactive) (tab-bar-select-tab 1)))
    (keymap-global-set "M-2" (lambda () (interactive) (tab-bar-select-tab 2)))
    (keymap-global-set "M-3" (lambda () (interactive) (tab-bar-select-tab 3)))
    (keymap-global-set "M-4" (lambda () (interactive) (tab-bar-select-tab 4)))
    (keymap-global-set "M-5" (lambda () (interactive) (tab-bar-select-tab 5)))
    (keymap-global-set "M-6" (lambda () (interactive) (tab-bar-select-tab 6)))
    (keymap-global-set "M-7" (lambda () (interactive) (tab-bar-select-tab 7)))
    (keymap-global-set "M-8" (lambda () (interactive) (tab-bar-select-tab 8)))
    (keymap-global-set "M-9" (lambda () (interactive) (tab-bar-select-tab 9)))
    (tab-bar-mode t)
    ;; These get modified after loading the theme
    (setq tab-bar-menu-bar-button (propertize " 󰍜 " 'face 'my/minibuffer-bg))
    )


(use-package tab-line
    :ensure nil
    :demand t
    :hook
    (help-mode . tab-line-mode)
    (helpful-mode . tab-line-mode)
    (Info-mode . tab-line-mode)
    (Custom-mode . tab-line-mode)
    :config
    (setq tab-line-close-button-show nil)
    (setq tab-line-switch-cycling t)
    (setq tab-line-close-tab-function 'kill-buffer)
    (setq tab-line-tab-face-functions '(tab-line-tab-face-modified))
    (defun my/tab-line-name (buf &optional _) (concat "  " (my/buffer-name buf) "  "))
    (setq tab-line-tab-name-function 'my/tab-line-name)
    (defun my/tab-line-tabs ()
      (let* ((bufs (tab-line-tabs-window-buffers))
             (visiting-file (buffer-file-name (current-buffer)))
             (tabs (if visiting-file (my/only-file-buffers bufs) (my/no-file-buffers bufs))))
        (sort tabs (lambda (a b) (string< (buffer-name a) (buffer-name b))))))
    (setq tab-line-tabs-function 'my/tab-line-tabs))

(use-package whitespace
    :ensure nil
    :hook (prog-mode . whitespace-mode)
    :config
    (setopt whitespace-style '(tab-mark))
    (setq-default indent-tabs-mode nil))

(use-package rainbow-delimiters
    :ensure t
    :hook (prog-mode . rainbow-delimiters-mode)
    :config
    (setopt rainbow-delimiters-outermost-only-face-count 1))

(use-package memoize
    :ensure t
    :demand t
    :config
    ;; defmemoize will complain if we use it to redefine existing functions
    (dolist (fn '(my/darken my/lighten my/desaturate my/saturate))
      (setplist fn nil))
    (defun my/normalize-color (color) ; memoizing this broke stuff
      (if (not (string-prefix-p "#" color))
          color
        (if (= (length color) 13)
            (let ((r (substring color 1 3))
                  (g (substring color 5 7))
                  (b (substring color 9 11)))
              (concat "#" r g b))
          color)))
    (defmemoize my/darken (color amt):
                (my/normalize-color (color-darken-name color amt)))
    (defmemoize my/lighten (color amt)
      (my/normalize-color (color-lighten-name color amt)))
    (defmemoize my/desaturate (color amt)
      (my/normalize-color (color-desaturate-name color amt)))
    (defmemoize my/saturate (color amt)
      (my/normalize-color (color-saturate-name color amt)))
    (defun my/adjust-fg (color amt)
      (pcase frame-background-mode
        ('light (my/darken color amt))
        ('dark (my/lighten color (* 2 amt)))))
    (defun my/adjust-bg (color amt)
      (pcase frame-background-mode
        ('light (my/lighten color amt))
        ('dark (my/darken color (* 2 amt))))))

(use-package catppuccin-theme
    :ensure t
    :config
    (setopt catppuccin-flavor 'frappe)
    (setopt catppuccin-italic-comments t)
    (defun my/customize-catppuccin ()
      (let ((bg (my/lighten (catppuccin-color 'base) 4)))
        (set-face-attribute 'default nil :background bg)
        (set-face-attribute 'fringe nil :background bg)
        (set-face-attribute 'my/invisible nil
                            :background bg
                            :foreground bg
                            :underline `(:color ,bg :position 0))
        (set-face-attribute 'tab-bar-tab nil
                            :height 120
                            :background bg
                            :underline `(:color ,bg :position 0)
                            :inherit '(my/current-tab)))
      (set-face-attribute 'cursor nil
                          :background (my/adjust-fg (catppuccin-color 'text) 20))
      (set-face-attribute 'mode-line-active nil
                          :background (my/adjust-bg (catppuccin-color 'base) 15)
                          :height 100
                          :overline (my/darken (catppuccin-color 'surface0) 10)
                          :underline `(:color ,(my/darken (catppuccin-color 'crust) 10) :position 0)
                          :box 'unspecified)
      ;; :box (list :line-width '(-1 . 1) :style 'released-button
      ;;            :color (my/adjust-fg (catppuccin-color 'crust) 20)))
      (set-face-attribute 'mode-line-inactive nil
                          :background (my/adjust-bg (catppuccin-color 'surface1) 5)
                          :foreground (my/adjust-fg (catppuccin-color 'base) -25)
                          :weight 'normal
                          :overline (my/darken (catppuccin-color 'base) 10)
                          :underline `(:color ,(catppuccin-color 'surface0) :position 0)
                          :height 100
                          :box 'unspecified)
      ;; (list :line-width '(-1 . 1) :style 'flat-button
      ;;            :color (catppuccin-color 'surface1)))
      (dolist (face '(doom-modeline-buffer-file doom-modeline-project-parent-dir
                      doom-modeline-project-dir doom-modeline-project-root-dir
                      doom-modeline-buffer-path doom-modeline-buffer-modified))
        (set-face-attribute face nil
                          :height 100
                          :family font-variable-pitch
                          :weight 'normal))
      (set-face-attribute 'doom-modeline-bar nil
                          :background (catppuccin-color 'base) :inherit 'unspecified)
      (set-face-attribute 'doom-modeline-bar-inactive nil
                          :background (catppuccin-color 'base) :inherit 'unspecified)
      (set-face-attribute 'doom-modeline-highlight nil
                          :background (my/adjust-bg (catppuccin-color 'surface0) 10)
                          :foreground (catppuccin-color 'text)
                          :inherit nil)
      ;; Default rainbow colors are too easy to mix up when side-by-side
      (dolist (pair '((rainbow-delimiters-depth-1-face . text)
                      (rainbow-delimiters-depth-2-face . teal)
                      (rainbow-delimiters-depth-3-face . yellow)
                      (rainbow-delimiters-depth-4-face . green)
                      (rainbow-delimiters-depth-5-face . peach)
                      (rainbow-delimiters-depth-6-face . blue)
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
      (set-face-attribute 'trailing-whitespace nil
                          :background (my/adjust-fg (catppuccin-color 'base) 15))
      (dolist (face '(region highlight))
        (set-face-attribute face nil
                            :box 'unspecified
                            :background (catppuccin-color 'surface0)))
      (face-spec-set 'window-divider-last-pixel
                     `((t (:foreground ,(catppuccin-color 'base)))))
      (face-spec-set 'window-divider-first-pixel
                     `((t (:foreground ,(catppuccin-color 'base)))))
      (face-spec-set 'window-divider
                     `((t (:foreground ,(catppuccin-color 'base)))))
      (let ((dark-border (my/darken (catppuccin-color 'crust)
                                    (pcase catppuccin-flavor
                                      ('latte 20)
                                      (_ 40)))))
        (set-face-attribute 'show-paren-match nil
                            :background (catppuccin-color 'crust)
                            :foreground 'unspecified
                            :inverse-video nil
                            :box `(:line-width (-2 . -2) :color ,dark-border)
                            :weight 'bold))
      ;; Probably too much work to make these actually look like tabs.
      ;; Puts an underline that's the same color as the background over the
      ;; bottom line of the box.
      (face-spec-set 'my/current-tab
                     `((t (:height 100
                           :font ,font-variable-pitch
                           :background unspecified
                           :weight normal
                           :foreground ,(catppuccin-color 'text)
                           :box (:line-width (1 . -1)
                                 :style released-button
                                 :color ,(catppuccin-color 'crust))))))
      (set-face-attribute 'tab-bar nil
                          :background (my/darken (catppuccin-color 'crust) 5)
                          :underline `(:color ,(catppuccin-color 'surface0) :position 0)
                          :extend t
                          :height 20
                          :family font-mono) ; tiny size shrinks spacer
      (set-face-attribute 'tab-bar-tab-inactive nil
                          :background (catppuccin-color 'crust)
                          :family font-variable-pitch
                          :height 120
                          :foreground (my/desaturate (my/adjust-bg (catppuccin-color 'text) 30) 60)
                          :box (list :line-width '(-1 . -1) :style 'released-button
                                     :color (my/adjust-bg (catppuccin-color 'crust) 20)))
      ;; TODO: It would be better to define a face for this
      (setq tab-bar-menu-bar-button
            (propertize " 󰍜 " 'face
                        `((:underline (:color ,(catppuccin-color 'surface0) :position 0))
                          my/minibuffer-bg)))

      (face-spec-set 'tab-line-tab '((t (:inherit my/current-tab))))
      (face-spec-set 'tab-line-tab-current '((t (:inherit my/current-tab))))
      (face-spec-set 'tab-line `((t (:height 10
                                     :extend t
                                     :background ,(catppuccin-color 'base)
                                     :underline (:color ,(catppuccin-color 'surface0)
                                                 :position 0)))))
      (face-spec-set 'tab-line-tab-inactive '((t (:height 100 :inherit 'tab-bar-tab-inactive))))
      (face-spec-set 'tab-line-highlight '((t (:background unspecified))))
      (set-face-attribute 'tab-line-tab-modified nil
                          :slant 'oblique
                          :weight 'unspecified
                          :foreground 'unspecified
                          :background 'unspecified
                          :inherit 'unspecified)
      (set-face-attribute 'tab-line-tab-special nil :slant 'unspecified :weight 'unspecified)
      (let ((bg (pcase frame-background-mode
                            ('light (my/lighten (my/saturate (catppuccin-color 'base) 100) 2))
                            ('dark (my/saturate (my/darken (catppuccin-color 'base) 20) 12)))))
        (set-face-attribute 'my/term-bg nil :background bg)
        (set-face-attribute 'my/invisible-term nil :background bg :foreground bg :underline `(:color ,bg :position 0)))
      (let ((bg (pcase frame-background-mode
                            ('light (my/lighten (my/saturate (catppuccin-color 'base) 25) 2))
                            ('dark (my/darken (my/desaturate (catppuccin-color 'base) 0) 15)))))
        (set-face-attribute 'my/help-bg nil
                            :foreground
                            (my/desaturate (my/adjust-bg (catppuccin-color 'text) 8) 40)
                            :background bg)
        (set-face-attribute 'my/invisible-help nil :background bg :foreground bg :underline `(:color ,bg :position 0)))
      (let ((bg (my/desaturate (catppuccin-color 'base) 6)))
        (set-face-attribute 'my/read-only nil
                          :foreground (catppuccin-color 'overlay2)
                          :background bg)
        (set-face-attribute 'my/invisible-read-only nil :background bg :foreground bg :underline `(:color ,bg :position 0)))
      (set-face-attribute 'my/minibuffer-bg nil
                          :background (my/adjust-bg (catppuccin-color 'crust) 15)
                          :height 120
                          :family font-mono)
      (let ((sidebar-color (pcase frame-background-mode
                             ('light (catppuccin-color 'base))
                             ('dark (my/darken (catppuccin-color 'crust) 25)))))
        (set-face-attribute 'my/treemacs-bg nil :background sidebar-color)
        (set-face-attribute 'my/treemacs-big-face nil :background sidebar-color)
        (set-face-attribute 'my/ibuffer-face nil :background sidebar-color))
      (set-face-attribute 'my/ibuffer-group nil
                          :foreground (catppuccin-color 'teal)
                          :weight 'normal)
      (set-face-attribute 'my/hl-line nil
                          :extend t
                          :background (catppuccin-color 'base))
      (set-face-attribute 'my/hl-line-box nil
                          :extend t
                          :box `(:line-width (-1 . -1) :color ,(catppuccin-color 'surface0)))
      (setopt hl-line-face 'my/hl-line)
      (set-face-attribute 'widget-field nil
                          :background (catppuccin-color 'surface0)
                          :box (list :line-width '(1 . -1)
                                     :color (catppuccin-color 'overlay0)))
      (face-spec-set 'custom-documentation `((t (:foreground ,(catppuccin-color 'base)))))
      )
    (add-hook 'after-load-theme-hook #'my/customize-catppuccin))

(use-package minions
    :ensure t
    :config
    (add-to-list 'minions-prominent-modes 'view-mode)
    (minions-mode t))

(use-package nerd-icons :ensure t :defer)

(use-package vertico
    :ensure t
    :defer t
    :bind (:map vertico-map ("TAB" . #'minibuffer-complete))
    :custom ((vertico-cycle t)
             (vertico-count 5))
    :init (vertico-mode))

(use-package vertico-reverse
    :ensure nil
    :after vertico
    :init (vertico-reverse-mode t))

(use-package corfu
    :ensure t
    :defer t
    :init (global-corfu-mode)
    :bind (:map corfu-map
                ("<tab>" . corfu-next)
                ("<backtab>" . corfu-previous))
    :config
    (setopt corfu-cycle t)
    (setopt corfu-preselect 'valid)
    (setopt corfu-preview-current 'insert)
    (setopt corfu-on-exact-match 'insert)
    (keymap-unset corfu-map "<remap> <move-beginning-of-line>")
    (keymap-unset corfu-map "<remap> <move-end-of-line>")
    (keymap-set corfu-map "<prior>" 'corfu-scroll-down)
    (keymap-set corfu-map "<next>" 'corfu-scroll-up)
    ;; For some reason, Corfu is hard-coded to not show the preview for
    ;; the first match in the list. This overrides the predicate to remove
    ;; that check.
    (defun corfu--preview-current-p ()
      (and corfu-preview-current (>= corfu--index 0))))

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
    :defer t
    :init
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-file))

(use-package orderless
    :ensure t
    :custom
    (orderless-component-separator #'orderless-escapable-split-on-space)
    (completion-styles '(orderless flex basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles partial-completion)))))

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
    :defer t
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

(use-package embark-consult :ensure t :defer t)

(use-package embark
    :ensure t
    :defer t
    :bind (("C-c a" . embark-act)))

(defun my/customize-shell ()
  (face-remap-set-base 'default 'my/term-bg)
  (face-remap-set-base 'fringe 'my/term-bg)
  (face-remap-set-base 'tab-line-tab 'my/current-tab 'my/invisible-term)
  (face-remap-set-base 'tab-line-tab-current 'my/current-tab 'my/invisible-term)
  (setq cursor-type 'box)
  (setq cursor-in-non-selected-windows t))

(use-package eshell
    :ensure t
    :defer t
    :defines eshell-mode-map
    :init (defun my/setup-eshell ()
            (keymap-set eshell-mode-map "C-r" 'consult-history)
            (my/customize-shell))
    :hook (eshell-mode . my/setup-eshell))

(use-package eat
    :ensure t
    :defer t
    :config
    (eat-eshell-mode)
    (eat-eshell-visual-command-mode)
    (setopt eat-term-name "xterm-256color")
    (setopt eat-default-cursor-type '(box 0.5 hollow))
    (keymap-set eat-eshell-char-mode-map "<escape>" 'eat-self-input)
    (keymap-set eat-char-mode-map "<escape>" 'eat-self-input)
    (add-hook 'eat-mode-hook #'my/customize-shell))

(use-package magit
    :ensure t
    :defer t
    :bind (("C-x g" . magit-status)
           ("C-x G" . magit-dispatch)
           :map transient-map ("<escape>" . transient-quit-one))
    :config (global-unset-key (kbd "C-x M-g")))

(use-package transpose-frame :ensure t :defer t)
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
    :defer t
    :init (projectile-mode t)
    :bind (:map projectile-mode-map ("C-c p" . projectile-command-map))
    :config
    (projectile-register-project-type 'godot '("project.godot")
                                      :project-file "project.godot"))

(use-package ibuffer
    :ensure nil
    :demand t
    :config
    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-use-header-line nil)
    (setq ibuffer-default-sorting-mode 'alphabetic)
    (add-to-list 'ibuffer-never-show-predicates "\\*scratch\\*")
    (defun my/read-only-p (buf)
      (and (buffer-file-name buf)
           (buffer-local-value 'buffer-read-only buf)))
    (defun my/editable-file-p (buf)
      (and (buffer-file-name buf)
           (not (buffer-local-value 'buffer-read-only buf))))
    (setq ibuffer-saved-filter-groups
          '(("Groups"
             ("Files" (predicate my/editable-file-p (current-buffer)))
             ("View" (predicate my/read-only-p (current-buffer)))
             ("Shell" (or (derived-mode . eat-mode)
                          (derived-mode . eshell-mode)))
             ("Process" (process))
             ("Docs" (or (derived-mode . help-mode)
                         (derived-mode . apropos-mode)
                         (derived-mode . Info-mode)
                         (derived-mode . helpful-mode)
                         (derived-mode . shortdoc-mode)
                         (derived-mode . eww-mode)
                         (derived-mode . doc-view-mode)))
             ("Customize" (derived-mode . Custom-mode))
             ("Magit" (name ."^magit"))
             ("Directory" (derived-mode . dired-mode))
             ("Other" (name . ".*"))
             )))
    (defun my/color-icon (name color)
      (let ((c (catppuccin-color color)))
        (propertize (nerd-icons-mdicon name)
                    'face `(:family ,nerd-icons-font-family :foreground ,c))))
    ;; Simplified version of doom-modeline--buffer-file-state-icon
    ;; TODO: finish the other states
    (defun my/buffer-state-icon ()
      (cond ((not (or (and (buffer-file-name) (file-remote-p buffer-file-name))
                      (verify-visited-file-modtime (current-buffer))))
             (concat " " (my/color-icon "nf-md-reload_alert" 'yellow)))
            ((and (buffer-file-name) (buffer-modified-p))
             (concat " " (my/color-icon "nf-md-content_save_edit" 'yellow)))
            ((and (buffer-file-name) buffer-read-only)
             (concat " " (my/color-icon "nf-md-lock" 'overlay0)))
            (t "")))
    (define-ibuffer-column icon (:name "Icon")
      (with-current-buffer buffer
        (concat (nerd-icons-icon-for-buffer)
                (my/buffer-state-icon))))
    (define-ibuffer-column doom-name
        (:name "Name"
         :inline t
         :header-mouse-map ibuffer-name-header-map
         :props ('mouse-face 'highlight
                 'keymap ibuffer-name-map
                 'ibuffer-name-column t))
        (my/buffer-name buffer))
    (setq ibuffer-hidden-filter-groups '("Other"))
    (defun my/ibuffer-load-groups ()
      (ibuffer-switch-to-saved-filter-groups "Groups"))
    (add-hook 'ibuffer-mode-hook #'my/ibuffer-load-groups)
    (defun my/ibuffer-kill-mouse-buffer (event)
      (interactive "e")
      (my/in-window-under-mouse
       (let ((pt (save-excursion
                   (mouse-set-point event)
                   (point))))
         (goto-char pt)
         (when-let ((buf (ignore-errors (ibuffer-current-buffer t))))
           (kill-buffer buf)))))
    (keymap-set ibuffer-name-map "<mouse-1>" #'ibuffer-mouse-visit-buffer)
    (keymap-set ibuffer-name-map "<mouse-2>" #'my/ibuffer-kill-mouse-buffer)
    (keymap-set ibuffer-mode-filter-group-map "<mouse-1>" #'ibuffer-mouse-toggle-filter-group)
    (keymap-set ibuffer-mode-filter-group-map "<mouse-2>" #'ibuffer-mouse-toggle-mark)
    ;; I get a weird mouse selection when I try to right-click in ibuffer when
    ;; ibuffer-sidebar isn't the selected window.
    (defun my/fixed-ibuffer-right-click (event)
      (interactive "e")
      (with-selected-window (my/get-window-under-mouse)
        (ibuffer-mouse-popup-menu event)))
    (keymap-set ibuffer-mode-filter-group-map "<down-mouse-3>" #'my/fixed-ibuffer-right-click)
    (keymap-set ibuffer-name-map "<down-mouse-3>" #'my/fixed-ibuffer-right-click)
    (defun my/ibuffer-remove-header (_)
      (save-excursion
        (let ((ro buffer-read-only))
          (setq-local buffer-read-only nil)
          (unwind-protect
               (progn
                 (goto-char 1)
                 (search-forward "-\n" nil t)
                 (delete-region 1 (point))
                 (goto-char (buffer-end 1)))
            (setq-local buffer-read-only ro)))))
    (advice-add 'ibuffer-update-title-and-summary :after #'my/ibuffer-remove-header))

(use-package ibuffer-sidebar
    :ensure t
    :demand t
    :after ibuffer
    :config
    ;; setq is used here because I get type errors if I use setopt
    (setq ibuffer-sidebar-mode-line-format nil)
    (setq ibuffer-sidebar-width 24)
    (setq ibuffer-sidebar-face 'my/ibuffer-face)
    (setq ibuffer-sidebar-use-custom-font t)
    (setq ibuffer-sidebar-display-alist
          '((side . left)
            (slot . -1)
            (window-height . 0.33)
            (window-parameters (no-other-window . t))))
    (setq ibuffer-sidebar-special-refresh-commands
          '((kill-buffer . 0)
            (find-file . 0)
            (delete-file . 0)
            (kill-current-buffer . 0)))
    (setq ibuffer-sidebar-formats '((" " mark " " icon " " doom-name)))
    (defun my/customize-ibuffer ()
      (face-remap-add-relative 'default 'my/ibuffer-face)
      (face-remap-add-relative 'fringe 'my/ibuffer-face)
      (face-remap-add-relative ibuffer-filter-group-name-face 'my/ibuffer-group)
      (add-hook 'post-command-hook 'my/eob-recenter)
      (set-fringe-mode 10))
    (add-hook 'ibuffer-sidebar-mode-hook #'my/customize-ibuffer)
    (defun my/ibuffer-sidebar-autoresize (&rest _)
      ;; Only resize when this is the sidebar buffer window and treemacs is visible
      (when (and (eq (selected-window) (ibuffer-sidebar-showing-sidebar-p))
                 (treemacs-get-local-window))
        (let* ((fit-window-to-buffer-horizontally nil)
               (lines (count-lines (buffer-end -1) (buffer-end 1) t))
               (size (max 3 (+ lines 1))))
          (fit-window-to-buffer (selected-window)
                                size size))))
    (advice-add 'ibuffer-update-title-and-summary :after #'my/ibuffer-sidebar-autoresize)
    )

(use-package treemacs
    :ensure t
    :demand
    :config
    (setopt treemacs-is-never-other-window t)
    (setopt treemacs-select-when-already-in-treemacs 'move-back)
    (setopt treemacs-width-is-initially-locked nil)
    (setopt treemacs-width 24)
    (setopt treemacs-move-files-by-mouse-dragging nil)
    (treemacs-follow-mode -1)
    (defun my/treemacs-hide-modeline (&rest _)
      (when (treemacs-is-treemacs-window-selected?)
        (text-scale-set -1)
        (setq mode-line-format nil)
        (add-hook 'post-command-hook 'my/eob-recenter)
        (set-window-fringes (selected-window) 10 1)))
    (defun my/customize-treemacs ()
      (keymap-set treemacs-mode-map "<mouse-1>" #'treemacs-single-click-expand-action)
      (face-remap-add-relative 'treemacs-root-face
                               'my/treemacs-big-face
                               :foreground (catppuccin-color 'teal))
      (face-remap-add-relative 'treemacs-root-remote-face
                               'my/treemacs-big-face
                               :foreground (catppuccin-color 'yellow))
      (face-remap-add-relative 'treemacs-root-remote-disconnected-face
                               'my/treemacs-big-face
                               :foreground (catppuccin-color 'mauve))
      (face-remap-add-relative 'treemacs-root-remote-unreadable-face
                               'my/treemacs-big-face
                               :foreground (catppuccin-color 'maroon))
      (dolist (face '(treemacs-directory-face treemacs-file-face
                      treemacs-git-unmodified-face treemacs-git-modified-face
                      treemacs-git-added-face treemacs-git-conflict-face
                      treemacs-git-renamed-face treemacs-git-untracked-face
                      treemacs-tags-face treemacs-term-node-face
                      treemacs-marked-file-face treemacs-git-commit-diff-face
                      treemacs-async-loading-face))
        (face-remap-add-relative face 'my/treemacs-face))
      (face-remap-add-relative 'treemacs-window-background-face 'my/treemacs-bg))
    (add-hook 'treemacs-select-functions #'my/treemacs-hide-modeline)
    (add-hook 'treemacs-mode-hook #'my/customize-treemacs))

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
    :defer t
    :hook (prog-mode . flycheck-mode)
    :bind (("C-c e" . flycheck-next-error)
           ("C-c E" . flycheck-previous-error)
           ("C-c C-e" . flycheck-list-errors))
    :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; (use-package anaconda-mode
;;     :ensure t
;;     :hook
;;     (python-mode . anaconda-mode)
;;     (python-mode . anaconda-eldoc-mode)
;;     :config (setq anaconda-mode-localhost-address "localhost"))

(use-package lsp-mode
    :ensure t
    :init (defun my/lsp-mode-setup-completion ()
            (setq-local completion-styles '(orderless)
                        completion-category-defaults nil))
    ;; (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
    ;;       '(flex)))
    :hook
    (lsp-completion-mode . my/lsp-mode-setup-completion)
    (python-mode . lsp)
    :config
    (setopt lsp-keymap-prefix "C-c l")
    (setopt lsp-completion-provider :none)
    (setopt lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
    :ensure t
    :hook (flycheck-mode . lsp-ui-mode)
    :config (setopt lsp-ui-sideline-enable nil))

(use-package dap-mode
    :ensure t
    :after lsp-mode)

;; TODO: Add actual snippets (maybe yasnippet-snippets)
(use-package yasnippet
    :ensure t
    :hook (python-mode . yas-minor-mode))

(use-package lsp-jedi
    :ensure t
    :after lsp-mode)

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
    :defer t
    :after gdscript-mode
    :defines lsp-register-client make-lsp-client lsp-gdscript-tcp-connect-to-port lsp-activate-on
    :config
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-gdscript-tcp-connect-to-port)
                      :activation-fn (lsp-activate-on "gdscript")
                      :server-id 'gdscript
                      :notification-handlers (ht ("gdscript/capabilities" 'ignore)))))

(use-package adaptive-wrap
    :ensure t
    :hook (prog-mode . adaptive-wrap-prefix-mode)
    :config
    (setopt adaptive-wrap-extra-indent 1)
    (set-fringe-bitmap-face 'right-curly-arrow 'ansi-color-magenta)
    (set-fringe-bitmap-face 'left-curly-arrow 'ansi-color-magenta))

(use-package helpful
    :ensure t
    :config
    (keymap-global-set "C-h f" #'helpful-callable)
    (keymap-global-set "C-h v" #'helpful-variable)
    ;; Helpful doesn't show your keypress, so keep the built-in C-h k binding
    (keymap-global-set "C-h C-k" #'helpful-key)
    (keymap-global-set "C-h x" #'helpful-command)
    (keymap-global-set "C-h C-h" #'helpful-at-point))

(use-package slime
    :ensure t
    :defer t
    :config (setopt inferior-lisp-program "sbcl"))

(use-package server
    :ensure nil
    :demand t
    :config (when (and (my/windows-p) (not (server-running-p)))
              (server-start)))

(use-package recentf
    :ensure nil
    :config
    (add-to-list 'recentf-exclude "/sudo:")
    (add-to-list 'recentf-exclude "/sudoedit:")
    (add-to-list 'recentf-exclude "/su:")
    (add-to-list 'recentf-exclude "/doas:"))

(use-package rect
    :ensure nil
    :config
    (keymap-set rectangle-mark-mode-map "C-i" 'string-insert-rectangle)
    (keymap-set rectangle-mark-mode-map "C-r" 'replace-rectangle))

(use-package browse-url
    :ensure nil
    :defer t
    :config
    (setopt browse-url-browser-function 'eww-browse-url))

(use-package apropos
    :ensure nil
    :defer t
    :bind (:map apropos-mode-map ("q" . kill-current-buffer))
    :config (setopt apropos-do-all t))


;;;; Customize built-in modes (use-package didn't work)
;;;; =========================================================================

(setopt info-lookup-other-window-flag nil)
(setopt Info-isearch-search nil)
(setopt custom-buffer-done-kill t)
(setopt help-window-select t)
(setopt help-downcase-arguments t)
(defun my/customize-help ()
  (face-remap-set-base 'default 'my/help-bg)
  (face-remap-set-base 'fringe 'my/help-bg)
  (face-remap-set-base 'tab-line-tab 'my/current-tab 'my/invisible-help)
  (face-remap-set-base 'tab-line-tab-current 'my/current-tab 'my/invisible-help))
(add-hook 'help-mode-hook #'my/customize-help)
(add-hook 'helpful-mode-hook #'my/customize-help)
(add-hook 'Custom-mode-hook #'my/customize-help)
(add-hook 'apropos-mode-hook #'my/customize-help)
(add-hook 'shortdoc-mode-hook #'my/customize-help)
(add-hook 'Info-mode-hook #'my/customize-help)

(defun my/customize-read-only ()
  (face-remap-set-base 'default 'my/read-only)
  (face-remap-set-base 'fringe 'my/read-only)
  (face-remap-set-base 'tab-line-tab 'my/current-tab 'my/invisible-read-only)
  (face-remap-set-base 'tab-line-tab-current 'my/current-tab 'my/invisible-read-only))

(add-hook 'view-mode-hook #'my/customize-read-only)

(defun my/messages-scroll-advice (&rest _)
  (with-current-buffer (messages-buffer)
    (when (eq (window-buffer (selected-window))
              (messages-buffer))
      (goto-char (point-max))
      (ignore-errors (backward-char 1))
      (recenter -1))))

(defun my/customize-messages ()
  (my/customize-read-only)
  (advice-add 'tab-line-select-tab :after 'my/messages-scroll-advice)
  (add-hook 'after-change-functions 'my/messages-scroll-advice 0 t))
(add-hook 'messages-buffer-mode-hook #'my/customize-messages)
(with-current-buffer (messages-buffer) ;; make sure we hit the initial messages buffer
  (my/customize-messages))

(defun my/customize-minibuffer ()
  (face-remap-add-relative 'default 'my/minibuffer-bg)
  (face-remap-add-relative 'fringe 'my/minibuffer-bg))
(add-hook 'minibuffer-mode-hook #'my/customize-minibuffer)


;;;; Global minor modes and related options
;;;; =========================================================================

(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode t)

(tool-bar-mode -1)
(menu-bar-mode -1) ; shown in tab bar instead
(indent-tabs-mode -1) ; always use spaces
(context-menu-mode t)
(unless (display-graphic-p) (xterm-mouse-mode t))
(delete-selection-mode t)
(undelete-frame-mode t) ; allow restoring closed frames
(set-fringe-style '(20 . 1))
(minibuffer-depth-indicate-mode t)

(setopt window-divider-default-right-width 1)
(setopt window-divider-default-bottom-width 1)
(setopt window-divider-default-places 'bottom-only)
(window-divider-mode t)


;;;; Options
;;;; =========================================================================

;; Fixes indentation of quoted lists
(setopt lisp-indent-function 'common-lisp-indent-function)
(setopt lisp-indent-defun-method '(2 &lambda &body))

;; Built-in completion options
(setopt enable-recursive-minibuffers t)
(setopt completion-cycle-threshold nil)
(setopt completions-detailed t)
(setopt tab-always-indent 'complete)
;; (setopt completion-styles '(basic flex))
(setopt completions-max-height 8)
(setopt completions-format 'one-column)
(setopt completions-header-format nil)
(setopt completion-on-separator-character t)
(setopt completion-show-help nil)

(setq initial-major-mode 'prog-mode)
(setq initial-scratch-message "")

(setopt history-delete-duplicates t)

(setq minibuffer-beginning-of-buffer-movement t)
(setq minibuffer-default-prompt-format "")
(setq minibuffer-prompt-properties
      '(read-only t
        cursor-intangible t
        face minibuffer-prompt))
(setopt inhibit-message-regexps '("Beginning of buffer" "End of buffer"))
(setopt set-message-functions '(inhibit-message set-minibuffer-message))

(add-to-list 'completion-ignored-extensions "__pycache__/")
(setopt read-extended-command-predicate
        #'command-completion-default-include-p)
(setopt sentence-end-double-space nil)
(setopt vc-follow-symlinks t)

(setopt text-scale-mode-step 1.05)
(setopt mouse-wheel-progressive-speed nil)
(setopt mouse-wheel-scroll-amount '(0.1
                                    ((shift) . 0.9)
                                    ((meta) . hscroll)
                                    ((control) . text-scale)
                                    ((control meta) . global-text-scale)))
(setopt mouse-buffer-menu-maxlen 64)
(setopt mouse-buffer-menu-mode-mult 999)
(setopt mouse-drag-and-drop-region-scroll-margin 2)
(setopt mouse-wheel-flip-direction t)
(setopt mouse-wheel-tilt-scroll t)

(setopt scroll-step 1) ; Allow scrolling line-by-line, mainly for terms
(setopt hscroll-step 1)
(setopt hscroll-margin 1)

(setopt x-underline-at-descent-line nil)
(setopt switch-to-buffer-obey-display-actions t)
(setopt column-number-mode t)
(setopt line-move-visual nil)
(setopt show-paren-delay 0.0)
(setopt show-paren-when-point-in-periphery t)
(setopt show-paren-when-point-inside-paren t)
(setopt cursor-in-non-selected-windows nil)
(setopt show-trailing-whitespace nil)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(setq-default buffer-file-coding-system 'utf-8-unix)

(setopt disabled-command-function 'ignore)
(setopt isearch-allow-scroll 'unlimited)
(setopt isearch-repeat-on-direction-change t)
(setopt isearch-wrap-pause 'no-ding)
(setopt search-default-mode t)

(setopt Man-width-max 120)

(setopt switch-to-buffer-in-dedicated-window 'pop)
(setopt switch-to-buffer-obey-display-actions t)

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
  (let ((buffer-quit-function (or buffer-quit-function #'keyboard-quit)))
    (funcall fun)))
(advice-add #'keyboard-escape-quit :around #'my/keyboard-escape-quit-adv)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; I keep accidentally hitting this when trying to exit a prompt
(global-unset-key (kbd "C-x ESC"))

(when (display-graphic-p) ; fix awful default GUI behavior
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

(defun recompile-or-prompt ()
  (interactive)
  (if (string= compile-command "make -k ")
      ;; Subtle change from the default to make this not trigger a second time
      (progn (setq compile-command "make -k")
             (call-interactively 'compile))
    (recompile)))

(keymap-global-set "<f5>" 'recompile-or-prompt)
(keymap-global-set "<f6>" 'compile)


;;; Style note: not including the my/ prefix for interactive commands.

(defun dark-theme ()
  (interactive)
  (setq frame-background-mode 'dark)
  (mapc 'frame-set-background-mode (frame-list))
  (setopt catppuccin-flavor 'frappe)
  (load-theme 'catppuccin t))

(defun light-theme ()
  (interactive)
  (setq frame-background-mode 'light)
  (mapc 'frame-set-background-mode (frame-list))
  (setopt catppuccin-flavor 'latte)
  (load-theme 'catppuccin t))

;; Make Ctrl+Backspace behave consistently with other editors
;; FIXME: This will still delete too many characters when used
;; like:     (1, 2)))<cursor>
;; This will delete the 2 as well, I would like to keep that.
(defun backward-kill-space-or-word ()
  (interactive)
  (if (looking-back "[ \n]" nil)
      (progn (delete-horizontal-space t)
             (while (looking-back "[ \n]" nil) (backward-delete-char 1)))
    (backward-kill-word 1)))
(keymap-global-set "C-<backspace>" 'backward-kill-space-or-word)

;; Both functions always skips *scratch*
(defun my/skip-file-buffers (_ buf _)
  (or (buffer-file-name buf) (string= "*scratch*" (buffer-name buf))))
(defun my/skip-non-file-buffers (_ buf _ )
    (not (my/skip-file-buffers nil buf nil)))

;; Cycle file buffers, skipping others
(defun previous-file-buffer ()
  (interactive)
  (let ((switch-to-prev-buffer-skip #'my/skip-non-file-buffers))
    (previous-buffer)))

(defun next-file-buffer ()
  (interactive)
  (let ((switch-to-prev-buffer-skip #'my/skip-non-file-buffers))
    (next-buffer)))

(defun previous-non-file-buffer ()
  (interactive)
  (let ((switch-to-prev-buffer-skip #'my/skip-file-buffers))
    (previous-buffer)))

(defun next-non-file-buffer ()
  (interactive)
  (let ((switch-to-prev-buffer-skip #'my/skip-file-buffers))
    (next-buffer)))

(defun next-similar-buffer ()
  (interactive)
  (if (my/skip-file-buffers nil (current-buffer) nil)
      (next-file-buffer)
    (next-non-file-buffer)))

(defun previous-similar-buffer ()
  (interactive)
  (if (my/skip-file-buffers nil (current-buffer) nil)
      (previous-file-buffer)
    (previous-non-file-buffer)))

(defun back-or-previous-buffer ()
  (interactive)
  (my/in-window-under-mouse
   (or (and (eq major-mode 'help-mode)
            (ignore-errors (help-go-back) t))
       (and (eq major-mode 'Info-mode)
            (ignore-errors (Info-history-back) t))
       (and tab-line-mode
            (ignore-errors (tab-line-switch-to-prev-tab) t))
       (previous-similar-buffer))))

(defun forward-or-next-buffer ()
  (interactive)
  (my/in-window-under-mouse
   (or (and (eq major-mode 'help-mode)
            (ignore-errors (help-go-forward) t))
       (and (eq major-mode 'Info-mode)
            (ignore-errors (Info-history-forward ) t))
       (and tab-line-mode
            (ignore-errors (tab-line-switch-to-next-tab) t))
       (next-similar-buffer))))

(keymap-global-set "M-[" 'previous-similar-buffer)
(keymap-global-set "M-]" 'next-similar-buffer)
;; Ctrl+Alt allows cycling any buffers, not just similar
(keymap-global-set "C-M-[" 'previous-buffer)
(keymap-global-set "C-M-]" 'next-buffer)
(keymap-global-set "<mouse-8>" 'back-or-previous-buffer)
(keymap-global-set "<mouse-9>" 'forward-or-next-buffer)
(keymap-global-set "<tab-line> <mouse-8>" 'tab-line-switch-to-prev-tab)
(keymap-global-set "<tab-line> <mouse-9>" 'tab-line-switch-to-next-tab)
(when (my/windows-p)
  (keymap-global-set "<mouse-4>" 'back-or-previous-buffer)
  (keymap-global-set "<mouse-5>" 'forward-or-next-buffer))
;; Middle clicking should delete the window instead of right click
(keymap-global-set "<mode-line> <mouse-2>" 'mouse-delete-window)
;; Right click should _not_ delete the window, much too easy to do accidentally.
;; Come up with something clever here, for now just show buffer menu.
(keymap-global-set "<mode-line> <mouse-3>" 'mouse-buffer-menu)

;; my custom sidebar, treemacs + ibuffer-sidebar
(defun show-sidebar ()
  (interactive)
  (ignore-errors (treemacs-select-window))
  (redisplay)
  (ignore-errors
    (when (treemacs-is-treemacs-window-selected?)
      (treemacs-select-window))
    (redisplay)) ; somehow keeps both in sync, sometimes breaks without
  (ibuffer-sidebar-show-sidebar)
  (with-selected-window (ibuffer-sidebar-showing-sidebar-p)
    (setq-local window-size-fixed nil)
    (my/ibuffer-sidebar-autoresize)))

(defun hide-sidebar ()
  (interactive)
  (when-let ((treemacs-window (treemacs-get-local-window)))
    (delete-window treemacs-window))
  ;; Documentation on this is wrong (it returns the window, not the buffer)
  (when-let ((ibuffer-window (ibuffer-sidebar-showing-sidebar-p)))
    (delete-window ibuffer-window)))

(defun toggle-sidebar ()
  (interactive)
  (if (and (treemacs-get-local-window)
           (ibuffer-sidebar-showing-sidebar-p))
      (hide-sidebar)
    (show-sidebar)))

(keymap-global-set "C-c C-d" 'toggle-sidebar)

;; previous-window-any-frame doesn't respect no-other-window
(defun other-other-window ()
  (interactive)
  (other-window -1))
(keymap-global-set "M-o" 'other-other-window)


(defvar bottom-window nil)
(defvar main-window nil)
;; Custom layout that uses a bottom split + tabline-mode
(defun reset-window-layout ()
  (interactive)
  (let ((buf (if (buffer-file-name)
                 (current-buffer)
               (get-scratch-buffer-create))))
    (hide-sidebar)
    (split-window-right)
    (other-window 1)
    (let ((ignore-window-parameters t))
      (delete-other-windows)
      (setq main-window (selected-window))
      (let ((switch-to-buffer-obey-display-actions nil))
        (switch-to-buffer buf nil t))
    (show-sidebar)
    (split-window-below -10)
    (let ((switch-to-buffer-obey-display-actions nil))
      (other-window 1)
      (setq bottom-window (selected-window))
      (switch-to-buffer (messages-buffer) t t)
      (tab-line-mode t)
      (goto-char (point-max))
      (other-window 1)))))

(defun my/buffer-is-read-only (buf)
  (buffer-local-value 'buffer-read-only (get-buffer buf)))
(defun my/buffer-is-not-file (buf)
  (null (buffer-file-name (get-buffer buf))))

(setq display-buffer-alist
      '(;; Sidebar with buffer list and file tree
        ("\\*:Buffers:\\*"
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-in-side-window)
         (side . left) (slot . -1) (dedicated . t))
        (" \\*Treemacs-"
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-in-side-window)
         (side . left) (slot . 1) (dedicated . t))
        ;; Open package list in a new tab
        ("\\*Packages\\*"
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-in-tab)
         (tab-name . "Packages")
         (dedicated . t))
        ;; Handle read-only buffers that visit real files. Usually from
        ;; find-function and similar.
        ;; FIXME: This doesn't quite work when using find-function in
        ;;        another frame
        ((and my/buffer-is-read-only (not my/buffer-is-not-file))
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-pop-up-frame)
         (mode fundamental-mode))
        ;; Real files. WIP, not quite where I want it.
        ((and (not my/buffer-is-not-file) (not my/buffer-is-read-only))
         (display-buffer-reuse-mode-window
          display-buffer-in-previous-window
          display-buffer-pop-up-window)
         (body-function . (lambda (win) t))
         (mode prog-mode fundamental-mode))
        ;; Everything that should show up in the bottom window
        ((and (or "\\*Async Shell Command\\*"
                  (major-mode . help-mode) (major-mode . helpful-mode)
                  (major-mode . Info-mode) (major-mode . Man-mode)
                  (major-mode . apropos-mode) (major-mode . messages-buffer-mode)
                  (major-mode . comint-mode) (major-mode . Custom-mode)
                  (major-mode . compilation-mode) (major-mode . occur-mode)
                  (major-mode . shell-mode) (major-mode . eshell-mode)
                  (major-mode . lisp-interaction-mode) (major-mode . debugger-mode))
          my/buffer-is-not-file)
         (display-buffer-reuse-window
          display-buffer-reuse-mode-window
          display-buffer-at-bottom)
         (window-height . 18)
         ;; Enable tab-line for every buffer displayed in the bottom window
         (body-function . (lambda (win)
                            (with-selected-window win
                              (if (string= (buffer-name) "*scratch*")
                                  (setq-local tab-line-exclude t)
                                (tab-line-mode t))
                              (when (eq major-mode 'messages-buffer-mode)
                                (my/eob-recenter)))))
         (mode messages-buffer-mode help-mode helpful-mode Info-mode
          apropos-mode Man-mode shell-mode compilation-mode comint-mode
          Custom-mode occur-mode special-mode fundamental-mode eshell-mode
          lisp-interaction-mode debugger-mode inferior-python-mode))
        ))


(defvar my/one-time-setup nil)
(defun my/first-time-theme-setup ()
  (unless my/one-time-setup
    (setq my/one-time-setup t)
    (dark-theme))
  (remove-hook 'window-setup-hook 'my/first-time-theme-setup)
  (remove-hook 'server-after-make-frame-hook 'my/first-time-theme-setup))

(unless server-mode
  (add-hook 'window-setup-hook #'my/first-time-theme-setup)
  (add-hook 'window-setup-hook #'reset-window-layout))

(add-hook 'server-after-make-frame-hook #'my/first-time-theme-setup)
(add-hook 'server-after-make-frame-hook #'reset-window-layout)

;; Make the sidebar play nice with transpose-frame and related functions.
;; Lots of paranoid redisplays here to make sure window changes have applied.
(defun my/hide-and-restore-sidebar (fn &rest args)
  (let ((treemacs-window (treemacs-get-local-window))
        (ibuffer-window (ibuffer-sidebar-showing-sidebar-p)))
    (when treemacs-window (delete-window treemacs-window))
    (when ibuffer-window (delete-window ibuffer-window))
    (redisplay)
    (apply fn args)
    (let ((cur-win (selected-window)))
      (when treemacs-window (treemacs-select-window) (redisplay))
      (when ibuffer-window (ibuffer-sidebar-show-sidebar) (redisplay))
      (select-window cur-win))))

(dolist (fn '(transpose-frame flip-frame flop-frame rotate-frame
              rotate-frame-clockwise rotate-frame-anticlockwise))
  (advice-add fn :around #'my/hide-and-restore-sidebar))


;; Make C-x k kill the current buffer without prompting to select a buffer
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(keymap-global-set "C-x k" 'kill-current-buffer)
(keymap-global-set "C-x K" 'kill-buffer-and-window)

(keymap-global-set "C-S-s" 'isearch-backward)
(keymap-set isearch-mode-map "C-S-s" 'isearch-repeat-backward)
(keymap-set help-mode-map "q" 'kill-current-buffer)
;(keymap-set backtrace-mode-map "q" 'kill-current-buffer)
(keymap-set special-mode-map "q" 'kill-current-buffer)


;; Vim-style, join down instead of up
(defun vim-join-line ()
  (interactive)
  (delete-indentation t))
(keymap-global-set "C-S-j" 'vim-join-line)

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)


;;;; Language-specific configuration
;;;; =========================================================================

(setopt python-indent-guess-indent-offset nil)


;;;; Misc
;;;; =========================================================================

(defun hide-buffer (name)
  (interactive "bBuffer to hide: ")
  (let ((b (get-buffer name)))
    (unless (string-match-p "^ " (buffer-name b))
      (with-current-buffer b
        (rename-buffer (concat " " (buffer-name b)))))))

;; File-type mode detection special cases
(add-to-list 'auto-mode-alist '("bashrc" . sh-mode)) ; no leading '.'

;; Make emacs source files (from help links, etc) open in view mode
;; NOTE: This should be after use-package stuff but before any files
;;       that might need to be edited by hand are added to load-path
(dir-locals-set-class-variables
 'emacs
 '((nil . ((eval . (when buffer-file-name
                     (view-mode-enter nil #'kill-buffer)))))))
(dolist (path (cons source-directory load-path))
  (dir-locals-set-directory-class path 'emacs))
