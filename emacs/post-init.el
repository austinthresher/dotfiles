;;; -*- no-byte-compile: t; lexical-binding: t; -*-

;; TODO: Pick a single key binding for buffer swapping, probably a function key
;; TODO: See if paredit is better behaved than smartparens

;;;; TODOs that probably require writing elisp:
;;;; ======================================================================
;; - Make keybinds for the mouse back/forward buttons that call appropriate
;;   functions in the window under the mouse, based on the major mode of that
;;   window's buffer. pdf-history-backward, help-go-back, etc.

;; - Try to figure out triggering sp-comment when the comment string is typed
;;   for the current language's mode.

;; - Write a function for fennel-mode to update `fennel-indent-function' for
;;   the current symbol (or current function definition, if possible). Also
;;   look at the `fennel-doc-string-elt' symbol property to add docstring
;;   support to function-defining macros.

;; - Figure out a way to have orderless-style search


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

(defun find-file-new-tab (filename &optional wildcards)
  "Like find-file-other-tab, but behaves when no frame yet exists."
  (let* ((buffer-names (mapcar 'buffer-name (buffer-list)))
         (is-initial-frame? (member " *server-dummy*" buffer-names)))
    (if is-initial-frame?
        (find-file filename wildcards)
      (find-file-other-tab filename wildcards)))
  (raise-frame))

;;;; Early dependencies
;;;; ======================================================================

(use-package dash :ensure t :demand t)
(require 'cl-lib)


;;;; Theme
;;;; ======================================================================

(use-package modus-themes :ensure t :demand t
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-slanted-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-to-rotate '(modus-operandi modus-vivendi))
  (modus-themes-common-palette-user
   '((bg-normal bg-cyan-intense)
     (bg-insert bg-green-intense)
     (bg-visual bg-yellow-intense)
     (bg-replace bg-red-intense)
     (bg-operator bg-blue-intense)
     (bg-emacs bg-magenta-intense)
     (fg-normal fg-main)
     (fg-insert fg-main)
     (fg-visual fg-main)
     (fg-replace fg-main)
     (fg-operator fg-main)
     (fg-emacs fg-main)))
  (modus-themes-common-palette-overrides
   '((fringe unspecified)
     (cursor blue-cooler)
     (prose-done bg-added-fringe)
     (bg-tab-bar bg-main)
     (bg-tab-other bg-inactive)
     (bg-tab-current bg-paren-expression)
     (bg-region bg-cyan-subtle)
     (bg-mode-line-active bg-sage)
     (fg-mode-line-inactive fg-dim)
     (bg-mode-line-inactive bg-main)
     (fg-heading-0 fg-main)
     (fg-heading-1 cyan-cooler)
     ;; (overline-heading-1 bg-cyan-subtle)
     (overline-heading-2 bg-yellow-subtle)
     (overline-heading-3 bg-blue-subtle)
     (overline-heading-4 bg-magenta-subtle)
     (overline-heading-5 bg-green-subtle)
     (overline-heading-6 bg-red-subtle)
     (overline-heading-7 bg-cyan-subtle)
     (overline-heading-8 fg-dim)))
  (modus-operandi-palette-overrides
   '((accent-2 "#0AA")
     (bg-normal bg-paren-match)
     (bg-insert bg-graph-green-0)
     (fg-completion-match-0 blue-intense)
     (fg-completion-match-1 magenta-intense)
     (fg-completion-match-2 green-intense)
     (fg-completion-match-3 yellow-intense)
     (overline-heading-2 bg-yellow-intense)
     (overline-heading-3 bg-blue-intense)
     (overline-heading-4 bg-magenta-intense)
     (overline-heading-5 bg-green-intense)
     (overline-heading-6 bg-red-intense)
     (overline-heading-7 bg-cyan-intense)))
  (modus-vivendi-palette-overrides
   '((bg-normal blue-intense)
     (bg-visual bg-green-intense)
     (bg-emacs magenta-intense)
     (bg-term-red "#e53c3c")
     (bg-term-green "#33bc33")
     (bg-term-yellow "#fec43f")
     (bg-term-blue "#2fafff")
     (bg-term-magenta "#c055c0")
     (bg-term-cyan "#00d3d0")
     (bg-term-white "#a6a6a6")
     (fg-term-red bg-term-red)
     (fg-term-green bg-term-green)
     (fg-term-yellow bg-term-yellow)
     (fg-term-blue bg-term-blue)
     (fg-term-magenta bg-term-magenta)
     (fg-term-cyan bg-term-cyan)
     (fg-term-white bg-term-white)
     (bg-term-black-bright "#595959")
     (bg-term-red-bright "#ff9095")
     (bg-term-green-bright "#80e080")
     (bg-term-yellow-bright "#ffef80")
     (bg-term-blue-bright "#79a8ff")
     (bg-term-magenta-bright "#ff88ff")
     (bg-term-cyan-bright "#6ae4b9")
     (bg-term-white-bright "#ffffff")
     (fg-term-black-bright bg-term-black-bright)
     (fg-term-red-bright bg-term-red-bright)
     (fg-term-green-bright bg-term-green-bright)
     (fg-term-yellow-bright bg-term-yellow-bright)
     (fg-term-blue-bright bg-term-blue-bright)
     (fg-term-magenta-bright bg-term-magenta-bright)
     (fg-term-cyan-bright bg-term-cyan-bright)
     (fg-term-white-bright bg-term-white-bright)))
  (modus-themes-headings '((1 . (variable-pitch 1.25 bold))
                           (2 . (variable-pitch 1.25 semibold))
                           (3 . (variable-pitch 1.20 semibold))
                           (4 . (variable-pitch 1.20 medium))
                           (5 . (variable-pitch 1.15 medium))
                           (6 . (variable-pitch 1.15 regular))
                           (7 . (variable-pitch 1.10 regular))
                           (8 . (variable-pitch 1.05 regular))))
  :config
  (load-theme 'modus-vivendi t))


;;;; Font and Faces
;;;; ======================================================================

;; TODO: Look at fontaine

(defvar my/font-height 135)
(defvar my/smaller-font-height 120)
(defvar my/mode-line-font-height 135)
(defvar my/tab-bar-font-height 110)
(defvar my/tab-line-font-height 110)
(defvar my/default-font "Iosevka")
(defvar my/variable-font "Noto Sans")
(defvar my/fixed-font "Iosevka Slab")
(defvar my/buffer-name-font "Roboto")
;; This has to be in points
(defvar my/which-key-font-pts 14)

;; Useful for including in inherit lists
(defface normal '((t (:weight normal))) "Normal weight")
(defface light '((t (:weight light))) "Light weight")
(defface medium '((t (:weight medium))) "Medium weight")
(defface smaller '((t (:height 0.8))) "Smaller height")

(defun my/blend-color (from to delta)
  (apply #'color-rgb-to-hex
         (color-blend (color-name-to-rgb to)
                      (color-name-to-rgb from)
                      delta)))

(defun my/update-faces-for-theme (&rest _)
  (modus-themes-with-colors
    (custom-set-faces
     `(default ((t (:family ,my/default-font :height ,my/font-height))) t)
     `(fixed-pitch-sans ((t (:family ,my/default-font))) t)
     `(fixed-pitch ((t (:family ,my/fixed-font))) t)
     `(fixed-pitch-serif ((t (:family ,my/fixed-font))) t)
     `(variable-pitch ((t (:family ,my/variable-font))) t)
     `(mode-line ((t (:family ,my/variable-font
                      :height ,my/mode-line-font-height)))
                 t)
     `(mode-line-active ((t (:family ,my/variable-font
                             :height ,my/mode-line-font-height)))
                        t)
     `(mode-line-inactive ((t (:family ,my/variable-font
                               :height ,my/mode-line-font-height
                               :background ,(my/blend-color bg-dim bg-main 0.25))))
                          t)
     `(mode-line-buffer-id ((t (:family ,my/buffer-name-font
                                :height ,my/mode-line-font-height))))
     ;; Custom faces for evil-state in the mode line
     `(state-normal ((t (:inherit (medium fixed-pitch)
                         :height ,(- my/mode-line-font-height 5)
                         :background ,bg-normal :foreground ,fg-normal)))
                    t)
     `(state-insert ((t (:inherit (medium fixed-pitch)
                         :height ,(- my/mode-line-font-height 5)
                         :background ,bg-insert :foreground ,fg-insert)))
                    t)
     `(state-visual ((t (:inherit (medium fixed-pitch)
                         :height ,(- my/mode-line-font-height 5)
                         :background ,bg-visual :foreground ,fg-visual)))
                    t)
     `(state-replace ((t (:inherit (medium fixed-pitch)
                          :height ,(- my/mode-line-font-height 5)
                          :background ,bg-replace :foreground ,fg-replace)))
                     t)
     `(state-operator ((t (:inherit (medium fixed-pitch)
                           :height ,(- my/mode-line-font-height 5)
                           :background ,bg-operator :foreground ,fg-operator)))
                      t)
     `(state-emacs ((t (:inherit (medium fixed-pitch)
                        :height ,(- my/mode-line-font-height 5)
                        :background ,bg-emacs :foreground ,fg-emacs)))
                   t)
     `(state-other ((t (:inherit (fixed-pitch error)))) t)
     `(tab-bar ((t (:family ,my/buffer-name-font
                    :overline ,bg-main
                    :height ,my/tab-bar-font-height)))
               t)
     `(tab-bar-tab ((t (:background ,bg-hl-line))) t)
     `(tab-bar-tab-inactive ((t (:foreground ,bg-active
                                 :underline (:position 0 :color ,bg-dim))))
                            t)
     `(tab-line ((t (:family ,my/buffer-name-font
                     :background ,(my/blend-color bg-main bg-dim 0.5)
                     :underline (:position 0 :color ,bg-main)
                     :height ,my/tab-line-font-height)))
                t)
     `(header-line ((t (:underline (:position 0 :color ,bg-inactive)
                        :height 0.9)))
                   t)
     `(org-dim-face ((t (:foreground ,(my/blend-color fg-main bg-main 0.2)))) t)
     `(org-note-face ((t (:family ,my/default-font
                          :foreground ,fg-dim
                          :weight normal
                          :height 0.9)))
                     t)
     `(org-idea-face ((t (:family ,my/default-font
                          :foreground ,blue-faint
                          :weight normal
                          :height 0.9
                          )))
                     t)
     )))

(my/update-faces-for-theme)

(add-hook 'enable-theme-functions 'my/update-faces-for-theme)
(add-hook 'server-after-make-frame-hook 'my/update-faces-for-theme)

(use-package spacious-padding :ensure t :demand t
  :custom (spacious-padding-widths
           '(:internal-border-width 8
             :header-line-width 4
             :mode-line-width 6
             :tab-line-width 5
             :tab-bar-width 0
             :right-divider-width 8
             :scroll-bar-width 0
             :fringe-width 0))
  :config
  ;; I'm not sure why this isn't already an option. Quick hack to add
  ;; bottom-divider-width too. It'd probably be better to use override advice
  ;; but I'm not sure I want to keep this in the first place.
  (defvar spacious-padding--bottom-divider-width nil)
  (spacious-padding--define-get-frame-param "bottom-divider-width" 8)
  ;; Copied from spacious-padding.el, just added bottom-divider-width
  (defun spacious-padding-modify-frame-parameters (&optional reset)
    (modify-all-frames-parameters
     `((internal-border-width . ,(spacious-padding--get-internal-border-width reset))
       (right-divider-width . ,(spacious-padding--get-right-divider-width reset))
       (bottom-divider-width . ,(spacious-padding--get-bottom-divider-width reset))
       (left-fringe . ,(or (spacious-padding--get-left-fringe-width reset)
                           (spacious-padding--get-fringe-width reset)))
       (right-fringe . ,(or (spacious-padding--get-right-fringe-width reset)
                            (spacious-padding--get-fringe-width reset)))
       (scroll-bar-width  . ,(spacious-padding--get-scroll-bar-width reset)))))
  (spacious-padding-mode))


;;;; General settings
;;;; ======================================================================

(setq-default line-spacing nil)
(setq-default truncate-lines nil)
(setq large-file-warning-threshold nil)
(setq fast-but-imprecise-scrolling nil)
(setq disabled-command-function nil)
(setq display-raw-bytes-as-hex t)
;; (setq scroll-conservatively 101)
(setq idle-update-delay 0.1)
(setq mouse-1-click-follows-link t)
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-progressive-speed nil)
(setq pixel-scroll-precision-interpolation-factor 1.0)
(setq mouse-wheel-scroll-amount '(0.05
                                  ((shift) . 0.9)
                                  ((control meta) . global-text-scale)
                                  ((control) . text-scale)
                                  ((meta) . hscroll)))
(setq scroll-bar-adjust-thumb-portion nil)
(setq window-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
(setq next-screen-context-lines 1)
(setq open-paren-in-column-0-is-defun-start nil)
(setq jit-lock-chunk-size 4096)
(setq jit-lock-antiblink-grace 1)
(setq shell-kill-buffer-on-exit t)
(setq eshell-kill-on-exit t)
(setq eshell-scroll-to-bottom-on-input 'this)
(setq view-inhibit-help-message t)
(setq compilation-scroll-output t)
(setq c-ts-mode-indent-style 'k&r)
(setq c-ts-mode-indent-offset 4)
(setq c-default-style '((c-mode . "stroustrup")
                        (c++-mode . "stroustrup")
                        (java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))
(setq-default tab-width 8)

;; This is added by minimal-emacs.d
(remove-hook 'after-init-hook 'window-divider-mode)


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

(defvar my/cursor-colors-normal '("#7F7F7F"))
(defvar my/cursor-colors-emacs '("#FF0FF"))
(defvar my/cached-cursor-theme nil)

(defun my/update-cursor-colors ()
  (require 'color)
  (let ((bg (color-name-to-rgb (modus-themes-get-color-value 'bg-dim t)))
        (cursor (color-name-to-rgb (modus-themes-get-color-value 'cursor t)))
        (emacs-cursor '(1.0 0.0 1.0))
        (steps '(1.0 0.75 0.50 0.25 0.0 0.25 0.50 0.75 1.0)))
    (setq my/cursor-colors-normal
          (--map (apply #'color-rgb-to-hex
                        `(,@(color-blend cursor bg it) 2))
                 steps))
    (setq my/cursor-colors-emacs
          (--map (apply #'color-rgb-to-hex
                        `(,@(color-blend emacs-cursor bg it) 2))
                 steps))))

(defun my/get-cursor-colors ()
  (when (not (eq (car custom-enabled-themes) my/cached-cursor-theme))
    (my/update-cursor-colors)
    (setq my/cached-cursor-theme (car custom-enabled-themes)))
  (if (and (boundp 'evil-state) (eq evil-state 'emacs))
      my/cursor-colors-emacs
    my/cursor-colors-normal))

(defvar my/cursor-color-idx 0)
(defun my/get-cursor-color () (nth my/cursor-color-idx (my/get-cursor-colors)))

(defvar my/underline-ov nil)

(defun my/cursor-delete-overlay (&rest _)
  (when (overlayp my/underline-ov)
    (delete-overlay my/underline-ov)
    (setq my/underline-ov nil)))

(defun my/update-underline-color ()
  (when (overlayp my/underline-ov)
    (overlay-put my/underline-ov 'face
                 `(:underline (:color ,(my/get-cursor-color) :position 0)))
    (overlay-put my/underline-ov 'window (selected-window))))

(defun my/cursor-over-image? ()
  (or (eq 'image (car-safe (get-text-property (point) 'display)))
      (--any? (eq 'image (car-safe (overlay-get it 'display)))
              (overlays-at (point)))))

(defun my/cursor-should-underline? ()
  (and (not (my/cursor-over-image?))
       blink-cursor-mode))

(defun my/update-cursor-overlay (&rest _)
  ;; Diminished cursor, usually reading a document or something
  (unless blink-cursor-mode
    (set-face-attribute 'cursor nil
                        :background (nth 3 (my/get-cursor-colors))))
  (or (ignore-errors
        (if (or (not (eq evil-state 'normal))
                (not (my/cursor-should-underline?)))
            (my/cursor-delete-overlay)
          (unless (overlayp my/underline-ov)
            (setq my/underline-ov (make-overlay (point) (point))))
          (let ((start (point)))
            (save-excursion
              (forward-char)
              (move-overlay my/underline-ov start (point) (current-buffer))
              (my/update-underline-color))))
        t)
      ;; If we had any errors, just delete the overlay entirely to try again
      ;; next time.
      (my/cursor-delete-overlay)))

(add-hook 'post-command-hook 'my/update-cursor-overlay)

(defun my/cursor-color-reset (&rest _)
  (setq my/cursor-color-idx 0)
  (setq blink-cursor-blinks 0)
  (set-face-attribute 'cursor nil :background (my/get-cursor-color))
  (my/update-underline-color))

(defun my/cursor-start-blink (&rest _)
  ;; The animation looks really weird when the cursor is on an image
  (if (my/cursor-over-image?)
       (progn (my/cursor-delete-overlay)
              (setq blink-cursor-blinks 1)
              (when blink-cursor-timer (cancel-timer blink-cursor-timer))
              (setq blink-cursor-timer nil)
              (blink-cursor-end))
     (my/cursor-color-reset)))

(defun my/cursor-color-advance (&rest _)
  (when (> blink-cursor-blinks-done 1) ; fixes flickering issue
   (setq my/cursor-color-idx (% (1+ my/cursor-color-idx)
                                (length (my/get-cursor-colors)))))
  (set-face-attribute 'cursor nil :background (my/get-cursor-color))
  (my/update-underline-color))

(advice-add 'blink-cursor-start :after 'my/cursor-start-blink)
(advice-add 'blink-cursor-end :before 'my/cursor-color-reset)
(advice-add 'blink-cursor-timer-function :before 'my/cursor-color-advance)
(advice-add 'what-cursor-position :before 'my/cursor-delete-overlay)
(advice-add 'describe-char :before 'my/cursor-delete-overlay)
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
(add-hook 'after-init-hook 'undelete-frame-mode)
;; Even though minimal-emacs.d tries to add this, (display-graphic-p) will
;; return nil when running as a daemon, preventing it from adding the hook.
(add-hook 'after-init-hook 'context-menu-mode)
;; TODO: remove some of these symbols, like "or", and re-enable
;; (add-hook 'prog-mode-hook 'global-prettify-symbols-mode)
(add-hook 'after-init-hook 'global-tab-line-mode)

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
  "Add this as a hook to modes that should not have a modeline. Leaves a tiny
line so that it still acts as a grabbable window divider."
  (dolist (face '(mode-line-active mode-line-inactive))
    (face-remap-add-relative face '(:height 10
                                    :box nil
                                    :foreground unspecified
                                    :background unspecified
                                    :inherit thin-mode-line)))
  (setq-local mode-line-format
              (propertize " "
                          'face '(:underline (:position 0))
                          'display '(space :width 4096))))

(defun my/no-fringes (&rest _)
  "Add this as a hook to buffers that should not show fringes"
  (setq-local left-fringe-width 1
              right-fringe-width 1))

(defun my/larger-left-fringe (&rest _)
  "Add this as a hook to buffers that need more left fringe space (clickable
folding elements, etc.)"
  (setq-local left-fringe-width 16))

(defun my/show-trailing-whitespace (&rest _)
  "Add this as a hook to show trailing whitespace in a buffer"
  (setq-local show-trailing-whitespace t))

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
  (face-remap-add-relative 'default `(:height ,my/smaller-font-height :weight normal))
  (face-remap-add-relative 'variable-pitch `(:height ,my/smaller-font-height))
  (face-remap-add-relative 'fixed-pitch `(:height ,my/smaller-font-height))
  (face-remap-add-relative 'fixed-pitch-serif `(:height ,my/smaller-font-height))
  (face-remap-add-relative 'header-line `(:height ,(- my/smaller-font-height 10))))

(defun my/font-weight (&rest _)
  "Add this as a hook to buffers that should have slightly heavier default
font weight"
  (face-remap-add-relative 'default '(:weight medium)))

(defun my/line-spacing (&rest _)
  "Add this as a hook to buffers that should have extra line spacing"
  (setq-local line-spacing 2))

(defun my/prog-word-syntax (&rest _)
  "Make _ behave as part of a word, not punctuation."
  (modify-syntax-entry ?_ "w"))

(defun my/lisp-word-syntax (&rest _)
  "Make - behave as part of a word, not punctuation."
  (modify-syntax-entry ?- "w"))

;; This helps with cases like (list "*" " " " *")
(defun my/string-underline (&rest _)
  "Add a faint underline to string literals to make it easier to tell them
apart in languages that only use whitespace to separate list elements."
  (face-remap-add-relative 'font-lock-string-face
                           '(:inherit string-underline)))

;; FIXME: I thought that this was breaking `describe-variable' sometimes,
;; but I think it was something else. Remove this comment if it's fixed.
(defun my/temp-buffer-view-mode (&rest args)
  "Make pretty-printed buffers use view-mode, be read-only, and easily
closable. I don't know what else uses with-output-to-temp-buffer so I'm
matching against the buffer name for now."
  (when (string-prefix-p "*Pp" (buffer-name))
    (view-mode-enter nil 'kill-buffer-if-not-modified)))
(add-hook 'temp-buffer-show-hook 'my/temp-buffer-view-mode)

;;;; Other commands and functions that need early definitions
;;;; ======================================================================

(defun my/side-window? (&optional win)
  (let ((win (or win (selected-window))))
    (window-parameter win 'window-side)))

(defun my/main-window? (&optional win)
  (not (my/side-window? win)))

;; It's hard to exclude matches with a regexp, so these include an explicit
;; check for *scratch* so that we treat it like a non-special buffer. Otherwise
;; switch-to-prev-buffer-skip-regexp could have done the job.
(defun my/match-special-buffers (buf &rest _)
  (let* ((buf (get-buffer buf)) ;; Ensure we have a buffer and not a name
         (buf-name (buffer-name buf)))
    (not (or (string= "*scratch*" buf-name)
             (string-prefix-p " " buf-name)
             (buffer-file-name buf)))))

;; I can't just invert match-special-buffers because both exclude hidden
(defun my/match-non-special-buffers (buf &rest _)
  (let* ((buf (get-buffer buf)) ;; Ensure we have a buffer and not a name
         (buf-name (buffer-name buf)))
    (and (or (string= "*scratch*" buf-name) (buffer-file-name buf))
         (not (string-prefix-p " " buf-name)))))

(defun my/switch-to-prev-buffer-skip (win target-buf bury-or-kill)
  (let ((side? (window-parameter win 'window-side)))
    (if side?
        (my/match-non-special-buffers target-buf)
      (my/match-special-buffers target-buf))))

(setq switch-to-prev-buffer-skip #'my/switch-to-prev-buffer-skip)


;; These are intended to be redefined to whatever actual buffer switching
;; functions I decide to use.
(defun my/switch-to-next-buffer ()
  (interactive)
  (if tab-line-mode
      (tab-line-switch-to-next-tab)
    (switch-to-next-buffer)))

(defun my/switch-to-prev-buffer ()
  (interactive)
  (if tab-line-mode
      (tab-line-switch-to-prev-tab)
    (switch-to-prev-buffer)))

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
  (evil-respect-visual-line-mode t)
  (evil-move-cursor-back nil)
  (evil-split-window-below t)
  (evil-want-C-w-delete nil)
  (evil-cross-lines t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  :custom-face
  (evil-ex-substitute-matches
   ((t (:box (:line-width (3 . -3) :style pressed-button)))))
  (evil-ex-substitute-replacement
   ((t (:box (:line-width (3 . -3) :style released-button)))))
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
                'inferior-python-mode 'vterm-mode)
  (setq evil-insert-state-modes
        (seq-remove (lambda (x) (memq x evil-emacs-state-modes))
                    evil-insert-state-modes))
  (add-hook 'evil-normal-state-entry-hook 'my/cursor-color-reset)
  (add-hook 'evil-emacs-state-entry-hook 'my/cursor-color-reset)
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; Evil doesn't give an option to hide the previous search term.
  ;; Always show a blank prompt when performing a search.
  (defun my/hide-prev-search (args)
    (if (string-prefix-p "evil-ex-search" (symbol-name this-command))
        `(,(car args) "" ,@(cddr args))
      args))
  (defun my/ensure-normal-or-emacs-state (buf)
    (let ((state (buffer-local-value 'evil-state buf)))
      (unless (member state '(normal emacs))
        (with-current-buffer buf (evil-normal-state)))))
  (defun my/normal-state-on-focus-change (&rest _)
    (let* ((old-win (old-selected-window))
           (old-buffer (and old-win (window-buffer old-win))))
      (when (buffer-live-p old-buffer)
        (my/ensure-normal-or-emacs-state old-buffer))))
  ;; TODO: Figure out a way to handle tab-line changes
  (defun my/normal-state-on-tab-change (prev-tab &rest _)
    ;; Instead of trying to parse the window state ourselves, just ensure
    ;; normal or emacs state on every buffer that was in the previous tab
    (let* ((ws (and prev-tab (alist-get 'ws prev-tab)))
           (bufs (and ws (mapcar 'get-buffer (window-state-buffers ws)))))
      (mapc 'my/ensure-normal-or-emacs-state (seq-filter 'buffer-live-p bufs))))
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
  (add-hook 'window-selection-change-functions
            'my/normal-state-on-focus-change)
  (add-hook 'tab-bar-tab-post-select-functions
            'my/normal-state-on-tab-change)
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
  :general-config
  ('(insert emacs)
   "C-w" 'evil-window-map
   "C-S-w" 'evil-delete-backward-word)
  ('(normal visual) 'prog-mode-map "gc" 'my/evil-comment-or-uncomment)
  ('evil-window-map
   "C-=" 'fit-window-to-buffer
   ;; It's too easy to get "only" "other" and "previous" mixed up. "Other"
   ;; seems to be the one I default to.
   "o" 'evil-window-mru
   ;; Sometimes evil-quit closes a frame when I expect it to close a side
   ;; window. Remapping this to try and avoid that.
   "q" 'evil-window-delete
   "C-q" 'evil-window-delete
   ;; Force opening stuff in the MRU window
   "O" 'other-window-prefix
   )
  ('insert 'prog-mode-map "<tab>" 'indent-for-tab-command)
  ('visual "<tab>" 'evil-shift-right
           "<backtab>" 'evil-shift-left)
  ('normal "<tab>" 'evil-shift-right-line
           "<backtab>" 'evil-shift-left-line))

(use-package evil-collection :ensure t
  :after evil
  :custom
  (evil-collection-key-blacklist '("C-j" "C-k"))
  (evil-collection-magit-use-z-for-folds t)
  (evil-collection-magit-section-use-z-for-folds t)
  (evil-collection-magit-want-horizontal-movement t)
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
  :custom-face
  (anzu-mode-line ((t (:foreground unspecified :inherit bold))))
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
  :commands magit
  :custom (magit-display-buffer-function 'display-buffer)
  :config
  (add-hook 'magit-mode-hook 'my/larger-left-fringe))

;; NOTE: C-q will quote the next input, so you can send ESC with C-q ESC
(use-package eat :ensure t
  :commands eat
  :custom (eat-kill-buffer-on-exit t)
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode))
  :config
  (defun my/eat-C-w () (interactive) (eat-self-input 1 23)) ; ASCII code for C-w
  :general-config
  ('(normal insert emacs) '(eat-mode-map eshell-mode-map)
   "C-S-w" 'my/eat-C-w
   "C-c P" 'eat-send-password
   "C-j" 'my/switch-to-next-buffer
   "C-k" 'my/switch-to-prev-buffer))


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
    ;; vertico--resize-window changed names recently. This can be removed
    ;; once I don't need compatibility with the old version.
    (when (fboundp 'vertico--resize-window) (vertico--resize-window (length lines)))
    (when (fboundp 'vertico--resize) (vertico--resize)))
  (defun my/crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add 'completing-read-multiple :filter-args 'my/crm-indicator)
  ;; Versions that wrap around the end of the list, assuming `vertico-cycle'
  (defun my/vertico-scroll-down (&optional n)
    (interactive "p")
    (let ((idx vertico--index))
      (vertico-scroll-down n)
      (when (eq idx vertico--index) (vertico-previous))))
  (defun my/vertico-scroll-up (&optional n)
    (interactive "p")
    (let ((idx vertico--index))
      (vertico-scroll-up n)
      (when (eq idx vertico--index) (vertico-next))))
  :general-config
  ('minibuffer-mode-map
   "<tab>" 'completion-at-point
   "TAB" 'completion-at-point)
  ('vertico-map
   "<next>" 'my/vertico-scroll-up
   "<prior>" 'my/vertico-scroll-down
   ;; Not sure which of these I'll use more, putting both for now
   "C-S-n" 'my/vertico-scroll-up
   "C-S-p" 'my/vertico-scroll-down
   "C-S-f" 'my/vertico-scroll-up
   "C-S-b" 'my/vertico-scroll-down
   ;; Complete up to prefix
   "<tab>" 'minibuffer-complete
   "TAB" 'minibuffer-complete
   "C-SPC" 'vertico-insert
   ;; Send typed input even if it doesn't match any candidates
   "C-<return>" 'vertico-exit-input
   "C-RET" 'vertico-exit-input
   ;; This one works in the terminal
   "C-c RET" 'vertico-exit-input))

(use-package orderless :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (eglot (styles orderless))
                                       (eglot-capf (styles orderless))))
  ;; :custom-face
  ;; (orderless-match-face-0 ((t (:inherit (underline blue-fg)))))
  ;; (orderless-match-face-1 ((t (:inherit (underline magenta-fg)))))
  ;; (orderless-match-face-2 ((t (:inherit (underline green-fg)))))
  ;; (orderless-match-face-3 ((t (:inherit (underline yellow-fg)))))
  )

(use-package marginalia :ensure t :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (vertico-mode . marginalia-mode))

(use-package embark :ensure t :defer t
  :general ("S-<f8>" 'embark-select
            "<f8>" 'embark-act
            "C-<f8>" 'embark-dwim
            "C-h B" 'embark-bindings)
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult :ensure t :demand t
  :general
  ("C-c M-x" 'consult-mode-command
   "C-c h" 'consult-history
   "C-c k" 'consult-kmacro
   "C-c m" 'consult-man
   "C-c i" 'consult-info
   [remap Info-search] 'consult-info
   ;; C-x bindings in `ctl-x-map'
   "C-x M-:" 'consult-complex-command
   "C-x b" 'consult-buffer-similar
   "C-x B" 'consult-buffer
   "C-x 4 b" 'consult-buffer-other-window
   "C-x 5 b" 'consult-buffer-other-frame
   "C-x t b" 'consult-buffer-other-tab
   "C-x r b" 'consult-bookmark
   "C-x p b" 'consult-project-buffer
   ;; Custom M-# bindings for fast register access
   "M-#" 'consult-register-load
   "M-'" 'consult-register-store
   "C-M-#" 'consult-register
   ;; Other custom bindings
   "M-y" 'consult-yank-pop
   ;; M-g bindings in `goto-map'
   "M-g e" 'consult-compile-error
   "M-g f" 'consult-flymake
   "M-g g" 'consult-goto-line
   "M-g M-g" 'consult-goto-line
   "M-g o" 'consult-outline
   "M-g m" 'consult-mark
   "M-g k" 'consult-global-mark
   "M-g i" 'consult-imenu
   "M-g I" 'consult-imenu-multi
   ;; M-s bindings in `search-map'
   "M-s d" (or (and (executable-find "fd") 'consult-fd)
               (and (executable-find "fdfind") 'consult-fd)
               'consult-find)
   "M-s c" 'consult-locate
   "M-s g" 'consult-grep
   "M-s G" 'consult-git-grep
   "M-s r" 'consult-ripgrep
   "M-s l" 'consult-line
   "M-s L" 'consult-line-multi
   "M-s k" 'consult-keep-lines
   "M-s f" 'consult-focus-lines
   ;; Isearch integration
   "M-s e" 'consult-isearch-history)
  ('isearch-mode-map
   "M-e" 'consult-isearch-history
   "M-s e" 'consult-isearch-history
   "M-s l" 'consult-line
   "M-s L" 'consult-line-multi)
  ('minibuffer-local-map
   "M-s" 'consult-history
   "M-r" 'consult-history)

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
  (defvar my/consult--source-buffer-side-window
    `(:name "Side Window Buffer"
      :narrow ?b
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :default t
      :items
      ,(lambda ()
         (consult--buffer-query
          :sort 'visibility
          :as #'consult--buffer-pair
          :predicate 'my/match-special-buffers))))
  (defvar my/consult--source-buffer-main-window
    `(:name "Main Window Buffer"
      :narrow ?b
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :default t
      :items
      ,(lambda ()
         (consult--buffer-query
          :sort 'visibility
          :as #'consult--buffer-pair
          :predicate 'my/match-non-special-buffers))))
  (defun consult-buffer-similar (&optional arg)
    "`consult-buffer' that only shows main window buffers when called from a
main window, side window buffers when called from a side window. With prefix,
show all buffers."
    (interactive "P")
    (if arg
        (consult-buffer '(consult--source-buffer))
      (if (my/main-window?)
          (consult-buffer '(my/consult--source-buffer-main-window))
        (consult-buffer '(my/consult--source-buffer-side-window)))))

  (keymap-global-set "C-x b" 'consult-buffer-similar)

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package corfu :ensure t
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :general
  ('emacs
   "C-S-SPC" 'set-mark-command)
  ('(insert emacs)
   "C-SPC" 'completion-at-point)        ; for when tab isn't usable
  ('(insert emacs) 'corfu-map
   "<prior>" 'corfu-scroll-down
   "<next>" 'corfu-scroll-up
   "<tab>" 'corfu-expand
   "TAB" 'corfu-expand
   "C-SPC" 'corfu-complete ;; Fill selected completion
   "<return>" 'corfu-send
   "C-n" 'corfu-next
   "C-p" 'corfu-previous)
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
  (tab-always-indent 'complete))

;; Note: this _might_ be conflicting with popupinfo in the GUI, needs testing
(use-package corfu-terminal :ensure t
  :config (corfu-terminal-mode))

(use-package cape :ensure t :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (general-add-hook 'completion-at-point-functions '( cape-dabbrev cape-file))
  :config
  ;; TODO: Look at corfu wiki for example merging elisp cap with dabbrev

  ;; Fix the issue where completion doesn't show all of the candidates
  (advice-add 'eglot-completion-at-point :around 'cape-wrap-buster))

(use-package form-feed-st :ensure t
  :hook
  (after-init . global-form-feed-st-mode)
  (server-after-make-frame-hook . global-form-feed-st-mode)
  :custom (form-feed-st-include-modes
           '(prog-mode text-mode compilation-mode))
  ;; :custom-face
  ;; ;; strike-through was giving a weird artifact, but this works pretty well
  ;; (form-feed-st-line ((t (:strike-through unspecified
  ;;                         :inherit form-feed-line-normal))))
  )


(use-package highlight-parentheses :ensure t
  :hook
  (minibuffer-setup . highlight-parentheses-minibuffer-setup)
  (lisp-mode . highlight-parentheses-mode)
  (lisp-data-mode . highlight-parentheses-mode)
  (fennel-mode . highlight-parentheses-mode)
  :custom
  (highlight-parentheses-delay 0.05)
  (highlight-parentheses-colors '(unspecified unspecified unspecified unspecified))
  (highlight-parentheses-background-colors '(unspecified unspecified unspecified unspecified))
  (highlight-parentheses-attributes '((:inherit ((:weight black) font-lock-punctuation-face))
                                      (:inherit ((:weight black) font-lock-punctuation-face))
                                      (:inherit ((:weight black) font-lock-punctuation-face))
                                      (:inherit ((:weight black) font-lock-punctuation-face)))))

;; Configured to emphasize the outermost matched pair and alternating pairs
(use-package rainbow-delimiters :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom
  (show-paren-delay 0.05)
  (rainbow-delimiters-max-face-count 3)
  (rainbow-delimiters-outermost-only-face-count 1)
  :custom-face
  (show-paren-match ((t (:weight black
                         :foreground "orange red"
                         :background unspecified))))
  (rainbow-delimiters-depth-1-face ((t (:foreground unspecified
                                        :background unspecified
                                        :weight medium
                                        :inherit nil))))
  (rainbow-delimiters-depth-2-face ((t (:foreground unspecified
                                        :background unspecified
                                        :weight light
                                        :inherit nil))))
  (rainbow-delimiters-depth-3-face ((t (:foreground unspecified
                                        :background unspecified
                                        :weight extralight
                                        :inherit nil)))))

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

(use-package adaptive-wrap :ensure t
  :hook (prog-mode . adaptive-wrap-prefix-mode)
  :custom (adaptive-wrap-extra-indent 1))

(use-package expand-region :ensure t
  :custom (expand-region-contract-fast-key "V")
  :general-config
  ('visual "v" 'er/expand-region))

(use-package lua-mode :ensure t :mode "\\.lua\\'")
(use-package vimrc-mode :ensure t :mode "[._]?g?vim\\(rc\\)?\\'")
(use-package fennel-mode :ensure t :mode "\\.fnl\\'")

(use-package slime :ensure t
  :config (setq inferior-lisp-program "sbcl"))

(use-package pdf-tools :ensure t :defer t
  :if (not (eq system-type 'windows-nt))
  :commands pdf-view-mode
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :config (pdf-loader-install))

(use-package org-superstar :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-special-todo-items nil)
  (org-superstar-cycle-headline-bullets nil)
  ;; Keeping bullet options here for quick reference:
  ;; Diamonds: ◆⬗⬥⬩🞘🞗
  ;; Triangles: ▶▷⯈▸▹🞂►▻ 🢒‣
  ;; Triangle Arrows: ⮞➤➢➣
  ;; Squares: ▧▨■◼🞍⬝
  ;; Boxes: 🯦🯧▮▯▬▭𜸞
  ;; Lines: ╶ ╺ ╼ ╾
  ;; Circles: ⬤●⚫
  ;; Three-point stars: 🟂🟁🟃🟀
  ;; Four-point stars: 🟆✧🟇⯌⯎🟅🟄
  ;; Brackets: ⟫ 🭬🯛🮥 ⟩ 🯟❱❯❭
  ;; Arrows: ➜ ⟶ ⟹ ➡ ⮕ ➞ ➝ 🠞🠚🠖 🠊🠆🠂 🡒 🡆🠺 🢂🡺🡲🡪 🢥 🢧
  ;; Others: ⁍ ⁌ ❫ ❩ ⏩ ⧽ › » 𜱺 𜲂 🠶
  (org-superstar-item-bullet-alist '((42 . "⸰") (43 . "‣") (45 . "━"))) ; *+-
  (org-superstar-headline-bullets-list '("◆" "⬗" "⬥" "⬩")))

(use-package org-variable-pitch :ensure t
  :config
  (general-add-hook '(after-init-hook server-after-make-frame-hook)
                    'org-variable-pitch-setup))

(use-package org-autolist :ensure t
  :hook (org-mode . org-autolist-mode))


;; TODO: Also try paredit + enhanced-evil-paredit

(use-package smartparens :ensure t
  :hook
  (prog-mode . smartparens-mode)
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-delete-blank-sexps t)
  :config
  (require 'smartparens-config)
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
  (minions-mode-line-lighter "=")
  (minions-mode-line-delimiters '(" " . " ")))

(use-package list-unicode-display :ensure t)

(use-package nov :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(use-package info-colors :ensure t
  :hook (Info-selection . info-colors-fontify-node))

(use-package markdown-mode :ensure t
  :custom
  (markdown-display-remote-images t)
  (markdown-enable-highlighting-syntax t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-markup t)
  (markdown-enable-wiki-links t)
  (markdown-list-item-bullets '("•" "‣" "‧" "╴")))

(use-package devdocs :ensure t
  :commands devdocs-lookup
  :config
  (defun devdocs-install-all ()
    "Doesn't actually install _everything_, just the stuff I care about"
    (interactive)
    (mapc 'devdocs-install
          '("bash" "c" "cpp" "clojure~1.11" "cmake" "css" "docker" "dom"
            "elisp" "gcc~13" "gcc~13_cpp" "gnu_make" "godot~4.2" "haskell~9"
            "html" "http" "javascript" "jq" "man" "markdown" "lua~5.3" "love"
            "numpy" "ocaml" "octave~9" "opengl~4" "opengl~2.1" "pygame"
            "python~3.12" "pytorch~22" "qt" "qt~5.15" "rust" "sqlite" "svg"
            "tcl_tk" "zig")))
  :bind ("C-h D" . devdocs-lookup))

(use-package treesit-auto :ensure t
  :hook
  (after-init . global-treesit-auto-mode)
  (server-after-make-frame . global-treesit-auto-mode)
  :custom (treesit-auto-install 'prompt)
  :config
  ;; The package author doesn't seem to update often. Patch or remove broken recipes.
  (defun my/find-treesit-auto-recipe (lang)
    (car (seq-filter (lambda (x) (eq (treesit-auto-recipe-lang x) lang))
                     treesit-auto-recipe-list)))
  (defun my/symcat (a b) (intern (concat (symbol-name a) (symbol-name b))))
  (defun my/patch-treesit-auto-recipe (lang field val)
    (when-let* ((recipe (my/find-treesit-auto-recipe lang)))
      (let ((fn-sym (my/symcat 'treesit-auto-recipe- field)))
        (eval `(setf (,fn-sym ,recipe) ,val)))))
  ;; Janet has the wrong name
  (my/patch-treesit-auto-recipe 'janet 'lang (quote 'janet-simple))
  ;; C++ is broken unless we get this specific revision
  (my/patch-treesit-auto-recipe 'cpp 'revision "v0.22.0")
  ;; Update list of languages with patched changes
  (setq treesit-auto-langs (seq-map #'treesit-auto-recipe-lang
                                    treesit-auto-recipe-list))
  ;; Now that the correct language names are in the list, we can filter out
  ;; ones that are still broken.
  (defconst broken-treesit-auto '(markdown latex c-sharp lua))
  (setq treesit-auto-langs
        (seq-difference (mapcar #'treesit-auto-recipe-lang
                                treesit-auto-recipe-list)
                        broken-treesit-auto)))

;; Note: install fd for faster file operations (package is named "fd-find" in apt/dnf)
(use-package projectile :ensure t :demand t
  :custom
  (consult-project-function 'projectile-project-root)
  (projectile-tags-file-name ".tags")
  :config (projectile-mode)
  :general
  ('projectile-mode-map "C-c p" 'projectile-command-map))

;; WIP: Try this out, figure out buffer switching
(use-package consult-projectile :ensure t)

;; TODO: Figure out isolating visible buffers / etc on a per-project basis
;; (use-package perspective :ensure t)

(unless (version< emacs-version "30")
  (use-package which-key-posframe :ensure t
    :commands which-key-posframe-mode
    :custom
    (which-key-posframe-font (format "%s %s" my/fixed-font my/which-key-font-pts))
    (which-key-posframe-border-width 1)
    (which-key-posframe-parameters '((left-fringe . 0)
                                     (right-fringe . 0)))
    (which-key-posframe-poshandler
     'posframe-poshandler-point-bottom-left-corner)))

;; (use-package bufler
;;   :vc (:url "https://github.com/alphapapa/bufler.el")
;;   :custom (bufler-columns '("Name" "Path")))


;;;; Built-in Packages
;;;; ======================================================================

(use-package eshell :ensure nil
  :custom (eshell-destroy-buffer-when-process-dies t)
  :config (when (require 'eat nil :noerror)
            (setq eshell-visual-commands '()))
  :general-config
  ('(insert emacs) 'eshell-mode-map
   "<tab>" 'completion-at-point
   "C-S-w" 'evil-delete-backward-word
   "C-p" 'eshell-previous-matching-input-from-input
   "C-n" 'eshell-next-matching-input-from-input))

(use-package comint :ensure nil
  :general-config
  ('comint-mode-map
   "C-j" 'my/switch-to-next-buffer
   "C-k" 'my/switch-to-prev-buffer))

(use-package winner-mode :ensure nil
  :custom (winner-dont-bind-my-keys t)
  :hook (after-init . winner-mode)
  :general ('evil-window-map ; C-w prefix
            "u" 'winner-undo
            "C-r" 'winner-redo))

(use-package custom :ensure nil
  :custom
  (custom-search-field nil)
  (custom-buffer-done-kill t))

;; TODO: Set up which-key-replacement-alist and which-key-special-keys
(unless (version< emacs-version "30")
  (use-package which-key :ensure nil
    :custom
    (which-key-dont-use-unicode nil)
    (which-key-idle-secondary-delay 0.1)
    (which-key-paging-key "<f1>")
    (which-key-show-operator-state-maps t)
    (which-key-min-column-description-width 32)
    (which-key-max-description-length 48)
    (which-key-max-display-columns 4)
    (which-key-echo-keystrikes 0.1)
    :config
    (setq which-key-idle-delay 0.5)
    (which-key-mode)
    (which-key-posframe-mode)
    (defun which-key-posframe--max-dimensions (_)
      ;; The default doesn't take font size into account, scale appropriately.
      ;; These values are total guesses but seem to work ok.
      (let ((approx-char-w (* my/which-key-font-pts 0.8))
            (approx-char-h (* my/which-key-font-pts 1.2)))
        (cons (- (truncate (frame-pixel-height) approx-char-h) 2)
              (truncate (frame-pixel-width) approx-char-w))))))

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
  :custom (doc-view-continuous t)
  :general
  ('normal 'doc-view-mode-map
           "C-j" 'my/switch-to-next-buffer
           "C-k" 'my/switch-to-prev-buffer))

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
    (when-let* ((str (read-string prompt nil history))
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
        (let* ((reg (rx (and (= 0 "user-content-")
                             (or "pagehead-actions" "HeaderMktg" "UnderlineNav-"
                                 "flash-warn" "TextInput-" "types__StyledButton"
                                 "form-control" "flash-full" "search"))))
               (nodes (dom-by-class document reg)))
          (dolist (child nodes)
            (dom-remove-node (dom-parent document child) child))))))
  (advice-add 'eww-display-document :before 'my/clean-github))

(use-package eldoc :ensure nil
  :init (defvar my/eldoc-help-message "")
  :custom
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-use-multiline-p 0.15)
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

(use-package eglot :ensure nil
  :custom (eglot-ignored-server-capabilities '(:inlayHintProvider)))

(use-package org :ensure nil
  :commands org-capture
  :bind (("C-c C-o c" . org-capture))
  :custom
  (org-cycle-level-faces nil)
  (org-pretty-entitites t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-adapt-indentation t)
  (org-startup-with-inline-images t)
  (org-ellipsis " ⤵")
  (org-cycle-separator-lines -1)
  (org-blank-before-new-entry '((heading . auto) (plain-list-item . nil)))
  (org-archive-default-command 'org-archive-to-archive-sibling)
  (org-archive-location "::* Archive")
  (org-todo-keywords '((sequence "TODO" "|" "DONE" "NOTE" "IDEA")))
  (org-todo-keyword-faces '(("TODO" . org-todo)
                            ("DONE" . org-done)
                            ("NOTE" . org-note-face)
                            ("IDEA" . org-idea-face)))
  :custom-face
  (org-done ((t (:height 0.9 :weight normal))))
  (org-headline-done ((t (:height 0.9 :inherit (normal org-dim-face)))))
  (org-archived ((t (:background unspecified))))
  :config
  (add-to-list 'org-modules 'org-mouse)
  (unless (file-exists-p org-directory) (make-directory org-directory t))
  (setq org-default-notes-file
        (concat (file-name-as-directory org-directory) "notes"))
  (defun my/org-update () (org-update-checkbox-count t))
  (defun my/org-mode-local-hooks ()
    (face-remap-add-relative 'variable-pitch :family my/buffer-name-font)
    (general-add-hook 'before-save-hook '( my/org-update) nil :local))
  (general-add-hook 'org-mode-hook '( auto-fill-mode
                                      my/org-mode-local-hooks))
  (defun my/org-extra-keywords ()
    (add-to-list 'org-font-lock-extra-keywords
                 `("^[ ]+\\([-+*][ ]\\|[0-9][.)][ ]\\)?\\(.*\\)"
                   (2 '(face org-dim-face)))))
  (add-hook 'org-font-lock-set-keywords-hook 'my/org-extra-keywords)
  :general-config
  ('(normal insert) 'org-mode-map
   "<backtab>" 'org-shifttab)
  ('normal 'org-mode-map
           "C-j" 'my/switch-to-next-buffer
           "C-k" 'my/switch-to-prev-buffer))

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

(use-package tab-bar :ensure nil :demand t
  :custom
  (tab-bar-format '(tab-bar-format-tabs
                    tab-bar-separator
                    tab-bar-format-add-tab))
  (tab-bar-close-button-show nil)
  (tab-bar-select-restore-windows nil)
  (tab-bar-show 1)
  (tab-bar-auto-width nil)
  :custom-face
  (tab-bar ((t (:weight normal))))
  (tab-bar-tab ((t (:weight normal :box nil))))
  (tab-bar-tab-inactive ((t (:weight normal :background unspecified :box nil))))
  :config
  (defun my/tab-bar-tab-name-format-spaces (name tab number)
    (let ((face (funcall tab-bar-tab-face-function tab)))
      (add-face-text-property 0 (length name) face t name)
      (with-work-buffer
        (setq-local face-remapping-alist
                    (list (cons 'default (list :inherit (list face 'tab-bar)))))
        (let* ((w (string-pixel-width name (current-buffer)))
               (remaining (- 200 w))
               (side (max 8 (truncate remaining 2))))
          (concat (propertize " " 'face face 'display `(space :width (,side)))
                  name
                  (propertize " " 'face face 'display `(space :width (,side))))))))
  (setopt tab-bar-tab-name-format-functions '(tab-bar-tab-name-format-hints
                                              tab-bar-tab-name-format-close-button
                                              my/tab-bar-tab-name-format-spaces))
  (setq tab-bar-separator (propertize " "
                                      'face 'default
                                      'display '(space :width (1) :height (16))))
  (tab-bar-mode t))

(use-package tab-line :ensure nil
  :init
  (defun my/tab-line-tab-name (buf &optional _)
    (concat "  " (buffer-name buf) "  "))
  ;; Based on this blog post for "modern" tab behavior, where closing the last
  ;; tab in a window will close the window and closing a tab will only kill a
  ;; buffer if it isn't displayed anywhere else.
  ;; https://andreyor.st/posts/2020-05-07-making-emacs-tabs-work-like-in-atom
  ;; This uses advice instead of changing tab-line-close-tab-function because
  ;; we already need to redo a lot of the work in tab-line-close-tab.
  (defun my/collect-tab-line-buffers ()
    (let ((fn (lambda (ls win)
                (select-window win :norecord)
                (cons (tab-line-tabs-window-buffers) ls))))
      (flatten-list (seq-reduce fn (window-list) nil))))
  ;; TODO: Make a version of this that can be bound to a key without having
  ;; to use the mouse
  (defun my/tab-line-close-tab (&optional e)
    (interactive "e")
    (let* ((pos (event-start e))
           (win (posn-window pos))
           (buf (get-pos-property 1 'tab (car (posn-string pos)))))
      (with-selected-window win
        (let ((tab-list (tab-line-tabs-window-buffers))
              (buf-list (my/collect-tab-line-buffers)))
          (select-window win)
          (if (>= 1 (--count (eq it buf) buf-list))
              (and (kill-buffer buf)
                   (unless (cdr tab-list)
                     (ignore-errors (delete-window win))))
            (if (eq buf (current-buffer))
                (bury-buffer)
              (set-window-prev-buffers win (assq-delete-all buf (window-prev-buffers)))
              (set-window-next-buffers win (delq buf (window-next-buffers))))
            (unless (cdr tab-list)
              (ignore-errors (delete-window win))))))
      (force-mode-line-update)))
  :custom
  (tab-line-close-button-show nil)
  (tab-line-new-button-show nil)
  (tab-line-tab-face-functions nil)
  (tab-line-tab-name-function 'my/tab-line-tab-name)
  ;; (tab-line-close-tab-function 'kill-buffer)
  :custom-face
  (tab-line-tab ((t (:box unspecified :inherit tab-line-tab-current))))
  (tab-line-tab-current ((t (:weight medium))))
  (tab-line-tab-inactive ((t (:weight normal :foreground "#7F7F7F"))))
  :config
  (advice-add 'tab-line-close-tab :override 'my/tab-line-close-tab)
  (setq tab-line-separator
        (propertize " " 'face 'default 'display '(space :width (1)))))

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
  ;; TODO: Make - in dired mode reuse the same buffer
  (defun my/dired-mouse-find-file-smart (event)
    (interactive "e" dired-mode)
    (dired-mouse-find-file event
                           'find-file-other-window
                           'find-alternate-file))
  (advice-add 'dired-mouse-find-file-other-window :override
              'my/dired-mouse-find-file-smart)
  (general-add-hook 'dired-mode-hook '( dired-hide-details-mode
                                        dired-omit-mode)))

(use-package which-func :ensure nil
  :custom-face
  (which-func ((t (:height 120 :inherit (shadow medium fixed-pitch)))))
  :custom
  (which-func-unknown "")
  (which-func-format `(-24 (:propertize which-func-current
                            local-map ,which-func-keymap
                            face which-func
                            mouse-face mode-line-highlight
                            help-echo ,(concat
                                        "Current function\n"
                                        "mouse-1: go to beginning\n"
                                        "mouse-2: toggle rest visibility\n"
                                        "mouse-3: go to end")))))

(use-package help-mode :ensure nil
  :init
  ;; These changes give *Help* buffers descriptive names, and allow opening
  ;; a new help buffer by using a prefix before following a link
  (defun my/help-name ()
    "Returns a descriptive name for the current help buffer or the current name
if one couldn't be determined."
    (or (when (derived-mode-p 'help-mode)
          (let ((name (plist-get help-mode--current-data :symbol)))
            (and name (format "*Help* <%s>" name))))
        (buffer-name)))
  (defun my/rename-help-buffer (&rest _)
    (when (derived-mode-p 'help-mode)
      (let ((new-name (my/help-name)))
        (unless (string= new-name (buffer-name))
          (rename-buffer (my/help-name) t)))))
  (defun my/help-buffer-advice (fn)
    (cond (current-prefix-arg (buffer-name (get-buffer-create "*Help*")))
          ((derived-mode-p 'help-mode) (buffer-name))
          (t (funcall fn))))
  (defun my/help-window-setup-advice (window &rest _)
    (with-selected-window window
      (my/rename-help-buffer)))
  (defun my/elisp-doc-buffer-advice (fn &rest args)
    ;; elisp--company-doc-buffer displays the help buffer and then hides it
    ;; to get documentation. This redirects it to its own hidden buffer that
    ;; won't interfere with existing help buffers.
    (cl-letf (((symbol-function 'help-buffer)
               (lambda (&rest _)
                 (buffer-name (get-buffer-create " *Help company-doc-buffer*"))))
              ;; Disable my advice so it doesn't interfere
              ((symbol-function 'my/help-buffer-advice) #'funcall)
              ((symbol-function 'my/rename-help-buffer) #'ignore))
      (apply fn args)))
  (advice-add 'elisp--company-doc-buffer :around 'my/elisp-doc-buffer-advice)
  (advice-add 'help-window-setup :after 'my/help-window-setup-advice)
  (advice-add 'help-buffer :around 'my/help-buffer-advice)
  (advice-add 'help-xref-go-forward :after 'my/rename-help-buffer)
  (advice-add 'help-xref-go-back :after 'my/rename-help-buffer))


(defvar my/lisp-mode-hooks
  '(lisp-mode-hook lisp-data-mode-hook fennel-mode-hook))

(general-add-hook 'prog-mode-hook
                  '(my/prog-word-syntax my/show-trailing-whitespace))

(general-add-hook my/lisp-mode-hooks
                  '(my/lisp-word-syntax
                    ;; my/string-underline ; disabling this for now
                    ))

(general-add-hook '(prog-mode-hook text-mode-hook) 'my/line-spacing)

(general-add-hook '( eww-mode-hook markdown-mode-hook markdown-view-mode-hook
                     gfm-mode-hook gfm-view-mode-hook)
                  'my/margins)

(general-add-hook '( eww-mode-hook markdown-mode-hook markdown-view-mode-hook
                     gfm-mode-hook gfm-view-mode-hook Info-mode-hook)
                  'variable-pitch-mode)

(general-add-hook '( eww-mode-hook markdown-mode-hook markdown-view-mode-hook
                     gfm-mode-hook gfm-view-mode-hook devdocs-mode-hook
                     doc-view-mode-hook)
                  '(my/word-wrap my/no-fringes))

(general-add-hook '( pdf-view-mode-hook nov-mode-hook doc-view-mode-hook)
                  'my/no-blink-cursor)

;; (general-add-hook '( evil-collection-eldoc-doc-buffer-mode-hook eww-mode-hook
;;                      help-mode-hook Custom-mode-hook messages-buffer-mode-hook)
;;                   'my/no-mode-line)

(general-add-hook '( help-mode-hook eww-mode-hook compilation-mode-hook
                     comint-mode-hook apropos-mode-hook Info-mode-hook
                     evil-collection-eldoc-doc-buffer-mode-hook
                     package-menu-mode-hook eat-mode-hook proced-mode-hook
                     shortdoc-mode-hook vterm-mode-hook Custom-mode-hook
                     bufler-list-mode-hook devdocs-mode-hook debugger-mode)
                  'my/smaller-fonts)


;;;; Customized Mode Line
;;;; ======================================================================

(defun my/vim-color ()
  (if (mode-line-window-selected-p)
      (pcase (symbol-name evil-state)
        ("normal"   'state-normal)
        ("insert"   'state-insert)
        ("visual"   'state-visual)
        ("replace"  'state-replace)
        ("operator" 'state-operator)
        ("emacs"    'state-emacs)
        (_ 'state-other))
    'mode-line-inactive))

(defun my/spacer (face width)
  `(:propertize " "
    face ,face
    display (space :width ,width)))

(defun my/color-spacer (color width)
  (my/spacer `(:background ,color :family ,my/default-font) width))

;; TODO: Memoize colors if it causes slowdown
(defun my/gradient-spacer (from-col to-col width)
  `(,(my/color-spacer (my/blend from-col to-col 0.25) width)
    ,(my/color-spacer (my/blend from-col to-col 0.5) width)
    ,(my/color-spacer (my/blend from-col to-col 0.75) width)))

;; Note that these assume they're being used in the selected mode line
(defun my/left-gradient (&optional width)
  (let ((mode-line-color (face-attribute 'mode-line :background nil 'default))
        (vim-color (face-attribute (my/vim-color) :background nil 'default)))
    (my/gradient-spacer vim-color mode-line-color
                        (or width '(2)))))

(defun my/right-gradient (&optional width)
  (let ((mode-line-color (face-attribute 'mode-line :background nil 'default))
        (vim-color (face-attribute (my/vim-color) :background nil 'default)))
    (my/gradient-spacer mode-line-color vim-color
                        (or width '(2)))))

(defun my/vim-state ()
  (if (mode-line-window-selected-p)
      (let* ((mode-text (concat " " (upcase (symbol-name evil-state)) " "))
             (col (face-attribute 'mode-line :box))
             (col (if (stringp col) col (plist-get col :color))))
        `(,(propertize mode-text 'face (my/vim-color))
          ;; (:propertize " " face (:background ,col) display (space :width (1)))
          ))))

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
  (let ((text (format-mode-line
               (if (mode-line-window-selected-p)
                   (butlast minions-mode-line-modes 3)
                 (butlast minions-mode-line-modes 4))
               t)))
    (put-text-property 0 (length text)
                       'face
                       `((:height ,my/font-height)
                         fixed-pitch) text)
  text))

(defun my/modeline-project ()
  (let ((home (concat (getenv "HOME") "/")))
    (unless (or (string= home (projectile-project-p))
                (string= "-" (projectile-project-name)))
      `("" (:propertize
             ,(projectile-project-name)
             face ((:height 0.9) shadow)
             mouse-face ((:box t) highlight)
             help-echo ,(projectile-project-p)
             pointer-shape arrow
             keymap ,(make-mode-line-mouse-map 'mouse-1
                                               #'projectile-mode-menu)
             display (raise 0.04))
        " "))))

(defun my/get-buffer-name ()
  (let ((fname (buffer-file-name))
        (proj (projectile-project-p)))
    (cond ((and fname proj)
           (let ((path (string-remove-prefix proj fname)))
             (if (length< path 32)
                 path
               (require 'rng-uri)
               (let ((parts (rng-split-path path))
                     (limit (lambda (x)
                              (string-limit x (if (string-prefix-p "." x) 2 1)))))
                 (rng-join-path (append (mapcar limit (butlast parts))
                                        (last parts)))))))
          (t (buffer-name)))))

(defun my/modeline-buffer-name ()
  (let* ((face-extra (when (and (buffer-file-name) (buffer-modified-p))
                       '(italic)))
         (face `((:weight ,(if (mode-line-window-selected-p) 'medium 'normal))
                 ,@face-extra mode-line-buffer-id))
         (text (propertize (my/get-buffer-name) 'face face)))
    (put-text-property 0 (length text)
                       'mouse-face '((:box t) highlight)
                       text)
    (put-text-property 0 (length text)
                       'help-echo (format "%s %s\nmouse-1: Switch buffer"
                                          major-mode
                                          (or (buffer-file-name) (buffer-name)))
                       text)
    (put-text-property 0 (length text)
                       'keymap
                       (make-mode-line-mouse-map 'mouse-1 #'mouse-buffer-menu)
                       text)
    `("  " ,(my/modeline-modified) ,text "  ")))

(defun my/propertize-position (pos)
  (let* ((col (face-attribute 'mode-line :box))
         (col (if (stringp col) col (plist-get col :color))))
    `("  "
      ;; (:propertize " " face (:background ,col) display (space :width (1)))
      ,(propertize (format-mode-line pos t) 'face (my/vim-color)))))

(defun my/modeline-position-default ()
  (my/propertize-position '( " %3l:%2C ")))

(defun my/modeline-position-pdf ()
  (or (ignore-errors
        (require 'pdf-view)
        (require 'pdf-info)
        (my/propertize-position (format " Page %d/%d  "
                                        (pdf-view-current-page)
                                        (pdf-info-number-of-pages))))
      (my/propertize-position "  ")))

(defun my/modeline-position-doc-view ()
  (my/propertize-position (format " Page %d/%d  "
                                  (doc-view-current-page)
                                  (doc-view-last-page-number))))

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
          (propertize (concat " " search-info))))
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
      (cond ((bound-and-true-p flymake-mode) '(" " (:eval (my/flymake-status)) "  "))
            ((bound-and-true-p flycheck-mode) '(" " (:eval (my/flycheck-status)) "  "))
            (t ""))
    ""))

(defun my/modeline-modified ()
  "Show an indicator for modified and read-only buffers, as long as they are
visiting a file."
  (if (buffer-file-name)
      (let* ((modified (buffer-modified-p))
             (view-str (when view-mode "🔍 "))
             (read-only-str (if buffer-read-only "🔒 " ""))
             (modified-str (if modified "∙ " "")))
        (propertize (concat (or view-str read-only-str)
                            (if (or modified (not buffer-read-only)) modified-str ""))
                    'face `(:inherit ,(if modified 'error 'shadow)
                            :weight medium
                            :family ,my/variable-font
                            :height ,my/mode-line-font-height)))
    ""))

(defun my/mode-line-height-hack ()
  "Terrible hack to fix the annoying resizing that keeps happening. Put an
invisible copy of the character causing the resizing so it's always present."
  (let* ((face (if (mode-line-window-selected-p)
                   'mode-line-active
                 'mode-line-inactive))
         (mode-line-color (face-attribute face :background nil 'mode-line)))
    (propertize "∙" 'face `(:foreground ,mode-line-color
                            :weight medium
                            :family ,my/variable-font
                            :height ,my/mode-line-font-height))))

(setopt mode-line-right-align-edge 'right-margin ;'window
        )
(setq-default mode-line-format
              `((:eval (my/evil-state))
                (:eval (my/modeline-buffer-name))
                (:eval (my/modeline-search))
                (:eval (my/modeline-eldoc))
                mode-line-misc-info
                (:eval (my/mode-line-height-hack))
                mode-line-format-right-align
                (:eval (my/modeline-project))
                (:eval (my/modeline-fly))
                (:eval (my/modeline-modes))
                (:eval (my/modeline-position))))

;; In daemon mode, the messages buffer is created too early to get the
;; mode line changes.
(add-hook 'messages-buffer-mode-hook 'my/smaller-fonts)
(with-current-buffer (messages-buffer)
  (setq mode-line-format (default-value 'mode-line-format))
  ;; (my/no-mode-line)
  (my/smaller-fonts))


;;;; Other keybinds
;;;; ======================================================================

(general-define-key
 "C-x k" 'kill-current-buffer
 "<mode-line> <mouse-2>" 'mouse-delete-window
 "<mode-line> <mouse-3>" 'menu-bar-open) ;; TODO: Something more useful than this

(general-def '(normal motion)
  "C-j" 'my/switch-to-next-buffer
  "C-k" 'my/switch-to-prev-buffer)

;; Allows switching buffers in modes that otherwise use C-j or C-k, like shells
(general-def 'evil-window-map
  "C-j" 'my/switch-to-next-buffer
  "C-k" 'my/switch-to-prev-buffer)

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


(defvar my/project-commands '())
(defun my/current-project-symbol ()
  (let ((cur (project-current)))
    (if cur (intern (project-name cur)) 'none)))
(defun my/current-project-compile-command ()
  (plist-get my/project-commands (my/current-project-symbol)))
(defun my/update-current-project-compile-command (cmd)
  (setq my/project-commands
        (plist-put my/project-commands (my/current-project-symbol) cmd)))

(defun compile-interactively (command)
  "Prompts for the compile command the first time it's called, or when called
with a prefix. Always starts the compilation buffer in comint mode to allow
interaction. Tracks the compile command used on a per-project basis."
  (interactive
   (list
    (let ((command (my/current-project-compile-command)))
      (if (or (null command)
              current-prefix-arg)
          (let ((new-command (compilation-read-command command)))
            (my/update-current-project-compile-command new-command)
            new-command)
        ;; default to the previous used command, but don't store it
        (or command compile-command)))))
  (compile command t))


(general-def
  "<f12>" 'window-toggle-side-windows
  "C-<f12>" 'modus-themes-rotate
  "<f5>" 'compile-interactively
  "C-x C-b" 'ibuffer)

;; I don't want the tutorial, license, hello, or any of the other junk that can
;; accidentally be fatfingered when using the otherwise useful C-h commands.
;; Any missing keybinds here are probably reused by one of the packages above.
(general-unbind
  "<f1> t" "C-h C-\\" "C-h C-a" "C-h C-c" "C-h C-e" "C-h C-n"
  "C-h C-o" "C-h C-p" "C-h C-q" "C-h C-t" "C-h C-w" "C-h RET" "C-h g"
  "C-h h" "C-h n" "C-h t")

;; I really want escape to do what it says
;; Unbind anything that waits for a key after Escape
(general-unbind "M-ESC :") ; the only default keybind prefixed by M-ESC
(general-unbind "ESC ESC ESC")
;; Double-tap is better than triple-tap at least. Should only matter on tty.
(general-def "ESC ESC" 'keyboard-escape-quit)
(general-def '(minibuffer-local-map minibuffer-local-ns-map
               minibuffer-local-completion-map minibuffer-local-must-match-map
               minibuffer-local-isearch-map)
  "<escape>" 'keyboard-escape-quit)

(general-def 'emacs "<escape>" 'keyboard-escape-quit)

(defvar-local my/entered-emacs-state-manually nil
  "This flag is set in `evil-emacs-state-entry-hook' if `this-command' was
`evil-emacs-state'. Theoretically this is only the case when the user manually
swapped into emacs-state. This is checked in `my/keyboard-escape-quit-advice'
to exit emacs-state without affecting modes that started in emacs-state by
default.")
(defun my/enter-emacs-state ()
  (when (and (eq this-command 'evil-emacs-state)
             (not (memq major-mode evil-emacs-state-modes)))
    (setq-local my/entered-emacs-state-manually t)))
(defun my/exit-emacs-state ()
  (setq-local my/entered-emacs-state-manually nil))
(add-hook 'evil-emacs-state-entry-hook 'my/enter-emacs-state)
(add-hook 'evil-emacs-state-exit-hook 'my/exit-emacs-state)

(defun my/keyboard-escape-quit-advice (fn)
  "Prevents Escape from messing with splits, and also exits emacs-state when
pressed twice in a row."
  (if (and (or my/entered-emacs-state-manually
               (eq last-command 'keyboard-escape-quit))
           (evil-emacs-state-p))
      (evil-normal-state nil)
    (let ((buffer-quit-function (or buffer-quit-function #'keyboard-quit)))
      (funcall fn))))
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
(setq window-sides-slots '(1 0 0 2))
(setq fit-window-to-buffer-horizontally t)

;; When balancing windows, preserve the height of the bottom side windows.
;; I'm assuming the windows will never have `window-size-fixed' set beforehand.
(defun my/balance-windows-advice (fn &rest args)
  (if (not (called-interactively-p 'any))
      (funcall fn args)
    (mapc (lambda (w) (window-preserve-size w nil t))
          (seq-filter 'my/side-window? (window-list)))
    (funcall-interactively fn args)
    (mapc (lambda (w) (window-preserve-size w nil nil))
          (seq-filter 'my/side-window? (window-list)))))
(advice-add 'balance-windows :around 'my/balance-windows-advice)

(defun my/one-time-hook (hook fn)
  (letrec ((one-shot (lambda ()
                       (ignore-errors
                         (remove-hook hook one-shot t)
                         (funcall fn)))))
    (ignore-errors (add-hook hook one-shot t))))

;; I don't need anything here right now, but I'm leaving it in case I do later
(defun my/main-window-body-fn (win) t)

(defun my/side-window-body-fn (win)
  (with-selected-window win
    (when (string= (buffer-name) "*Warnings*")
      (my/smaller-fonts))))

(defun my/window-body-fn (win)
  "Sometimes (`other-window-prefix', for example) the display rule chosen will
not match the actual location a buffer is about to be displayed. Decide which
kind of window we're setting up after that decision is made instead of basing
it on the buffer itself."
  ;; Some modes clear all local variables after body-function runs.
  ;; Deferring the body functions until the end of the current command
  ;; ensures that their changes aren't undone by the major mode.
  (my/one-time-hook 'post-command-hook
                    (lambda ()
                      (when (window-valid-p win)
                        (if (window-parameter win 'window-side)
                            (my/side-window-body-fn win)
                          (my/main-window-body-fn win))))))


;; TODO: Advise customize functions so they stop hijacking the current window
(setq display-buffer-alist
      (let* ((bot-common '(display-buffer-in-side-window
                           (side . bottom) (preserve-size . t)
                           (body-function . my/window-body-fn)))
             (bot-left-window `(,@bot-common (slot . -1)))
             (bot-right-window `(,@bot-common (slot . 1)))
             (bot-left-rx (rx bos
                              (| " " "*" " *")
                              (| "Help" "Custom" "info" "eldoc" "Occur" "grep"
                                 "devdocs" "Pp" "eww")))
             (bot-right-rx (rx bos
                               (| " " "*" " *")
                               (| (regex "[e]?shell") (regex "[v]?term")
                                  (regex ".*[Rr][Ee][Pp][Ll].*")
                                  "compilation" "lua" "Python" "inferior"
                                  "Warnings" "Messages" "Ibuffer" "eat*")))
             (bot-left-modes '( dired-mode eww-mode magit-mode))
             (bot-right-modes '( comint-mode special-mode messages-buffer-mode
                                 ibuffer-mode))
             (derived-rule (lambda (x) (cons 'derived-mode x)))
             )
        `(("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
           display-buffer-in-direction
           (direction . rightmost)
           (window-parameters (mode-line-format . none)))
          ((or ,bot-left-rx
               ,@(mapcar derived-rule bot-left-modes))
           ,@bot-left-window)
          ((or ,bot-right-rx
               ,@(mapcar derived-rule bot-right-modes))
           ,@bot-right-window)
          ;; Catch anything that fell through
          (my/match-non-special-buffers
           nil
           (body-function . my/window-body-fn))
          (my/match-special-buffers
           (display-buffer-in-side-window display-buffer-no-window)
           (body-function . my/window-body-fn)
           (side . bottom))
          )))

(defvar my/custom-loaded nil)
(when (and custom-file (not my/custom-loaded))
  (load custom-file)
  (setq my/custom-loaded t))

