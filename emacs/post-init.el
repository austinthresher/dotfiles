;;; -*- no-byte-compile: t; lexical-binding: t; -*-

;; TODO: Pick a single key binding for buffer swapping, probably a function key

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
(require 'color)
(require 'cl-lib)


;;;; Theme and font
;;;; ======================================================================

;; All colors are hex strings and `alpha' is a float from 0 to 1.
(defun my/blend (hex-from hex-to alpha)
  "Interpolates between `hex-from' and `hex-to'. Returns `hex-from' when
`alpha' is 0 and returns `hex-to' when `alpha' is 1. `alpha' can be outside
the range of 0-1, in which case the resulting color is extrapolated."
  (let ((from (color-name-to-rgb hex-from))
        (to (color-name-to-rgb hex-to)))
    (seq-let (r g b) (mapcar #'color-clamp (color-blend to from alpha))
      (color-rgb-to-hex r g b 2))))
(defun my/darken (hex-color alpha) (my/blend hex-color "#000" alpha))
(defun my/lighten (hex-color alpha) (my/blend hex-color "#FFF" alpha))
;; color-saturate-hsl feels like it's far too sensitive to small arguments.
;; This version lerps the saturation from the current value instead of just
;; adding / subtracting to it.
(defun my/saturate (hex-color alpha)
  (seq-let (h s l) (->> hex-color color-name-to-rgb (apply #'color-rgb-to-hsl))
    (let ((s (color-clamp (+ alpha (* s (- 1.0 alpha))))))
      (seq-let (r g b) (mapcar #'color-clamp (color-hsl-to-rgb h s l))
        (color-rgb-to-hex r g b 2)))))
(defun my/desaturate (hex-color alpha)
  (seq-let (h s l) (->> hex-color color-name-to-rgb (apply #'color-rgb-to-hsl))
    (let ((s (color-clamp (* s (- 1.0 alpha)))))
      (seq-let (r g b) (mapcar #'color-clamp (color-hsl-to-rgb h s l))
        (color-rgb-to-hex r g b 2)))))


(defun my/expand-fg-bg (plist)
  "Replace :bg and :fg shorthand keys in a property list."
  (mapcar (lambda (x)
            (pcase x (:bg :background) (:fg :foreground) (_ x)))
          plist))

(cl-defun my/face-spec (&key light dark (default nil))
  "Make it easier to define faces without having to remember the exact number
of required nestings. This will also replace :fg and :bg properties with
:foreground and :background to make face specs slightly more concise. If
default is the only value given, it applies to t instead of default."
  (let ((light-ls (when light `(( ((background light)) ,@(my/expand-fg-bg light)))))
        (dark-ls (when dark `(( ((background dark)) ,@(my/expand-fg-bg dark)))))
        (default-ls (when default `((default ,@(my/expand-fg-bg default))))))
    `(,@default-ls ,@light-ls ,@dark-ls)))

(defun my/face (name &rest args)
  "Wrap my/face-spec to act as a more concise defface replacement. If args
doesn't start with a recognized keyword it's used as :default. In this case,
the property list can optionally be given directly in the arguments instead of as a list."
  (declare (indent 1))
  (when (facep name) (face-spec-reset-face name))
  (cond ((memq (car args) (list :light :dark :default))
         (face-spec-set name (apply #'my/face-spec args)))
        ((keywordp (car args))
         (face-spec-set name (funcall #'my/face-spec :default args)))
        (t (face-spec-set name (apply #'my/face-spec :default args)))))

;; NOTE: Because Emacs never merges :inherit attribtues, the only way to
;; mix and match background/foreground colors is to have them on separate
;; faces and then inherit from each. Inheriting from a face that tries to
;; use 'reset or 'unspecified as a mask does not work.

;; These will be overwritten by actual values in setup-fonts
(defvar my/dark-fg "#EEE")
(defvar my/dark-bg "#222")
(defvar my/light-fg "#333")
(defvar my/light-bg "#FFF")

(defun my/face-calculated-impl (name fn)
  "Accepts a function that takes a foreground and background color and returns
a face spec, automatically applying that to light and dark colors."
  (declare (indent 1))
  (my/face name
    :light (funcall fn my/light-fg my/light-bg)
    :dark (funcall fn my/dark-fg my/dark-bg)))

(defmacro my/face* (name &rest spec)
  "Anaphoric macro, where `bg' and `fg' are bound to the foreground and
background colors of the current theme. `spec' should be a face spec and will
be evaluated twice, once for light and once for dark."
  (declare (indent 1))
  `(my/face-calculated-impl ,name (lambda (fg bg) (list ,@spec))))

(defun my/face-alias (dst src)
  "Make the face named `dst' an alias for the face named `src'."
  (put dst 'face-alias src))

(defun my/make-cursor-colors (start-col end-col)
  (mapcar
   (apply-partially #'my/blend start-col end-col)
   ;; Doubling up the first one helps hide a stutter when it starts blinking
   '(0.0 0.0 0.25 0.5 0.75 0.9 0.75 0.5 0.25)))

;; These will also be overwritten when applying a theme
(defvar my/cursor-colors-normal (my/make-cursor-colors "#888" "#DDD"))
(defvar my/cursor-colors-emacs (my/make-cursor-colors "#F0F" "#888"))

(use-package doom-themes :ensure t :demand t)

(defvar my/light-theme-name 'doom-tomorrow-day)
(defvar my/dark-theme-name 'doom-monokai-machine)

;; TODO: Do all themes follow this naming scheme? Make the symbol from the name if so
(require-theme 'doom-tomorrow-day-theme)
(require-theme 'doom-monokai-machine-theme)

;; Each of these will be populated on theme load
(defvar my/light-theme-faces '())
(defvar my/dark-theme-faces '())

;; These need to exist so that the mode line doesn't get invalid faces.
;; They'll be overwritten on theme load.
(my/face 'state-normal :background "blue")
(my/face 'state-insert :background "green")
(my/face 'state-visual :background "yellow")
(my/face 'state-replace :background "red")
(my/face 'state-operator :background "cyan")
(my/face 'state-emacs :background "magenta")
(my/face 'default-fg :background 'unspecified :inherit 'default)

(defun my/get-fg-light (face &optional default)
  (or (plist-get (cdr (assoc face my/light-theme-faces)) :foreground)
      default
      my/light-fg))
(defun my/get-bg-light (face &optional default)
  (or (plist-get (cdr (assoc face my/light-theme-faces)) :background)
      default
      my/light-bg))
(defun my/get-fg-dark (face &optional default)
  (or (plist-get (cdr (assoc face my/dark-theme-faces)) :foreground)
      default
      my/dark-fg))
(defun my/get-bg-dark (face &optional default)
  (or (plist-get (cdr (assoc face my/dark-theme-faces)) :background)
      default
      my/dark-bg))

(defun my/face-fg (name source &optional default-light default-dark)
  "Create a new face that copies the foreground color from another face"
  (my/face name
    :light `(:fg ,(my/get-fg-light source default-light))
    :dark `(:fg ,(my/get-fg-dark source (or default-dark default-light)))))

(defun my/face-bg (name source &optional default-light default-dark)
  "Create a new face that copies the background color from another face"
  (my/face name
    :light `(:bg ,(my/get-bg-light source default-light))
    :dark `(:bg ,(my/get-bg-dark source (or default-dark default-light)))))

(defun my/collect-faces (theme-settings)
  (cl-flet ((extract-spec (lambda (theme-entry)
                            (pcase theme-entry
                              (`(theme-face ,face-name ,_ ,spec)
                               `(,face-name . ,(face-spec-choose spec)))))))
    (remq nil (mapcar #'extract-spec theme-settings))))

(defun my/no-frames-exist? () (string= "initial_terminal" (terminal-name)))

(defun my/extract-theme-faces ()
  (unless (my/no-frames-exist?)
    (unless (get my/light-theme-name 'theme-settings)
      (load-theme my/light-theme-name :no-confirm :no-enable))
    (unless (get my/dark-theme-name 'theme-settings)
      (load-theme my/dark-theme-name :no-confirm :no-enable))
    (let* ((light-plist (get my/light-theme-name 'theme-settings))
           (dark-plist (get my/dark-theme-name 'theme-settings)))
      (setq my/light-theme-faces (my/collect-faces light-plist)
            my/dark-theme-faces (my/collect-faces dark-plist)
            my/dark-fg (my/get-fg-dark 'default)
            my/dark-bg (my/get-bg-dark 'default)
            my/light-fg (my/get-fg-light 'default)
            my/light-bg (my/get-bg-light 'default)))))

(defun my/setup-faces ()
  (my/extract-theme-faces)
  (my/face 'default :family "Iosevka" :height 140)
  (my/face 'variable-pitch :family "Noto Sans" :height 140)
  (my/face 'variable-pitch-text :inherit 'variable-pitch)
  (my/face 'fixed-pitch :family "Iosevka Slab" :height 140)
  (my/face 'fixed-pitch-serif :family "Iosevka Slab" :height 140)
  (my/face* 'mode-line :family "Roboto" :height 140 :weight 'light
            :box `(:line-width (1 . 1) :color ,(my/blend (my/saturate bg 0.5)
                                                         (my/saturate fg 0.5)
                                                         0.5)))
  (my/face* 'mode-line-inactive
    :family "Roboto" :height 140 :weight 'light
    :box `(:line-width (1 . 1) :color ,(my/darken (my/blend bg fg 0.1) 0.2))
    ;:background (my/blend bg fg 0.05)
    )
  (my/face* 'fringe :bg bg)  ;; Never let themes set different fringe colors
  (my/face* 'vertical-border :fg (my/blend bg fg 0.2))
  (my/face* 'thin-mode-line :fg (my/darken bg 0.25) :bg (my/darken bg 0.05))
  ;; The stipple here is a simple checkerboard pixel pattern. Noticable, but
  ;; relatively unobtrusive.
  (my/face 'trailing-whitespace :fg "#844" :stipple '(8 2 "\xAA\x55"))
  (my/face* 'font-lock-punctuation-face :fg fg :weight 'light)
  (set-face-attribute 'font-lock-builtin-face nil :family "Iosevka Slab")
  (set-face-attribute 'font-lock-keyword-face nil :family "Iosevka Slab")
  (set-face-attribute 'font-lock-comment-face nil :weight 'light :slant 'unspecified)
  (set-face-attribute 'font-lock-doc-face nil :weight 'light :slant 'unspecified)
  (set-face-attribute 'font-lock-regexp-grouping-backslash nil :weight 'semibold)
  (set-face-attribute 'font-lock-regexp-grouping-construct nil :weight 'bold)
  (set-face-attribute 'font-lock-constant-face nil :family "Iosevka Slab")
  (set-face-attribute 'font-lock-escape-face nil
                      :foreground 'unspecified
                      :inherit '(font-lock-constant-face))
  (set-face-attribute 'font-lock-number-face nil
                      :foreground 'unspecified
                      :inherit '(font-lock-constant-face))
  (set-face-attribute 'font-lock-preprocessor-face nil :weight 'normal)
  (set-face-attribute 'header-line nil :weight 'normal)
  (my/face* 'default-bg :bg bg)
  (my/face* 'default-fg :fg fg)
  (my/face* 'dark-border :fg (my/darken bg 0.3) :bg (my/darken bg 0.3))
  (my/face 'popup-border
    :light `(:fg ,(my/darken my/light-bg 0.20)
             :bg ,(my/darken my/light-bg 0.20))
    :dark `(:fg ,(my/lighten my/dark-bg 0.20)
            :bg ,(my/lighten my/dark-bg 0.20)))
  (my/face 'popup-bg
    :light `(:bg ,(my/darken my/light-bg 0.03))
    :dark `(:bg ,(my/lighten my/dark-bg 0.10)))
  (my/face* 'dimmed-background :bg (my/darken bg 0.15))
  (my/face* 'faded :fg (my/blend fg bg 0.5))
  (my/face* 'thick-strike-through
    :strike-through "#F00" :bg "#A22" :fg 'unspecified
    :underline '(:color "#F44" :position 15) :weight 'unspecified
    :box `(:line-width (-1 . -12) :color ,bg))
  (my/face* 'replacement-box
    :box `(:line-width (1 . -1) :color "#A3BE8C")
    :fg 'unspecified :weight 'unspecified :bg bg)
  (my/face* 'form-feed-line-normal
    :box `(:line-width (-1 . -12) :color ,bg)
    :underline `(:color ,(my/blend bg fg 0.4) :position 13)
    :background (my/blend bg fg 0.1))
  (my/face* 'form-feed-line-small
    :box `(:line-width (-1 . -9) :color ,bg)
    :underline `(:color ,(my/blend bg fg 0.4) :position 10)
    :background (my/blend bg fg 0.1))

  (my/face 'normal-weight :weight 'normal)
  (my/face 'medium-weight :weight 'medium)
  (my/face 'light-weight :weight 'light)
  (my/face 'extralight-weight :weight 'extralight)
  (my/face 'no-slant :slant 'reset)
  (my/face 'oblique :slant 'oblique)
  (my/face 'selected-item
    :light '(:bg "#D0F0FF" :box (:line-width (1 . -1) :color "#DDD"))
    :dark `(:bg "#434C5E" :box (:line-width (1 . -1)
                                :color ,(my/lighten my/dark-bg 0.20))))

  ;; Default colors are taken from Tomorrow and Tomorrow Night.
  (my/face-fg 'blue-fg    'ansi-color-blue    "#4271AE" "#81A2BE")
  (my/face-fg 'magenta-fg 'ansi-color-magenta "#8959A8" "#B294BB")
  (my/face-fg 'green-fg   'ansi-color-green   "#718C00" "#B5BD68")
  (my/face-fg 'yellow-fg  'ansi-color-yellow  "#EAB700" "#F0C674")
  (my/face-fg 'red-fg     'ansi-color-red     "#C82829" "#CC6666")
  (my/face-fg 'cyan-fg    'ansi-color-cyan    "#3E999F" "#8ABEB7")

  (my/face-bg 'blue-bg    'ansi-color-blue    "#4271AE" "#81A2BE")
  (my/face-bg 'magenta-bg 'ansi-color-magenta "#8959A8" "#B294BB")
  (my/face-bg 'green-bg   'ansi-color-green   "#718C00" "#B5BD68")
  (my/face-bg 'yellow-bg  'ansi-color-yellow  "#EAB700" "#F0C674")
  (my/face-bg 'red-bg     'ansi-color-red     "#C82829" "#CC6666")
  (my/face-bg 'cyan-bg    'ansi-color-cyan    "#3E999F" "#8ABEB7")

  ;; The ANSI colors in a lot of themes aren't legible as backgrounds, so this
  ;; blends them towards the theme's background color.
  (defun my/face-blended-bg (name source light-default dark-default)
    (my/face name
      :light `(:bg ,(my/blend (my/get-bg-light source light-default) my/light-bg 0.5))
      :dark `(:bg ,(my/blend (my/get-bg-dark source dark-default) my/dark-bg 0.5))))

  (my/face-blended-bg 'blue-blended-bg    'ansi-color-blue    "#4271AE" "#81A2BE")
  (my/face-blended-bg 'magenta-blended-bg 'ansi-color-magenta "#8959A8" "#B294BB")
  (my/face-blended-bg 'green-blended-bg   'ansi-color-green   "#718C00" "#B5BD68")
  (my/face-blended-bg 'yellow-blended-bg  'ansi-color-yellow  "#EAB700" "#F0C674")
  (my/face-blended-bg 'red-blended-bg     'ansi-color-red     "#C82829" "#CC6666")
  (my/face-blended-bg 'cyan-blended-bg    'ansi-color-cyan    "#3E999F" "#8ABEB7")

  ;; For some reason I couldn't redefine these with my/face, but this seems to work.
  ;; This is still broken when toggling between the light/dark theme.
  (set-face-attribute 'state-normal nil :background (face-attribute 'blue-blended-bg :background))
  (set-face-attribute 'state-insert nil :background (face-attribute 'green-blended-bg :background))
  (set-face-attribute 'state-visual nil :background (face-attribute 'yellow-blended-bg :background))
  (set-face-attribute 'state-replace nil :background (face-attribute 'red-blended-bg :background))
  (set-face-attribute 'state-operator nil :background (face-attribute 'cyan-blended-bg :background))
  (set-face-attribute 'state-emacs nil :background (face-attribute 'magenta-blended-bg :background))

  (my/face 'link-visited :fg 'unspecified :inherit '(magenta-fg link))
  (my/face* 'string-underline
    :underline `(:color ,(my/blend bg fg 0.1) :position 3))
  (my/face* 'tabs-bar
    :bg (my/darken (my/desaturate bg 0.05) 0.1)
    :fg (my/blend fg bg 0.1)
    :underline `(:color ,(my/blend (my/darken bg 0.1) fg 0.1) :position 0 :extend t))
  (my/face* 'tabs-active
    :fg fg
    :bg bg
    :inherit '(variable-pitch tabs-bar)
    :weight 'medium
    :box `(:line-width (-1 . -1) :color ,(my/darken bg 0.15))
    :underline `(:color ,bg :position 0))
  (my/face* 'tabs-inactive
    :fg (my/blend fg bg 0.2)
    :bg (my/blend (my/saturate bg 0.1) (my/darken fg 0.1) 0.1)
    :inherit '(variable-pitch tabs-bar)
    :weight 'light)
  (my/face* 'tabs-bar-overline
    :inherit 'tabs-bar
    :overline (my/blend (my/darken bg 0.25) fg 0.2))
  )

(defun light-theme ()
  "Activates a light-mode theme."
  (interactive)
  (disable-theme my/dark-theme-name)
  (enable-theme my/light-theme-name)
  (my/setup-faces)
  (setq frame-background-mode 'light)
  (mapc 'frame-set-background-mode (frame-list))
  (let ((cursor (my/get-bg-light 'cursor)))
    (setq my/cursor-colors-normal (my/make-cursor-colors cursor my/light-bg)
          my/cursor-colors-emacs (my/make-cursor-colors "#F0F" my/light-bg))))

(defun dark-theme ()
  "Activates a dark-mode theme."
  (interactive)
  (disable-theme my/light-theme-name)
  (enable-theme my/dark-theme-name)
  (my/setup-faces)
  (setq frame-background-mode 'dark)
  (mapc 'frame-set-background-mode (frame-list))
  (let ((cursor (my/get-bg-dark 'cursor)))
    (setq my/cursor-colors-normal (my/make-cursor-colors cursor my/dark-bg)
          my/cursor-colors-emacs (my/make-cursor-colors "#F0F" my/dark-bg))))

(defun my/after-theme-enabled (&rest _)
  "Add this as a hook to `enable-theme-functions' to automatically update
faces when the theme changes (useful with `consult-theme'). Only runs when
the theme wasn't set with `light-theme' or `dark-theme'."
  (when (not (or (eq this-command 'light-theme)
                 (eq this-command 'dark-theme)))
    (require 'faces)
    (let* ((theme-name (car custom-enabled-themes))
           (bg (face-attribute 'default :background))
           (theme-is-dark? (color-dark-p (color-name-to-rgb bg))))
      (if theme-is-dark?
          (setq my/dark-theme-name theme-name
                frame-background-mode 'dark)
        (setq my/light-theme-name theme-name
              frame-background-mode 'light)))
    (mapc 'frame-set-background-mode (frame-list))
    (my/setup-faces)
    (let ((cursor (face-attribute 'cursor :background))
          (bg (face-attribute 'default :background)))
      (setq my/cursor-colors-normal (my/make-cursor-colors cursor bg))
      (setq my/cursor-colors-emacs (my/make-cursor-colors "#F0F" bg)))))

(add-hook 'enable-theme-functions 'my/after-theme-enabled)

;; When running as a daemon, we need to wait for a frame to exist before
;; trying to set up fonts / etc.
(if (not (daemonp))
    (dark-theme)
  (defun my/set-theme-for-new-frame ()
    (cond ((eq nil custom-enabled-themes) (dark-theme))
          ((eq (car custom-enabled-themes) my/dark-theme-name) (dark-theme))
          ((eq (car custom-enabled-themes) my/light-theme-name) (light-theme))))
  (add-hook 'server-after-make-frame-hook 'my/set-theme-for-new-frame))


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
(setq mouse-wheel-scroll-amount '(0.1
                                  ((shift) . 0.9)
                                  ((control meta) . global-text-scale)
                                  ((control) . text-scale)
                                  ((meta) . hscroll)))
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

(remove-hook 'after-init-hook 'window-divider-mode)
;; (set-face-attribute 'window-divider-first-pixel nil :foreground "#FFFFFF")
;; (set-face-attribute 'window-divider-last-pixel nil :foreground "#FFFFFF")
;; (setopt window-divider-default-right-width 3)


;;;; Cursor customization
;;;; ======================================================================

(setq blink-cursor-blinks 0)
(setq blink-cursor-delay 0.1)
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

(defun my/get-cursor-colors ()
  (if (and (boundp 'evil-state) (eq evil-state 'emacs))
      my/cursor-colors-emacs
    my/cursor-colors-normal))

(defvar my/cursor-color-idx 0)
(defun my/get-cursor-color () (nth my/cursor-color-idx (my/get-cursor-colors)))

(defvar my/underline-ov nil)

(defun my/update-underline-color ()
  (when (overlayp my/underline-ov)
    (overlay-put my/underline-ov 'face
                 `(:underline (:color ,(my/get-cursor-color) :position 0)))
    (overlay-put my/underline-ov 'window (selected-window))))

(defun my/cursor-over-image? ()
  (let ((prop (get-text-property (point) 'display)))
    (eq 'image (car-safe prop))))

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
            (when (overlayp my/underline-ov) (delete-overlay my/underline-ov))
          (unless (overlayp my/underline-ov)
            (setq my/underline-ov (make-overlay (point) (point))))
          (let ((start (point)))
            (save-excursion
              (forward-char)
              (move-overlay my/underline-ov start (point) (current-buffer))
              (my/update-underline-color))))
        t)
      ;; If we had any errors, just delete the overlay ;entirely to try again
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
  (setq my/cursor-color-idx (% (1+ my/cursor-color-idx)
                               (length (my/get-cursor-colors))))
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
(add-hook 'after-init-hook 'undelete-frame-mode)
;; Even though minimal-emacs.d tries to add this, (display-graphic-p) will
;; return nil when running as a daemon, preventing it from adding the hook.
(add-hook 'after-init-hook 'context-menu-mode)
;; TODO: remove some of these symbols, like "or", and re-enable
;; (add-hook 'prog-mode-hook 'global-prettify-symbols-mode)


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
  (face-remap-add-relative 'default '(:height 120 :weight normal))
  (face-remap-add-relative 'variable-pitch '(:height 110))
  (face-remap-add-relative 'fixed-pitch '(:height 120))
  (face-remap-add-relative 'fixed-pitch-serif '(:height 120))
  (face-remap-add-relative 'header-line '(:height 120))
  (face-remap-add-relative 'form-feed-st-line
                           '(:inherit form-feed-line-small)))

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

;; FIXME: This is breaking describe-variable sometimes? Maybe it's not this,
;; but pp--region is giving me random 'End of file during parsing' errors.
;; (defun my/temp-buffer-view-mode (&rest args)
;;   "Make pretty-printed buffers use view-mode, be read-only, and easily
;; closable. I don't know what else uses with-output-to-temp-buffer so I'm
;; matching against the buffer name for now."
;;   (when (string-prefix-p "*Pp" (buffer-name))
;;     (view-mode-enter nil 'kill-buffer-if-not-modified)))
;; (add-hook 'temp-buffer-show-hook 'my/temp-buffer-view-mode)

;;;; Other commands and functions that need early definitions
;;;; ======================================================================

;; These are intended to be redefined to whatever actual buffer switching
;; functions I decide to use.
(defun my/switch-to-next-buffer ()
  (interactive)
  (switch-to-next-buffer))

(defun my/switch-to-prev-buffer ()
  (interactive)
  (switch-to-prev-buffer))

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
  (evil-ex-substitute-matches ((t (:inherit thick-strike-through))))
  (evil-ex-substitute-replacement ((t (:inherit replacement-box))))
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
  :general-config
  ('(insert emacs)
   "C-w" 'evil-window-map
   "C-S-w" 'evil-delete-backward-word)
  ('(normal visual) 'prog-mode-map "gc" 'my/evil-comment-or-uncomment)
  ('evil-window-map "o" 'evil-window-mru)
  ('insert 'prog-mode-map "<tab>" 'indent-for-tab-command)
  ('visual "<tab>" 'evil-shift-right
           "<backtab>" 'evil-shift-left)
  ('normal "<tab>" 'evil-shift-right-line
           "<backtab>" 'evil-shift-left-line))

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
  :custom-face
  (anzu-mode-line ((t (:foreground unspecified :inherit medium-weight))))
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
  ('(normal insert emacs) eshell-mode-map "C-c P" 'eat-send-password))


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
  :custom-face
  (vertico-current ((t (:inherit selected-item))))
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
   "C-c RET" 'vertico-exit-input))

(use-package orderless :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  :custom-face
  (orderless-match-face-0 ((t (:inherit (underline blue-fg)))))
  (orderless-match-face-1 ((t (:inherit (underline magenta-fg)))))
  (orderless-match-face-2 ((t (:inherit (underline green-fg)))))
  (orderless-match-face-3 ((t (:inherit (underline yellow-fg))))))

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
  :general
  ("C-c M-x" 'consult-mode-command
   "C-c h" 'consult-history
   "C-c k" 'consult-kmacro
   "C-c m" 'consult-man
   "C-c i" 'consult-info
   [remap Info-search] 'consult-info
   ;; C-x bindings in `ctl-x-map'
   "C-x M-:" 'consult-complex-command
   "C-x b" 'consult-buffer
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

(use-package corfu :ensure t
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :general
  ('emacs
   "C-S-SPC" 'set-mark-command)
  ('(insert emacs)
   "C-SPC" 'completion-at-point)        ; for when tab isn't usable
  ('(emacs insert) 'corfu-map
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
  (corfu-default ((t (:inherit default-bg))))
  (corfu-current ((t (:inherit selected-item))))
  (corfu-border ((t (:inherit popup-border))))
  (corfu-popupinfo ((t (:inherit popup-bg))))
  )

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
  :custom-face
  ;; strike-through was giving a weird artifact, but this works pretty well
  (form-feed-st-line ((t (:strike-through unspecified
                          :inherit form-feed-line-normal)))))


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
                                        :inherit (default-fg medium-weight)))))
  (rainbow-delimiters-depth-2-face ((t (:foreground unspecified
                                        :inherit (default-fg light-weight)))))
  (rainbow-delimiters-depth-3-face ((t (:foreground unspecified
                                        :inherit (default-fg extralight-weight))))))

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
(use-package vimrc-mode :ensure t :mode "[._]?g?vim\\(rc\\)?")

(use-package fennel-mode :ensure t
  :mode "\\.fnl\\'"
  :config
  (add-hook 'fennel-mode-hook 'my/lisp-word-syntax))

(use-package slime :ensure t
  :config (setq inferior-lisp-program "sbcl"))

(use-package pdf-tools :ensure t :defer t
  :if (not (eq system-type 'windows-nt))
  :commands pdf-view-mode
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :config (pdf-loader-install))

(use-package org-superstar :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom (org-superstar-special-todo-items t))

(use-package org-variable-pitch :ensure t
  :hook (org-mode . org-variable-pitch-setup))

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
  (minions-mode-line-face 'variable-pitch)
  (minions-mode-line-lighter " = ")
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
  (markdown-list-item-bullets '("•" "‣" "‧" "╴"))
  :custom-face
  (markdown-code-face ((t (:background unspecified
                           :inherit (normal-weight fixed-pitch))))))

(use-package devdocs :ensure t
  :commands devdocs-lookup
  :bind ("C-h D" . devdocs-lookup))


(use-package consult-dash :ensure t
  :bind ("C-h C-d" . consult-dash)
  )

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
                        broken-treesit-auto))

  )

;; Note: install fd for faster file operations (package is named "fd-find" in apt/dnf)
(use-package projectile :ensure t :demand t
  :custom (consult-project-function 'projectile-project-root)
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
    :custom-face
    (which-key-posframe-border ((((background light)) :background "#EEE")
                                (((background dark)) :background "#777")))
    :custom
    (which-key-posframe-font "Iosevka Term Slab 12")
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
   "C-p" 'eshell-previous-matching-input-from-input
   "C-n" 'eshell-next-matching-input-from-input))

(use-package winner-mode :ensure nil
  :config (winner-mode)
  :general (:keymaps 'evil-window-map            ; C-w prefix
            "u" 'winner-undo
            "C-r" 'winner-redo)
  :custom (winner-dont-bind-my-keys t))

;; FIXME: Maybe this setting is what's breaking describe-variable
;; (use-package pp :ensure nil
;;   :custom (pp-default-function 'pp-emacs-lisp-code))

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
    ;; The default doesn't take font size into account, scale appropriately
    (defun which-key-posframe--max-dimensions (_)
      (cons (- (truncate (frame-pixel-height) 12) 2)
            (truncate (frame-pixel-width) 9)))))

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

(use-package compile :ensure nil
  :bind (:map compilation-mode-map
         ("C-j" . my/switch-to-next-buffer)
         ("C-k" . my/switch-to-prev-buffer)
         ("<normal-state> C-j" . my/switch-to-next-buffer)
         ("<normal-state> C-k" . my/switch-to-prev-buffer)))

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
  (org-blank-before-new-entry '((heading . auto) (plain-list-item . nil)))
  :custom-face
  (org-block-begin-line ((((background light)) :background "#F5F5F5")
                         (((background dark)) :background "#363E4C")))
  :config
  (unless (file-exists-p org-directory) (make-directory org-directory t))
  (setq org-default-notes-file
        (concat (file-name-as-directory org-directory) "notes"))
  :general-config
  ('(normal insert) 'org-mode-map
   "<backtab>" 'org-shifttab)
  ('normal 'org-mode-map
           "C-j" 'my/switch-to-next-buffer
           "C-k" 'my/switch-to-prev-buffer))

(use-package shr :ensure nil :defer t
  :custom-face
  (shr-code ((t (:inherit (medium-weight fixed-pitch-serif)))))
  (shr-text ((t (:inherit variable-pitch))))
  (shr-h1 ((t (:height 1.50 :inherit (no-slant bold underline shr-text)))))
  (shr-h2 ((t (:height 1.45 :inherit (no-slant bold shr-text)))))
  (shr-h3 ((t (:height 1.30 :inherit (no-slant bold shr-text)))))
  (shr-h4 ((t (:height 1.20 :inherit (no-slant medium-weight shr-text)))))
  (shr-h5 ((t (:height 1.15 :inherit (no-slant normal-weight shr-text)))))
  (shr-h6 ((t (:height 1.10 :inherit (oblique normal-weight shr-text))))))

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
                    tab-bar-format-add-tab
                    tab-bar-format-align-right
                    tab-bar-format-global))
  (tab-bar-close-button-show nil)
  (tab-bar-select-restore-windows nil)
  (tab-bar-show 1)
  (tab-bar-auto-width nil)
  :custom-face
  (tab-bar ((t (:background unspecified :foreground unspecified
                :underline unspecified :box unspecified
                :height 120 :inherit tabs-bar))))
  (tab-bar-tab ((t (:background unspecified :foreground unspecified
                    :underline unspecified :box unspecified
                    :height 130 :box unspecified :inherit tabs-active))))
  (tab-bar-tab-inactive ((t (:background unspecified :foreground unspecified
                             :underline unspecified :box unspecified
                             :height 130 :inherit tabs-inactive))))
  :config
  ;; This doesn't work for format-spaces because the text properties are overwritten
  (defun my/make-pixel-spacer (px &rest props)
    (apply #'propertize " " 'display `(space :width (,px)) props))
  (defun my/tab-bar-tab-name-format-spaces (name tab number)
    (concat "   " name "   "))
  (setopt tab-bar-tab-name-format-functions '(tab-bar-tab-name-format-hints
                                              tab-bar-tab-name-format-close-button
                                              my/tab-bar-tab-name-format-spaces
                                              tab-bar-tab-name-format-face))
  (setq tab-bar-separator (my/make-pixel-spacer 1 'face '(:inherit dark-border)))
  (tab-bar-mode t))

(use-package tab-line :ensure nil
  :init
  (defun my/tab-line-tab-name (buf &optional _)
    (my/tab-bar-tab-name-format-spaces (buffer-name buf) nil nil))
  :custom
  (tab-line-close-button-show nil)
  (tab-line-new-button-show nil)
  (tab-line-tab-face-functions nil)
  (tab-line-tab-name-function 'my/tab-line-tab-name)
  :custom-face
  (tab-line ((t (:background unspecified :foreground unspecified
                 :underline unspecified :box unspecified
                 :inherit tabs-bar-overline :height 100))))
  (tab-line-tab ((t (:background unspecified :foreground unspecified
                     :underline unspecified :box unspecified
                     :inherit tabs-active :height 100))))
  (tab-line-tab-current ((t (:background unspecified :foreground unspecified
                             :underline unspecified :box unspecified
                             :inherit tabs-active :height 100))))
  (tab-line-tab-inactive ((t (:background unspecified :foreground unspecified
                              :underline unspecified :box unspecified
                              :inherit tabs-inactive :height 100))))
  (tab-line-highlight ((t (:background unspecified :foreground unspecified
                           :underline unspecified :box unspecified
                           :overline nil :inherit unspecified))))
  :config
  (setq tab-line-separator
        (my/make-pixel-spacer
         1 'face '(:inherit ((:height 100) dark-border)))))

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

(general-add-hook '( evil-collection-eldoc-doc-buffer-mode-hook eww-mode-hook
                     help-mode-hook Custom-mode-hook messages-buffer-mode-hook)
                  'my/no-mode-line)

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
        (_ 'default-bg))
    'mode-line-inactive))

(defun my/spacer (face width height)
  `(:propertize " "
    face ,face
    display (space :width ,width :height ,height)))

(defun my/color-spacer (color width height)
  (my/spacer `(:background ,color :family "Iosevka") width height))

(defun my/inactive-left ()
  (if (mode-line-window-selected-p)
      ""
    `(,(my/spacer '((:family "Iosevka") popup-border) 0.5 1.25) " ")))

;; TODO: Memoize colors if it causes slowdown
(defun my/gradient-spacer (from-col to-col width height)
  `(,(my/color-spacer (my/blend from-col to-col 0.25) width height)
    ,(my/color-spacer (my/blend from-col to-col 0.5) width height)
    ,(my/color-spacer (my/blend from-col to-col 0.75) width height)))

;; Note that these assume they're being used in the selected mode line
(defun my/left-gradient (&optional width height)
  (let ((mode-line-color (face-attribute 'mode-line :background nil 'default))
        (vim-color (face-attribute (my/vim-color) :background)))
    (my/gradient-spacer vim-color mode-line-color
                        (or width 0.25)
                        (or height 1.25))))

(defun my/right-gradient (&optional width height)
  (let ((mode-line-color (face-attribute 'mode-line :background nil 'default))
        (vim-color (face-attribute (my/vim-color) :background)))
    (my/gradient-spacer mode-line-color vim-color
                        (or width 0.25)
                        (or height 1.25))))

(defun my/vim-state ()
  (if (mode-line-window-selected-p)
      (let ((mode-text (concat " " (upcase (symbol-name evil-state)) " ")))
        `((:propertize ,mode-text
           face
           (:inherit (,(my/vim-color) default-fg)
            :weight normal
            :family "Iosevka")
           display (raise 0.05)
           )
          ,(my/left-gradient)
          " "
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
  (if (mode-line-window-selected-p)
      `(:eval (butlast minions-mode-line-modes 3))
    `(:eval (butlast minions-mode-line-modes 4))))

(defun my/modeline-project ()
  (let ((home (concat (getenv "HOME") "/")))
    (unless (or (string= home (projectile-project-p))
                (string= "-" (projectile-project-name)))
      `(" "
        (:propertize (:eval (concat (projectile-project-name) " "))
         face (:family "Noto Sans"
               :weight normal
               :height 0.8 :inherit faded)
         mouse-face highlight
         help-echo ,(projectile-project-p)
         display (raise 0.075)
         pointer-shape arrow
         keymap ,(make-mode-line-mouse-map 'mouse-1
                                           #'projectile-mode-menu))
        " "))))

(defun my/modeline-buffer-name ()
  `(:propertize (:eval (buffer-name))
    face ,(if (mode-line-window-selected-p)
              `((:weight normal)
                ,@(when (and (buffer-file-name) (buffer-modified-p))
                    '(italic))
                mode-line-buffer-id)
            `((:weight light)
              ,@(when (and (buffer-file-name) (buffer-modified-p))
                  '(italic))))
    display (raise 0.05)))

(defun my/propertize-position (pos)
  `(,(my/right-gradient)
    (:propertize ,pos
     face (:inherit (,(my/vim-color) default-fg) :family "Iosevka" :weight normal)
     display (raise 0.05)
     )))

(defun my/modeline-position-default ()
  (my/propertize-position '( " %3l : %2C  ")))

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
          (propertize (concat " " search-info)
                      'display '(raise 0.05))))
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
        (propertize
         (concat (or view-str read-only-str)
                 (if (or modified (not buffer-read-only)) modified-str ""))
         'face `(:inherit ,(if modified 'error 'shadow))
         'display '(raise 0.05)
         ))
    ""))

;; Note: If this starts getting slow, a dummy :eval at the start that caches
;; some of the faces / strings / etc could help speed things up.
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
                (:eval (my/modeline-project))
                (:eval (my/modeline-fly))
                (:eval (my/modeline-modes))
                (:eval (my/modeline-position))
                (:eval (if (mode-line-window-selected-p) "" "   "))))

;; In daemon mode, the messages buffer is created too early to get the
;; mode line changes.
(add-hook 'messages-buffer-mode-hook 'my/smaller-fonts)
(with-current-buffer (messages-buffer)
  (my/no-mode-line)
  (my/smaller-fonts))


;;;; Other keybinds
;;;; ======================================================================

(general-define-key
 "C-x k" 'kill-current-buffer
 "<mode-line> <mouse-2>" 'mouse-delete-window
 "<mode-line> <mouse-3>" 'mouse-buffer-menu)

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

(general-def "<f5>" 'compile-interactively)

(keymap-global-set "C-x C-m" 'pp-macroexpand-last-sexp)

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
(general-def '(minibuffer-local-map minibuffer-local-ns-map
               minibuffer-local-completion-map minibuffer-local-must-match-map
               minibuffer-local-isearch-map)
  "<escape>" 'keyboard-escape-quit)

;; I want to be able to quit any prefix sequence with escape
(dolist (keys (list "<escape>" "C-M-g" "C-h <escape>" "C-c <escape>"
                    "C-x <escape>" "C-c p <escape>" "C-x p <escape>"
                    "M-s <escape>" "M-g <escape>"))
  (general-def keys 'keyboard-escape-quit))

(defun my/keyboard-escape-quit-advice (fn)
  "Prevents Escape from messing with splits, and also exits emacs-state when
pressed twice in a row."
  (if (and (eq last-command 'keyboard-escape-quit)
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

;; It's hard to exclude matches with a regexp, so these include an explicit
;; check for *scratch* so that we treat it like a non-special buffer. Otherwise
;; switch-to-prev-buffer-skip-regexp could have done the job.
(defun my/match-special-buffers (buf &rest _)
  (let ((buf-name (buffer-name (get-buffer buf))))
    (and (string-prefix-p "*" buf-name)
         (not (string= "*scratch*" buf-name)))))

(defun my/match-non-special-buffers (buf &rest _)
  (let ((buf-name (buffer-name (get-buffer buf))))
    (or (string-match-p "\\`[^* ]" buf-name)
        (string= "*scratch*" buf-name))))

(defun my/switch-to-prev-buffer-skip (win target-buf bury-or-kill)
  (let ((side? (window-parameter win 'window-side)))
    (if side?
        (my/match-non-special-buffers target-buf)
      (my/match-special-buffers target-buf))))

(setq switch-to-prev-buffer-skip #'my/switch-to-prev-buffer-skip)

(defun my/one-time-hook (hook fn)
  (letrec ((one-shot (lambda ()
                       (ignore-errors
                         (remove-hook hook one-shot t)
                         (funcall fn)))))
    (ignore-errors (add-hook hook one-shot t))))

(defun my/side-window-body-fn (win)
  ;; Some modes clear all local variables after body-function runs.
  ;; Deferring the customization until the end of the current command
  ;; ensures that all modes get the tab line.
  (my/one-time-hook 'post-command-hook
                    (lambda () (with-selected-window win
                                 (tab-line-mode t)))))

(defun my/main-window-body-fn (win)
  ;; Leaving this in just in case it turns out I need to run something here
  t)


;; TODO: Advise customize functions so they stop hijacking the current window
(setq display-buffer-alist
      (let* ((bot-common '(display-buffer-in-side-window
                           (side . bottom) (preserve-size . (nil . t))
                           (body-function . my/side-window-body-fn)))
             (bot-left-window `(,@bot-common (slot . -1)))
             (bot-right-window `(,@bot-common (slot . 1)))
             (bot-left-rx (rx bos
                              (| " " "*" " *")
                              (| "Help" "Custom" "info" "eldoc" "Occur"
                                 "grep" "devdocs" "Pp" "eww")))
             (bot-right-rx (rx bos
                               (| " " "*" " *")
                               (| (regex "[e]?shell") (regex "[v]?term")
                                  (regex ".*[Rr][Ee][Pp][Ll].*")
                                  "compilation" "lua" "Python" "inferior")))
             (bot-left-modes '(dired-mode eww-mode))
             (bot-right-modes '(comint-mode))
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
          (body-function . my/main-window-body-fn))
         (my/match-special-buffers
          (display-buffer-in-side-window display-buffer-no-window)
          (body-function . my/side-window-body-fn)
          (side . bottom))
         )))

(when custom-file
  (load custom-file))

