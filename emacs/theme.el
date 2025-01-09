;; NOTE: Not actually using this right now, it's way too much of a mess.
;; Dig through and figure out what's worth keeping later.


(require 'color)
(require 'ansi-color)

;;;; Fonts and fallbacks
;;;; ======================================================================




;;;; Colors
;;;; ======================================================================

(defconst my/term-color-alist
  '(("black" . "black") ("red" . "red4")
    ("green" . "green4") ("yellow" . "yellow4")
    ("blue" . "blue4") ("magenta" . "magenta4")
    ("cyan" . "cyan4") ("white" . "grey")
    ("brightblack" . "dim grey") ("brightred" . "red1")
    ("brightgreen" . "green1") ("brightyellow" . "yellow1")
    ("brightblue" . "blue1") ("brightmagenta" . "magenta1")
    ("brightcyan" . "cyan1") ("brightwhite" . "white"))
  "Mapping of 16 terminal colors to X11 colors.")

(defun my/xterm-color (idx)
  "Get RGB hex string from an XTerm color index. Modified from eat.el."
  (setq idx (mod idx 256))
  (cond ((<= idx 15) (cdr (nth idx my/term-color-alist)))
        ((<= idx 231) (let ((col (- idx 16)) (res 0) (frac (* 6 6)))
                        (while (<= 1 frac)
                          (setq res (* res #x000100))
                          (let ((color-num (mod (/ col frac) 6)))
                            (unless (zerop color-num)
                              (setq res (+ res #x37 (* #x28 color-num)))))
                          (setq frac (/ frac 6)))
                        (format "#%06X" res)))
        (t (format "#%06X" (* #x010101 (+ 8 (* 10 (- idx 232))))))))

(defvar my/seen-unknown-colors '())
(defun my/color->rgb (color)
  "Wrapper around `color-name-to-rgb' that attempts to handle invalid color
names gracefully. Probably not terminal compatible."
  (or (color-name-to-rgb color)
      ;; Maybe it's a hex triplet missing the #
      (and (eq 0 (mod (length color) 3)) (color-values (concat "#" color)))
      ;; Handle colors that are specified as terminal colors
      (when-let* ((color-name (cdr-safe (assoc color my/term-color-alist))))
        (color-values color-name))
      ;; Handle XTerm 256 color palette indices
      (and (string-prefix-p "color-" color)
           (color-values (my/xterm-color (string-to-number (substring color 6)))))
      ;; Dummy entry to record any colors that haven't matched a previous option
      (and (add-to-list 'my/seen-unknown-colors color) nil)
      ;; If we still haven't found a color, resort to brute force. Find the most
      ;; similar name in the list of valid colors. Slow, but this shouldn't happen
      ;; often, and only when loading a theme. Print a message so it's obvious if
      ;; this is happening constantly.
      (color-values
       (let ((best-dist 999) (best-color "grey"))
         (dolist (maybe-this-color (defined-colors))
           (let ((this-dist (string-distance color maybe-this-color)))
             (when (< this-dist best-dist)
               (setq best-dist this-dist
                     best-color maybe-this-color))))
         (message "selected '%s' as closest match for unknown color '%s'" best-color color)
         best-color))))

;; ported from colorsys.py
(defun my/hsv->rgb (h s v)
  (let ((s (color-clamp s)))
    (if (= s 0.0)
        (list v v v)
      (let* ((i (truncate (* h 6.0)))
             (f (- (* h 6.0) i))
             (p (* v (- 1.0 s)))
             (q (* v (- 1.0 (* s f))))
             (u (* v (- 1.0 (* s (- 1.0 f)))))
             (i (% i 6)))
        (pcase i
          (0 (list v u p))
          (1 (list q v p))
          (2 (list p v u))
          (3 (list p q v))
          (4 (list u p v))
          (5 (list v p q)))))))

;; All colors are strings and `alpha' is a float from 0 to 1.
(defun my/blend (hex-from hex-to alpha)
  "Interpolates between `hex-from' and `hex-to'. Returns `hex-from' when
`alpha' is 0 and returns `hex-to' when `alpha' is 1. `alpha' can be outside
the range of 0-1, in which case the resulting color is extrapolated."
  (let ((from (my/color->rgb hex-from))
        (to (my/color->rgb hex-to)))
    (seq-let (r g b) (mapcar #'color-clamp (color-blend to from alpha))
      (color-rgb-to-hex r g b 2))))
(defun my/darken (hex-color alpha) (my/blend hex-color "#000" alpha))
(defun my/lighten (hex-color alpha) (my/blend hex-color "#FFF" alpha))
;; color-saturate-hsl feels like it's far too sensitive to small arguments.
;; This version lerps the saturation from the current value instead of just
;; adding / subtracting to it.
(defun my/saturate (hex-color alpha)
  (seq-let (h s l) (->> hex-color my/color->rgb (apply #'color-rgb-to-hsl))
    (let ((s (color-clamp (+ alpha (* s (- 1.0 alpha))))))
      (seq-let (r g b) (mapcar #'color-clamp (color-hsl-to-rgb h s l))
        (color-rgb-to-hex r g b 2)))))
(defun my/desaturate (hex-color alpha)
  (seq-let (h s l) (->> hex-color my/color->rgb (apply #'color-rgb-to-hsl))
    (let ((s (color-clamp (* s (- 1.0 alpha)))))
      (seq-let (r g b) (mapcar #'color-clamp (color-hsl-to-rgb h s l))
        (color-rgb-to-hex r g b 2)))))

(defun my/brighten (hex-color alpha)
  (seq-let (h s v) (->> hex-color my/color->rgb (apply #'color-rgb-to-hsv))
    (let ((v (color-clamp (+ alpha (* v (- 1.0 alpha))))))
      (seq-let (r g b) (mapcar #'color-clamp (my/hsv->rgb h s v))
        (color-rgb-to-hex r g b 2)))))

(defun my/rotate-hue (color delta)
  (seq-let (h s l) (apply #'color-rgb-to-hsl (my/color->rgb color))
    (let ((h (mod (+ h delta) 1.0)))
      (seq-let (r g b) (color-hsl-to-rgb h s l)
        (color-rgb-to-hex r g b 2)))))
(defun my/hue (color)
  (nth 0 (apply #'color-rgb-to-hsl (my/color->rgb color))))
(defun my/saturation (color)
  (nth 1 (apply #'color-rgb-to-hsl (my/color->rgb color))))
(defun my/luminance (color)
  (nth 2 (apply #'color-rgb-to-hsl (my/color->rgb color))))
(defun my/brightness (color)
  (nth 2 (apply #'color-rgb-to-hsv (my/color->rgb color))))
(defun my/ignore-rest (first &rest _) first)
(defun my/with-luminance (color lum &optional reduce-fn)
  (let ((reduce-fn (or reduce-fn 'my/ignore-rest)))
    (seq-let (h s old-lum) (apply #'color-rgb-to-hsl (my/color->rgb color))
      (seq-let (r g b) (color-hsl-to-rgb h s (funcall reduce-fn lum old-lum))
        (color-rgb-to-hex r g b 2)))))
(defun my/with-hue (color hue &optional reduce-fn)
  (let ((reduce-fn (or reduce-fn 'my/ignore-rest)))
    (seq-let (old-hue s l) (apply #'color-rgb-to-hsl (my/color->rgb color))
      (seq-let (r g b) (color-hsl-to-rgb (funcall reduce-fn hue old-hue) s l)
        (color-rgb-to-hex r g b 2)))))
(defun my/with-saturation (color sat &optional reduce-fn)
  (let ((reduce-fn (or reduce-fn 'my/ignore-rest)))
    (seq-let (h old-sat l) (apply #'color-rgb-to-hsl (my/color->rgb color))
      (seq-let (r g b) (color-hsl-to-rgb h (funcall reduce-fn sat old-sat) l)
        (color-rgb-to-hex r g b 2)))))
(defun my/with-brightness (color val &optional reduce-fn)
  (let ((reduce-fn (or reduce-fn 'my/ignore-rest)))
    (seq-let (h s old-val) (apply #'color-rgb-to-hsv (my/color->rgb color))
      (seq-let (r g b) (my/hsv->rgb h s (funcall reduce-fn val old-val))
        (color-rgb-to-hex r g b 2)))))


;;;; Faces
;;;; ======================================================================

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

(defvar my/light-theme-name 'modus-operandi)
(defvar my/dark-theme-name 'doom-spacegrey)

(setq modus-themes-mixed-fonts t
      modus-themes-slanted-constructs t
      modus-themes-variable-pitch-ui t)
(setq modus-themes-common-palette-overrides modus-themes-preset-overrides-intense)

;; TODO: Do all themes follow this naming scheme? Make the symbol from the name if so
(require-theme 'modus-operandi-theme)
(require-theme 'doom-spacegrey-theme)

;; Each of these will be populated on theme load
(defvar my/light-theme-faces '())
(defvar my/dark-theme-faces '())

;; These need to exist so that the mode line doesn't get invalid faces.
;; They'll be overwritten on theme load.
(my/face 'default-fg :foreground 'reset)
(dolist (state-colors '((state-normal . ansi-color-blue)
                        (state-insert . ansi-color-green)
                        (state-visual . ansi-color-yellow)
                        (state-replace . ansi-color-red)
                        (state-operator . ansi-color-cyan)
                        (state-emacs . ansi-color-magenta)
                        (state-other . ansi-color-bright-black)))
  (my/face (car state-colors)
    :inherit `(default-fg ,(cdr state-colors))
    :foreground 'reset))

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

(defun my/get-bg-current (face)
  (if (eq 'light frame-background-mode)
      (my/get-bg-light face)
    (my/get-bg-dark face)))

(defun my/get-fg-current (face)
  (if (eq 'light frame-background-mode)
      (my/get-fg-light face)
    (my/get-fg-dark face)))

(defun my/face-fg (name source &optional default-light default-dark)
  "Create a new face that copies the foreground color from another face"
  (my/face name
    :light `(:foreground ,(my/get-fg-light source default-light))
    :dark `(:foreground ,(my/get-fg-dark source (or default-dark default-light)))))

(defun my/face-bg (name source &optional default-light default-dark)
  "Create a new face that copies the background color from another face"
  (my/face name
    :light `(:background ,(my/get-bg-light source default-light))
    :dark `(:background ,(my/get-bg-dark source (or default-dark default-light)))))

;; All theme colors will be tinted towards these colors by the given amount
(defconst my/light-fg-tint-color "dodger blue")
(defconst my/light-bg-tint-color my/light-fg-tint-color)
(defconst my/dark-fg-tint-color "#FFFFFF")
(defconst my/dark-bg-tint-color my/dark-fg-tint-color)
(defconst my/light-fg-tint-amount 0.05)
(defconst my/light-bg-tint-amount my/light-fg-tint-amount)
(defconst my/dark-fg-tint-amount 0.0)
(defconst my/dark-bg-tint-amount my/dark-fg-tint-amount)

;; These filter the value of `my/[light|dark]-[fg|bg]', which are used for the
;; default face. The values passed to each function are the unmodified values
;; provided by the theme.
(defun my/tweak-light-default-fg (fg bg) ; Nudge towards darker text
  ;; (my/blend (my/blend fg (my/with-luminance fg 0.25 #'min) 0.5)
  ;;           my/light-fg-tint-color
  ;;           my/light-fg-tint-amount)
  (my/with-luminance fg 0.1)
  )
(defun my/tweak-light-default-bg (fg bg) ; Force light themes to (nearly) white
  ;; (my/blend (my/lighten (my/desaturate bg 0.75) 0.75)
  ;;           my/light-bg-tint-color
  ;;           my/light-bg-tint-amount)
  "#FFF"
  )
(defun my/tweak-dark-default-fg (fg bg)
  (my/blend (my/with-saturation (my/blend fg (my/with-luminance fg 0.75) 0.75)
                                0.2
                                #'min)
            my/dark-fg-tint-color
            my/dark-fg-tint-amount))
(defun my/tweak-dark-default-bg (fg bg)
  (my/blend (my/blend bg
                      (my/with-saturation (my/with-luminance bg 0.15) 0.2 #'max)
                      0.66)
            my/dark-bg-tint-color
            my/dark-bg-tint-amount))


;; Assumes that the farther a color is from white or black, the more saturated
;; it should be.
(defun my/target-saturation (col)
  (min (* 1.2 (my/saturation col))
       (- 0.5 (abs (- 0.5 (my/luminance col))))))

;; Algorithmically tweak each face color. Note that at this point,
;; `my/[light|dark]-[fg|bg]' have their final values, so those can be used to
;; refer to the colors of the default face.
(defun my/tweak-light-fg (fg bg)
  ;; (let* ((target-sat (my/target-saturation fg))
  ;;        (col (my/with-saturation fg target-sat #'max)))
  ;;   (my/blend col (my/with-luminance col 0.3 #'min) 0.5))
  fg)
(defun my/tweak-light-bg (fg bg) bg)
(defun my/tweak-dark-fg (fg bg)
  (my/with-saturation (my/lighten (my/saturate fg (my/target-saturation fg)) 0.1)
                      0.4
                      #'min))
(defun my/tweak-dark-bg (fg bg)
  (my/saturate bg 0.25)
  ;;(my/darken (my/saturate (my/lighten bg 0.25) 0.25) 0.5)
  ;; (my/lighten
  ;;  (my/saturate (my/blend bg (my/with-luminance bg 0.2) 0.5) 0.1)
  ;;  0.05)
  )

(defun my/tweak-light-bg-tinted (fg bg)
  (my/blend (my/tweak-light-bg fg bg)
            my/light-bg-tint-color my/light-bg-tint-amount))
(defun my/tweak-light-fg-tinted (fg bg)
  (my/blend (my/tweak-light-fg fg bg)
            my/light-fg-tint-color my/light-fg-tint-amount))
(defun my/tweak-dark-bg-tinted (fg bg)
  (my/blend (my/tweak-dark-bg fg bg)
            my/dark-bg-tint-color my/dark-bg-tint-amount))
(defun my/tweak-dark-fg-tinted (fg bg)
  (my/blend (my/tweak-dark-fg fg bg)
            my/dark-fg-tint-color my/dark-fg-tint-amount))

;; TODO: Box color / underlines / etc?
(defun my/tweak-theme-faces ()
  (cl-flet ((tweak-fg (if (eq 'dark frame-background-mode)
                          #'my/tweak-dark-fg-tinted #'my/tweak-light-fg-tinted))
            (tweak-bg (if (eq 'dark frame-background-mode)
                          #'my/tweak-dark-bg-tinted #'my/tweak-light-bg-tinted)))
    (dolist (face (face-list))
      (unless (string-match-p
               (rx bos (| "eat-term-" "ansi-color-"
                          (: (or "default" "cursor") eos)))
               (symbol-name face))
        (let ((bg (face-attribute face :background))
              (fg (face-attribute face :foreground))
              ;; Ensure a valid value is passed as the reference color
              (inherited-bg (face-attribute face :background nil 'default))
              (inherited-fg (face-attribute face :foreground nil 'default)))
          (when (stringp bg)
            (set-face-attribute face nil
                                :background (tweak-bg inherited-fg bg)))
          (when (stringp fg)
            (set-face-attribute face nil
                                :foreground (tweak-fg fg inherited-bg))))))))

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
            my/dark-theme-faces (my/collect-faces dark-plist))
      ;; Apply modifications to the default face colors before other faces get
      ;; to use them as references. The original theme's colors can still be
      ;; accessed with `my/get-[fg|bg]-[dark|light]'
      (let ((dark-fg (my/get-fg-dark 'default))
            (dark-bg (my/get-bg-dark 'default))
            (light-fg (my/get-fg-light 'default))
            (light-bg (my/get-bg-light 'default)))
        (setq my/dark-fg (my/tweak-dark-default-fg dark-fg dark-bg)
              my/dark-bg (my/tweak-dark-default-bg dark-fg dark-bg)
              my/light-fg (my/tweak-light-default-fg light-fg light-bg)
              my/light-bg (my/tweak-light-default-bg light-fg light-bg))))))

(defun my/setup-faces ()
  (my/extract-theme-faces)
  (my/tweak-theme-faces)
  ;; For some reason, the default face specifically gives issues using my/face
  (face-spec-set
   'default
   `((default :family ,my/default-font-family :height ,my/font-height)
     (((background light)) :foreground ,my/light-fg :background ,my/light-bg)
     (((background dark)) :foreground ,my/dark-fg :background ,my/dark-bg)))
  (face-spec-set
   'default
   `((default :family ,my/default-font-family :height ,my/font-height)
     (((background light)) :foreground ,my/light-fg :background ,my/light-bg)
     (((background dark)) :foreground ,my/dark-fg :background ,my/dark-bg))
   'face-defface-spec
   )
  (my/face 'variable-pitch :family my/variable-font-family :height my/font-height)
  (my/face 'variable-pitch-text :inherit 'variable-pitch)
  (my/face 'fixed-pitch :family my/fixed-font-family :height my/font-height)
  (my/face 'fixed-pitch-serif :family my/fixed-font-family :height my/font-height)
  ;; This is kind of a mess
  (let ((bg (my/get-bg-current 'default))
        (fg (my/get-fg-current 'default)))
    (set-face-attribute 'mode-line nil
                        :family my/mode-line-font-family
                        :height my/mode-line-font-height
                        :weight 'light
                        ;; :overline (my/blend bg fg 0.5)
                        ;; :underline `(:position 0 :color ,(my/blend bg fg 0.5))
                        ;; :box `(:line-width (1 . 1)
                        ;;        :color ,(my/blend (my/saturate bg 0.5)
                        ;;                          (my/saturate fg 0.5)
                        ;;                          0.5))
      )

    (set-face-attribute 'mode-line-inactive nil
                        :family my/mode-line-font-family
                        :height my/mode-line-font-height :weight 'light
                        ;; :box `(:line-width (1 . 1)
                        ;;        :color ,(my/saturate (my/darken bg 0.2) 0.5))
      ))

  (my/face* 'fringe :bg bg)  ;; Never let themes set different fringe colors
  (my/face* 'vertical-border :fg (my/blend bg fg 0.2))
  (my/face* 'thin-mode-line :fg (my/darken bg 0.25) :bg (my/darken bg 0.05))
  ;; The stipple here is a simple checkerboard pixel pattern. Noticable, but
  ;; relatively unobtrusive.
  (my/face 'trailing-whitespace :fg "#844" :stipple '(8 2 "\xAA\x55"))
  (my/face* 'font-lock-punctuation-face :fg fg :weight 'light)
  (set-face-attribute 'font-lock-builtin-face nil :family my/fixed-font-family :weight 'medium)
  (set-face-attribute 'font-lock-keyword-face nil :family my/fixed-font-family :weight 'medium)
  (set-face-attribute 'font-lock-comment-face nil :weight 'light :slant 'unspecified)
  (set-face-attribute 'font-lock-doc-face nil :weight 'light :slant 'unspecified)
  (set-face-attribute 'font-lock-regexp-grouping-backslash nil :weight 'semibold)
  (set-face-attribute 'font-lock-regexp-grouping-construct nil :weight 'bold)
  (set-face-attribute 'font-lock-constant-face nil :family my/fixed-font-family)
  (set-face-attribute 'font-lock-escape-face nil
                      :foreground 'unspecified
                      :inherit '(font-lock-constant-face))
  (set-face-attribute 'font-lock-number-face nil
                      :foreground 'unspecified
                      :inherit '(font-lock-constant-face))
  (set-face-attribute 'font-lock-preprocessor-face nil :weight 'normal)
  (set-face-attribute 'header-line nil :weight 'normal)
  (my/face 'default-family :family my/default-font-family)
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

  (my/face 'smaller :height 120)
  (my/face 'normal-weight :weight 'normal)
  (my/face 'medium-weight :weight 'medium)
  (my/face 'light-weight :weight 'light)
  (my/face 'extralight-weight :weight 'extralight)
  (my/face 'no-slant :slant 'reset)
  (my/face 'oblique :slant 'oblique)
  (my/face* 'selected-item
    :bg (let ((contrasted-bg (my/blend bg fg -1.0)))
          (my/blend contrasted-bg fg 0.1))
    :box `(:line-width (1 . -1) :color ,(my/blend bg fg 0.5))
    :distant-foreground fg)


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
      :light `(:bg ,(my/blend (my/saturate (my/get-bg-light source light-default) 0.25)
                              my/light-bg
                              0.5))
      :dark `(:bg ,(my/blend (my/saturate (my/get-bg-dark source dark-default) 0.25)
                             my/dark-bg
                             0.5))))

  (my/face-blended-bg 'blue-blended-bg    'ansi-color-blue    "#4271AE" "#81A2BE")
  (my/face-blended-bg 'magenta-blended-bg 'ansi-color-magenta "#8959A8" "#B294BB")
  (my/face-blended-bg 'green-blended-bg   'ansi-color-green   "#718C00" "#B5BD68")
  (my/face-blended-bg 'yellow-blended-bg  'ansi-color-yellow  "#EAB700" "#F0C674")
  (my/face-blended-bg 'red-blended-bg     'ansi-color-red     "#C82829" "#CC6666")
  (my/face-blended-bg 'cyan-blended-bg    'ansi-color-cyan    "#3E999F" "#8ABEB7")

  (set-face-attribute 'state-normal nil
                      :inherit '(medium-weight blue-blended-bg fixed-pitch-serif))
  (set-face-attribute 'state-insert nil
                      :inherit '(medium-weight green-blended-bg fixed-pitch-serif))
  (set-face-attribute 'state-visual nil
                      :inherit '(medium-weight yellow-blended-bg fixed-pitch-serif))
  (set-face-attribute 'state-replace nil
                      :inherit '(medium-weight red-blended-bg fixed-pitch-serif))
  (set-face-attribute 'state-operator nil
                      :inherit '(medium-weight cyan-blended-bg fixed-pitch-serif))
  (set-face-attribute 'state-emacs nil
                      :inherit '(medium-weight magenta-blended-bg fixed-pitch-serif))

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


(defun my/theme-dark? (theme-name)
  (-> (--find (pcase it (`(theme-face default . ,_) t))
              (get theme-name 'theme-settings))
      -fourth-item
      face-spec-choose
      (plist-get :background)
      my/color->rgb
      color-dark-p))

(defun my/after-theme-enabled (theme-name)
  "Add this as a hook to `enable-theme-functions' to automatically update
faces when the theme changes (useful with `consult-theme'). Only runs when
the theme wasn't set with `light-theme' or `dark-theme'."
  (when (not (or (eq theme-name 'user)
                 (eq this-command 'light-theme)
                 (eq this-command 'dark-theme)))
    (require 'faces)
    (if (my/theme-dark? theme-name)
        (setq my/dark-theme-name theme-name
              frame-background-mode 'dark)
      (setq my/light-theme-name theme-name
            frame-background-mode 'light))
    (mapc 'frame-set-background-mode (frame-list))
    (remove-hook 'enable-theme-functions 'my/after-theme-enabled)
    (my/setup-faces)
    (add-hook 'enable-theme-functions 'my/after-theme-enabled)
    (let ((cursor (face-attribute 'cursor :background))
          (bg (face-attribute 'default :background)))
      (setq my/cursor-colors-normal (my/make-cursor-colors cursor bg))
      (setq my/cursor-colors-emacs (my/make-cursor-colors "#F0F" bg)))
    theme-name))

(add-hook 'enable-theme-functions 'my/after-theme-enabled)


;;;; Theme switching commands
;;;; ======================================================================

(defun light-theme ()
  "Activates a light-mode theme."
  (interactive)
  (disable-theme my/dark-theme-name)
  (enable-theme my/light-theme-name)
  (setq frame-background-mode 'light)
  (mapc 'frame-set-background-mode (frame-list))
  (my/setup-faces)
  (let ((cursor (my/get-bg-light 'cursor)))
    (setq my/cursor-colors-normal (my/make-cursor-colors cursor my/light-bg)
          my/cursor-colors-emacs (my/make-cursor-colors "#F0F" my/light-bg)))
  my/light-theme-name)

(defun dark-theme ()
  "Activates a dark-mode theme."
  (interactive)
  (disable-theme my/light-theme-name)
  (enable-theme my/dark-theme-name)
  (setq frame-background-mode 'dark)
  (mapc 'frame-set-background-mode (frame-list))
  (my/setup-faces)
  (let ((cursor (my/get-bg-dark 'cursor)))
    (setq my/cursor-colors-normal (my/make-cursor-colors cursor my/dark-bg)
          my/cursor-colors-emacs (my/make-cursor-colors "#F0F" my/dark-bg)))
  my/dark-theme-name)


;; When running as a daemon, we need to wait for a frame to exist before
;; trying to set up fonts / etc.
(if (not (daemonp))
    (add-hook 'after-init-hook 'dark-theme)
  (defun my/set-theme-for-new-frame ()
    (cond ((eq nil custom-enabled-themes) (dark-theme))
          ((eq (car custom-enabled-themes) my/dark-theme-name) (dark-theme))
          ((eq (car custom-enabled-themes) my/light-theme-name) (light-theme))))
  (add-hook 'server-after-make-frame-hook 'my/set-theme-for-new-frame))


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

