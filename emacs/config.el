;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;(setq user-full-name "John Doe"
;      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq confirm-kill-emacs nil)
(setq-default evil-shift-width 4)

; evil bindings
(map!
    :g "C-j" #'evil-next-buffer
    :g "C-k" #'evil-prev-buffer
    :g "M-j" #'evil-window-down
    :g "M-k" #'evil-window-up
    :g "M-h" #'evil-window-left
    :g "M-l" #'evil-window-right
    :g "M-p" #'+workspace/switch-left
    :g "M-n" #'+workspace/switch-right
    :g "<leader>bd" #'evil-delete-buffer
    )

(evil-define-key 'normal 'global [backtab] "<<")
(evil-define-key 'normal 'global [tab] ">>")
(evil-define-key 'visual 'global [backtab] "<")
(evil-define-key 'visual 'global [tab] ">")

(eval-after-load "vterm" '(progn
        (setq vterm-kill-buffer-on-exit t)
        (define-key vterm-mode-map (kbd "M-j") #'windmove-down)
        (define-key vterm-mode-map (kbd "M-k") #'windmove-up)
        (define-key vterm-mode-map (kbd "M-h") #'windmove-left)
        (define-key vterm-mode-map (kbd "M-l") #'windmove-right)
        (define-key vterm-mode-map (kbd "M-p") #'+workspace/switch-left)
        (define-key vterm-mode-map (kbd "M-n") #'+workspace/switch-right)
    ))

; Remap vterm to term
(defun term ()
  "Terminal emulator"
  (interactive)
  (vterm))
