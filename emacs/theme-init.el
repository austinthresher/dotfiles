;; -*- lexical-binding: t -*-

;;;; Macros and helper functions that are useful for taming community themes.
;;;; Currently not necessary while using my own theme.

;;;; Add a hook that runs after a theme is loaded or enabled

(defvar after-load-theme-hook nil)
(defadvice load-theme (after run-after-load-theme-hook activate)
  (run-hooks 'after-load-theme-hook))
(defadvice enable-theme (after run-after-enable-theme-hook activate)
  (run-hooks 'after-load-theme-hook))


;;;; Prep fonts to be loaded after any theme

;; Windows doesn't respect font weight settings, requiring a different family instead
(defface my/default-light '((t (:family "Iosevka NFP Light" :inherit (default)))) "Light default font")

(defun my/set-fonts ()
  (set-face-font 'default "Iosevka NFP-12")
  (set-face-font 'fixed-pitch "IosevkaTermSlab NFP-12:weight=normal")
  (set-face-font 'fixed-pitch-serif "IosevkaTermSlab NFP-12:weight=normal")
  (set-face-font 'variable-pitch "Roboto Condensed-11:weight=semilight"))
(add-hook 'after-load-theme-hook 'my/set-fonts)


;;;; Helpers for assigning theme-relative colors

(defun color-blend (src dst amt)
  "`SRC' and `DST' are color strings. `AMT' should be from 0.0 to 1.0"
  (require 'cl-lib)
  (let* ((c-src (color-name-to-rgb src))
         (c-dst (color-name-to-rgb dst))
         (inv-amt (- 1.0 amt))
         (vals (cl-mapcar (lambda (s d)
                         (color-clamp (+ (* s inv-amt) (* d amt))))
                       c-src c-dst))
         (r (truncate (* 255 (nth 0 vals))))
         (g (truncate (* 255 (nth 1 vals))))
         (b (truncate (* 255 (nth 2 vals)))))
    (format "#%02x%02x%02x" r g b)))


(defun copy-face-attribute (dst-face dst-attr src-face src-attr &optional blend-amt)
  "Positive blend amount moves towards default :foreground, negative towards :background.
Supports non-color attributes as long as blend-amt is nil"
  (let ((val (or (face-attribute src-face src-attr) 'unspecified)))
    (when (and blend-amt (stringp val))
      (cond ((> blend-amt 0)
             (setq val (color-blend val (face-attribute 'default :foreground)
                                    blend-amt)))
            ((< blend-amt 0)
             (setq val (color-blend val (face-attribute 'default :background)
                                    (- blend-amt))))))
    (set-face-attribute dst-face nil dst-attr val)))

;; TODO? This could include a theme argument to support switching between multiple
;; themes with different linked faces
(defmacro link-face-attribute (dst-face dst-attr src-face src-attr &optional blend-amt)
  "Automatically applies linked attributes on theme change.
Example usage:
  (link-face-attribute window-divider-first-pixel :foreground default :background)"
    (let* ((args (mapcar #'eval (list dst-face dst-attr src-face src-attr blend-amt)))
           (fn-name (intern (apply #'format "link--%s%s-to-%s%s"
                                   (mapcar #'symbol-name (take 4 args))))))
      `(progn (defun ,fn-name ()
                (copy-face-attribute ,@(mapcar (lambda (x) `(quote ,x)) args)))
              (add-hook 'after-load-theme-hook (quote ,fn-name)))))

(defmacro link-face-attributes (&rest args)
  (let ((current (take 4 args))
        (rest (nthcdr 4 args))
        stmts)
    (while current
      (push `(link-face-attribute ,@current) stmts)
      (setq current (take 4 rest))
      (setq rest (nthcdr 4 rest)))
    `(progn ,@stmts)))


;;;; Macro + helpers for easily remapping faces in specific modes

(defun define-mode-face--build-defun (face-name modes target-faces)
  (let* ((target-names (string-join (mapcar #'symbol-name target-faces) "-"))
         (fn-name (intern (concat "remap--" (symbol-name face-name) "--"
                                  target-names))))
    `(defun ,fn-name (&rest _)
       ,@(mapcar (lambda (target)
                   `(face-remap-add-relative (quote ,target)(quote ,face-name)))
                 target-faces))))

(defun define-mode-face--build-hooks (modes fn-name)
  (let* ((mode-names (mapcar #'symbol-name modes))
         (hook-names (mapcar (apply-partially #'format "%s-hook") mode-names))
         (hooks (mapcar #'intern hook-names)))
    (mapcar (lambda (hook) `(add-hook (quote ,hook) (quote ,fn-name))) hooks)))

(defun define-mode-face--build-entry (face-name alist-entry)
  (let* ((modes (ensure-list (car alist-entry)))
         (target-faces (cdr alist-entry))
         (fn-def (define-mode-face--build-defun face-name modes target-faces))
         (fn-name (cadr fn-def))
         (hooks (define-mode-face--build-hooks modes fn-name)))
    `(progn ,fn-def ,@hooks)))

(defmacro define-mode-face (name mode-face-alist defface-args)
  "Automates the process of defining a face and then remapping a buffer-local face in certain modes.
Example usage:
  (define-mode-face my/shell-face
    '(((shell-mode eshell-mode) default fringe)
      (eat-mode default))
    '((t (:background \"black\" :foreground \"white\" :inherit '()))))"
  (declare (indent defun))
  (cl-assert (symbolp name) t (format "define-mode-face: %S is not a symbol" name))
  (let ((mfa (eval mode-face-alist)))
    `(progn
       (defface ,name nil
         ,(format "Generated by define-mode-face for these modes: %S"
                  (flatten-list (mapcar #'car mfa))))
       (face-spec-set (quote ,name) ,defface-args) ; Allow overwriting
       ,@(mapcar (apply-partially #'define-mode-face--build-entry name) mfa)
       (quote ,name))))


;;;; Mode-specific face changes

;; (define-mode-face my/shell-face '(((eshell-mode shell-mode) fringe default))
;;   '((t (:background "gray30"))))

(define-mode-face my/variable-face
  '((Info-mode
     default header-line header-line-highlight)
    ((markdown-mode gfm-mode markdown-view-mode gfm-view-mode)
     default markdown-blockquote-face)
    )
  '((t (:inherit (variable-pitch)))))

(link-face-attributes
 'window-divider-first-pixel :foreground 'default :background
 'window-divider-last-pixel :foreground 'default :background
 'tab-line-tab-current :background 'default :background
 'tab-line-tab-current :foreground 'default :foreground
 'tab-line-tab :background 'default :background
 'tab-line-tab :foreground 'default :foreground
 'tab-line-tab :box 'default :box
 'tab-line-tab-inactive :box 'default :box
 'tab-line-highlight :box 'default :box
 'tab-line-tab-current :box 'default :box
 'tab-line :box 'default :box
 'font-lock-comment-face :family 'my/default-light :family
 'font-lock-comment-delimiter-face :family 'my/default-light :family
 'font-lock-doc-face :family 'my/default-light :family
 'mode-line :family 'variable-pitch :family
 'mode-line-inactive :family 'variable-pitch :family
 'tab-line-tab-current :family 'variable-pitch :family
 'tab-line-tab :family 'variable-pitch :family
 'tab-line-tab-inactive :family 'variable-pitch :family
 'fringe :background 'default :background
 'tab-line :family 'variable-pitch :family
 )

(link-face-attribute 'tab-line-tab-inactive :background 'default :background 0.5)
(link-face-attribute 'tab-line-tab-inactive :foreground 'default :foreground -0.1)
(link-face-attribute 'tab-line :background 'default :background 0.25)



;;;; Indicate when the minibuffer is active

(defface my/minibuffer-unfocused nil "Text when minibuffer is unfocused")
(defface my/minibuffer-prompt-unfocused nil "Prompt when minibuffer is unfocused")
(face-spec-set 'my/minibuffer-unfocused '((t (:foreground "grey60" :background unspecified))))
(face-spec-set 'my/minibuffer-prompt-unfocused
               '((t (:foreground "grey80" :background unspecified) :weight normal)))

(defun my/highlight-minibuffer-when-active (&rest _)
  (cond ((minibuffer-window-active-p (selected-window))
         (face-remap-set-base 'marginalia-key 'marginalia-key)
         (face-remap-set-base 'minibuffer-prompt 'minibuffer-prompt)
         (face-remap-set-base 'fringe 'fringe)
         (face-remap-set-base 'default 'default)
         (face-remap-set-base 'completions-annotations 'completions-annotations))
        (t
         (when-let ((win (minibuffer-window)))
           (with-selected-window win
             (face-remap-set-base 'marginalia-key 'my/minibuffer-unfocused)
             (face-remap-set-base 'minibuffer-prompt 'my/minibuffer-prompt-unfocused)
             (face-remap-set-base 'fringe 'my/minibuffer-unfocused)
             (face-remap-set-base 'default 'my/minibuffer-unfocused)
             (face-remap-set-base 'completions-annotations 'my/minibuffer-unfocused))))))
(add-hook 'window-selection-change-functions 'my/highlight-minibuffer-when-active)

;; (load-theme 'leuven t)
