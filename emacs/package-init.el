;; -*- lexical-binding: t -*-

(my/user-load "bootstrap-elpaca.el")
(add-hook 'elpaca-after-init-hook (apply-partially #'my/user-load "custom.el"))

(elpaca sublime-themes
  (load-theme 'mccarthy t)
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
  (dolist (face '(whitespace-tab whitespace-big-indent whitespace-trailing))
    (face-spec-set face '((t (:background "misty rose" :foreground "gray50"))))))

(elpaca doom-modeline
  (column-number-mode)
  (line-number-mode)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-icon nil
        doom-modeline-highlight-modified-buffer-name t)
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
  (set-face-attribute 'doom-modeline-buffer-modified nil
                      :foreground "DarkOrange"
                      :weight 'bold
                      :inherit '(variable-pitch)))

(elpaca helm
  (setq tab-always-indent 'complete)
  (setq helm-always-two-windows nil
        helm-scroll-amount 8
        helm-move-to-line-cycle-in-source nil
        helm-display-buffer-default-height 16
        helm-default-display-buffer-functions '(display-buffer-at-bottom)
        helm-left-margin-width 1
        helm-buffers-fuzzy-matching t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-visible-mark-prefix "◎"
        helm-display-header-line nil)
  (keymap-global-set "C-c h" 'helm-command-prefix)
  (keymap-global-set "M-x" 'helm-M-x)
  (keymap-global-set "C-x C-S-f" 'helm-for-files)
  (keymap-global-set "C-x C-f" 'helm-find-files)
  (keymap-global-set "M-s o" 'helm-occur)
  (keymap-global-set "C-h a" 'helm-apropos)
  (keymap-global-set "C-x C-b" 'helm-buffers-list)
  (keymap-global-set "C-x b" 'helm-mini)
  (keymap-global-set "C-R" 'helm-resume)
  (keymap-global-set "M-I" 'helm-semantic-or-imenu)
  (keymap-global-set "C-S-o" 'helm-all-mark-rings)
  ;; (defun my/hide-helm-modeline ()
  ;;   (with-helm-buffer (setq-local mode-line-format nil)))
  ;; (fset 'helm-display-mode-line #'ignore)
  ;; (add-hook 'helm-after-initialize-hook 'my/hide-helm-modeline)
  (defun my/helm-eshell ()
    (eshell-cmpl-initialize)
    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))
  (add-hook 'eshell-mode-hook 'my/helm-eshell)
  ;; For some reason M-p in helm-find-files does not show previously opened files.
  (defun my/fix-helm-history (&rest _) (setq helm-ff-history file-name-history))
  (advice-add 'helm-find-files-history :before 'my/fix-helm-history)
  (defun my/customize-helm ()
    (keymap-set helm-map "<escape>" 'helm-keyboard-quit)
    (let ((bg-col (face-attribute 'default :background)))
      (set-face-attribute 'helm-source-header nil
                          :inherit '(variable-pitch) :background 'unspecified
                          :weight 'normal :height 1.5 :underline t)
      (set-face-attribute 'helm-selection nil
                          :background "LightBlue1" :underline '(:color "black" :position 0))
      (set-face-attribute 'helm-visible-mark nil
                          :foreground 'unspecified :background "gold"
                          :weight 'bold
                          :box `(:line-width (-1 . -2) :color ,bg-col)))
    (dolist (face (list 'helm-ff-directory 'helm-ff-dotted-symlink-directory
                        'helm-buffer-directory 'helm-ff-dotted-directory))
      (set-face-attribute face nil :extend 'unspecified :weight 'bold
                          :foreground "DodgerBlue3" :background 'unspecified))
    (dolist (face (list 'helm-ff-invalid-symlink 'helm-resume-need-update 'helm-ff-suid))
      (set-face-attribute face nil :foreground "red" :background 'unspecified))
    (face-spec-set 'helm-lisp-show-completion '((t (:foreground "purple"))))
    (face-spec-set 'helm-swoop-target-word-face
                   '((t (:foreground "white" :background "#7700FF"
                         :box (:line-width (-16 . -16) :color "#7700FF")))))
    (face-spec-set 'helm-swoop-target-line-face '((t (:background "gold"))))
    (face-spec-set 'helm-swoop-target-line-block-face '((t (:background "goldenrod1")))))
  (add-hook 'helm-mode-hook 'my/customize-helm)
  (helm-mode))

(elpaca helm-descbinds)
(elpaca helm-themes)
(elpaca helm-xref)
(elpaca helm-unicode)
(elpaca helm-swoop
  (setq helm-swoop-use-line-number-face t)
  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-split-with-multiple-windows t)
  (keymap-global-set "C-M-s" 'helm-swoop)
  (keymap-set isearch-mode-map "C-M-s" 'helm-swoop-from-isearch))
(elpaca helm-ext (helm-ext-minibuffer-enable-header-line-maybe t))

;; trying these out
(elpaca helm-ls-git)


(elpaca yasnippet)
(elpaca yasnippet-snippets)
(elpaca helm-c-yasnippet)

(elpaca dash)

;; TODO: https://bard.github.io/emacs-run-command/quickstart
;; (elpaca run-command)


;; FIXME: They show up in the menu, but I can't jump to them?
(defun my/elisp-imenu ()
  (add-to-list 'imenu-generic-expression '(nil "^(elpaca \\([^ )]*\\)" 1)))
(add-hook 'emacs-lisp-mode-hook 'my/elisp-imenu)
