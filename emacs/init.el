(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(defun display-startup-echo-area-message ())

(load (expand-file-name "bootstrap-elpaca.el" user-emacs-directory) 'noerror 'nomessage)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror 'nomessage)))

(elpaca sublime-themes
  (load-theme 'mccarthy t)
  (set-face-attribute 'show-paren-match nil
                      :box '(:line-width (-1 . -1))
                      :weight 'black
                      :background 'unspecified)
  (set-face-attribute 'show-paren-mismatch nil
                      :foreground "white"
                      :background "IndianRed4"
                      :inverse-video nil)
  (set-face-attribute 'whitespace-trailing nil
                      :background "gainsboro"))

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
    (set-face-attribute face nil :inherit '(variable-pitch))))

(elpaca helm
  (setq tab-always-indent 'complete)
  (setq helm-always-two-windows nil
        helm-scroll-amount 8
        helm-display-buffer-default-height 15
        helm-move-to-line-cycle-in-source nil
        helm-default-display-buffer-functions '(display-buffer-in-side-window)
        helm-buffers-fuzzy-matching t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-display-header-line nil)
  (keymap-global-set "M-x" 'helm-M-x)
  (keymap-global-set "C-x C-f" 'helm-find-files)
  (keymap-global-set "M-s o" 'helm-occur)
  (keymap-global-set "C-h a" 'helm-apropos)
  (keymap-global-set "C-x C-b" 'helm-buffers-list)
  (keymap-global-set "C-x b" 'helm-mini)
  (keymap-global-set "C-R" 'helm-resume)
  (keymap-global-set "M-I" 'helm-semantic-or-imenu)
  (keymap-global-set "C-O" 'helm-all-mark-rings)
  (defun my/hide-helm-modeline ()
    (with-helm-buffer (setq-local mode-line-format nil)))
  (fset 'helm-display-mode-line #'ignore)
  (add-hook 'helm-after-initialize-hook 'my/hide-helm-modeline)
  (defun my/helm-eshell ()
    (eshell-cmpl-initialize)
    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
    (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))
  (add-hook 'eshell-mode-hook 'my/helm-eshell)
  (defun my/customize-helm ()
    (keymap-set helm-map "<escape>" 'helm-keyboard-quit)
    (set-face-attribute 'helm-source-header nil
                        :inherit '(variable-pitch) :background 'unspecified
                        :weight 'normal :height 1.5 :underline t)
    (set-face-attribute 'helm-selection nil
                        :background "azure2"
                        :underline '(:position 0)))
  (add-hook 'helm-mode-hook 'my/customize-helm)
  (helm-mode))

(elpaca helm-descbinds)
(elpaca helm-themes)
(elpaca helm-xref)
(elpaca helm-unicode)
(elpaca helm-swoop
  (keymap-global-set "C-M-s" 'helm-swoop)
  (keymap-global-set "C-M-S" 'helm-swoop-from-isearch))
(elpaca helm-ext
  (helm-ext-ff-enable-skipping-dots t)
  (setq helm-ext-ff-skipping-dots-recenter t)
  (helm-ext-minibuffer-enable-header-line-maybe t))

(elpaca yasnippet)
(elpaca yasnippet-snippets)
(elpaca helm-c-yasnippet)

;; TODO: https://bard.github.io/emacs-run-command/quickstart
;; (elpaca run-command)

(setq mouse-wheel-scroll-amount '(0.1 ((shift) . 0.9)
                                      ((meta) . hscroll)
                                      ((control) . text-scale)
                                      ((control meta) . global-text-scale)))

(setopt show-paren-delay 0)
(show-paren-mode)

(add-hook 'prog-mode 'whitespace-mode)
(setopt whitespace-style '(face tabs trailing))

;; make isearch work more like vim
(setopt disabled-command-function 'ignore)
(setopt isearch-allow-scroll 'unlimited)
(setopt isearch-repeat-on-direction-change t)
(setopt isearch-wrap-pause 'no-ding)
(setopt search-default-mode t)
(keymap-global-set "C-S-s" 'isearch-backward)
(keymap-set isearch-mode-map "C-S-s" 'isearch-repeat-backward)

(setq-default buffer-file-coding-system 'utf-8-unix)

(set-window-scroll-bars (minibuffer-window) nil nil)
(keymap-set minibuffer-mode-map "<escape>" 'abort-minibuffers)

;; FIXME: They show up in the menu, but I can't jump to them?
(defun my/elisp-imenu ()
  (add-to-list 'imenu-generic-expression '(nil "^(elpaca \\([^ )]*\\)" 1)))
(add-hook 'emacs-lisp-mode-hook 'my/elisp-imenu)
(setq imenu-auto-rescan t)

(cond ((eq system-type 'windows-nt)
       (set-face-font 'default "Iosevka NF-12:antialias=natural")
       (set-face-font 'fixed-pitch "Iosevka NF-12:antialias=natural")
       (set-face-font 'variable-pitch "Segoe UI-12:antialias=natural"))
      (t
       (set-face-font 'default "Iosevka Nerd Font Propo-12")
       (set-face-font 'fixed-pitch "Iosevka Nerd Font Propo-12")
       (set-face-font 'variable-pitch "Asap SemiCondensed-12")))

(setq help-clean-buttons t)
(setq custom-buffer-done-kill t)
(setq shell-kill-buffer-on-exit t)

(tool-bar-mode -1)
(indent-tabs-mode -1)
(context-menu-mode)
(undelete-frame-mode)
(cua-selection-mode t)
(auto-save-mode -1)

(keymap-global-set "C-x SPC" 'cua-rectangle-mark-mode)
(keymap-global-set "C-x ESC" 'keyboard-quit)
(keymap-global-set "C-c ESC" 'keyboard-quit)
(keymap-global-set "C-M-g" 'keyboard-quit)

(defun kill-current-buffer () (interactive) (kill-buffer (current-buffer)))
(keymap-global-set "C-x k" 'kill-current-buffer)
(keymap-global-set "C-x K" 'kill-buffer-and-window)
(keymap-set special-mode-map "q" 'kill-buffer-and-window)

(defun vim-join-line () (interactive) (delete-indentation t))
(keymap-global-set "C-S-j" 'vim-join-line)

(defun other-other-window () (interactive) (other-window -1))
(keymap-global-set "M-o" 'other-other-window)

(keymap-global-set "<mode-line> <mouse-2>" 'mouse-delete-window)
(keymap-global-set "<mode-line> <mouse-3>" 'mouse-buffer-menu)

(keymap-global-set "C-M-[" 'previous-buffer)
(keymap-global-set "C-M-]" 'next-buffer)

(defun my/skip-file-buffers (_ buf _)
  (or (buffer-file-name buf)
      (string= "*scratch*" (buffer-name buf))
      (string-prefix-p "*helm" (buffer-name buf))))
(defun my/skip-non-file-buffers (_ buf _) (null (buffer-file-name buf)))

(defun previous-similar-buffer ()
  (interactive)
  (if (my/skip-file-buffers nil (current-buffer) nil)
      (let ((switch-to-prev-buffer-skip 'my/skip-non-file-buffers))
        (previous-buffer))
    (let ((switch-to-prev-buffer-skip 'my/skip-file-buffers))
      (previous-buffer))))

(defun next-similar-buffer ()
  (interactive)
  (if (my/skip-file-buffers nil (current-buffer) nil)
      (let ((switch-to-prev-buffer-skip 'my/skip-non-file-buffers))
        (next-buffer))
    (let ((switch-to-prev-buffer-skip 'my/skip-file-buffers))
      (next-buffer))))

(keymap-global-set "M-[" 'previous-similar-buffer)
(keymap-global-set "M-]" 'next-similar-buffer)
