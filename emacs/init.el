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
                      :background "white"
                      :background "IndianRed4"
                      :inverse-video nil)
  (set-face-attribute 'trailing-whitespace nil
                      :background "gainsboro")
  (set-face-attribute 'isearch-fail nil :underline "red"
		      :foreground "red3" :background 'unspecified)
  (set-face-attribute 'highlight nil :background "LightBlue1")
  (set-face-attribute 'region nil :background "LightBlue1")
  )

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
  )

(elpaca helm
  (setq tab-always-indent 'complete)
  (setq helm-always-two-windows nil
        helm-scroll-amount 8
        helm-move-to-line-cycle-in-source nil
	helm-default-display-buffer-functions '(display-buffer-at-bottom)
        helm-buffers-fuzzy-matching t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-apropos-fuzzy-match t
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
    (dolist (face (list 'helm-ff-invalid-symlink 'helm-resume-need-update
			'helm-ff-suid))
      (set-face-attribute face nil :foreground "red" :background 'unspecified))
    (face-spec-set 'helm-lisp-show-completion '((t (:foreground "purple"))))
    (face-spec-set 'helm-swoop-target-word-face
                   '((t (:foreground "white" :background "#7700FF"
		        :box (:line-width (-16 . -16) :color "#7700FF")))))
    (face-spec-set 'helm-swoop-target-line-face
                   '((t (:background "gold"))))
    (face-spec-set 'helm-swoop-target-line-block-face
                   '((t (:background "goldenrod1")))))
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

(setq vc-follow-symlinks t)
(setq history-delete-duplicates t)
(setq set-mark-command-repeat-pop t)
(setq custom-buffer-done-kill t)
(setq custom-unlispify-tag-names nil)
(setq custom-unlispify-menu-entries nil)
(setq shell-kill-buffer-on-exit t)
(setq window-resize-pixelwise t)
;;(setopt widget-mouse-face 'highlight-no-extend)
(setopt window-divider-default-right-width 1)
(window-divider-mode)

(tool-bar-mode -1)
(indent-tabs-mode -1)
(context-menu-mode)
(undelete-frame-mode)
(auto-save-mode -1)
(savehist-mode)
(add-to-list 'savehist-additional-variables 'file-name-history)

(setq cua-auto-mark-last-change t)
(setq cua-auto-tabify-rectangles nil)
(setq cua-enable-cursor-indications t)
(setq cua-enable-auto-region-help t)
(setq cua-enable-rectangle-auto-help t)
(setq cua-overwrite-cursor-color "red")
(setq cua-read-only-cursor-color "grey70")
(setq cua-virtual-rectangle-edges nil)
(cua-selection-mode t)

(keymap-global-set "C-x SPC" 'cua-rectangle-mark-mode)
(keymap-global-unset "C-h t")
(keymap-global-unset "<f1> t")

;; I really want escape to do what it says
(keymap-global-set "C-h ESC" 'keyboard-quit)
(keymap-global-set "ESC ESC" 'keyboard-quit)
(keymap-global-set "C-x ESC" 'keyboard-quit)
(keymap-global-set "C-c ESC" 'keyboard-quit)
(keymap-global-set "C-M-g" 'keyboard-quit)

(defun kill-current-buffer () (interactive) (kill-buffer (current-buffer)))	
(keymap-global-set "C-x k" 'kill-current-buffer)
(keymap-global-set "C-x K" 'kill-buffer-and-window)

(defun kill-and-close-if-popup ()
  "Kill the current buffer. If no other buffer has been shown in this window,
consider it a pop-up and also close the window."
  (interactive)
  (cond ((and (null (window-prev-buffers (selected-window)))
	      (null (window-next-buffers (selected-window))))
	 (kill-buffer-and-window))
	(t (kill-current-buffer))))
(keymap-set special-mode-map "q" 'kill-and-close-if-popup)

(defun vim-join-line () (interactive) (delete-indentation t))
(keymap-global-set "C-S-j" 'vim-join-line)

(defun other-other-window () (interactive) (other-window -1))
(keymap-global-set "M-o" 'other-other-window)

(keymap-global-set "<mode-line> <mouse-2>" 'mouse-delete-window)
(keymap-global-set "<mode-line> <mouse-3>" 'mouse-buffer-menu)

;; Also consider helm buffers hidden, those things get everywhere...
(defun my/skip-hidden-buffers (_ buf _)
  (let ((name (buffer-name buf)))
    (or (string-prefix-p " " name)
	(string-prefix-p "*helm" name)
	(string-prefix-p "*Helm" name))))

(defun previous-non-hidden-buffer ()
  (interactive)
  (let ((switch-to-prev-buffer-skip 'my/skip-hidden-buffers))
    (previous-buffer)))
(defun next-non-hidden-buffer ()
  (interactive)
  (let ((switch-to-prev-buffer-skip 'my/skip-hidden-buffers))
    (next-buffer)))

(keymap-global-set "C-M-[" 'previous-non-hidden-buffer)
(keymap-global-set "C-M-]" 'next-non-hidden-buffer)

(defun my/skip-file-buffers (_ buf _)
  (let ((name (buffer-name buf)))
    (or (buffer-file-name buf)
	(string= "*scratch*" name)
	(string-prefix-p " " name)
	(string-prefix-p "*helm" name)
	(string-prefix-p "*Helm" name))))
(defun my/skip-non-file-buffers (_ buf _)
  (or (null (buffer-file-name buf))
      (string-prefix-p " " (buffer-name buf))))

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

;; Prevents ESC from messing with splits
(defun my/keyboard-escape-quit-advice (fn)
  (let ((buffer-quit-function (or buffer-quit-function #'keyboard-quit)))
    (funcall fn)))
(advice-add 'keyboard-escape-quit :around 'my/keyboard-escape-quit-advice)

(setq initial-scratch-message nil)
(setq initial-major-mode 'prog-mode)

(defun my/recent-files-scratch-buffer ()
  "Display recent files in the scratch buffer."
  (setq debug-on-error t)
  (when file-name-history
    (with-current-buffer (get-scratch-buffer-create)
      (setq buffer-face-mode-face 'variable-pitch)
      (buffer-face-mode t) ;;FIXME: this doesn't seem to work
      (insert "    Recent Files\n")
      (dolist (f (take 10 file-name-history))
	(insert "  • " (buttonize (propertize f :face 'bold) #'find-file f) "\n")))))

(add-hook 'elpaca-after-init-hook 'my/recent-files-scratch-buffer)

