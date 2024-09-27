;; -*- lexical-binding: t -*-

(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(defun display-startup-echo-area-message ())

(defun my/user-load (filename)
  (load (expand-file-name filename user-emacs-directory) 'noerror 'nomessage))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; NOTE: Need to actually load custom file if not loading packages.
;; Otherwise, custom is loaded in elpaca-after-init-hook
;;(my/user-load "custom.el")
(my/user-load "package-init.el")

(setopt show-paren-delay 0)
(setopt show-paren-when-point-inside-paren t)
(show-paren-mode)

(add-hook 'prog-mode-hook 'whitespace-mode)
(setopt whitespace-style '(face trailing tabs tab-mark))

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

(setq imenu-auto-rescan t)

(cond ((eq system-type 'windows-nt)
       (set-face-font 'default "Iosevka NF-12")
       (set-face-font 'fixed-pitch "Iosevka NF-12")
       (set-face-font 'variable-pitch "Roboto Condensed-12"))
      (t
       (set-face-font 'default "Iosevka Nerd Font Propo-11")
       (set-face-font 'fixed-pitch "Iosevka Nerd Font Propo-11")
       (set-face-font 'variable-pitch "Asap SemiCondensed-11")))
(setq inhibit-compacting-font-caches t)

(setq vc-follow-symlinks t)
(setq history-delete-duplicates t)
(setq set-mark-command-repeat-pop t)
(setq custom-buffer-done-kill t)
(setq custom-unlispify-tag-names nil)
(setq custom-unlispify-menu-entries nil)
(setq shell-kill-buffer-on-exit t)
(setq window-resize-pixelwise t)
(setq text-scale-mode-step 1.05)
(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq line-move-visual nil)
(setq cursor-in-non-selected-windows nil)
(setq c-ts-mode-indent-style 'k&r)
(setq c-default-style '((c-mode . "stroustrup")
                        (java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))

(defun c-indent-or-complete (&optional arg region)
  (interactive)
  (if (member (char-before) '(?\s ?\t))
      (funcall-interactively 'c-indent-line-or-region arg region)
    (indent-for-tab-command arg)))
(defun my/fix-c-tab ()
  (keymap-set c-mode-map "TAB" 'c-indent-or-complete))
(add-hook 'c-mode-hook 'my/fix-c-tab)

;; TODO: Add a hook to disable this when eglot is on
;; (setopt help-at-pt-timer-delay 0.25)
;; (setopt help-at-pt-display-when-idle '(keymap local-map button kbd-help))

(setq eldoc-idle-delay 0.05)
(setq minibuffer-beginning-of-buffer-movement t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(setq tab-always-indent 'complete)
(setq completion-cycle-threshold nil)
(setq completions-detailed t)
(setq completions-max-height 16)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(setopt window-divider-default-right-width 1)
(window-divider-mode)

(setq-default indent-tabs-mode nil)
(indent-tabs-mode -1)

(tool-bar-mode -1)
(context-menu-mode)
(undelete-frame-mode)
(auto-save-mode -1)
(savehist-mode)
(add-to-list 'savehist-additional-variables 'file-name-history)

(setq cua-auto-mark-last-change t)
(setq cua-auto-tabify-rectangles nil)
(setq cua-enable-auto-region-help t)
(setq cua-enable-rectangle-auto-help t)
(setq cua-virtual-rectangle-edges nil)
(cua-selection-mode t)

(keymap-global-set "C-x SPC" 'cua-rectangle-mark-mode)
(keymap-global-unset "C-h t")
(keymap-global-unset "<f1> t")

(keymap-global-set "<escape>" 'keyboard-escape-quit)

;; I really want escape to do what it says
(keymap-global-set "C-h ESC" 'keyboard-quit)
(keymap-global-set "C-x ESC" 'keyboard-quit)
(keymap-global-set "C-c ESC" 'keyboard-quit)
(keymap-global-set "C-M-g" 'keyboard-quit)


(keymap-global-set "ESC ESC" 'keyboard-quit)
;; Something, somewhere is rebinding this. It doesn't look like the debugger can
;; break on keymap modification.
;; (defun my/brute-force-esc-map ()
;;   (keymap-global-set "ESC ESC" 'keyboard-quit))
;; (add-hook 'window-configuration-change-hook 'my/brute-force-esc-map)

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
(setq double-click-fuzz 5)
(setq mouse-buffer-menu-maxlen 100)

(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(0.1
                                  ((shift) . 0.9)
                                  ((meta) . hscroll)
                                  ((control) . text-scale)
                                  ((control meta) . global-text-scale)))

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

(defun recompile-or-prompt ()
  (interactive)
  (if (string= compile-command "make -k ")
      ;; Subtle change from the default to make this not trigger a second time
      (progn (setq compile-command "make -k")
             (call-interactively 'compile))
    (call-interactively 'recompile)))

(keymap-global-set "<f5>" 'recompile-or-prompt)
(keymap-global-set "<f6>" 'compile)

;; Prevents ESC from messing with splits
(defun my/keyboard-escape-quit-advice (fn)
  (let ((buffer-quit-function (or buffer-quit-function #'keyboard-quit)))
    (funcall fn)))
(advice-add 'keyboard-escape-quit :around 'my/keyboard-escape-quit-advice)

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

;; Not sure if I want to keep this as a keybind yet
(defun select-minibuffer ()
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))
(keymap-global-set "<f1>" 'select-minibuffer)

(defun my/buffer-is-read-only (buf)
  (and (buffer-local-value 'buffer-read-only (get-buffer buf))
       (buffer-file-name (get-buffer buf))))

;; TODO: Advise the mouse buffer menu to force it to use the clicked window
(setq display-buffer-alist
      '(("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
         nil
         (window-parameters (mode-line-format . none)))
        ((or (derived-mode . special-mode)
             (mode . Custom-mode))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.3)
         (mode special-mode Custom-mode))
        (my/buffer-is-read-only
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (body-function . (lambda (win)
                            (with-selected-window win
                              (unless view-mode (view-mode)))))
         (mode special-mode))
        ((or (derived-mode . prog-mode)
             (derived-mode . text-mode))
         (display-buffer-reuse-mode-window display-buffer-in-direction)
         (direction . right)
         (mode prog-mode text-mode))))

(my/user-load "theme-init.el")

(setq initial-scratch-message nil)

(defun my/recent-files-scratch-buffer ()
  "Display recent files in the scratch buffer."
  (with-current-buffer (get-scratch-buffer-create)
    (insert "  " (propertize "Recent Files" 'font-lock-face 'bold) "\n")
    (when file-name-history
      (dolist (f (take 10 file-name-history))
        (when (file-exists-p f)
          (let ((txt (apply #'propertize f 'font-lock-face '(variable-pitch link)
                            (button--properties #'find-file f nil))))
            (insert "  • " txt "\n")))))
    (insert "\n")
    (unless (cdr command-line-args) (scratch-buffer))))
(defun recent-files () (interactive) (my/recent-files-scratch-buffer) (scratch-buffer))
(add-hook 'elpaca-after-init-hook 'my/recent-files-scratch-buffer)

