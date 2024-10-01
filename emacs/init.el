;; -*- lexical-binding: t -*-

;; TODO: The order of this file doesn't really make any sense. Maybe look at
;; literate org config files for inspiration.

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
(defun my/trailing-whitespace () (setq-local show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'my/trailing-whitespace)

(setq whitespace-style '(face tabs tab-mark))
(add-hook 'prog-mode-hook 'whitespace-mode)

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

(setq alternate-fontname-alist
      '(("Iosevka NFP" "Iosevka Nerd Font Propo" "courier" "fixed")
        ("IosevkaTermSlab NFP" "IosevkaTermSlab Nerd Font Propo" "courier" "fixed")
        ("JetBrainsMono NF" "JetBrainsMonoNL NF" "Consolas" "FreeMono" "courier" "fixed")
        ("Roboto Condensed" "Roboto" "Arial" "helv" "helvetica" "fixed")))
(setq face-font-family-alternatives '(("Monospace" "Iosevka NF")
                                      ("Monospace Serif" "JetBrainsMono NF")
                                      ("Sans Serif" "Roboto Condensed")
                                      ("helv" "helvetica" "arial" "fixed")))
(set-face-font 'default "Iosevka NFP-11")
(set-face-font 'fixed-pitch "IosevkaTermSlab NFP-11:weight=normal")
(set-face-font 'fixed-pitch-serif "IosevkaTermSlab NFP-11:weight=normal")
(set-face-font 'variable-pitch "Roboto Condensed-11:weight=semilight")
;; Windows doesn't respect font weight settings, requiring a different family instead
(defface my/default-light '((t (:family "Iosevka NFP Light" :inherit (default)))) "Light default font")

(setq inhibit-compacting-font-caches t)

(setq backup-directory-alist '(("." . "~/.emacs-backups")))
(setq fit-window-to-buffer-horizontally t)
(setq read-minibuffer-restore-windows nil)
(setq resize-mini-windows t)
(setq vc-follow-symlinks t)
(setq history-delete-duplicates t)
(setq set-mark-command-repeat-pop t)
(setq custom-buffer-done-kill t)
(setq custom-unlispify-tag-names nil)
(setq custom-unlispify-menu-entries nil)
(setq shell-kill-buffer-on-exit t)
(setq window-resize-pixelwise t)
(setq text-scale-mode-step 1.05)
(setq switch-to-buffer-obey-display-actions nil)
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq line-move-visual nil)
(setq cursor-in-non-selected-windows nil)
(setq c-ts-mode-indent-style 'k&r)
(setq c-default-style '((c-mode . "stroustrup")
                        (c++-mode . "stroustrup")
                        (java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))

(defun c-indent-or-complete (&optional arg region)
  (interactive)
  (if (member (char-before) '(?\s ?\t))
      (funcall-interactively 'c-indent-line-or-region arg region)
    (indent-for-tab-command arg)))
(defun my/fix-c-tab () (keymap-set c-mode-base-map "TAB" 'c-indent-or-complete))
(add-hook 'c-mode-hook 'my/fix-c-tab)
(add-hook 'c++-mode-hook 'my/fix-c-tab)

;; TODO: Add a hook to disable this when eglot is on
;; (setopt help-at-pt-timer-delay 0.25)
;; (setopt help-at-pt-display-when-idle '(keymap local-map button kbd-help))

(setq eldoc-idle-delay 0.05)
(setq minibuffer-beginning-of-buffer-movement t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)

;; Don't truncate eval results in minibuffer
(setq eval-expression-print-level nil)
(setq eval-expression-print-length nil)

(setq tab-always-indent 'complete)
(setq completion-cycle-threshold nil)
(setq completions-detailed t)
(setq completions-max-height 16)
(setq read-extended-command-predicate #'command-completion-default-include-p)
(setq read-buffer-completion-ignore-case t)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(setopt window-divider-default-right-width 3)
(window-divider-mode)

(setq-default indent-tabs-mode nil)
(indent-tabs-mode -1)

(setq tab-line-new-button-show nil)
(setq tab-line-close-button-show nil)
;; TODO: Re-enable showing modified in tab-line but make sure it updates in every window
(setq tab-line-tab-face-functions '())

;; Use advice here instead of tab-line-tab-name-function so that it's after
;; propertize has been applied to the text.
(defun my/tab-line-tab-custom (name)
  (let* ((bg-col (face-attribute (get-text-property 0 'face name) :background))
         (space (propertize "  " 'face `(:background ,bg-col :underline nil)))
         (name (concat space name space)))
    ;; help-echo is redundant and blocks other tabs
    (ignore-errors (remove-text-properties 0 (length name) '(help-echo nil) name))
    name))
(advice-add 'tab-line-tab-name-format-default :filter-return 'my/tab-line-tab-custom)
(global-tab-line-mode) ;; Probably temporary until I figure something better out

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
(keymap-global-unset "M-ESC :")

(keymap-global-set "<escape>" 'keyboard-escape-quit)

;; I really want escape to do what it says
(keymap-global-set "C-h ESC" 'keyboard-quit)
(keymap-global-set "C-x ESC" 'keyboard-quit)
(keymap-global-set "C-c ESC" 'keyboard-quit)
(keymap-global-set "C-M-g" 'keyboard-quit)

;; Bigger line jumps. Set the mark on the first use if a prefix was given.
(defun next-line-x8 (&optional arg)
  (interactive "P")
  (when arg (unless (eq last-command 'next-line-x8) (push-mark)))
  (let ((current-prefix-arg (* 8 (or arg 1))))
    (call-interactively #'next-line)))

(defun previous-line-x8 (&optional arg)
  (interactive "P")
  (when arg (unless (eq last-command 'previous-line-x8) (push-mark)))
  (let ((current-prefix-arg (* 8 (or arg 1))))
    (call-interactively #'previous-line)))

(keymap-global-set "M-n" 'next-line-x8)
(keymap-global-set "M-p" 'previous-line-x8)

;; Might not need this anymore?
;; (keymap-global-set "ESC ESC" 'keyboard-quit)

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
(keymap-global-set "C-c o" 'other-other-window)

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

(defun my/skip-hidden-buffers (_ buf _)
  (let ((name (buffer-name buf)))
    (or (string-prefix-p " " name))))

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

;; TODO: Write predicates to categorize buffers, then re-use those instead of
;; duplicating this logic in multiple places (tab-line, etc).

;; Assigned directly to switch-to-prev-buffer-skip
(defun my/skip-file-buffers (_ buf &rest _)
  (let ((name (buffer-name buf)))
    (or (buffer-file-name buf)
        (string= "*scratch*" name)
        (string-prefix-p " " name))))
(defun my/skip-non-file-buffers (_ buf &rest _)
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

(defun my/is-file-buffer (&optional buf)
  (let ((b (or buf (current-buffer))))
    (or (buffer-file-name b)
        (string= (buffer-name b) "*scratch*"))))

;; Used for tab-line tab filtering
(defun my/only-non-file-buffers (bufs)
  (seq-remove (lambda (b) (and (or (buffer-file-name b)
                                   (string= (buffer-name b) "*scratch*")
                                   (string-prefix-p " " (buffer-name b)))))
              bufs))
(defun my/only-file-buffers (bufs)
  (seq-filter (lambda (b) (and (my/is-file-buffer b)
                               (not (string-prefix-p " " (buffer-name b)))))
               bufs))

;; Modified tab-line-tabs-window-buffers to include all similar buffers, not
;; only ones seen in this window, following the same order as next-buffer
;; TODO: Add a list of regexps to filter things out of these lists,
;; particularly *Async-native-compile-log*
(defun my/tab-line-tabs ()
  (let* ((bufs (buffer-list (selected-frame)))
         (visiting-file (my/is-file-buffer))
         (tabs (if visiting-file (my/only-file-buffers bufs)
                 (my/only-non-file-buffers bufs))))
    tabs
    (sort tabs (lambda (a b) (string< (buffer-name a) (buffer-name b))))))
(setq tab-line-tabs-function 'my/tab-line-tabs)

;; Use C-tab to navigate in displayed tab order instead of LRU order
(defun next-tab-line-tab ()
  (interactive)
  (let* ((tabs (my/tab-line-tabs))
         (after (member (current-buffer) tabs)))
    (if (cdr after)
        (switch-to-buffer (cadr after))
      (switch-to-buffer (car tabs)))))

(defun previous-tab-line-tab ()
  (interactive)
  (let* ((tabs (reverse (my/tab-line-tabs)))
         (after (member (current-buffer) tabs)))
    (if (cdr after)
        (switch-to-buffer (cadr after))
      (switch-to-buffer (car tabs)))))
(keymap-global-set "C-<tab>" 'next-tab-line-tab)
(keymap-global-set "C-<backtab>" 'previous-tab-line-tab)
(keymap-global-set "C-<iso-lefttab>" 'previous-tab-line-tab)

(defun my/flymake-set-keys ()
  (keymap-set flymake-mode-map "M-g ]" 'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-g [" 'flymake-goto-prev-error))
(add-hook 'flymake-mode-hook 'my/flymake-set-keys)

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

;; Other window, but prioritize the minibuffer when active
(defun select-minibuffer-or-other-window (&rest args)
  (interactive "p\ni\np")
  (cond ((active-minibuffer-window)
         (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
         (select-window (active-minibuffer-window)))
        (t (apply #'funcall-interactively #'other-window args))))
(keymap-global-set "C-x O" 'select-minibuffer-or-other-window)

;; Useful for debugging display-buffer-alist and similar
(defun my/major-mode-parents (mode)
  (when-let ((parent (get mode 'derived-mode-parent)))
    (cons parent (my/major-mode-parents parent))))

(defun major-mode-parents (&optional mode)
  (interactive)
  (let ((current-mode (or mode major-mode)))
    (message "%s" (cons current-mode (my/major-mode-parents current-mode)))))


(defun my/make-follow-buffer-output (buf)
  (lambda (&rest _)
    (when-let ((win (get-buffer-window buf)))
      (with-selected-window win
        (goto-char (point-max))
        (ignore-errors (backward-char 1))
        (recenter -1)))))

(defun my/setup-follow-buffer-output ()
  (add-hook 'after-change-functions (my/make-follow-buffer-output (current-buffer)) 0 'local))

(add-hook 'compilation-mode-hook 'my/setup-follow-buffer-output)
(add-hook 'messages-buffer-mode-hook 'my/setup-follow-buffer-output) ;; this doesn't seem consistent
(kill-buffer (get-buffer "*Messages*"))

;; Monkey patch helper macro
(defmacro with-function-replaced (target-fn new-fn &rest body)
  (declare (indent 2))
  (let ((old-fn (gensym))
        (block-result (gensym))
        (new-fn-value (if (symbolp new-fn) `(symbol-function ,new-fn) new-fn)))
    `(let ((,old-fn (symbol-function ,target-fn)))
       (fset ,target-fn ,new-fn-value)
       (let ((,block-result (progn ,@body)))
         (fset ,target-fn ,old-fn)
         ,block-result))))

;; customize-group specifically uses pop-to-buffer-same-window
(defun my/customize-group-advice (fn &rest args)
  (let ((switch-to-buffer-obey-display-actions t))
    (with-function-replaced 'pop-to-buffer-same-window 'pop-to-buffer
      (apply fn args))))
(advice-add 'customize-group :around 'my/customize-group-advice)
(add-hook 'Custom-mode-hook 'tab-line-mode)


(defun my/buffer-is-read-only (buf)
  (and (buffer-local-value 'buffer-read-only (get-buffer buf))
       (buffer-file-name (get-buffer buf))))


(defun my/find-bottom-window ()
  "Find the bottom-most window, excluding the minibuffer and any window in the
upper-half of the frame"
  (let* ((windows (remove (active-minibuffer-window) (window-list)))
         (bot (if (active-minibuffer-window)
                  (- (frame-height) (window-height (active-minibuffer-window)))
                (frame-height)))
         (eligible (seq-filter (lambda (x) (not (= 0 (window-top-line x))))
                               windows))
         (sorted (sort eligible (lambda (a b) (> (nth 3 (window-edges a))
                                                 (nth 3 (window-edges b)))))))
    (or (car sorted)
        (split-root-window-below (- (min (truncate (frame-height) 5) 16))))))

(defun my/display-buffer-in-bottom-tab (buffer alist)
  (let* ((candidate-win (my/find-bottom-window))
         (actual-win (or (and candidate-win
                              (window--display-buffer buffer candidate-win 'reuse alist))
                         (display-buffer-at-bottom buffer alist))))
    (when actual-win
      (with-selected-window actual-win (tab-line-mode))
      actual-win)))


(defun my/display-file-buffer-in-same-window (buffer alist)
    "If we're trying to display a file buffer in a window that is already
showing a file buffer, reuse it. Otherwise use the most recently focused window
that is showing a file buffer. This should be doable with built-in configuration
of display-buffer-alist, but every attempt I made failed."
    (when (my/is-file-buffer (current-buffer))
        (let ((switch-to-buffer-obey-display-actions nil))
          (switch-to-buffer buffer))))

(setq display-comint-buffer-action '(my/display-buffer-in-bottom-tab ()))
(setq display-tex-shell-buffer-action '(my/display-buffer-in-bottom-tab ()))

;; Note that Custom-mode seems to set its mode _after_ the window is created,
;; so we have to match by name.
(defconst bottom-window-modes
  '(special-mode compilation-mode comint-mode Custom-mode))

;; TODO: Advise help-function-def--button-function to set read-only and use view-mode
(setq display-buffer-alist
      `(("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
         nil
         (window-parameters (mode-line-format . none)))
        ("\\*Async-native-compile-log\\*"
         (display-buffer-no-window))
        ((or "\\*Custom" "\\*e?shell\\*" ,@(mapcar (apply-partially #'cons 'derived-mode) bottom-window-modes))
         (my/display-buffer-in-bottom-tab)
         (mode ,@bottom-window-modes))
        (my/buffer-is-read-only
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (body-function . (lambda (win)
                            (with-selected-window win
                              (unless view-mode (view-mode)))))
         (mode special-mode))
        ((or (derived-mode . prog-mode)
             (derived-mode . text-mode)
             (derived-mode . fundamental-mode))
         (my/display-file-buffer-in-same-window
          display-buffer-reuse-mode-window
          display-buffer-in-direction)
         (direction . right)
         (body-function . (lambda (win) (select-window win)))
         (mode prog-mode text-mode fundamental-mode))))


(when (treesit-available-p)
  (when (treesit-language-available-p 'cmake)
    (add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))))

(my/user-load "theme-init.el")

(setq initial-scratch-message nil)

(defun my/replace-scratch (file)
  (let ((new-buf (find-file file)))
    (delete-other-windows)
    (kill-buffer "*scratch*")))

(defun my/recent-files-scratch-buffer ()
  "Display recent files in the scratch buffer."
  (with-current-buffer (get-scratch-buffer-create)
    (insert "Welcome to Emacs ")
    (insert (format "%s.%s!\n\n" emacs-major-version emacs-minor-version))
    (insert (propertize "Recent Files" 'font-lock-face 'bold) "\n")
    (when file-name-history
      (dolist (f (take 10 file-name-history))
        (when (file-exists-p f)
          (let ((txt (apply #'propertize f 'font-lock-face '(variable-pitch link)
                            (button--properties #'my/replace-scratch f nil))))
            (insert "  • " txt "\n")))))
    (insert "\n")
    (goto-char (point-max)))
  (unless (cdr command-line-args)
    (pop-to-buffer (messages-buffer))
    (pop-to-buffer (scratch-buffer))
    ))
(defun recent-files () (interactive) (my/recent-files-scratch-buffer) (scratch-buffer))
(add-hook 'elpaca-after-init-hook 'my/recent-files-scratch-buffer)
