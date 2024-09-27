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
  (set-face-attribute 'mode-line nil :inherit '(variable-pitch))
  (set-face-attribute 'mode-line-inactive nil :inherit '(variable-pitch))
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
                      :foreground 'unspecified ;"DarkOrange"
                      :weight 'bold
                      :inherit '(variable-pitch)))

(elpaca minions
  (minions-mode)
  (add-to-list 'minions-prominent-modes 'view-mode))

(elpaca consult
  (keymap-global-set "C-M-x" 'consult-mode-command)
  ;;(keymap-global-set [remap Info-search] 'consult-info)
  (keymap-global-set "C-x b" 'consult-buffer)
  (keymap-global-set "C-M-:" 'consult-complex-command)
  (keymap-global-set "C-x r b" 'consult-bookmark)
  (keymap-global-set "C-x p b" 'consult-project-buffer)
  (keymap-global-set "M-y" 'consult-yank-pop)
  (keymap-global-set "M-g e" 'consult-compile-error)
  (keymap-global-set "M-g f" 'consult-flymake)
  (keymap-global-set "M-g g" 'consult-goto-line)
  (keymap-global-set "M-g M-g" 'consult-goto-line)
  (keymap-global-set "M-g o" 'consult-outline)
  (keymap-global-set "M-g M" 'consult-global-mark)
  (keymap-global-set "M-g m" 'consult-mark)
  (keymap-global-set "M-g i" 'consult-imenu)
  (keymap-global-set "M-g I" 'consult-imenu-multi)
  (keymap-global-set "M-s d" 'consult-fd)
  (keymap-global-set "M-s g" 'consult-grep)
  (keymap-global-set "M-s G" 'consult-git-grep)
  (keymap-global-set "M-s r" 'consult-ripgrep)
  (keymap-global-set "M-s l" 'consult-line)
  (keymap-global-set "M-s L" 'consult-line-multi)
  (keymap-global-set "M-s k" 'consult-keep-lines)
  (keymap-global-set "M-s u" 'consult-focus-lines)
  (keymap-global-set "M-s h" 'consult-isearch-history)
  (keymap-global-set "M-s e" 'consult-eglot-symbols)
  (keymap-set isearch-mode-map "M-h" 'consult-isearch-history)
  (keymap-set isearch-mode-map "M-s h" 'consuilt-isearch-history)
  (keymap-set isearch-mode-map "M-s l" 'consult-line)
  (keymap-set isearch-mode-map "M-s L" 'consult-line-multi)
  (keymap-set minibuffer-local-map "M-s" 'consult-history)
  (keymap-set minibuffer-local-map "M-r" 'consult-history)
  (setq register-preview-delay 0.25
        register-preview-function #'consult-register-format)
  (advice-add 'register-preview :override 'consult-register-window)
  (setq xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref)
  (require 'consult)
  (consult-customize consult-theme :preview-key '(:debounce 0.2 any)
                     consult-ripgrep consult-git-grep consult-grep
                     consult-bookmark consult-recent-file consult-xref
                     consult--source-file-register consult--source-recent-file
                     consult--source-bookmark consult--source-project-recent-file
                     :preview-key '(:debounce 0.4 any)))

(elpaca vertico
  (setq vertico-cycle t)
  (vertico-mode)
  (vertico-reverse-mode)
  (keymap-set vertico-map "TAB" 'minibuffer-complete))

(elpaca corfu
  (setq corfu-cycle t
        corfu-popupinfo-delay '(0.25 . 0.1)
        corfu-popupinfo-hide nil
        corfu-preview-current 'insert
        corfu-on-exact-match 'insert
        corfu-quit-no-match nil
        corfu-quit-at-boundary 'separator)
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (keymap-unset corfu-map "<remap> <move-beginning-of-line>")
  (keymap-unset corfu-map "<remap> <move-end-of-line>")
  (keymap-set corfu-map "<prior>" 'corfu-scroll-down)
  (keymap-set corfu-map "<next>" 'corfu-scroll-up)
  (keymap-set corfu-map "<tab>" 'corfu-next)
  (keymap-set corfu-map "<backtab>" 'corfu-previous)
  (keymap-set corfu-map "<space>" 'corfu-insert-separator)
  (corfu-history-mode)
  (require 'savehist)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (defun corfu--preview-current-p ()
    (and corfu-preview-current (>= corfu--index 0))))

(elpaca cape
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(elpaca embark
  (keymap-global-set "C-." 'embark-act)
  (keymap-global-set "C-," 'embark-dwim)
  (keymap-global-set "C-h B" 'embark-bindings)
  (setq prefix-help-command 'embark-prefix-help-command))
(elpaca embark-consult
  (add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode))

(elpaca marginalia (marginalia-mode))

(elpaca orderless
  (setq orderless-component-separator 'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless flex basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

(elpaca consult-eglot)
(elpaca consult-eglot-embark (consult-eglot-embark-mode))

(elpaca yasnippet)
(elpaca yasnippet-snippets)
(elpaca consult-yasnippet
  (setq consult-yasnippet-use-thing-at-point t))


;; TODO: https://bard.github.io/emacs-run-command/quickstart
;; (elpaca run-command)


(defun my/elisp-imenu ()
  (add-to-list 'imenu-generic-expression '(nil "^(elpaca \\([^ )]*\\)" 1)))
(add-hook 'emacs-lisp-mode-hook 'my/elisp-imenu)
