;; -*- lexical-binding: t -*-

;;;; Add a hook that runs after a theme is loaded

(defvar after-load-theme-hook nil)
(defadvice load-theme (after run-after-load-theme-hook activate)
  (run-hooks 'after-load-theme-hook))


;;;; Macro + helpers for easily remapping faces in specific modes

(defun define-mode-face--build-defun (face-name modes target-faces)
  (let* ((target-names (string-join (mapcar #'symbol-name target-faces) "-"))
         (fn-name (intern (concat "remap--" (symbol-name face-name) "--"
                                  target-names))))
    `(defun ,fn-name (&rest _)
       ,@(mapcar (lambda (target)
                   `(face-remap-set-base (quote ,target)(quote ,face-name)))
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

(define-mode-face my/shell-face '(((eshell-mode shell-mode) fringe default))
  '((t (:background "gray30"))))
