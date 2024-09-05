;; -*- lexical-binding: t -*-
;; Based on the treemacs extension tutorial:
;; https://github.com/Alexander-Miller/treemacs/blob/master/Extensions.org

;; TODO:
;; - Right click menu on buffers
;; - Make buffers open in the previous window?
;;   This might be working now but it wasn't working with a side-by-side split.

(require 'dash)
(require 'treemacs)
(require 'treemacs-treelib)

;; Force earmuff buffer names to the bottom
(defun buffertree--comparator (a b)
  (let ((a (buffer-name a))
        (b (buffer-name b)))
    (cond ((and (string-prefix-p "*" a)
                (not (string-prefix-p "*" b)))
           nil)
          ((and (not (string-prefix-p "*" a))
                (string-prefix-p "*" b))
           t)
          (t (string< a b)))))

(defun buffertree--buffer-major-modes ()
  (->> (buffer-list)
       (--reject (string-prefix-p " " (buffer-name it)))
       (--map (buffer-local-value 'major-mode it))
       (-distinct)
       (-sort #'string<)))

(defun buffertree--buffers-by-mode (mode)
  (->> (buffer-list)
       (--filter (eq mode (buffer-local-value 'major-mode it)))
       (--reject (string-prefix-p " " (buffer-name it)))
       (-sort #'buffertree--comparator)))

(defun buffertree--all-buffers ()
  ;; Use this to group by major mode
  ;; (flatten-list (mapcar #'buffertree--buffers-by-mode
  ;;                       (buffertree--buffer-major-modes)))
  (->> (buffer-list)
       (--reject (string-prefix-p " " (buffer-name it)))
       (-sort #'buffertree--comparator)))

(defun buffertree--refresh ()
  (treemacs-update-node 'buffertree-buffers t))

(defun buffertree-RET-buffer-action (&optional _)
  (let ((buffer (-some-> (treemacs-current-button)
                  (treemacs-button-get :buffer))))
    (when (buffer-live-p buffer)
      (pop-to-buffer buffer))))

(defun buffertree-visit-buffer-action (btn)
  (let ((buffer (treemacs-safe-button-get btn :buffer)))
    (when (buffer-live-p buffer)
      (pop-to-buffer buffer))))

(treemacs-define-entry-node-type buffertree-buffers
  :label (propertize "Buffers" 'face 'treemacs-root-face)
  :key 'buffertree-buffers
  :open-icon (treemacs-get-icon-value 'list)
  :closed-icon (treemacs-get-icon-value 'list)
  :children (buffertree--all-buffers)
  :child-type 'buffertree-buffer-leaf
  :visit-action #'treemacs-toggle-node
  :double-click-action #'treemacs-toggle-node)

(treemacs-define-leaf-node-type buffertree-buffer-leaf
  :icon (concat
         (nerd-icons-icon-for-mode (buffer-local-value 'major-mode item))
         " ")
  :label (propertize (or (buffer-name item) "#<killed buffer>")
                     'face 'treemacs-file-face
                     'help-echo
                     (concat " " (symbol-name (buffer-local-value 'major-mode item)) " "
                             (or (buffer-file-name item)
                                 "")))
  :key item
  :more-properties `(:buffer ,item)
  :visit-action #'buffertree-visit-buffer-action
  :double-click-action #'buffertree-RET-buffer-action
  :ret-action #'buffertree-RET-buffer-action)

(treemacs-enable-top-level-extension
 :extension 'buffertree-buffers
 :position 'top
 :predicate #'always)

(defun buffertree--goto-current-buffer (&rest _)
  (let* ((win (selected-window))
         (buf (current-buffer))
         (visible (progn (treemacs--select-visible-window)
                        (treemacs-is-treemacs-window? (selected-window)))))
    (when visible
      (buffertree--refresh)
      (treemacs-goto-extension-node `(buffertree-buffers ,buf))
      (treemacs--maybe-recenter 'on-distance)
      (select-window win t))))

(defun buffertree--refresh-when-visible (&rest _)
  (let* ((win (selected-window))
         (visible (progn (treemacs--select-visible-window)
                        (treemacs-is-treemacs-window? (selected-window)))))
    (when visible
      (buffertree--refresh)
      (select-window win t))))

(defadvice switch-to-next-buffer (after follow-after-next-buffer activate)
  (buffertree--goto-current-buffer))
(defadvice switch-to-prev-buffer (after follow-after-prev-buffer activate)
  (buffertree--goto-current-buffer))
(defadvice switch-to-buffer (after follow-after-swap-buffer activate)
  (buffertree--goto-current-buffer))

(add-hook 'treemacs-after-visit-functions #'buffertree--goto-current-buffer)
(add-hook 'window-configuration-change-hook #'buffertree--refresh-when-visible)
(defadvice delete-buffer (after delete-buffer-refresh-advice)
  (buffertree--refresh-when-visible))

(provide 'buffer-tree)
