;;; tui-buffer.el --- Buffer container component       -*- lexical-binding: t; -*-


;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(tui-define-component tui-buffer
  ;; TODO: suppoort both major and minor modes
  ;; TODO: support inheriting text properties?
  :documentation "Component representing a buffer."
  :prop-documentation
  (
   :buffer "Buffer or buffer name to use or create."
   :children "Content to display in the buffer."
   :mode "Major mode to set in the buffer."
   :keymap "Local keymap to use in the buffer."
   )
  :get-default-props
  (lambda ()
    (list :mode 'special-mode))
  :mount
  (lambda (component &optional start end parent)
    (let* ((props (tui--plist-merge (tui--funcall #'tui-get-default-props component)
                                 (tui--get-props component)))
           (buffer (or (plist-get props :buffer)
                       (generate-new-buffer "*tui-buffer*")))
           (mode (plist-get props :mode))
           (keymap (plist-get props :keymap))
           (marker-list (tui-marker-list-create))
           (init-fn (plist-get props :init-fn))
           start end)
      (setf (tui-component-props component) props)
      (with-current-buffer (get-buffer-create buffer)
        (let ((inhibit-read-only t))
          (tui--unmount-buffer-content)
          (push component tui--content-trees)
          (erase-buffer)
          (setq-local revert-buffer-function (lambda (ignore-auto noconfirm)
                                               (tui-force-update-buffer)))
          (if mode
              (funcall mode))
          (when keymap
            (let* ((keymap (copy-keymap keymap)))
              (set-keymap-parent keymap (current-local-map))
              (use-local-map keymap)))
          ;;(switch-to-buffer buffer) ;; TODO: make configurable?
          (setf (tui-node-marker-list component) marker-list)
          (setq start (tui-marker-list-insert marker-list (point-marker)))
          (setq end (cl-second (tui-marker-list-split-node marker-list start)))
          (cl-call-next-method component start end parent marker-list)))
      (with-current-buffer (get-buffer-create buffer)
        (when init-fn
          (funcall init-fn)))))
  :render
  (lambda ()
    (plist-get (tui-get-props) :children)))

(provide 'tui-buffer)

;;; tui-buffer.el ends here
