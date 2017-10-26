;;; tui-buffer.el --- Buffer container component


;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(tui-define-component tui-buffer
  ;; TODO: suppoort both major and minor modes
  :documentation "Component representing a buffer."
  :prop-documentation
  (:buffer "Buffer or buffer name to use or create."
           :children "Content to display in the buffer."
           :mode "Major mode to set in the buffer."
           :keymap "Local keymap to use in the buffer.")
  :get-default-props
  (lambda ()
    (list :mode 'special-mode))
  :mount
  (lambda (component &optional start end parent)
    (let* ((props (tui--get-props component))
           (buffer (or (plist-get props :buffer)
                       (generate-new-buffer "*tui-buffer*")))
           (mode (plist-get props :mode))
           (keymap (plist-get props :keymap))
           ;; TODO
           ;; (revert-force-update (plist-get props :revert-props-function))
           (marker-list (tui-marker-list-create))
           start end)
      (with-current-buffer (get-buffer-create buffer)
        (let ((inhibit-read-only t))
          (tui--unmount-buffer-content)
          (push component tui--content-trees)
          (erase-buffer)
          ;; TODO
          ;; (when revert-props-function
          ;;   (setq-local revert-buffer-function (lambda (ignore-auto noconfirm)
          ;;                                        (listgrid-refresh-grid))))
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
          (cl-call-next-method component start end parent marker-list)))))
  :render
  (lambda ()
    (plist-get (tui-get-props) :children)))

(provide 'tui-buffer)

;;; tui-buffer.el ends here
