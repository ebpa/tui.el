;;; tui-buffer.el --- Buffer container component       -*- lexical-binding: t; -*-


;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(defvar-local tui-buffer--ref nil)

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
    ;;(message "MOUNT")
    (let* ((props (tui--plist-merge (tui--funcall #'tui-get-default-props component)
                                 (tui--get-props component)))
           (buffer (or (plist-get props :buffer)
                       (get-buffer-create (format " *tui-buffer-%d*" (tui-node-id component)))))
           (mode (plist-get props :mode))
           (keymap (plist-get props :keymap))
           (marker-list (tui-marker-list-create))
           (init-fn (plist-get props :init-fn))
           start end)
      (setf (tui-component-props component) props)
      (setf (tui-component-state component) (list :buffer-ref buffer))
      (with-current-buffer (get-buffer-create buffer)
        (let ((inhibit-read-only t))
          ;; (save-excursion
          ;;   (tui--unmount-buffer-content))
          (push component tui--content-trees)
          (erase-buffer)
          (setq-local revert-buffer-function (lambda (ignore-auto noconfirm)
                                               (tui-force-update-buffer)))
          (if mode
              (funcall mode))
          ;; Reference for local buffer logic
          (setq-local tui-buffer--ref component)
          ;; (message "component value: %S" (and component (tui--type component)))
          ;; (message "tui-buffer--ref value: %S" (and tui-buffer--ref (tui--type tui-buffer--ref)))
          ;; (message "Set tui-buffer--ref in (%s) to node (%s) with id: %d" (buffer-name (current-buffer)) (tui--type component) (tui-node-id component))
          (when keymap
            (let* ((keymap (copy-keymap keymap)))
              (set-keymap-parent keymap (current-local-map))
              (use-local-map keymap)))
          ;;(switch-to-buffer buffer) ;; TODO: make configurable?
          (setf (tui-node-marker-list component) marker-list)
          (setq start (tui-marker-list-insert marker-list (point-marker)))
          (setq end (cl-second (tui-marker-list-split-node marker-list start)))
          (cl-call-next-method component start end parent marker-list)
          ;; (message "Set tui-buffer in (%s) to node (%s) with id: %d" (buffer-name (current-buffer)) (tui--type component) (tui-node-id component))
          ;; (message "tui-buffer--ref value: %S" (and tui-buffer--ref (tui--type tui-buffer--ref)))
          ;; (make-local-variable 'after-change-functions)
          ;; (add-to-list 'after-change-functions #'tui-absolute-container--update-parent)
          (when init-fn
            (funcall init-fn))))))
  :render
  (lambda ()
    (plist-get (tui-get-props) :children))
  ;; :component-did-update
  ;; (lambda (next-props next-state)
  ;;   (with-current-buffer (marker-buffer (tui-start component))
  ;;     ;;(-when-let* ((parent (tui-parent component 'tui-absolute-container)))
  ;;     (funcall #'tui-absolute-container--update-parent)))
  )


(cl-defmethod tui-buffer--get-content ((buffer tui-buffer))
  "Return the `buffer-string' value for BUFFER."
  (let* ((buffer-ref (plist-get (tui--get-state buffer) :buffer-ref)))
    (with-current-buffer buffer-ref
      (buffer-string))))

(provide 'tui-buffer)

;;; tui-buffer.el ends here
