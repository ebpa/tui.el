;;; tui-buffer.el --- Buffer container this       -*- lexical-binding: t; -*-


;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(defvar-local tui-buffer--ref nil
  "Reference for local buffer logic.")

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
   :init-fn "Function to call after the buffer has been set up."
   )
  :get-default-props
  (lambda ()
    (list :mode 'special-mode))
  :mount
  (lambda (this &optional start end parent)
    (let* ((props (tui--plist-merge (tui-get-default-props this)
                                    (tui--get-props this)))
           (buffer (get-buffer-create
                    (or (plist-get props :buffer)
                        (format " *tui-buffer-%d*" (tui-node-id this)))))
           (mode (plist-get props :mode))
           (keymap (plist-get props :keymap))
           (marker-list (tui-marker-list-create))
           (init-fn (plist-get props :init-fn))
           start end)
      (setf (tui-component-props this) props)
      (setf (tui-component-state this) (list :buffer-ref buffer))
      (with-current-buffer buffer
        (let* ((inhibit-read-only t))
          (when mode (funcall mode))
          (erase-buffer)
          (push this tui--content-trees)
          (setq-local revert-buffer-function (lambda (ignore-auto noconfirm)
                                               (tui-force-update-buffer)))
          (setq-local tui-buffer--ref this)
          (when keymap
            (let* ((keymap (copy-keymap keymap)))
              (set-keymap-parent keymap (current-local-map))
              (use-local-map keymap)))
          (setf (tui-node-marker-list this) marker-list)
          (setq start (tui-marker-list-insert marker-list (point-marker)))
          (setq end (cl-second (tui-marker-list-split-node marker-list start)))
          ;; (condition-case err
          (cl-call-next-method this start end parent marker-list)
          ;; (t (message "Error: %s" err)))
          ;; (make-local-variable 'after-change-functions)
          ;; (add-to-list 'after-change-functions #'tui-absolute-container--update-parent)
          (when init-fn
            (funcall init-fn))))
      this))
  :render
  (lambda (this)
    (tui-let* ((&props children buffer) this)
      (message "tui-buffer render() %s" buffer)
      children))
  :component-did-update
  (lambda (this next-props next-state)
    (with-current-buffer (marker-buffer (tui-start this))
      (-when-let* ((parent (tui-parent this 'tui-absolute-container)))
        (funcall #'tui-absolute-container--update-parent)))))

(cl-defmethod tui--update ((this tui-buffer) &optional next-props next-state force)
  "Pass updates through to content."
  ;; (edebug)
  ;;tui-force-update
  ;; (mapcar #'tui--update (tui-component-content this))
  (cl-call-next-method))

(cl-defmethod tui-buffer--get-content ((buffer tui-buffer))
  "Return the `buffer-string' value for BUFFER."
  (let* ((buffer-ref (plist-get (tui--get-state buffer) :buffer-ref)))
    (with-current-buffer buffer-ref
      (buffer-string))))

(provide 'tui-buffer)
;;; tui-buffer.el ends here
