;;; tui-absolute.el --- Absolute positioning container       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'artist)
(require 'tui-buffer "components/tui-buffer.el")

;;; Code:

(tui-define-component tui-absolute
  :documentation "A helper component to render a subtree at a particular position using a separate buffer."
  :prop-documentation
  (
   :x "Distance from the beginning of the line for positioning of content (indexed from zero)."
   :y "Distance from the top of the buffer for positioning content (indexed from zero)."
   :width "Width reserved for the content."
   :height "Height reserved for the content."
   )
  :get-initial-state
  (lambda ()
    (list :id (tui--new-id)))
  :render
  (lambda ()
    (tui-let (&props children &state id)
      (tui-buffer
       :buffer (format " *Tui-%d*" id)
       children)))
  :component-did-mount
  (lambda ()
    (tui--register-instance component)
    (tui-absolute--copy-contents component))
  :component-did-update
  (lambda (pref-props prev-state)
    (tui-absolute--copy-contents component)))

(defun tui-absolute--copy-contents (component)
  "Copy string content to the dependent buffer location represented by COMPONENT."
  (-let* ((props (tui--get-props component))
          (parent (tui-parent component))
          (width (plist-get props :width))
          (height (plist-get props :height))
          (x (plist-get props :x))
          (y (plist-get props :y))
          (children (tui-child-nodes component)))
    (when children
      (save-current-buffer
        (tui--save-point-row-column
         (tui--goto (tui-start (cl-first children)))
         (let* ((content (artist-copy-generic 0 0 (- width 1) (- height 1))))
           (when parent
             (tui--goto (tui-start parent)))
           (tui-absolute--paste content x y width height)))))))

(defun tui-absolute--paste (content x y width height)
  "Paste CONTENT at position X,Y filling WIDTH and HEIGHT."
  (dotimes (line height)
    (let ((string (pop content)))
      (artist-move-to-xy x (+ y line))
      (delete-region (point) (min (+ (point) width)
                                  (save-excursion (end-of-line)
                                                  (point))))
      (when string
        (insert string)))))

(defun tui-absolute--update-all ()
  "Update all indirect ‘tui-absolute’ elements."
  ;; TODO: track whether a buffer has been modified since last update
  (let ((instances
         (-sort
          (lambda (a b)
            (> (tui--node-height a)
               (tui--node-height a)))
          (tui-component-instances 'tui-absolute))))
    (mapc
     #'tui-absolute--copy-contents
     instances)))

(add-hook 'tui-update-hook #'tui-absolute--update-all)

(defmacro tui--save-point-row-column (&rest body)
  "Utility macro to restore point based on the row and column."
  (let ((row-num-var (make-symbol "row-num"))
        (col-num-var (make-symbol "col-num")))
    `(let* ((,row-num-var (line-number-at-pos))
            (,col-num-var (current-column)))
       (prog1 (progn ,@body)
         (goto-char (point-min))
         (forward-line (1- ,row-num-var))
         (move-to-column ,col-num-var)))))

(cl-defmethod tui--update ((this tui-absolute) &optional next-props next-state force)
  "Wrap the update lifecycle method to preserve position based on the row and column of the point rather than using a marker."
  (tui--save-point-row-column
   (cl-call-next-method this next-props next-state force)))

(provide 'tui-absolute)

;;; tui-absolute.el ends here
