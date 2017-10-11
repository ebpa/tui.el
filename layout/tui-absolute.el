;;; tui-absolute.el --- Absolute positioning container

;;; Commentary:
;; 

(require 'artist)
(require 'tui-buffer "components/tui-buffer.el")

;;; Code:

(tui-define-component tui-absolute
  :documentation "A helper component to render a subtree at a particular position using a separate buffer."
  :prop-documentation
  (:x "Distance from the beginning of the line for positioning of content (indexed from zero)."
      :y "Distance from the top of the buffer for positioning content (indexed from zero)."
      :width "Width reserved for the content."
      :height "Height reserved for the content.")
  :render
  (lambda ()
    (-let* ((props (tui-get-props))
            (children (plist-get props :children)))
      (tui-buffer
       :buffer (format "*Tui-%d*" (tui--new-id))
       children)))
  :component-did-mount
  (lambda ()
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
        (save-excursion
          (let (content)
            (tui--goto (tui-start (first children)))
            (setq content (buffer-substring (point-min) (point-max)))
            (when parent
              (tui--goto (tui-start parent)))
            (tui-absolute--paste (s-split "\n" content) x y width height)))))))

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
          (tui-component-instances 'cl-struct-tui-absolute))))
    (mapc
     #'tui-absolute--copy-contents
     instances)))

(add-hook 'tui-update-hook 'tui-absolute--update-all)

(provide 'tui-absolute)

;;; tui-absolute.el ends here
