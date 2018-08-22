;;; tui-canvas.el --- Tui canvas layout component

;;; Commentary:
;; 

(require 'tui-core)
(require 'tui-buffer)

(tui-define-component tui-canvas
  :documentation ""
  :prop-documentation
  ()
  :render
  (lambda ()
    (tui-let (&state canvas-content buffer)
      (message "Render canvas! %S %S" (not (null canvas-content)) (not (null buffer)))
      (tui-div
       canvas-content)))
  :component-did-mount
  (lambda ()
    (tui-let (&props children)
      (tui-set-state
       (list :buffer (get-buffer-create (format " *tui-canvas-%d*" (tui-node-id component)))
             :child-buffers (make-hash-table :test #'equal)))))
  :component-did-update
  (lambda (next-props next-state)
    (when (not (eq next-props (tui-get-props)))
      (tui-canvas-update component))))

;;(tui-canvas-redraw tui-element)

(cl-defmethod tui-canvas-update ((canvas tui-canvas))
  "Re-render CANVAS-- replacing its content with its separately rendered children."
  (tui-let (&props children &state buffer child-buffers)
    (with-current-buffer buffer
        (erase-buffer))
    (let* ((keyed-children (-map-indexed
                            (lambda (index child)
                              (let* ((key (or (when (tui-node-p child)
                                                (plist-get (tui--get-props child) :key))
                                              index)))
                                (cons key child)))
                            children)))
      (mapcar
       (-lambda ((key . child))
         (-let* ((child-buffer (gethash key child-buffers))
                 ((&plist :x x :y y) (tui--get-props child)))
           (if child-buffer
               (tui--set-props child-buffer (list :children (list child)))
             (setq child-buffer (tui-buffer
                                 :buffer (format " *tui-canvas-%d-%s*" (tui-node-id canvas) (tui--new-id))
                                 child))
             (puthash key child-buffer child-buffers)
             (tui-render-element child-buffer))
           (let* ((child-content (tui-buffer--get-content child-buffer)))
             (message "Updating with child content: %S" child-content)
             (with-current-buffer buffer
               (tui-canvas--paste child-content (or x 0) (or y 0))))))
       keyed-children)
      (let* ((canvas-content (with-current-buffer buffer
                               (buffer-string))))
        (tui--set-state canvas `(:canvas-content canvas-content))))))

(defun tui-canvas--paste (content x y)
  "Paste CONTENT at position X,Y"
  (cl-loop for line in (split-string content "\n")
           for line-num from 0
           do
           (artist-move-to-xy x (+ y line-num))
           (delete-region (point) (min (+ (point) (length line))
                                       (save-excursion (end-of-line)
                                                       (point))))
           (when line
             (insert line))))

(cl-defmethod tui-canvas-draw ((canvas tui-canvas) element)
  "Draw ELEMENT to CANVAS at position according to :x and :y properties on ELEMENT.  Draws elements at the origin by default (0,0)."
  (let* ((props (tui--get-props canvas))
         (state (tui--get-state canvas)))
    
    ))

(provide 'tui-canvas)

;;; tui-canvas.el ends here
