;;; tui-canvas.el --- Canvas component

;;; Commentary:
;; 

(tui-define-component tui-canvas
  :documentation ""
  :prop-documentation
  (
   :initial-content ""
   )
  :get-initial-state
  (lambda ()
    (list :buffer (get-buffer-create (format " *tui-canvas-%d*" (tui-node-id component)))
          :canvas-content ""))
  :render
  (lambda ()
    (tui-let (&state canvas-content)
      canvas-content)))

(cl-defmethod tui-canvas-erase ((canvas tui-canvas) &optional no-update)
  ""
  (-let* ((buffer (plist-get (tui--get-state canvas) :buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (tui--set-state canvas `(:canvas-content ,(buffer-string)) no-update))))

(cl-defmethod tui-canvas--paste-content-at ((canvas tui-canvas) content x y &optional no-update)
  "Paste CONTENT in CANVAS at position X,Y"
  ;;(when (tui-mounted-p canvas)
  (-let* ((buffer (plist-get (tui--get-state canvas) :buffer)))
    (with-current-buffer buffer
      ;; Disable stickiness on inserted content to avoid bleeding of
      ;; properties across blank space by the insertion behavior within
      ;; move-to-column (used in artist-move-to-xy).
      (setq content
            (propertize content 'front-sticky nil 'rear-nonsticky t))
      (cl-loop for line in (split-string content "\n")
               for line-num from 0
               do
               (artist-move-to-xy x (+ y line-num))
               (delete-region (point) (min (+ (point) (length line))
                                           (save-excursion (end-of-line)
                                                           (point))))
               (when line
                 (insert line)))
      (tui--set-state canvas `(:canvas-content ,(buffer-string)) no-update))))

(provide 'tui-canvas)
;;; tui-canvas.el ends here
