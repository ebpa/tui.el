;;; tui-absolute-container.el --- Container for absolute positioning of contents        -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;; TODO: eliminate artist dependency
(require 'artist)
(require 'tui-buffer "components/tui-buffer.el")

;;; Code:

(tui-define-component tui-absolute-container
  ;; TODO: support labels for indirect buffers to help with development
  :documentation "A layout component to enable absolute positioning of children within this container.  Child elements are rendered using indirect buffers.

Child elements will be positioned by properties on the child elements themselves:
:x - Distance from the beginning of the line for positioning of content (defaults to 0).
:y - Distance from the top of the buffer for positioning content (defaults to 0).
:max-width - Maximum width of the copied content.
:max-height - Maximum height of the copied content."
  :get-initial-state
  (lambda ()
    (list :canvas-content ""
          :buffer (get-buffer-create (format " *tui-absolute-container-%d*" (tui-node-id component)))
          :child-buffers (make-hash-table :test #'equal)))
  :render
  (lambda ()
    (tui-let (&props children &state canvas-content buffer child-buffers)
      (tui-div
       (tui-span
        :children
        (-map-indexed
         (lambda (index child)
           (tui-buffer
            :ref `(lambda (ref)
                    (puthash ,index ref ,child-buffers))
            child))
         children))
       canvas-content)))
  :component-did-mount
  (lambda ()
    (tui-absolute-container--update component))
  :component-did-update
  (lambda (next-props next-state)
    (when (or (not (eq next-props (tui-get-props)))
              (not (tui--plist-equal (tui--plist-delete next-state :canvas-content) (tui--plist-delete (tui-get-state) :canvas-content))))
      (tui-absolute-container--update component))))

;; (defun tui-absolute--copy-contents (component)
;;   "Copy string content to the dependent buffer location represented by COMPONENT."
;;   (-let* ((props (tui--get-props component))
;;           (parent (tui-parent component))
;;           (width (plist-get props :width))
;;           (height (plist-get props :height))
;;           (x (plist-get props :x))
;;           (y (plist-get props :y))
;;           (children (tui-child-nodes component)))
;;     (when (and (tui-mounted-p parent)
;;                (-every-p
;;                 #'tui-mounted-p
;;                 children))
;;       (save-current-buffer
;;         (tui--save-point-row-column
;;          (tui--goto (tui-start (cl-first children)))
;;          (let* ((buffer-contents (buffer-substring (point-min) (point-max)))
;;                 (content (with-temp-buffer
;;                            (insert buffer-contents)
;;                            (artist-copy-generic 0 0 (- width 1) (- height 1)))))
;;            (when parent
;;              (tui--goto (tui-start parent)))
;;            (tui-absolute--paste content x y width height)))))))

(cl-defmethod tui-absolute-container--update ((container tui-absolute-container))
  "Re-render CANVAS-- replacing its content with its separately rendered children."
  ;; CLEANUP: split this function
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
                                 :buffer (format " *tui-absolute-container-%d-%s*" (tui-node-id container) (tui--new-id))
                                 child))
             (puthash key child-buffer child-buffers)
             (tui-render-element child-buffer))
           (let* ((child-content (tui-buffer--get-content child-buffer)))
             (with-current-buffer buffer
               (tui-absolute-container--paste child-content (or x 0) (or y 0))))))
       keyed-children)
      (let* ((canvas-content (with-current-buffer buffer
                               (buffer-string))))
        (tui--set-state container `(:canvas-content ,canvas-content))))))

(defun tui-absolute-container--paste (content x y)
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

;; (defun tui-absolute--paste (content x y width height)
;;   "Paste CONTENT at position X,Y filling WIDTH and HEIGHT."
;;   (dotimes (line height)
;;     (let ((string (pop content)))
;;       (artist-move-to-xy x (+ y line))
;;       (delete-region (point) (min (+ (point) width)
;;                                   (save-excursion (end-of-line)
;;                                                   (point))))
;;       (when string
;;         (insert string)))))

;; (defun tui-absolute--update-all ()
;;   "Update all indirect ‘tui-absolute-container’ elements."
;;   ;; TODO: track whether a buffer has been modified since last update
;;   (let ((instances
;;          (-sort
;;           (lambda (a b)
;;             (> (tui--node-height a)
;;                (tui--node-height a)))
;;           (tui-component-instances 'tui-absolute-container))))
;;     (mapc
;;      #'tui-absolute--copy-contents
;;      instances)))

;;(add-hook 'tui-update-hook #'tui-absolute--update-all)

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

(cl-defmethod tui--update ((this tui-absolute-container) &optional next-props next-state force)
  "Wrap the update lifecycle method to preserve position based on the row and column of the point rather than using a marker."
  (tui--save-point-row-column
   (cl-call-next-method this next-props next-state force)))

;; (defun tui-absolute-container--show-indirect-buffer (&optional element)
;;   "Show indirect buffer in a separate window."
;;   (interactive)
;;   (unless element
;;     (setq element (tui-get-element-at (point) 'tui-absolute-container)))
;;   (-when-let* ((buffer-name (plist-get (tui--get-state element) :buffer-name)))
;;     (switch-to-buffer-other-window buffer-name)))

;; (defun tui-absolute--show-all-indirect-buffers ()
;;   "Show all indirect buffers for the current content tree."
;;   (interactive)
;;   (tui-map-subtree
;;    (lambda (element)
;;      (when (tui-absolute-p element)
;;        (tui-absolute--show-indirect-buffer element)))
;;    (tui-root-node)))

;; (defun tui-absolute--clean-up-indirect-buffers ()
;;   ""
;;   (interactive)
;;   (kill-matching-buffers "\\ \\*Tui-"))

(provide 'tui-absolute-container)

;;; tui-absolute-container.el ends here
