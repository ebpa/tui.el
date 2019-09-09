;;; tui-absolute-container.el --- Container for absolute positioning of contents        -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;; TODO: eliminate artist dependency
(require 'artist)
(require 'tui-canvas "components/tui-canvas.el")
(require 'tui-buffer "components/tui-buffer.el")
(require 'subr-x)

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
  (lambda (this)
    (list :canvas-ref (tui-create-ref)
          ;; :buffer (get-buffer-create (format " *tui-absolute-container-%d*" (tui-node-id component)))
          :child-buffers (make-hash-table :test #'equal)))
  :render
  (lambda (this)
    (tui-let (&props children &state canvas-content canvas-ref child-buffers)
      (tui-div
       (tui-span
        :children
        (-map-indexed
         (lambda (index child)
           (tui-buffer
            :ref `(lambda (ref)
                    (puthash ,index ref ,child-buffers))
            :index index
            child))
         children))
       (tui-canvas
        :ref canvas-ref))))
  :component-did-mount
  (lambda (this)
    (tui-absolute-container--update component))
  :component-did-update
  (lambda (this next-props next-state)
    ;; (when (or (not (eq next-props (tui-get-props)))
    ;;           (not (tui--plist-equal (tui--plist-delete next-state :canvas-content) (tui--plist-delete (tui-get-state) :canvas-content))))
    ;;   (tui-absolute-container--update this))
    ))

(defun tui-absolute-container--update-parent (&rest ignore)
  "Helper to update the parent container."
  (message (format "BUFFER-UPDATED EVENT (%s)" (buffer-name (current-buffer))))
  (-when-let* ((container (tui-parent tui-buffer--ref 'tui-absolute-container)))
    ;;(message (format "BUFFER-UPDATED EVENT %d (%s)" (tui-node-id container) (buffer-name (current-buffer))))
    ;;(display-warning 'tui-absolute-container (format "BUFFER-UPDATED EVENT %d (%s)" (tui-node-id container) (buffer-name (current-buffer))) :debug tui-log-buffer-name)
    (tui-absolute-container--update container)))

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
  "Re-render CANVAS - replacing its content with its separately rendered children."
  ;; CLEANUP: split this function
  (message "UPDATE %S (%d)" (tui--type container) (tui-node-id container))
  (-let* ((props (tui--get-props container))
          (children (plist-get props :children))
          (state (tui--get-state container))
          (canvas (tui-ref-element (plist-get state :canvas-ref)))
          (child-buffers (plist-get state :child-buffers)))
    (tui-canvas-erase canvas t)
    ;; (let* ((keyed-children (-map-indexed
    ;;                         (lambda (index child)
    ;;                           (let* ((key (or (when (tui-node-p child)
    ;;                                             (plist-get (tui--get-props child) :key))
    ;;                                           index)))
    ;;                             (cons key child)))
    ;;                         children)))
    (mapc
     (lambda (child-buffer)
       (-let* ((child-content (tui-buffer--get-content child-buffer))
               ((child) (plist-get (tui--get-props child-buffer) :children));; (child-buffer (gethash key child-buffers))
               ((&plist :x x :y y) (tui--get-props child)))
         ;;   (if child-buffer
         ;;       (tui--set-props child-buffer (list :children (list child)))
         ;;     (setq child-buffer (tui-buffer
         ;;                         :buffer (format " *tui-absolute-container-%d-%s*" (tui-node-id container) (tui--new-id))
         ;;                         child))
         ;;     (puthash key child-buffer child-buffers)
         ;;     (tui-render-element child-buffer))
         (tui-canvas--paste-content-at canvas child-content (or x 0) (or y 0) t)))
     (-sort 
      (lambda (child-a child-b)
        (<
         (plist-get (tui--get-props child-a) :index)
         (plist-get (tui--get-props child-b) :index)))
      (hash-table-values child-buffers)))
    (tui-force-update canvas)))

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

(defun tui-absolute-container--needing-update ()
  ""
  (-let* ((-compare-fn #'eq)
          (containers
           (-uniq
            (-non-nil
             (mapcar
              (lambda (buffer)
                (tui--mark-buffer-clean buffer)
                (-when-let* ((buffer-element (buffer-local-value 'tui-buffer--ref buffer)))
                  (tui-parent buffer-element 'tui-absolute-container)))
              (tui--updated-buffers)))))
          (containers-by-height (-sort
                                 (-lambda ((height-a) (height-b))
                                   (> height-a height-b))
                                 (mapcar (lambda (container)
                                           (cons (tui--node-height container) container))
                                         containers))))
    (mapcar #'cdr containers-by-height)))

(defun tui-absolute-container--update-all ()
  "Update all indirect ‘tui-absolute-container’ elements."
  (let* ((inhibit-modification-hooks t)
         (containers (tui-absolute-container--needing-update)))
    (when containers
      (display-warning 'tui-absolute-container (format "UPDATING %S" (mapcar #'tui-node-id containers)) :debug tui-log-buffer-name))
    (mapc #'tui-absolute-container--update containers)))

(add-hook 'tui-update-hook #'tui-absolute-container--update-all)

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

(defun tui--show-indirect-buffer (element)
  "Show indirect buffer in a separate window."
  (interactive (list (tui-get-element-at (point) 'tui-absolute-container)))
  (-when-let* ((buffer-name (plist-get (tui--get-state element) :buffer-ref)))
    (switch-to-buffer-other-window buffer-name)))

(defun tui--show-all-indirect-buffers ()
  "Show all indirect buffers for the current content tree."
  (interactive)
  (tui-map-subtree
   (lambda (element)
     (when (tui-buffer-p element)
       (tui--show-indirect-buffer element)))
   (tui-root-node)))

;; (defun tui-absolute--clean-up-indirect-buffers ()
;;   ""
;;   (interactive)
;;   (kill-matching-buffers "\\ \\*Tui-"))

(provide 'tui-absolute-container)
;;; tui-absolute-container.el ends here
