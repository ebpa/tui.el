;;; tui-fixed-width.el --- Fixed width container

;;; Commentary:
;; Constrain contents to a fixed width

;; Implementation possibilities
;;   -'display text property
;;   -'display overlay property
;;   -delete overflow (and store as a property)

(require 'tui-core)
(require 'tui-shared-size)
(require 'tui-layout)

;;; Code:

(defvar tui-fixed-width-minimum-padding 0)

(tui-define-component tui-fixed-width
  :documentation "Render :contents with a fixed :width.

It :width value is nil, the component width is variable."
  :get-default-props
  (lambda ()
    (list :minimum-padding (or tui-fixed-width-minimum-padding
                               0)))
  :get-initial-state
  (lambda ()
    (list :padding-node (tui-text-node-create)))
  :render
  (lambda ()
    (let* ((props (tui-get-props))
           (state (tui-get-state))
           (content (plist-get props :children))
           (padding-node (plist-get state :padding-node)))
      (list content
            padding-node)))
  :component-did-mount
  (lambda ()
    (tui-fixed-width--request-width component))
  :component-did-update
  (lambda (pref-props prev-state)
    (tui-fixed-width--request-width component)))

(defun tui-fixed-width--request-width (component)
  "Respond to a change in content size in COMPONENT."
  (lexical-let ((component component)) ;; TODO: is this needed?
    (-let* ((props (tui-get-props))
            (desired-width (plist-get props :width)))
      (cond
       ((tui-shared-size-p desired-width)
        (tui-request-size desired-width (tui-visible-width component) (lambda () (tui-fixed-width--update component))))
       (desired-width
        (tui-fixed-width--update component))))))

;; TODO: callback?
;; TODO: max-width and min-width
(defun tui-fixed-width--update (component)
  "Helper component to constrain content within COMPONENT to a declared width."
  (-let* ((inhibit-read-only t)
          (props (tui--get-props component))
          (state (tui--get-state component))
          (padding-node (plist-get state :padding-node))
          (desired-width (plist-get props :width))
          (minimum-padding (plist-get props :minimum-padding))
          (padding-width nil)
          (overflow-length nil))
      (when (and desired-width
                 (not (eq desired-width 'variable)))
        ;; Shared width; we are only interested in its prescribed width
        (if (tui-shared-size-p desired-width)
            (setq desired-width (tui-size desired-width)))
        ;; TODO: accept complex pixel specifications (https://www.gnu.org/software/emacs/manual/html_node/elisp/Pixel-Specification.html#Pixel-Specification)
        (cond
         ((and desired-width
               (listp desired-width))
          (-let* ((current-width (tui-pixel-width component))
                  (desired-width (car desired-width))
                  (width-difference (tui--width-difference desired-width current-width)))
            (cond
             ((> width-difference 0)
              (setq padding-width (list width-difference)))
             ((< width-difference 0)
              (setq overflow-length (tui--overflow-length (tui-start component) (tui-end component) desired-width))
              (setq padding-width (list (- desired-width (tui-segment-pixel-width (tui-start component) (- (tui-end component) overflow-length)))))))))
         ((numberp desired-width)
          (-let* ((current-width (- (tui-end component) (tui-start component)))
                  (width-difference (- desired-width current-width)))
            (cond
             ((> width-difference 0)
              (setq padding-width width-difference))
             ((< width-difference 0)
              (setq overflow-length (- width-difference))))))
         ((null desired-width))
         (t
          (warn "Unknown width value format")))
        (when overflow-length
          (delete-region (- (tui-start padding-node) overflow-length) (tui-start padding-node)))
        (setq padding-width (tui--add-widths (or padding-width 0) minimum-padding)) ;; TODO: incorporate padding into width (currently just added)
        (when (and padding-width
                   (not (listp padding-width)))
          (tui--with-open-node
            padding-node
            (delete-region (tui-start padding-node) (tui-end padding-node))
            (insert-and-inherit (propertize (make-string padding-width ? )
                                            'font-lock-ignore t
                                            'invisible nil
                                            'display `(space :width ,padding-width))))))))

;; TODO
;; (tui-define-component listgrid-align-to
;;   :render
;;   (lambda ()
;;     (let* ((props (tui-get-props))
;;           (pos (plist-get props :pos))
;;           (children (plist-get props :children)))
;;       (list
;;        (propertize " " 'display `(space :align-to ,pos))
;;        children))))
  
;; (defun listgrid--align-to-zero (string)
;;   "Returns a string that has been aligned to the zero'th column."
;;   (concat
;;    (propertize " " 'display `(space :align-to 0))
;;    string))

;; TODO
;; (defun listgrid--align-to-offset (string offset)
;;   "Return a recomputed OFFSET based on align-to text properties in STRING."
;;   ;; find last
;;   )

;; TODO
;; (defun listgrid--substring-align-to-safe (string from &optional to)
;;   "Align-to -safe version of `substring'."
;;   (let ((props (listgrid--text-property-list string)))
;;     ;; find an align-to larger than
;;     )

;; (defun listgrid-align-right (string max-length)
;;   ""
;;   (concat
;;    (if (and max-length
;;             (> max-length (string-width string)))
;;        (make-string (- max-length (string-width string)) " "))
;;    string))

;; TODO
;; (defun listgrid--guess-alignment (value)
;;   ""
;;   (cond
;;    ((numberp value)
;;     :right)
;;    ((stringp value)
;;     :left)
;;    (t
;;     :left)))

(provide 'tui-fixed-width)

(provide 'tui-fixed-width)

;;; tui-fixed-width.el ends here
