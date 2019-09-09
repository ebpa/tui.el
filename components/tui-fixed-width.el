;;; tui-fixed-width.el --- Constrain contents to a fixed width       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;; Implementation possibilities
;;   -'display text property
;;   -'display overlay property
;;   -delete overflow (and store as a property)

(eval-when-compile (require 'cl))
(require 'tui-core)
(require 'tui-shared-size)
(require 'tui-layout)

;;; Code:

(defvar tui-fixed-width--suppress-recalculation nil)

(tui-define-component tui-fixed-width
  :documentation "Container component to constrain content to a declared width.

It :width value is nil, the component width is variable."
  :prop-documentation
  (:minimum-padding "Minimum padding"
                    :align "One of: `left', `center', `right'.  (**not yet implemented**)"
                    :width "Width (integer count of characters) or a shared width object.")
  :get-default-props
  (lambda ()
    (list :minimum-padding 0
          :align 'left))
  :render
  (lambda (_)
    (tui-let (&props children align)
      (list "" children "")))
  :component-did-mount
  (lambda (_)
    (unless tui-fixed-width--suppress-recalculation
      (tui-fixed-width--request-width component)))
  :component-did-update
  (lambda (_ pref-props prev-state)
    (tui-fixed-width--request-width component)))

(defun tui-fixed-width--request-width (component)
  "Respond to a change in content size in COMPONENT."
  (lexical-let ((component component)) ;; TODO: is this needed?
    (-let* (((&plist :width desired-width
                     :minimum-padding minimum-padding)
             (tui--get-props component)))
      (cond
       ((tui-shared-size-p desired-width)
        (tui-request-size desired-width
                          (+ (tui-length (tui-fixed-width--content component))
                             (or minimum-padding 0))
                          component))
       (desired-width
        (tui-fixed-width--update component))))))

(defun tui-fixed-width--update (component)
  "Update COMPONENT width."
  (-let* ((inhibit-read-only t)
          ((&plist :padding-node padding-node)
           (tui--get-state component))
          ((&plist :width desired-width
                   :minimum-padding minimum-padding)
           (tui--get-props component))
          (padding-width nil)
          (overflow-length nil))
    (when (and desired-width
               (not (eq desired-width 'variable))
               (tui-mounted-p component))
      ;; Shared width; we are only interested in its prescribed width
      (if (tui-shared-size-p desired-width)
          (setq desired-width (tui-size desired-width)))
      (cond
       ;; TODO: accept complex pixel specifications (https://www.gnu.org/software/emacs/manual/html_node/elisp/Pixel-Specification.html#Pixel-Specification)
       ;; TODO: testing of pixel-based widths
       ;; ((and desired-width
       ;;       (listp desired-width))
       ;;  (-let* ((current-width (tui-pixel-width component))
       ;;          (desired-width (car desired-width))
       ;;          (width-difference (tui--width-difference desired-width current-width)))
       ;;    (cond
       ;;     ((> width-difference 0)
       ;;      (setq padding-width (list width-difference)))
       ;;     ((< width-difference 0)
       ;;      (setq overflow-length (tui--overflow-length (tui-start component) (tui-end component) desired-width))
       ;;      (setq padding-width (list (- desired-width (tui-segment-pixel-width (tui-start component) (- (tui-end component) overflow-length)))))))))
       ((numberp desired-width)
        (let* ((content-length (tui-string-width (tui-fixed-width--content component))))
          (tui--truncate-overflow component desired-width)
          (when (< content-length desired-width)
            (tui--set-padding component (- desired-width content-length)))))
       ((null desired-width))
       (t
        (warn "Unknown width value format"))))))

(defun tui-fixed-width--content (component)
  (second (tui-child-nodes component)))

(defun tui--truncate-overflow (component length)
  "Truncate overflow of COMPONENT that overflows LENGTH characters."
  (let* ((content (tui-fixed-width--content component)))
    (when (> (tui-end component) (+ (tui-start component) length))
      (with-current-buffer (marker-buffer (tui-start component))
        (delete-region (+ (tui-start component) length)
                       (tui-end component))))))

(defun tui--set-padding (component width)
  "Internal function to manually update the width of padding nodes."
  (-let* (((&plist :align align) (tui--get-props component))
          (child-nodes (tui-child-nodes component)))
    (if (eq align 'center)
        (-let* (((left-padding content right-padding) child-nodes))
          (tui--set-padding-node-width right-padding (/ width 2))
          (tui--set-padding-node-width left-padding (- width (/ width 2))))
      (let* ((padding (pcase align
                        ('left (third child-nodes))
                        ('right (first child-nodes))
                        (_ (error "Unexpected alignment value: %S" align)))))
        (tui--set-padding-node-width padding width)))))

(defun tui--set-padding-node-width (padding-node width)
  "Internal function to manually update the width of padding nodes."
  (tui--with-open-node
    padding-node
    (delete-region (tui-start padding-node) (tui-end padding-node))
    (insert (propertize (make-string width ? )
                        'font-lock-ignore t
                        'cursor-intangible t))
    (tui--apply-inherited-text-props (tui-start padding-node) (tui-end padding-node) padding-node)))

(provide 'tui-fixed-width)
;;; tui-fixed-width.el ends here
