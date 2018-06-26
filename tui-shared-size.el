;;; tui-shared-size.el --- Shared sizes       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(eval-when-compile (require 'cl-lib))
(require 'tui-core)

;; TODO: based on length distribution  (95th percentile?)
;;; Code:

(cl-defstruct (tui-shared-size (:constructor nil)
                            (:constructor tui-shared-size-create (&key size)))
  size
  element-sizes
  callbacks)

(cl-defmethod tui-size (size)
  size)

(cl-defmethod tui-size ((shared-size tui-shared-size))
  ""
  (tui-shared-size-size shared-size))

(cl-defmethod tui-request-size ((shared-size tui-shared-size) size &optional callback)
  ""
  (push size (tui-shared-size-element-sizes shared-size))
  (push callback (tui-shared-size-callbacks shared-size)))

(cl-defmethod tui-recalculate-size ((shared-size tui-shared-size))
  (let* ((element-sizes (tui-shared-size-element-sizes shared-size))
         (pixel-unit (listp (cl-first element-sizes)))
         (new-size (when element-sizes
                     (apply #'max (if pixel-unit
                                      (mapcar #'car element-sizes)
                                    element-sizes))))
         (callbacks (tui-shared-size-callbacks shared-size)))
    (when new-size
      (setf (tui-shared-size-size shared-size)
            (if pixel-unit
                (list new-size)
              new-size))
      (mapc #'funcall callbacks))))

(provide 'tui-shared-size)

;;; tui-shared-size.el ends here
