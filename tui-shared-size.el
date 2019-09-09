;;; tui-shared-size.el --- Shared sizes       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(eval-when-compile (require 'cl-lib))
(require 'tui-core)
(require 'subr-x)

;;; Code:

(cl-defstruct (tui-shared-size (:constructor nil)
                               (:constructor tui-shared-size-create (&key size override-p)))
  size
  (element-sizes (make-hash-table))
  (override-p nil))

(cl-defmethod tui-size (size)
  "Treat non-`tui-shared-size' SIZE object as a size value."
  size)

(cl-defmethod tui-size ((shared-size tui-shared-size))
  "Return the current prescribed size of SHARED-SIZE object."
  (tui-shared-size-size shared-size))

(cl-defmethod tui-request-size ((shared-size tui-shared-size) size element)
  "Register request by ELEMENT that SHARED-SIZE equal SIZE."
  (puthash element size (tui-shared-size-element-sizes shared-size)))

(cl-defmethod tui-recalculate-size ((shared-size tui-shared-size))
  "Recalculate SHARED-SIZE based requested sizes by its elements."
  (when (not (tui-shared-size-override-p shared-size))
    (let* ((element-sizes (hash-table-values (tui-shared-size-element-sizes shared-size)))
           ;; (pixel-unit (listp (cl-first element-sizes)))
           (new-size (when element-sizes
                       (apply #'max element-sizes))))
      (when new-size
        (setf (tui-shared-size-size shared-size) new-size)
        (mapcar
         (lambda (element)
           (cond
            ((tui-fixed-width-p element)
             (tui-fixed-width--update element))))
         (hash-table-keys (tui-shared-size-element-sizes shared-size)))))))

(provide 'tui-shared-size)
;;; tui-shared-size.el ends here
