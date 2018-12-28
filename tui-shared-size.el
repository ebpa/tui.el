;;; tui-shared-size.el --- Shared sizes       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(eval-when-compile (require 'cl-lib))
(require 'tui-core)
(require 'subr-x)

;;; Code:


(cl-defstruct (tui-shared-size (:constructor nil)
                            (:constructor tui-shared-size-create (&key size)))
  size
  (element-sizes (make-hash-table)))

(cl-defmethod tui-size (size)
  ;; TODO: implement as a generic size lookup? (WIDTH . HEIGHT)?
  size)

(cl-defmethod tui-size ((shared-size tui-shared-size))
  "Return the current prescribed size of SHARED-SIZE object."
  (tui-shared-size-size shared-size))

(cl-defmethod tui-request-size ((shared-size tui-shared-size) size element)
  "Register request by ELEMENT that SHARED-SIZE equal SIZE."
  ;; TODO: overload ELEMENT to optionally be a callback
  (puthash element size (tui-shared-size-element-sizes shared-size)))

(cl-defmethod tui-recalculate-size ((shared-size tui-shared-size))
  "Recalculate SHARED-SIZE based requested sizes by its elements."
  ;; TODO: support complex sizing (ex: based on length distribution  (95th percentile?)
  ;; TODO: work out behavior of mixed character-pixel sizing (prefer one over the other? merge the two values?)
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
       (hash-table-keys (tui-shared-size-element-sizes shared-size))))))

(provide 'tui-shared-size)

;;; tui-shared-size.el ends here
