;;; tui-live-reloading.el --- Live reloading and instance tracking       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defvar tui-live-reloading t
  "Update components whenever their definitions are updated.")

(defvar tui--component-instance-table
  (make-hash-table)
  "Hash table for tracking object instances instances.")

(defun tui--register-instance (instance)
  "Internal function to track INSTANCE."
  (let* ((class (tui--object-class instance))
         (instances (or (gethash class tui--component-instance-table)
                        (puthash class (make-hash-table :weakness 'key)
                                 tui--component-instance-table))))
    (puthash instance nil instances)))

(defun tui-component-instances (class)
  "Return a list of live instances of CLASS."
  (-when-let* ((instance-table (gethash class tui--component-instance-table)))
    (-filter
     #'tui-mounted-p
     (hash-table-keys instance-table))))

(defun tui-force-update-component-instances (class)
  "Force instances of CLASS to update.  Updated rendering lifecycle logic is applied."
  (remhash class tui--default-props-table)
  (mapc #'tui-force-update (tui-component-instances class)))

(provide 'tui-live-reloading)

;;; tui-live-reloading.el ends here
