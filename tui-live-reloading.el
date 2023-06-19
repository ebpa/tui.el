;;; tui-live-reloading.el --- Live reloading and instance tracking       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(require 'dash)
(require 'subr-x)

(require 'tui-core)

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
    (puthash instance nil instances)
    instance))

(defun tui-live-reloading--update-instances (component)
  ""
  (when (and tui-live-reloading
             (tui-component-symbol-p component))
    (tui-force-update-component-instances component)))

(defun tui-component-instances (class)
  "Return a list of live instances of CLASS."
  (-when-let* ((instance-table (gethash class tui--component-instance-table)))
    (-filter
     #'tui-mounted-p
     (hash-table-keys instance-table))))

(defun tui-force-update-component-instances (class)
  "Force instances of CLASS to update.

Updated rendering lifecycle logic is applied."
  (interactive (list (tui-read-component-type)))
  (remhash class tui--default-props-table)
  (--map
   (when (tui-mounted-p it)
     (tui-force-update it))
   (tui-component-instances class)))

(advice-add
 #'tui-create-element
 :filter-return #'tui--register-instance)
;; (advice-remove #'tui-create-element #'tui--register-instance)

(advice-add
 #'eval-defun
 :filter-return #'tui-live-reloading--update-instances)

(provide 'tui-live-reloading)
;;; tui-live-reloading.el ends here
