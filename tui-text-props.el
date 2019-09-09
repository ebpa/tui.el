;;; tui-text-props.el --- Text properies       -*- lexical-binding: t; -*-

;;; Commentary:
;;

(require 'tui-util)

(defvar tui--text-props
  (make-hash-table :weakness 'key)
  "Cache for inherited text properties (keys and values) of elements.  Keys are elements and values are text properties encoded as alists.")

(defvar tui--own-text-prop-keys
  (make-hash-table :weakness 'key)
  "Cache of text property keys set on each element.")

(defvar tui--grouped-text-props
  (make-hash-table :weakness 'key)
  "Cache for text properties of elements (not inherited).  Text properties are alists grouped by replacement mode (REPLACE PUSH APPEND SAFE).")

(defun tui--get-merged-text-props (element)
  "Return the calculated text props of ELEMENT as an alist.

This includes inherited text properties."
  (let* ((cached-props (gethash element tui--text-props 'miss)))
    (if (eq cached-props 'miss)
        (let* ((parent (tui-parent element)))
          (puthash element
                   (tui--extend-text-props (when parent (tui--get-merged-text-props parent))
                                           (tui--get-grouped-text-props element))
                   tui--text-props))
      cached-props)))

(defun tui--get-text-prop-keys (element)
  ""
  (let* ((keys (gethash element tui--own-text-prop-keys 'miss)))
    (if (eq keys 'miss)
        (puthash element
                 (-uniq
                  (mapcar #'car
                          (apply #'append
                                 (tui--get-grouped-text-props element))))
                 tui--own-text-prop-keys)
      keys)))

(defun tui-get-text-props (element)
  "Return the calculated text properties of ELEMENT as a plist."
  (let* ((keys (make-hash-table :test #'equal))
         text-props)
    (cl-loop for (key value) on (tui--get-merged-text-props element) by #'cddr
             do
             (unless (gethash key keys)
               (push value text-props)
               (push key text-props)
               (puthash key key keys)))
    text-props))

(defun tui--get-grouped-text-props (element)
  "Return text props grouped by their application rule (replace push append safe) applied that should be applied to children of ELEMENT."
  (or (gethash element tui--grouped-text-props)
      (let* ((props (tui--get-props element))
             (replace (or (plist-get props :text-props)
                          (plist-get props :text-props-replace)))
             (push (plist-get props :text-props-push))
             (append (plist-get props :text-props-append))
             (safe (plist-get props :text-props-safe)))
        (puthash element
                 (list replace push append safe)
                 tui--grouped-text-props))))

(defun tui--extend-text-props (target-props grouped-props)
  "Internal function.

Merge GROUPED-PROPS text property descriptions structured as `(replace push append safe)' into TARGET-PROPS with proper inheritance."
  (-let* (((replace push append safe) grouped-props)
          (target-props (nreverse (cl-loop for (key value) on target-props by #'cddr
                                           collect (cons key value))))
          (prop-table (make-hash-table :test #'equal))
          (miss (make-symbol "miss"))
          (merged-props nil))
    (cl-loop for (key . value) in target-props
             do
             (puthash key value prop-table))
    (cl-loop for (key value) on safe by #'cddr
             for existing-value = (gethash key prop-table miss)
             do
             (if (eq existing-value miss)
                 (puthash key value prop-table)))
    (cl-loop for (key value) on push by #'cddr
             for existing-value = (gethash key prop-table miss)
             do
             (cond
              ((eq existing-value miss)
               (puthash key value prop-table))
              ((listp existing-value)
               (puthash key (cons value existing-value) prop-table))
              (t
               (puthash key (cons value (list existing-value)) prop-table))))
    (cl-loop for (key value) on append by #'cddr
             for existing-value = (gethash key prop-table miss)
             do
             (cond
              ((eq existing-value miss)
               (puthash key value prop-table))
              ((listp existing-value)
               (puthash key (append existing-value
                                    value)
                        prop-table))
              (t
               (puthash key (cons existing-value (list value))
                        prop-table))))
    (cl-loop for (key value) on replace by #'cddr
             do
             (puthash key value prop-table))
    (maphash (lambda (key value)
               (setf merged-props (append (list key value) merged-props)))
             prop-table)
    merged-props))

;; (defun tui--get-calculated-grouped-text-props (node)
;;   "Return a list of text properties inherited from NODE and NODE's parent elements."
;;   (if (not node)
;;       '(nil nil nil nil)
;;     (or (tui--clean-plist (gethash node tui--text-props))
;;         (puthash node
;;                  (tui--apply-grouped-text-props
;;                   (tui--get-calculated-grouped-text-props (tui-parent node))
;;                   (tui--get-grouped-text-props node))
;;                  tui--text-props))))

;; (defun tui--clear-cached-text-props (node)
;;   ;; TODO: inadvisable; doesn't leverage inheritance structure
;;   "Clear cache text props for NODE.";; and all of its descendents."
;;   (remhash node tui--text-props)
;;   (remhash node tui--grouped-text-props)
;;   ;; (let ((nodes (list node)))
;;   ;;   (while nodes
;;   ;;     (let ((node (pop nodes)))
;;   ;;       (remhash node tui--text-props)
;;   ;;       (remhash node tui--grouped-text-props)
;;   ;;       (setf nodes (append (tui-child-nodes node)
;;   ;;                           nodes)))))
;;   )

;; (cl-defmethod tui--apply-text-props ((node tui-node))
;;   ""
;;   nil)

;; (cl-defmethod tui--apply-text-props ((element tui-element))
;;   ""
;;   (-let* (((start . end) (tui-segment element))
;;           ((replace push append safe) (tui--get-grouped-text-props element))
;;           (buffer (marker-buffer start)))
;;     (when replace
;;       (tui-put-text-properties start end replace buffer t))
;;     (when push
;;       (tui-put-text-properties start end push buffer 'push))
;;     (when append
;;       (tui-put-text-properties start end append buffer 'append))
;;     (when safe
;;       (tui-put-text-properties start end safe buffer nil))))

(defun tui--apply-inherited-text-props (start end element &optional object)
  ;; CLEANUP: bad function signature
  "Internal function to apply inherited text properties.
Applies text properties to region between START and END inherited from ELEMENT.

Optional argument OBJECT is a string to which the properties be applied.  START and END should indicate positions within that string."
  (unless start (setq start (tui-start element)))
  (unless end (setq end (tui-end element)))
  (-let* ((text-props (tui-get-text-props element)))
    (tui-put-text-properties start end text-props (marker-buffer start) t)))

(defun tui--update-text-props (subtree changed-props)
  "Update text properties in SUBTREE."
  (cl-loop for (key value) on changed-props by #'cddr
           do
           ;; set the text properties for the entire region
           (let* ((start (tui-start subtree))
                  (end (tui-end subtree)))
             (put-text-property start end key value (marker-buffer start)))
           ;; find first descendents that touch changed text-props
           (tui-map-subtree
            (lambda (node)
              (when (member key (gethash node tui--text-props))
                ;; update this node
                ;;(tui--apply-inherited-text-props 
                
                ))
            subtree)))



(provide 'tui-text-props)
;;; tui-text-props.el ends here
