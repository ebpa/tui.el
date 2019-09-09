;;; tui-node-types.el --- Core Tui Node Types       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

;;;; Node

(cl-defstruct tui-node
  ""
  start ;; "Start of segment"
  end ;; "End of segment"
  content ;; "Current content of the node"
  mounted ;; "t if node is mounted in some buffer"
  relative-index ;; "Index position of node within parent (cached value)"
  marker-list ;; "Reference to the containing content tree's ordered marker list."
  id ;; "Unique identifier"
  (update-count 0))

;;;; Text Node

(cl-defstruct (tui-text-node (:include tui-node)
                             (:constructor nil)
                             (:constructor tui-text-node-create (&key content
                                                                      &aux (id (tui--new-id)))))
  nil)


;;;; Element

(cl-defstruct (tui-element (:include tui-node)
                           (:constructor nil)
                           (:constructor tui-element-create (&key props invisible
                                                                  &aux (id (tui--new-id)))))
  props
  invisible ;; "Indicates whether the content element should be ignored when rendering."
  )

;;;; Component

(cl-defstruct (tui-component (:include tui-element))
  ;; "The base class for all components."
  state)

;;;; Print methods

(cl-defmethod cl-print-object ((node tui-node) stream)
  (princ (format "#<tui-node %d>" (tui-node-id node)) stream))

(cl-defmethod cl-object-print ((node tui-node) stream)
  (princ (format "#<tui-node %d>" (tui-node-id node)) stream))

(cl-defmethod cl-print-object ((element tui-element) stream)
  (princ (format "#<tui-element %d>" (tui-node-id element)) stream))

(cl-defmethod cl-object-print ((element tui-element) stream)
  (princ (format "#<tui-element %d>" (tui-node-id element)) stream))

(cl-defmethod cl-print-object ((component tui-component) stream)
  (princ (format "#<tui-component %s %d>" (tui--type component) (tui-node-id component)) stream))

(cl-defmethod cl-object-print ((component tui-component) stream)
  (princ (format "#<tui-component %s %d>" (tui--type component) (tui-node-id component)) stream))

;;;; Type helpers

(defun tui--object-class (obj)
  "Return the struct tag if OBJ is a ‘cl-defstruct’ or the class symbol if an EIEIO class."
  ;; CLEANUP
  (cond
   ((tui-node-p obj)
    (aref obj 0))
   ((and (featurep 'eieio)
         (eieio-object-p obj))
    (eieio-object-class obj))))

(defun tui--object-of-class-p (obj class)
  "Return non-nil if OBJ is an instance of CLASS or CLASS' subclasses."
  (if (and (featurep 'eieio)
           (eieio-object-p obj))
      (child-of-class-p (tui--object-class obj) class)
    (funcall (intern (format "%s-p" (symbol-name class))) obj)))

(defun tui--list-content-p (content)
  ;; CLEANUP: Eliminate this function? It shouldn't be necessary (element content is always a list and images should be detected first by the normalize function)
  "Return t if CONTENT is a list of content elements rather than potentially conflated content types (i.e. images)."
  (and (not (tui--image-p content))
       (listp content)))

(defun tui--image-p (content)
  "Return t if CONTENT is an image."
  (and (listp content)
       (eq (car content) 'image)))

(cl-defmethod tui-equal ((node-a tui-node) node-b)
  "An `equal' function for `tui-node' objects."
  (and (tui-node-p node-b)
       (tui-equal (tui-node-content node-a)
                  (tui-node-content node-b))))

(cl-defmethod tui-equal ((component-a tui-component) component-b)
  "An `equal' function for `tui-component' objects."
  (not (tui--plist-changes (tui--get-props component-a)
                           (tui--get-props component-b))))

(cl-defmethod tui-equal (obj-a obj-b)
  "An `equal' function which handles `tui-*' objects recursively as a special case."
  (and (not (tui-node-p obj-b))
       (cond
        ((eq obj-a obj-b)
         t)
        ((stringp obj-a)
         (equal obj-a obj-b))
        ((-cons-pair-p obj-a)
         (equal obj-a obj-b))
        ((and (listp obj-a)
              (listp obj-b)
              (not (-cons-pair-p obj-b)))
         (and (eq (length obj-a)
                  (length obj-b))
              (cl-loop for elt-a in obj-a
                       for elt-b in obj-b
                       always
                       (tui-equal elt-a elt-b)))))))

(provide 'tui-node-types)
;;; tui-node-types.el ends here
