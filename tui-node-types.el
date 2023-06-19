;;; tui-node-types.el --- Core Tui Node Types       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'dash)
(require 'eieio)
(require 'tui-plist)

;;;; Node

(cl-defstruct tui-node
  "Base tui UI type."
  start ;; "Start of segment"
  end ;; "End of segment"
  content ;; "Current content of the node"
  mounted ;; "t if node is mounted in some buffer"
  relative-index ;; "Index position of node within parent (cached value)"
  marker-list ;; "Reference to the containing content tree's ordered marker list."
  id ;; "Unique identifier"
  (update-count 0))

(defun tui--new-id ()
  "Generate a TUI ID."
  ;; TODO: use a non-repeating pseudo-random sequence?
  (abs (random)))

;;;; Text Node

(cl-defstruct (tui-text-node (:include tui-node)
                             (:constructor nil)
                             (:constructor tui-text-node-create (&key content
                                                                      &aux (id (tui--new-id)))))
  "Primitive tui UI type for rendering text."
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

(defun tui--type (node)
  "Return the NODE's type as a symbol."
  (when (tui-node-p node)
    (aref node 0)))

(defun tui--object-class (obj)
  "Return symbol indicating the type of OBJ.

Return a struct tag if OBJ is a `cl-defstruct' or the class
symbol if an EIEIO class."
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
  "An `equal' function for `tui-node' objects.

Return t if NODE-A and NODE-B have content that is respectively `tui-equal'."
  (and (tui-node-p node-b)
       (tui-equal (tui-node-content node-a)
                  (tui-node-content node-b))))

(cl-defmethod tui-equal ((component-a tui-component) component-b)
  "An `equal' function for `tui-component' objects.

Return t if COMPONENT-A and COMPONENT-B are the same type and have equal properties."
  (and (eq (tui--type component-a)
           (tui--type component-b))
       (not (tui--plist-changes (tui--get-props component-a)
                                (tui--get-props component-b)))))

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

(cl-defun tui-component--docstring (documentation prop-documentation state-documentation)
  "Internal function for building component docstrings."
  (concat
   documentation
   (if prop-documentation
       (format "\n\nValid parameters include:\n%s"
               (s-join "\n"
                       (cl-loop for (key docstring) on prop-documentation by #'cddr
                                collect
                                (format "\t%S\t\t%s\n" key docstring))))
     "")
   (if state-documentation
       (format "\n\nInternal State variables:\n%s"
               (s-join "\n"
                       (cl-loop for (key docstring) on state-documentation by #'cddr
                                collect
                                (format "\t%S\t\t%s\n" key docstring))))
     "")))

(defun tui--cl-generic-remove-method (name qualifiers specializers)
  "cl-generic appears to lack an equivalent for common lisp's remove-method, so this should be sufficient for now to help clean up component definitions."
  (let* ((generic (cl-generic-ensure-function name))
         (mt (cl--generic-method-table generic))
         (me (cl--generic-member-method specializers qualifiers mt)))
    (unless (null me)
      (setf (cl--generic-method-table generic)
            (-filter (lambda (x) (not (eq x (car me)))) mt)))))

(defun tui-unintern (type)
  "Remove all definitions for component TYPE.

Return t if a component definition exists and was successfully
removed and return nil otherwise."
  (interactive (tui-read-component-type "Unintern tui component: "))
  (when (symbolp type)
    (setf (symbol-function type) nil)
    ;; TODO: Additional cl cleanup of struct definition
    (remhash type tui--default-props-table)
    (tui--cl-generic-remove-method 'tui-get-initial-state nil `(,type))
    (tui--cl-generic-remove-method 'tui--mount nil '(,type))
    (tui--cl-generic-remove-method 'tui-component-did-mount nil `(,type))
    (tui--cl-generic-remove-method 'tui-get-derived-state-from-props nil `(,type))
    (tui--cl-generic-remove-method 'tui-should-component-update nil `(,type))
    (tui--cl-generic-remove-method 'tui-render nil `(,type))
    (tui--cl-generic-remove-method 'tui-component-did-update nil `(,type))
    (tui--cl-generic-remove-method 'tui-component-will-unmount nil `(,type))))

(cl-defmacro tui--component-defstruct (name)
  "Internal helper macro for defining component structs.

Defines NAME as a struct resembling the structure of `tui-component'."
  `(progn
     (cl-defstruct (,name (:include tui-component)
                          (:constructor nil)
                          (:constructor ,(intern (format "%s-create" (symbol-name name)))
                                        (&key props invisible
                                              &aux (id (tui--new-id))))))
     ;; FIXME: this is a rather hacky way of suppressing function creation for component slots
     (mapc (-lambda ((slot _ignore))
             (unless (eq slot 'cl-tag-slot)
               (setf (symbol-function (intern (concat (symbol-name ',name) "-" (symbol-name slot)))) nil)
               (setf (symbol-function (intern (concat (symbol-name ',name) "-" (symbol-name slot) "--cmacro"))) nil)))
           (cl-struct-slot-info 'tui-component))
     ',name))

(provide 'tui-node-types)
;;; tui-node-types.el ends here
