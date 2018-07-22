;;; tui-core.el --- Core functions        -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(eval-when-compile (require 'cl-lib))
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'tui-dom)
(require 'tui-reconciler)
(require 'tui-layout)
(require 'tui-live-reloading)
(require 'tui-log)
(require 'tui-marker-list)
(require 'tui-node-types)
(require 'tui-text-props)
(require 'tui-util)

;;; Content constants

;;; Code:

(declare-function tui-marker-list-node-start "tui-marker-list.el")
(declare-function tui-marker-list-node-end "tui-marker-list.el")

(defvar tui-error-placeholder-string "�" "Placeholder string to indicate a broken component.")
(defvar tui--update-queue nil "Queue of updates to be committed.")
(defvar tui--applying-updates nil "Dynamic scope variable to indicate whether queued updates are being processed.")
(defvar tui-update-hook nil "Run after the update queue has been cleared.")

;;; Component lifecycle methods

(cl-defgeneric tui-get-default-props ()
  (:documentation "Return default properties for all instances of the component type.

Equivalent to React's createReactClass form: https://reactjs.org/docs/react-without-es6.html#declaring-default-props"))

(cl-defmethod tui-get-default-props ((component tui-component))
  "Empty default method"
  nil)

(cl-defgeneric tui-get-initial-state ()
  (:documentation "Return the initial component state for a new component instance.

Equivalent to React's createReactClass form: https://reactjs.org/docs/react-without-es6.html#setting-the-initial-state"))

(cl-defmethod tui-get-initial-state ((component tui-component))
  "Empty default method"
  nil)

(cl-defgeneric tui-component-did-mount ()
  (:documentation "Called immediately after a component is mounted.  `tui-set-state' may be used in this method.

React documentation: https://reactjs.org/docs/react-component.html#componentdidmount"))

(cl-defmethod tui-component-did-mount ((component tui-component))
  "Empty default method"
  nil)

(cl-defgeneric tui-get-derived-state-from-props (props state)
  (:documentation "Return a plist of any state values derived from PROPS or the current STATE.

React documentation: https://reactjs.org/docs/react-component.html#static-getderivedstatefromprops"))

(cl-defmethod tui-get-derived-state-from-props ((component tui-component) props state)
  "Empty default method"
  nil)

(cl-defgeneric tui-should-component-update (next-props next-state)
  (:documentation "Performance optimization method to prevent component updates.

Return nil to indicate that the component's output is not affected by the current change in props or state.

React documentation: https://reactjs.org/docs/react-component.html#shouldcomponentupdate"))

(cl-defmethod tui-should-component-update ((component tui-component) next-props next-state)
  "Empty default method"
  t)

(cl-defgeneric tui-render ()
  (:documentation "Return the component output content.

React documentation: https://reactjs.org/docs/react-component.html#render"))

(cl-defmethod tui-render ((component tui-component))
  "Empty default method"
  nil)

(cl-defgeneric tui-component-did-update (next-props next-state)
  (:documentation "Is called after all component updates.  It is not called for the initial render.

React documentation: https://reactjs.org/docs/react-component.html#componentdidupdate"))

(cl-defmethod tui-component-did-update ((component tui-component) next-props next-state)
  "Empty default method"
  nil)

(cl-defgeneric tui-component-will-unmount
    (:documentation "Called immediately before a component will unmount.

React documentation: https://reactjs.org/docs/react-component.html#componentwillunmount"))

(cl-defmethod tui-component-will-unmount ((component tui-component))
  "Empty default method"
  nil)

;;;; Internal Lifecycle helpers

(defun tui--funcall (func component &rest args)
  "Internal helper for invoking lifecycle methods.

Calls FUNC for COMPONENT (ARGS are arguments for the lifecycle
method) and appropriately binds `tui-get-props' and
`tui-get-state'."
  (when func
    (let* ((component component))
      (cl-letf (((symbol-function 'tui-get-props)
                 (lambda () (tui--get-props component)))
                ((symbol-function 'tui-get-state)
                 (lambda () (tui--get-state component))))
        (if (member func '(tui-component-did-mount tui-component-did-update tui--mount))
            (cl-letf (((symbol-function 'tui-set-state)
                       (lambda (new-state &optional no-update) (tui--set-state component new-state no-update))))
              (apply func component args))
          (apply func component args))))))

(cl-defmethod tui--mount ((node tui-node) start &optional end parent marker-list)
  "Internal use only.  Mount and insert NODE between START and END divisions."
  (when (tui-node-mounted node)
    (error "Component already mounted"))

  ;; CLEANUP: need a better solution to indicate the desired mount is in progress
  (setf (tui-node-mounted node) 'pending)

  (tui--set-parent node parent marker-list)

  (unless end (setq end start))
  (cl-assert (tui-marker-list-node-p start) t "Mount point is a tui-marker-list-node")
  (cl-assert (tui-marker-list-node-p end) t "Mount point is a tui-marker-list-node")
  (when (eq start end)
    (-let* (((start-division end-division) (tui--split-division node start)))
      (setq start start-division)
      (setq end end-division)))
  (setf (tui-node-start node) start)
  (setf (tui-node-end node) end)

  (when (and (tui-element-p node)
             (not (tui-component-p node)))
    (setf (tui-element-content node)
          (tui--normalize-content (plist-get (tui--get-props node) :children))))
  ;; (display-warning 'tui-diff (format "MOUNT %S between %S and %S (%s)" (tui--object-class node) start end (unless (eq start end) "eq" "distinct")) :debug tui-log-buffer-name)
  (tui--insert node)
  (when parent
    (tui--apply-inherited-text-props start end parent))
  (setf (tui-node-mounted node) t)
  ;; (cl-assert (or (< start end)
  ;;               (eq start end)) t "Segment endpoints should be ordered if not represented by the same marker.")
  node)

(cl-defmethod tui--mount ((component tui-component) start &optional end parent marker-list)
  "Internal use only.  Mount and insert COMPONENT between START and END divisions."
  (when (tui-node-mounted component)
    (error "Component already mounted"))
  ;; Build the prop list for this instance
  (setf (tui-component-props component)
        (tui--plist-merge (tui--funcall #'tui-get-default-props component)
                       (tui--get-props component)))
  ;; Set the initial state (w/o forcing an update)
  (let* ((initial-state (tui--funcall #'tui-get-initial-state component)))
    (setf (tui-component-state component)
          (tui--plist-merge initial-state
                         (tui-get-derived-state-from-props component
                                                        (tui-component-props component)
                                                        initial-state))))
  ;; Call the render method
  (setf (tui-component-content component)
        (tui--normalize-content (tui--funcall #'tui-render component))) ;; TODO: condition-case -> tui-error-placeholder-string
  (setf tui--update-queue
        (append (let ((tui--update-queue nil))
                  (cl-call-next-method)
                  tui--update-queue)
                `((component-did-mount ,component))
                tui--update-queue))
  component)

(cl-defmethod tui--insert ((node tui-node))
  "Default (empty method)."
  node)

(cl-defmethod tui--insert ((text-node tui-text-node))
  "Insert content of TEXT-NODE."
  (cl-assert (tui-text-node-mounted text-node) t "Can only insert nodes once they have been mounted.")
  ;; "Open" markers, so insertion takes place between them
  ;;(tui--with-open-node text-node
  (save-current-buffer
    (save-excursion
      (tui--open-segment text-node)
      (let ((start (tui-start text-node))
            (end (tui-end text-node))
            (parent (tui-parent text-node)))
        (tui--goto start)
        (delete-region start end)
        ;; (when (equal (tui-text-node-content text-node) "2. ")
        ;;   (edebug))
        (-when-let* ((content (tui-text-node-content text-node)))
          (insert (tui--get-string content)))
        (put-text-property start end 'tui-node text-node (marker-buffer start))
        (tui--apply-inherited-text-props start end text-node (marker-buffer start))))))

(cl-defmethod tui--insert ((element tui-element))
  "Insert content of ELEMENT."
  (cl-assert (tui-element-mounted element) t "Can only insert nodes once they have been mounted.")
  ;; Invisible elements don't get inserted
  (unless (tui-invisible-p element)
    (-when-let* ((children (tui-child-nodes element)))
      (if (eq (tui-node-mounted (cl-first children)) t)
          (mapcar #'tui--insert children)
        (let* ((marker-list (tui-node-marker-list element))
               (subdivisions (cl-rest (tui-marker-list-split-node marker-list (tui--start-division element) (* (length children) 2))))
               (i 0))
          (tui--update-node-index-positions children)
          (cl-loop for child in children
                   for (left-division right-division) on subdivisions by #'cddr
                   do
                   (cl-assert (tui-marker-list--nodes-adjacent-p left-division right-division) t "We should be looking at adjacent divisions")
                   (push (list 'mount child left-division right-division element) tui--update-queue))
          (setq i (+ i 1)))))))

(cl-defmethod tui--update ((text-node tui-text-node))
  "Update displayed element."
  (tui--insert text-node))

(cl-defmethod tui--update ((element tui-element) &optional (next-props nil next-props-supplied-p))
  "Update ELEMENT."
  (save-excursion
    (let* ((props (if next-props-supplied-p
                      (setf (tui-element-props element) next-props)
                    (tui--get-props element)))
           (old-content (tui-element-content element))
           (new-content (tui--normalize-content (plist-get props :children)))) ;; condition-case -> tui-error-placeholder-string element
      (tui--reconcile-content old-content new-content element)
      element)))

(cl-defmethod tui--update ((component tui-component) &optional next-props next-state force)
  "Update COMPONENT."
  (save-current-buffer
    (save-excursion
      (let* ((next-props (or next-props
                             (tui--get-props component)))
             (next-state (or next-state
                             (tui--get-state component)))
             (prev-props (tui--get-props component))
             (prev-state (tui--get-state component))
             (old-content (tui-component-content component)))
        ;; TODO: restore
        ;; (-when-let* ((changed-text-props (tui--text-prop-changes prev-props next-props)))
        ;;   (push `(update-text-props ,component ,changed-text-props) tui--update-queue))
        (setf (tui-component-props component) next-props)
        (setf (tui-component-state component) next-state)
        (let* ((new-content (tui--normalize-content (tui--funcall #'tui-render component)))) ;; condition-case -> tui-error-placeholder-string element
          (tui--reconcile-content old-content new-content component)
          (push `(component-did-update ,component ,prev-props ,prev-state) tui--update-queue)
          component)))))

(cl-defmethod tui--unmount ((node tui-node))
  "Internal use only.  Unmount COMPONENT, but leave unmounted
component in its current context.  Replacement/removal of
COMPONENT should be handled by the calling method.

Returns NODE."
  (-let* (((start . end) (tui-segment--nodes node))
          (parent (tui-parent node))
          (inhibit-read-only t))
    (save-current-buffer
      (save-excursion
        (when (tui-node-mounted node)
          (when parent
            (remhash node tui--element-parent-table)
            (setf (tui-node-content parent) (remove node (tui-element-content parent)))
            (tui--update-node-index-positions (tui-child-nodes parent)))
          (tui--goto (tui-start node))
          (delete-region (tui-marker-list-node-marker start)
                         (tui-marker-list-node-marker end))
          (tui-marker-list-delete-node-segment (tui-node-marker-list node) start end)
          (setf (tui-node-mounted node) nil)
          node)))))

(cl-defmethod tui--unmount ((element tui-element))
  "Internal use only.  Unmount COMPONENT, but leave unmounted
component in its current context.  Replacement/removal of
COMPONENT should be handled by the calling method.

Returns ELEMENT."
  (mapc #'tui--unmount (tui-child-nodes element))
  (cl-call-next-method)
  (tui--make-ref-callback element t)
  element)

(cl-defmethod tui--unmount ((component tui-component))
  "Internal use only.  Unmount COMPONENT, but leave unmounted
component in its current context.  Replacement/removal of
COMPONENT should be handled by the calling method.

Returns COMPONENT."
  (tui--funcall #'tui-component-will-unmount component)
  (cl-call-next-method)
  component)


;;; Props and State functions

(defvar tui--default-props-table
  (make-hash-table)
  "Default props for component classes.")

(defun tui-get-props ()
  "Get current component properties from within a lifecycle method of a component."
  (error "`tui-get-props' must be called from within a component lifecycle method"))

(cl-defmethod tui--get-props ((element tui-element))
  "Internal use only."
  (tui-element-props element))

(cl-defmethod tui--get-props ((node tui-node))
  "Internal use only."
  nil)

(defun tui-get-state ()
  "Get current component state from within a lifecycle method of a component."
  (error "`tui-get-state' must be called from within one of `component-did-mount', or `component-did-update' component lifecycle methods"))

(defun tui--get-state (component)
  "Internal function to get COMPONENT state.  Do not call this directly; use `tui-get-state' within one of `component-did-mount', or `component-did-update' component lifecycle methods."
  (cl-copy-list (tui-component-state component)))

(cl-defmethod tui--set-props ((component tui-component) next-props)
  "Internal use only."
  (display-warning 'comp (format "SET-PROPS %S" (tui--object-class component)) :debug tui-log-buffer-name)
  (let* ((prev-props (tui--get-props component))
         (next-props (tui--plist-merge prev-props next-props))
         (prev-state (tui--get-state component))
         (next-state (tui--plist-merge (tui--get-state component)
                                    (tui-get-derived-state-from-props component next-props prev-state))))
    (when (tui--funcall #'tui-should-component-update component next-props next-state)
      (cl-call-next-method component next-props))))

(cl-defmethod tui--set-props ((element tui-element) next-props)
  "Internal use only."
  (display-warning 'comp (format "SET-PROPS %S" (tui--object-class element)) :debug tui-log-buffer-name)
  (let ((prev-props (tui--get-props element)))
    ;; TODO: verify operation
    (when (tui--text-prop-changes prev-props next-props)
      ;; TODO
      ;; (tui--clear-cached-text-props component)
      )
    (tui--update element next-props)))

(defun tui--set-state (component next-state &optional no-update)
  "Internal function to set COMPONENT state.

Do not call this directly; use `tui-set-state'.

Sets the current state of COMPONENT to NEXT-STATE.  Does not
cause the component to update when NO-UPDATE is truthy."
  (display-warning 'comp (format "SET-STATE %S" (tui--object-class component)) :debug tui-log-buffer-name)
  (let ((prev-state (tui-component-state component)))
    (when (not (equal prev-state next-state))
      (unless no-update
        (tui--update component nil next-state))
      (unless tui--applying-updates
        (tui--process-update-queue)))))


;;; Composition API

(defmacro tui-with-rendered-element (element &rest body)
  "Renders ELEMENT in a dedicated temporary buffer.
Binds `tui-element' to ELEMENT for evaluation of BODY."
  (declare (indent defun))
  `(with-temp-buffer
     (let ((tui-element (tui-render-element ,element)))
       ,@body)))

(defun tui-render-to-string (element)
  "Return the string representation of rendered ELEMENT."
  ;; TODO: define side-effects and behavior for mounted elements
  (tui-with-rendered-element element
    (buffer-string)))

(defun tui-create-element (type &optional props &rest children)
  "Create a new element TYPE with PROPS properties and child elements CHILDREN."
  (-let* ((invisible (plist-get props :invisible)))
    (when children
      (setq props (append props
                          (list :children (if (eq (length children) 1)
                                              (car children)
                                            children)))))
    (funcall (intern (format "%s-create" (symbol-name type)))
             :props props
             :invisible invisible)))

(cl-defmacro tui-define-component (name &key
                                     documentation
                                     prop-documentation
                                     state-documentation
                                     get-default-props
                                     get-initial-state
                                     mount
                                     component-did-mount
                                     get-derived-state-from-props
                                     should-component-update
                                     render
                                     component-did-update
                                     component-will-unmount)
  "Macro for defining `tui-component' types.

Lifecycle signatures:
get-default-props ()
get-initial-state ()
mount ()
component-did-mount ()
get-derived-state-from-props (props state)
should-component-update (next-props next-state)
render ()
component-did-update (prev-props prev-state)
component-will-unmount ()

See React's documentation (https://reactjs.org/docs/react-component.html) for a good explanation of how these methods should be used."
  (declare (indent defun)) ;; TODO: support an optional docstring as the third parameter (as an alternative to the keyword form)
  (tui--check-key-value-documentation prop-documentation)
  (tui--check-key-value-documentation state-documentation)
  (let* ((prop-names (delq nil
                           (cl-loop for (prop-keyword docstring) on prop-documentation by #'cddr
                                    collect
                                    (intern (substring (symbol-name prop-keyword) 1)))))
         (method-generated-by "(generated by tui-define-component)"))
    `(progn
       ;; Remove the prior component definition
       (tui-unintern ',name)

       ;; Define the component struct that all methods are predicated on
       (tui--component-defstruct ,name)

       ;; Define implementations of generic lifecycle methods
       ,(when get-default-props
          `(cl-defmethod tui-get-default-props ((component ,name))
             ""
             (let ((class (tui--object-class component)))
               (cl-copy-seq
                (let* ((cache-value (gethash class tui--default-props-table 'miss)))
                  (if (eq cache-value 'miss)
                      (puthash class (funcall ,get-default-props) tui--default-props-table)
                    cache-value))))))
       ,(when get-initial-state
          `(cl-defmethod tui-get-initial-state ((component ,name))
             ,method-generated-by
             (funcall ,get-initial-state)))
       ,(when mount
          `(cl-defmethod tui--mount ((component ,name) start &optional end parent)
             ,method-generated-by
             (funcall ,mount component start end parent)))
       ,(when component-did-mount
          `(cl-defmethod tui-component-did-mount ((component ,name))
             ,method-generated-by
             (funcall ,component-did-mount)))
       ,(when get-derived-state-from-props
          `(cl-defmethod tui-get-derived-state-from-props ((component ,name) props state)
             ,method-generated-by
             (funcall ,get-derived-state-from-props props state)))
       ,(when should-component-update
          `(cl-defmethod tui-should-component-update ((component ,name) next-props next-state)
             ,method-generated-by
             (funcall ,should-component-update next-props next-state)))
       ,(when render
          `(cl-defmethod tui-render ((component ,name))
             ,method-generated-by
             (funcall ,render)))
       ,(when component-did-update
          `(cl-defmethod tui-component-did-update ((component ,name) prev-props prev-state)
             ,method-generated-by
             (funcall ,component-did-update prev-props prev-state)))
       ,(when component-will-unmount
          `(cl-defmethod tui-component-will-unmount ((component ,name))
             ,method-generated-by
             (funcall ,component-will-unmount)))

       ;; Constructor function
       (cl-defun ,name ,(append
                         '(&rest args)
                         (apply #'append
                                (mapcar
                                 (lambda (prop-name)
                                   `(&key ,prop-name))
                                 prop-names))
                         '(&allow-other-keys))
         ,(tui-component--docstring documentation prop-documentation state-documentation)
         (let (children props)
           ;; Parse keyword key-value pairs permitting shorthand children (omitted :children)
           (while (keywordp (car args))
             (-let* (((prop value) (-take 2 args)))
               (setq args (cddr args))
               (if (eq prop :children)
                   (setq children value)
                 (setq props (append (list prop value)
                                     props)))))
           (when args
             (setq children args))
           (unless (listp children)
             (setq children (list children))) ;; TODO: also issue a warning?
           (let ((component (funcall #'tui-create-element ',name props children)))
             (when tui-live-reloading
               (tui--register-instance component))
             component))))))

(defun tui-force-update (component)
  "Force COMPONENT to re-render."
  (let* ((new-props (tui--get-props component))
         (new-state (tui--get-state component)))
    (push `(component-did-update ,component ,new-props ,new-state) tui--update-queue)
    (tui--update component)
    (unless tui--applying-updates
      (tui--process-update-queue))))

(defun tui-force-update-buffer (&optional buffer)
  "Update all tui components in BUFFER or current buffer."
  (interactive)
  (mapc
   #'tui-force-update 
   tui--content-trees))

(defun tui-render-element (node &optional target)
  "Primary function for rendering content to a buffer.

Input CONTENT is converted to a well-formed content tree.
Returns a reference to the root node of the rendered content.

Optionally specify TARGET context for rendering NODE.  TARGET may
be a character position, marker, buffer name, buffer, or another
tui-element."
  ;; FIXME: temporary measure until there's a mechanism for cleaning up the queue after errors
  ;; (should probably just examine the queue and skip targets that are not live)
  (setq tui--update-queue nil)
  (save-excursion
    (save-current-buffer
      (if (tui-element-p target)
          (tui-append-child target node)
        (cond
         ((number-or-marker-p target)
          (when (markerp target)
            (set-buffer target))
          (goto-char target))
         ((stringp target)
          (set-buffer (get-buffer-create target)))
         ((bufferp target)
          (set-buffer target)))
        (let* ((node (tui--make-root-node node))
               (marker-list (tui-node-marker-list node)))
          (tui--mount node (tui-marker-list-insert marker-list (point-marker)))
          (unless (tui-buffer-p node)
            (push node tui--content-trees))
          (tui--process-update-queue)
          ;;(tui-valid-content-tree-p node)
          node)))))

(cl-defmethod tui-rendered-p ((node tui-node))
  "Return t if ELEMENT has been rendered."
  (and (tui-segment node)
       t))

(cl-defmethod tui-mounted-p ((node tui-node))
  "Return t if NODE is mounted in a live buffer and nil otherwise."
  (and (tui-node-mounted node)
       (not (eq (tui-node-mounted node) 'pending))
       (let* ((start (tui-start node)))
         (and start
              (marker-buffer start)))
       t))

;;;; Internal

(defun tui--normalize-node (node)
  ;; TODO: set parent opportunistically?
  "Convert NODE's content tree to normalized form.

A normalized content tree:

1. Is represented by a reference to the topmost node which is an
instance of `tui-node' (or an inheriting class).

2. All child `tui-node' objects are themselves in normalized
form.

3. All `tui-element' nodes contain (in :content slot) a list of
`tui-node' objects (zero or more)."
  (cond
   ((tui-text-node-p node)
    ;; TODO: check that content is a primitive
    node)
   ((or (tui-element-p node)
        (tui--list-content-p node))
    (tui--normalize-element node))
   ((and (featurep 'collection)
         (tui--object-of-class-p node 'collection))
    (tui--normalize-element (collection-to-list node)))
   ((or (stringp node)
        (numberp node))
    (tui-text-node-create :content node))
   (t
    (error "Unexpected node element type could not be normalized"))))

(defun tui--normalize-element (element)
  "Same as `tui-normalize-node'- except that it ensures that ELEMENT is an instance of `tui-element' (content is wrapped with a `tui-element' if necessary)."
  ;; TODO: remove unnecessary nesting by plain tui-element wrappers?
  ;; CLEANUP: reconcile with tui--normalize-component as tui--normalize ?
  (unless (tui-element-p element)
    (setq element
          (apply #'tui-create-element 'tui-element nil
                 (tui--normalize-content element))))
  (setf (tui-element-content element) (tui--normalize-content (tui-element-content element)))
  element)

(defun tui--normalize-content (content)
  "Complementary method to `tui-normalize-element' that normalizes CONTENT for `tui-element's :content slot (see `tui-normalize-element').  Return value is always a list."
  (-non-nil
   (cond
    ((tui--list-content-p content)
     (mapcar #'tui--normalize-node content))
    (content
     (list (tui--normalize-node content))))))

(defun tui--normalized-element-p (element)
  "Return whether ELEMENT content tree is in normal form (see `tui--normalize' a description)."
  (and (tui-element-p element)
       (let ((content (tui-element-content element)))
         (or (null content)
             (not (tui--list-content-p (tui-element-content element)))
             (and (tui--list-content-p (tui-element-content element))
                  (-all-p
                   (lambda (content-item)
                     (tui-element-p content-item))
                   content))))))

(defun tui--process-update-queue ()
  "Process the update queue.

Very basic now; simply apply updates until the queue is empty."
  (let ((tui--applying-updates t)
        (inhibit-read-only t))
    (while tui--update-queue
      (tui--apply-update (pop tui--update-queue)))
    (run-hooks 'tui-update-hook)))

(defun tui--make-ref-callback (component &optional with-nil-p)
  "Call COMPONENT :ref callback (if defined).  When WITH-NIL-P is truthy, make callback with nil as the argument rather than the component reference."
  (let* ((ref-callback (plist-get (tui--get-props component) :ref)))
    (when (functionp ref-callback)
      (funcall ref-callback (when (not with-nil-p)
                              component)))))

(defun tui--apply-update (update)
  "Apply UPDATE to corresponding content tree."
  (pcase update
    (`(component-did-mount ,component)
     (tui--make-ref-callback component)
     (tui--funcall #'tui-component-did-mount component))
    (`(mount ,child ,start ,end ,parent)
     (tui--mount child start end parent))
    (`(insert ,item ,parent ,index)
     (display-warning 'tui-diff (format "INSERT-NODE %S %S %d" (tui--object-class item) (tui--object-class parent) index) :debug tui-log-buffer-name)
     (tui-insert-node item parent index))
    (`(remove ,node)
     (display-warning 'tui-diff (format "REMOVE-NODE %S" (tui--object-class node)) :debug tui-log-buffer-name)
     (tui-remove node))
    (`(replace ,old-node ,new-node)
     (display-warning 'tui-diff (format "RELACE-NODE %S %S" (tui--object-class old-node) (tui--object-class new-node)) :debug tui-log-buffer-name)
     (tui-replace-node old-node new-node))
    (`(update-content ,node ,new-content)
     (display-warning 'tui-diff (format "UPDATE-CONTENT %S %S" (tui--object-class node) new-content) :debug tui-log-buffer-name)
     (setf (tui-node-content node) new-content)
     (-let* (((start . end) (tui-segment node)))
       (tui--insert node)))
    (`(update-props ,component ,updated-props)
     (-let* ((old-props (tui--get-props component))
             (invisible (plist-get updated-props :invisible))
             (old-ref (plist-get old-props :ref))
             (new-ref (plist-get updated-props :ref))
             (ref-changed (not (equal old-ref new-ref))))
       (display-warning 'tui-diff (format "UPDATE-PROPS %S" (tui--object-class component)) :debug tui-log-buffer-name)
       (when (and ref-changed
                  (functionp old-ref))
         (funcall old-ref))
       (tui--set-props component updated-props)
       (tui--make-ref-callback component)
       (when (not (eq (plist-get old-props :invisible) invisible))
         (if invisible
             (tui-hide-element component)
           (tui--show-element component)))))
    (`(update-text-props ,component ,changed-text-props)
     (tui--update-text-props component changed-text-props))
    (`(component-did-update ,component ,new-props ,new-state)
     (tui--funcall #'tui-component-did-update component new-props new-state))
    (_
     (error "Unknown update format: %S" (first update)))))

(cl-defmethod tui--split-division (node division)
  ""
  (let ((marker-list (tui-node-marker-list node)))
    (tui-marker-list-split-node marker-list division)))

(defvar-local tui--content-trees nil "Content trees local to the current buffer")

(defun tui--unmount-buffer-content ()
  "Unmount all content trees in the current buffer."
  (mapc #'tui--unmount tui--content-trees)
  (setq tui--content-trees nil))

(add-hook 'kill-buffer-hook #'tui--unmount-buffer-content)

;; (cl-defmethod tui--mark-subtree-dirty ((node tui-node))
;;   ""
;;   (let* ((nodes (list node)))
;;     (while nodes
;;       (let* ((node (pop nodes))
;;              (children (tui-child-nodes node)))
;;         (when children
;;           (push children nodes))
;;         (setf (tui-node-dirty-p node) t)))))

;; (defmacro tui--mark-clean (node)
;;   "Mark NODE clean."
;;   `(setf (tui-node-dirty-p ,node) nil))

;; (cl-defmethod tui--dirty-subtrees ((node tui-node))
;;   "Return a list of descendents of NODE that require re-rendering."

;;   )

(provide 'tui-core)

;;; tui-core.el ends here
