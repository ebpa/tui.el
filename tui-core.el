;;; tui-core.el --- Core functions

;;; Commentary:
;; 

(eval-when-compile (require 'cl-lib))
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'tui-diff)
(require 'tui-layout)
(require 'tui-live-reloading)
(require 'tui-log)
(require 'tui-marker-list)
(require 'tui-node-types)
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

(cl-defmethod tui-get-default-props ((component tui-component))
  "Empty default method"
  nil)

(cl-defmethod tui-get-initial-state ((component tui-component))
  "Empty default method"
  nil)

(cl-defmethod tui-component-will-mount ((component tui-component))
  "Empty default method"
  nil)

(cl-defmethod tui-component-did-mount ((component tui-component))
  "Empty default method"
  nil)

(cl-defmethod tui-component-will-receive-props ((component tui-component) next-props)
  "Empty default method"
  nil)

(cl-defmethod tui-should-component-update ((component tui-component) next-props next-state)
  "Empty default method"
  t)

(cl-defmethod tui-component-will-update ((component tui-component) next-props next-state)
  "Empty default method"
  nil)

(cl-defmethod tui-render ((component tui-component))
  "Empty default method"
  nil)

(cl-defmethod tui-component-did-update ((component tui-component) next-props next-state)
  "Empty default method"
  nil)

(cl-defmethod tui-component-will-unmount ((component tui-component) prev-props prev-state)
  "Empty default method"
  nil)

;;;; Internal Lifecycle helpers

(cl-defmethod tui--mount ((node tui-node) start &optional end parent marker-list)
  "Internal use only.  Mount and insert NODE between START and END divisions."
  (when (tui-node-mounted node)
    (error "Component already mounted"))

  ;; CLEANUP: need a better solution to indicate the desired mount is in progress
  (setf (tui-node-mounted node) 'pending)

  (tui--set-parent node parent marker-list)

  (unless end (setq end start))
  (assert (tui-marker-list-node-p start) t "Mount point is a tui-marker-list-node")
  (assert (tui-marker-list-node-p end) t "Mount point is a tui-marker-list-node")
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
  ;; TODO: restore
  ;; (when parent
  ;;   (tui--apply-inherited-text-props start end parent))
  (setf (tui-node-mounted node) t)
  ;; (assert (or (< start end)
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
  (tui--set-state component (tui--funcall #'tui-get-initial-state component) t)
  ;; Call the render method
  (setf (tui-component-content component)
        (tui--normalize-content (tui--funcall #'tui-render component))) ;; TODO: condition-case -> tui-error-placeholder-string
  (tui--funcall #'tui-component-will-mount component)
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
  (assert (tui-text-node-mounted text-node) t "Can only insert nodes once they have been mounted.")
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
        (when parent
          (tui--apply-inherited-text-props start end parent))))))

(cl-defmethod tui--insert ((element tui-element))
  "Insert content of ELEMENT."
  (assert (tui-element-mounted element) t "Can only insert nodes once they have been mounted.")
  ;; FIXME: need a safer way of doing this? (to avoid improperly relocated markers)
  ;;(delete-region start end)
  ;; Invisible elements don't get inserted
  ;; CLEANUP: confusing
  ;; (assert (not (and (tui-invisible-p element)
  ;;                   (> (tui-length element) 0))) t "Shouldn't be trying to insert an invisible element that has currently viisble content.")
  (unless (tui-invisible-p element)
    (-when-let* ((children (tui-child-nodes element)))
      (if (eq (tui-node-mounted (first children)) t)
          (mapcar #'tui--insert children)
        (let* ((marker-list (tui-node-marker-list element))
               (subdivisions (rest (tui-marker-list-split-node marker-list (tui--start-division element) (* (length children) 2))))
               (i 0))
          (tui--update-node-index-positions children)
          (cl-loop for child in children
                   for (left-division right-division) on subdivisions by #'cddr
                   do
                   (assert (tui-marker-list--nodes-adjacent-p left-division right-division) t "We should be looking at adjacent divisions")
                   (push (list 'mount child left-division right-division element) tui--update-queue))
          (setq i (+ i 1)))))
    ;;(tui--apply-text-props element)
    ))

(cl-defmethod tui--update ((text-node tui-text-node))
  "Update displayed element."
  (tui--insert text-node))

(cl-defmethod tui--update ((element tui-element))
  "Update displayed element."
  (save-excursion
    (-let* ((inhibit-read-only t))
      (let* ((props (tui--get-props element))
             (old-content (tui-element-content element))
             (new-content (tui--normalize-content (plist-get props :children)))) ;; condition-case -> tui-error-placeholder-string element
        (tui--reconcile-content old-content new-content element)
        ;; TODO: (force-window-update (current-buffer))?
        (set-buffer-modified-p nil)
        ;;(tui-valid-element-p element) ;; CLEANUP: better method for recursive assertions?
        ))))

(cl-defmethod tui--update ((component tui-component))
  "Update displayed component."
  (save-current-buffer
    (save-excursion
      (-let* ((inhibit-read-only t))
        (let* ((old-content (tui-component-content component))
               (new-content (tui--normalize-content (tui--funcall #'tui-render component)))) ;; condition-case -> tui-error-placeholder-string element
          (tui--reconcile-content old-content new-content component)
          ;; TODO: (force-window-update (current-buffer))?
          (set-buffer-modified-p nil)
          ;;(tui-valid-element-p component) ;; CLEANUP: better method for recursive assertions?
          ))))) ;; TODO: restore original modification status

(cl-defmethod tui--unmount ((node tui-node))
  "Internal use only.  Unmount COMPONENT, but leave unmounted
component in its current context.  Replacement/removal of
COMPONENT should be handled by the calling method."
  (-let* (((start . end) (tui-segment--nodes node))
          (parent (tui-parent node)))
    (save-current-buffer
      (save-excursion
        (tui--goto (tui-start node))
        (delete-region (tui-marker-list-node-marker start)
                       (tui-marker-list-node-marker end))
        ;;(tui--consolidate-markers node)
        (tui-marker-list-delete-node-segment (tui-node-marker-list node) start end)))))

(cl-defmethod tui--unmount ((element tui-element))
  "Internal use only.  Unmount COMPONENT, but leave unmounted
component in its current context.  Replacement/removal of
COMPONENT should be handled by the calling method."
  (mapc #'tui--unmount (tui-child-nodes element))
  (cl-call-next-method))

(cl-defmethod tui--unmount ((component tui-component))
  "Internal use only.  Unmount COMPONENT, but leave unmounted
component in its current context.  Replacement/removal of
COMPONENT should be handled by the calling method."
  (tui--funcall #'tui-component-will-unmount component (tui--get-props component) (tui--get-state component))
  (cl-call-next-method))


;;; Layout / Visibility Helpers

(defun tui-get-element-at (&optional pos type)
  "Get the element at POS.  Search ancestors for element of type TYPE when TYPE is non-nil."
  (unless pos (setq pos (point)))
  (if type
      (first (tui-ancestor-elements-at pos type))
    (get-text-property pos 'tui-element)))

(cl-defmethod tui--get-string ((node tui-node))
  "Return the string representation of ELEMENT's content tree.  Returns nil if ELEMENT is not mounted."
  (-let* ((start (tui-start node))
          (end (tui-end node)))
    (when (and start end)
      (buffer-substring start end))))

(cl-defmethod tui--get-string (content)
  "Return the string representation of CONTENT."
  (if (tui--list-content-p content)
      (mapconcat #'tui--get-string content)
    (cond
     ((null content)
      "")
     ((stringp content)
      content)
     ((tui--image-p content)
      (propertize "[image]" 'display content))
     ((numberp content)
      (format "%d" content))
     (t
      tui-error-placeholder-string))))

(defun tui-invisible-p (element)
  "Return t if ELEMENT has been marked invisible."
  (and (tui-element-p element)
       (tui-element-invisible element)))

;; (defun tui-visible-p (element)
;;   "Return t if ELEMENT is not marked invisible.  This does not
;; indicate that ELEMENT is in fact visible.  A parent element may
;; be marked invisible which would cause ELEMENT to not be visible
;; to the user even if ELEMENT is not marked invisible."
;;   (and (tui--object-of-class-p element 'tui-element)
;;        (not (oref element :invisible))))

;; Segment-related

(defvar tui--marker-node-table
  (make-hash-table :test #'eq) ;; TODO restore :weakness 'key)
  "Keep track of elements that start and end at markers.  Values are formatted as cons cells (START-ELEMENTS . END-ELEMENTS).")

(cl-defmethod tui-segment ((node tui-node))
  "Return markers denoting the start and end of NODE formatted as a cons cell (START . END)."
  (cons (tui-marker-list-node-marker (tui-node-start node))
        (tui-marker-list-node-marker (tui-node-end node))))

(cl-defmethod tui-segment--nodes ((node tui-node))
  "Return marker nodes denoting the start and end of NODE formatted as a cons cell (START . END)."
  (cons (tui-node-start node) (tui-node-end node)))

(defmacro tui--with-open-node (node &rest body)
  "\"Open\" segment of NODE and execute BODY.  Ensure that markers are consolidated following evaluation of BODY."
  (declare (indent defun))
  `(save-current-buffer
     (save-excursion
       (-let* ((node ,node))
         (tui--open-segment node)
         (tui--goto (tui-start node))
         (progn . ,body)))))

(cl-defmethod tui--open-segment ((node tui-node))
  "Return the segment for NODE formatted as a cons cell (START . END)."
  (-let* ((start (tui-node-start node))
          (end (tui-node-end node)))
    (assert (tui-marker-list-node-p start) "Start of segment must be a tui-marker-list-node.")
    (assert (tui-marker-list-node-p end) "End of segment must be a tui-marker-list-node.")
    (tui-marker-list-open-segment (tui-node-marker-list node) start end)))

(defun tui-start (node)
  "Return a marker denoting the start of NODE."
  ;; TODO: defensively return copies of markers?
  (tui-marker-list-node-marker (tui-node-start node)))

(defun tui-end (node)
  "Return a marker denoting the end of NODE."
  ;; TODO: defensively return copies of markers?
  (tui-marker-list-node-marker (tui-node-end node)))

(defun tui--start-division (node)
  "Return division denoting the start of NODE."
  (tui-node-start node))

(defun tui--end-division (node)
  "Return division denoting the end of NODE."
  (tui-node-end node))

;;;; Markers and segments

(cl-defmethod tui-markers ((node tui-node))
  "Return a list of unique markers associated with NODE."
  (-let* (((start . end) (tui-segment node)))
    (list (tui-marker-list-node-start start)
          (tui-marker-list-node-end end))))

;; (cl-defmethod tui-markers ((element tui-element))
;;   "Return a list of unique markers within the subtree of ELEMENT."
;;   (-let* (((start . end) (tui-segment element)))
;;     (tui-marker-list-range (tui-element-marker-list element) start end)))

;; (cl-defmethod tui-child-markers ((element tui-element))
;;   "Return markers of NODE's children."
;;   (apply #'append
;;          (mapcar
;;           (lambda (child)
;;             (list (tui-start child) (tui-end child)))
;;           (tui-child-nodes element))))

;;;; Internal Segment-related

;; (cl-defmethod tui--set-segment ((node tui-node) start-node end-node)
;;   "Set NODE buffer segment to START-NODE and END-NODE."
;;   ;; (when (and start-node end-node)
;;   ;;   (assert (<= start-node end-node) t "Segment should be ordered."))
;;   (tui--set-start node start-node)
;;   (tui--set-end node end-node))

(defun tui--elements-starting-at (marker)
  "Return a list of nodes starting at MARKER."
  (car (gethash marker tui--marker-node-table)))

(defun tui--incident-node-p (node pos)
  "Return t if NODE starts or ends at POS."
  (or (= (tui-start node) pos)
      (= (tui-end node) pos)))

(cl-defmethod tui--set-markers ((node tui-node) marker)
  "Internal function to set all segment endpoints in the subtree of NODE to MARKER."
  (setf (tui-node-start node) marker)
  (setf (tui-node-end node) marker))

(cl-defmethod tui--set-markers ((element tui-element) marker)
  "Internal function to set all segment endpoints in the subtree of NODE to MARKER."
  (mapc (lambda (child)
          (tui--set-markers child marker))
        (tui-child-nodes element))
  (cl-call-next-method))

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
  (error "`tui-get-state' must be called from within one of `component-will-mount',`component-did-mount',`component-will-receive-props', or `component-did-update' component lifecycle methods"))

(defun tui--get-state (component)
  "Internal function to get COMPONENT state.  Do not call this directly; use `tui-get-state' within one of `component-will-mount',`component-did-mount',`component-will-receive-props', or `component-did-update' component lifecycle methods."
  (cl-copy-list (tui-component-state component)))

(cl-defmethod tui--set-props ((component tui-component) next-props)
  "Internal use only."
  ;;(display-warning 'comp (format "SET-PROPS %S" (tui--object-class component)) :debug tui-log-buffer-name)
  (let ((prev-props (tui--get-props component))
        (prev-state (tui--get-state component)))
    (tui--funcall #'tui-component-will-receive-props component next-props)
    (let ((next-state (tui--get-state component)))
      (when (tui--funcall #'tui-should-component-update component next-props next-state)
        (tui--funcall #'tui-component-will-update component next-props next-state)
        (setf (tui-component-props component) next-props)
        (push `((component-did-update ,component ,prev-props ,prev-state)) tui--update-queue)
        (tui--update component)
        ;; (tui--funcall #'tui-component-did-update component prev-props prev-state)
        ))))

(cl-defmethod tui--set-props ((element tui-element) next-props)
  "Internal use only."
  ;;(display-warning 'comp (format "SET-PROPS %S" (tui--object-class component)) :debug tui-log-buffer-name)
  (let ((prev-props (tui--get-props element)))
    (setf (tui-element-props element) next-props)
    (tui--update element)))

(defun tui--set-state (component next-state &optional no-update)
  "Internal function to set COMPONENT state.

Do not call this directly; use `tui-set-state'.

Sets the current state of COMPONENT to NEXT-STATE.  Does not
cause the component to update when NO-UPDATE is truthy."
  ;;(display-warning 'comp (format "SET-STATE %S" (tui--object-class component)) :debug tui-log-buffer-name)
  (let ((prev-state (tui-component-state component)))
    (when (not (equal prev-state next-state))
      (setf (tui-component-state component) next-state)
      (unless no-update
        (tui--update component)))))

;;; Content tree

(defvar tui--element-parent-table
  (make-hash-table :weakness 'key)
  "Parent element lookup table.")

(defun tui-ancestor-elements-at (&optional pos type)
  "Get a list of comp elements at POS.  The root element is last.

Filter returned elements according to TYPE.  All ancestors are returned when TYPE is nil."
  ;; CLEANUP: reconcile tui-ancestor-elements-at and tui-ancestor-elements?
  (-when-let* ((element (tui-get-element-at pos)))
    (-filter
     (if type
         (-rpartial #'tui--object-of-class-p type)
       #'identity)
     (cons element
           (tui-ancestor-elements element)))))

(defun tui-ancestor-elements (node &optional type)
  "Return ancestor elements of NODE with the root node is last.

Filter returned elements according to TYPE.  All ancestors are returned when TYPE is nil."
  (when node
    (-when-let* ((parent-ancestry (gethash node tui--element-parent-table)))
      (if type
          (-filter (-rpartial #'tui--object-of-class-p type)
                   parent-ancestry)
        parent-ancestry))))

(defun tui-parent (node &optional type)
  "Return the first ancestor element of NODE matching TYPE.
Returns nil if NODE is a root node or has no ancestors matching TYPE."
  (let ((ancestor-elements (tui-ancestor-elements node)))
    (if type
        (-first (-rpartial #'tui--object-of-class-p type)
                (cdr ancestor-elements))
      (cadr ancestor-elements))))

(defun tui--set-parent (node parent &optional marker-list)
  "Internal function to set the parent element of NODE to PARENT.

MARKER-LIST may be passed when mounting child elements in an alternate buffer."
  (let* ((parent-ancestry (tui-ancestor-elements parent))
         (ancestry (gethash node tui--element-parent-table)))
    (when (or parent marker-list)
      (setf (tui-node-marker-list node) (or marker-list
                                         (tui-element-marker-list parent))))
    (if ancestry
        (setcdr ancestry parent-ancestry)
      (puthash node (cons node parent-ancestry) tui--element-parent-table))))

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
        (if (member func '(tui-component-will-mount tui-component-did-mount tui-component-will-receive-props tui-component-did-update tui--mount))
            (cl-letf (((symbol-function 'tui-set-state)
                       (lambda (new-state &optional no-update) (tui--set-state component new-state no-update))))
              (apply func component args))
          (apply func component args))))))

(cl-defmethod tui-append-child ((parent tui-element) child)
  "Add CHILD to end of the list of PARENT element's children."
  (let ((content (tui-element-content parent)))
    (tui-insert-node child parent (length content))))

(defun tui-insert-node (node parent index)
  "Insert NODE as a child of PARENT at INDEX.

In case NODE is already mounted, this function removes NODE from
existing parent and moves it to new location in PARENT.

Returns NODE."
  (assert (tui--object-of-class-p parent 'tui-element) t "Nodes can only be inserted into elements.")
  (unless (tui-node-p node)
    (setq node (tui--normalize-node node)))
  (if (tui-node-mounted node) ;; TODO: special case with unmounted NODE, but with parent `(tui-parent node)'
      (tui-move-subtree node parent index)
    (-let* ((content (tui-element-content parent))
            (target-division (car (tui--separating-divisions parent index)))
            ((target-start target-end) (rest (tui-marker-list-split-node (tui-node-marker-list parent)
                                                                     target-division 2))))
      ;; (if (eq node node-after-target)
      ;;     (display-warning 'comp (format "Node %S already at index %d" (tui--object-class node) index) :debug tui-log-buffer-name)
      (assert (not (null target-division)) t "Target marker for insertion not found.")
      (setf (tui-element-content parent) (-insert-at index node content))
      (tui--update-node-index-positions (tui-child-nodes parent))
      (tui--mount node target-start target-end parent)))
  (unless tui--applying-updates
    (tui--process-update-queue))
  node)

(defun tui--separating-divisions (element index)
  "Internal function.

Return pair of divisions within ELEMENT at INDEX corresponding to be used for inserting content at INDEX."
  ;; TODO: no endpoints available in an unmounted element
  (let* ((content (tui-element-content element))
         (content-length (length content)))
    (cond
     ((eq content-length 0)
      (cons (tui--start-division element)
            (tui--end-division element)))
     ((eq index 0)
      (cons (tui--start-division element)
            (tui--start-division (first content))))
     ((< index content-length)
      (cons (tui--end-division (nth (- index 1) content))
            (tui--start-division (nth index content))))
     ((= index content-length)
      (cons (tui--end-division (-last-item content))
            (tui--end-division element)))
     (t
      (error "Invalid index")))))

(defun tui-replace-node (old-node new-node)
  "Relaces OLD-NODE with NEW-NODE in its content tree."
  (let ((parent (tui-parent old-node))
        (target-index (tui-node-relative-index old-node)))
    (tui-remove-child parent old-node )
    (tui-insert-node new-node parent target-index)))

(cl-defmethod tui-replace-with ((old-child tui-node) new-child)
  "Replace OLD-CHILD node with NEW-CHILD node."
  (assert (tui-parent old-child) t "Target must be a child node.")
  (tui-replace-node old-child new-child))

(defun tui-remove (node)
  "Remove NODE from its current tree position."
  (let* ((parent (tui-parent node)))
    (tui--unmount node)
    (when parent
      (remhash node tui--element-parent-table)
      (setf (tui-node-content parent) (remove node (tui-element-content parent)))
      (tui-marker-list-remove-node (tui-node-marker-list node)
                               (tui-node-start node))
      (tui-marker-list-remove-node (tui-node-marker-list node)
                               (tui-node-end node))
      (tui--update-node-index-positions (tui-child-nodes parent)))))

(defun tui-remove-child (node child)
  "Remove CHILD node of NODE."
  (assert (eq (tui-parent child) node) t "Node is not a child of specified node.")
  (tui-remove child))

(defun tui-root-node (&optional thing)
  "Return the root node of the tree that THING is part of."
  ;; CLEANUP
  (unless thing (setq thing (tui-get-element-at (point))))
  (if (tui-node-p thing)
      (or (-last-item (tui-ancestor-elements thing))
          thing)
    (-last-item (tui-ancestor-elements-at thing))))

(defun tui-root-node-p (node)
  "Return t if NODE is the root element of its content tree."
  (not (tui-parent node)))

;;; Misc.

(cl-defmethod cl-print-object ((node tui-node) stream)
  (princ "#<tui-node>" stream))

(cl-defmethod print-object ((node tui-node) stream)
  (princ "#<tui-node>" stream))

;; (cl-defmethod object-print (node tui-node) stream
;;   (princ "#<tui-node>" stream))

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

(defun tui--normalize-node (node)
  ;; TODO: set parent opportunisticly?
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
   ((and (featurep 'collections-collection)
         (tui--object-of-class-p node 'collections-collection))
    (collections-collection-to-list node))
   ((or (stringp node)
        (numberp node))
    (tui-text-node-create :content node))
   (t
    (error "Unexpected node element type could not be normalized"))))

(defun tui--normalize-element (element)
  "Same as `tui-normalize-node'- except that it ensures that ELEMENT is an instance of `tui-element' (content is wrapped with a `tui-element' if necessary)."
  ;; TODO: remove unnecessary nesting by plain tui-element wrappers?
  ;; CLEANUP: reconcile with tui--normalize-component as tui--normalize ?
  (unless (tui--object-of-class-p element 'tui-element)
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
  (and (tui--object-of-class-p element 'tui-element)
       (let ((content (tui-element-content element)))
         (or (null content)
             (not (tui--list-content-p (tui-element-content element)))
             (and (tui--list-content-p (tui-element-content element))
                  (-all-p
                   (lambda (content-item)
                     (tui--object-of-class-p content-item 'tui-element))
                   content))))))

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
  (tui-equal (tui-node-content node-a)
          (tui-node-content node-b)))

(cl-defmethod tui-equal ((component-a tui-component) component-b)
  "An `equal' function for `tui-component' objects."
  (and (cl-call-next-method)
       (not (tui--plist-changes (tui--get-props component-a)
                             (tui--get-props component-b)))))

(cl-defmethod tui-equal (obj-a obj-b)
  "An `equal' function which handles `tui-*' objects recursively as a special case."
  (eq obj-a obj-b))

(cl-defmethod tui-child-nodes ((node tui-node))
  "Return a list of child nodes of NODE."
  nil)

(cl-defmethod tui-child-nodes ((element tui-element))
  "Return a list of child nodes of NODE."
  (tui-element-content element))

(cl-defmethod tui-visible-child-nodes ((node tui-node))
  "Return a list of child nodes of NODE."
  (when (tui-element-p node)
    (-filter
     (lambda (child)
       (and child
            (not (tui-invisible-p child))))
     (tui-node-content node))))

(defun tui-hide-element (element)
  "Hide ELEMENT and its subtree."
  ;; OPTIMIZE: can preserve (cache) the content of the segment to potentially avoid re-render when made visible
  (interactive)
  ;;(display-warning 'tui-diff (format "HIDE %S" (tui--object-class element)) :debug tui-log-buffer-name)
  (-when-let* ((inhibit-read-only t)
               ((start . end) (tui-segment element))
               (marker-tree (tui-element-marker-list element)))
    (setf (tui-element-invisible element) t)
    (delete-region start end))
  ;;(tui-valid-content-tree-p element)
  element)

(defun tui--show-element (element)
  "Show ELEMENT and its subtree."
  (setf (tui-element-invisible element) nil)
  (tui--insert element)
  ;;(tui-valid-content-tree-p element)
  element)

(defvar tui--inheritited-text-props
  (make-hash-table :weakness 'key)
  "Cache for inherited text properties of elements.  Grouped by replacement mode (REPLACE PUSH APPEND SAFE).")

(defvar tui--grouped-text-props
  (make-hash-table :weakness 'key)
  "Cache for text properties of elements (not inherited).  Grouped by replacement mode (REPLACE PUSH APPEND SAFE).")

(defun tui-get-text-props (element)
  "Return text props applied to the children of ELEMENT by ELEMENT."
  (apply #'append (tui--get-grouped-text-props element)))

(defun tui-get-inherited-text-props (element)
  "Return text props applied to the children of ELEMENT by ELEMENT and its parents."
  (apply #'append (tui--get-inherited-grouped-text-props element)))

(defun tui--get-grouped-text-props (element)
  "Return calculated text props applied to the children of ELEMENT."
  ;; TODO: ensure precedence within text property categories?
  ;; TODO: complex behaviors (ex: between push and safe)?
  (let* ((props (tui--get-props element))
         (replace (plist-get props :text-props-replace))
         (push (plist-get props :text-props-push))
         (append (plist-get props :text-props-append))
         (safe (append (list 'tui-element element)
                       (or (plist-get props :text-props-safe)
                           (plist-get props :text-props)))))
    (list replace push append safe)))

(cl-defun tui--merge-grouped-text-props ((this-replace this-push this-append this-safe)
                                      (other-replace other-push other-append other-safe))
  "Internal function to combine multiple grouped text property descriptions."
  ;; (cl-loop for (key value) in this-replace by #'cddr
  ;;          do
  ;;          (setq other-replace (tui--plist-delete other-replace key))
  ;;          (setq other-push (tui--plist-delete other-push key))
  ;;          (setq other-append (tui--plist-delete other-append key))
  ;;          (setq other-safe (tui--plist-safe other-push key)))
  ;; TODO: incomplete; need to finish defining the desired behavior (can assume that provided groups have precedence rules applied)
  (list
   (append this-replace other-replace)
   (append this-push other-push)
   (append this-append other-append)
   (append this-safe other-safe)))

(defun tui--get-inherited-grouped-text-props (node)
  "Return a list of text properties inherited from NODE and NODE's parent elements."
  (if (not node)
      '(nil nil nil nil)
    (or (gethash node tui--inheritited-text-props)
        (puthash node
                 (tui--merge-grouped-text-props
                  (tui--get-grouped-text-props node)
                  (tui--get-inherited-grouped-text-props (tui-parent node)))
                 tui--inheritited-text-props))))

(defun tui-valid-element-p (element &optional invisible-context)
  "Return t if ELEMENT is a valid `tui-element'.
Optional argument INVISIBLE-CONTEXT track whether the this node is within an invisible section of the content tree."
  (and (not (assert (tui-element-p element) t "Element should be a tui-element."))
       (or (not (tui-node-mounted element))
           (tui--object-of-class-p element 'tui-buffer) ;; CLEANUP: is this exclusion necessary?
           (-let* (((start . end) (tui-segment element))
                   (children (tui-child-nodes element))
                   (-compare-fn #'eq))
             (and (not (assert (or (not start)
                                   (and (markerp start)
                                        (marker-buffer end)
                                        (marker-position end))) t "When set, start marker should be a marker object that points somewhere."))
                  (not (assert (or (not end)
                                   (and (markerp end)
                                        (marker-buffer start)
                                        (marker-position start))) t "When set, end marker should be a marker object that points somewhere."))
                  (not (assert (listp children) t "Children should be represented by a list"))
                  ;; all children are adjacent with consolidated markers
                  (or invisible-context
                      (tui-invisible-p element)
                      (-all-p
                       (lambda (child)
                         (not (assert (and (>= (tui-start child) start)
                                           (<= (tui-start child) end)
                                           (>= (tui-end child) start)
                                           (<= (tui-end child) end)) t "Internal child markers should exist within the parent's segment")))
                       children)))))
       ;; All child nodes are valid as well
       (-all-p
        (lambda (child)
          (or (and (not (tui-element-p child))
                   (tui-node-p child))
              (tui-valid-element-p child (or invisible-context
                                          (tui-invisible-p element)))))
        (tui-child-nodes element))))

(defun tui-valid-content-tree-p (node)
  "Return t if NODE's content tree is valid."
  ;; CLEANUP: better method for recursive assertions?
  ;; TODO: restore
  ;; (tui-valid-element-p (tui-root-node node))
  )

(cl-defmethod tui--apply-text-props ((element tui-element))
  ""
  (-let* (((start . end) (tui-segment element))
          ((replace push append safe) (tui--get-grouped-text-props element)))
    (tui-put-text-properties start end replace nil t)
    (tui-put-text-properties start end push nil 'push)
    (tui-put-text-properties start end append nil 'append)
    (tui-put-text-properties start end safe nil nil)
    ;; (if (and (eq (tui--object-class element) 'tui-div)
    ;;          safe))
    ))

(cl-defmethod tui--apply-text-props ((node tui-node))
  ""
  nil)

(defun tui--apply-inherited-text-props (start end element &optional object)
  "Internal function to apply inherited text properties.
Applyes text properties to region between START and END inherited from ELEMENT.

Optional argument OBJECT is a string to which the properties be applied.  START and END should indicate positions within that string."
  (-let* (((start . end) (tui-segment element))
          ((replace push append safe) (tui--get-inherited-grouped-text-props element)))
    (tui-put-text-properties start end replace nil t)
    (tui-put-text-properties start end push nil 'push)
    (tui-put-text-properties start end append nil 'append)
    (tui-put-text-properties start end safe nil nil)))

(cl-defmethod tui-length ((node tui-node))
  "Return the length (number of characters) of NODE."
  (-let* (((start . end) (tui-segment node)))
    (- end start)))

(defun tui-descendent-nodes (node)
  "Return all descendent nodes of NODE."
  (apply #'append
         (mapcar (lambda (child)
                   (cons child
                         (tui-descendent-nodes child)))
                 (tui-child-nodes node))))

;;; "DOM" API

(defun tui-move-subtree (node parent index)
  "Move subtree rooted at NODE to INDEX position within PARENT.

Returns NODE."
  ;; TODO: must be within the same content tree?
  ;; TODO: ensure proper behavior for unmounted nodes (combinations of unmounted source and target)
  ;; TODO: test that this works for movement within an invisible section (edge case with same markers everywhere)
  ;; TODO: handle special case of movement of a zero-length node (ex: invisible)
  (display-warning 'comp (format "MOVE-SUBTREE %S to position %d in %S" (tui--object-class node) index (tui--object-class parent)) :debug tui-log-buffer-name)
  (-let* ((current-parent (tui-parent node))
          (new-parent parent)
          (content (tui-element-content parent))
          (marker-list (tui-node-marker-list node)))
    (if (and (eq current-parent new-parent)
             (eq (nth index content) node))
        (display-warning 'comp (format "Node %S already at target location" (tui--object-class node)) :debug tui-log-buffer-name)
      (-let* (((target-start . target-end) (tui--separating-divisions parent index))
              (source-start (tui-marker-list-node-previous (tui--start-division node)))
              (source-end (tui-marker-list-node-next (tui--end-division node))))
        ;; Remove node from current parent
        (when current-parent
          (setf (tui-element-content current-parent)
                (remove node (tui-element-content current-parent))))
        ;; Insert node as child of parent
        (tui--set-parent node new-parent)
        (setf (tui-element-content new-parent)
              (-insert-at index node (tui-element-content new-parent)))
        (tui--update-node-index-positions (tui-child-nodes new-parent))
        ;; Move node markers
        (tui-marker-list-move-segment marker-list source-start source-end target-start target-end)
        (tui--apply-inherited-text-props (tui-start node) (tui-end node) parent)))
    node))

(defun tui--update-node-index-positions (nodes)
  "Update cached node positions on or after INDEX in NODES list by adding OFFSET to their index position."
  (-map-indexed
   (lambda (item-index item)
     (setf (tui-node-relative-index item) item-index))
   nodes))

(defun tui-ancestor-p (parent node)
  "Return t if PARENT is a parent node of NODE."
  (and (tui-element-p parent)
       (cl-position parent (tui-ancestor-elements node))
       t))

(defun tui--relative-tree-position (a b)
  "Return the relative position of nodes A and B within the content tree.
Returns -1 if A is left of B, 1 if A is right of B, and 0
if one is a parent of the other.

Signals an error if A and B are not in the same content tree."
  (tui--relative-position (tui-index-position a)
                       (tui-index-position b)))

(defun tui-lowest-common-ancestor (node-a node-b)
  "Return the lowest common ancestor node of NODE-A and NODE-B.
Returns nil if NODE-A and NODE-B reside in distinct content
trees."
  (let* ((ancestors-a (tui-ancestor-elements node-a))
         (ancestors-b (tui-ancestor-elements node-b))
         (-compare-fn #'eq))
    (cl-loop for ancestor-a in ancestors-a
             for common = (-contains-p ancestors-b ancestor-a)
             until common
             finally return ancestor-a)))

(defun tui-precedes-p (a b)
  "Return t if A precedes B within the content tree (depth-first traversal).

Nodes do not precede or follow any of their ancestor
elements."
  (or (< (tui-end a) (tui-start b))
      (and (= (tui-end a) (tui-start b))
           (tui--position-precedes (tui-index-position a)
                                (tui-index-position b)))))

(defun tui--position-precedes (position-a position-b)
  "Return t if POSITION-A precedes POSITION-B."
  (cl-loop for a in position-a
           for b in position-b
           if (< a b) return t
           if (> a b) return nil
           finally return nil))

(defun tui--relative-position (position-a position-b)
  "Comparator of tree list-based tree positions POSITION-A and POSITION-B.
Returns -1 if POSITION-A precedes POSITION-B, 1 if POSITION-B
precedes POSITION-A, and 0 if one is position-a parent of the
other."
  (cl-loop for position-a in position-a
           for position-b in position-b
           if (< position-a position-b) return -1
           if (> position-a position-b) return 1
           finally return 0))

(defun tui-follows-p (a b)
  "Return t if A follows B in terms in the content tree.
Nodes do not precede or follow any of their ancestor elements."
  (tui-precedes-p b a))

(defun tui-encloses-p (thing part)
  "Return t if THING fully encloses PART.
Returns nil if either THING or PART shares a boundary and is
represented by a point or marker.

Markers are ambiguous; without an element context, coincident
markers cannot be compared."
  (cond
   ((consp thing)
    (-when-let* (((thing-start . thing-end) thing))
      (and (tui-precedes-p thing-start part)
           (tui-follows-p thing-end part))))
   ((tui-node-p thing)
    (if (tui-node-p part)
        (tui-ancestor-p thing part)
      (-let* (((thing-start . thing-end) (tui-segment thing)))
        (or (and (number-or-marker-p part)
                 (< part thing-start)
                 (> part thing-end))
            (and (consp part)
                 (-when-let* (((part-start . part-end) part))
                   (and (number-or-marker-p part-start)
                        (< part-start thing-start)
                        (> part-start thing-end)
                        (number-or-marker-p part-end)
                        (< part-end thing-start)
                        (> part-end thing-end))))))))))

(defun tui-overlaps-p (a b)
  "Return t if segments represented by A and B overlap in any way.

A and B overlap, for example, if one is a parent of the other.  Coincident points/markers don't count."
  (or (< (tui-end a) (tui-start b))
      (and (= (tui-end a) (tui-start b))
           (tui--position-coincides (tui-index-position a)
                                 (tui-index-position b)))))

(defun tui--position-coincides (position-a position-b)
  "Return t if POSITION-A coincides with POSITION-B (one is a parent of the other)."
  (cl-loop for a in position-a
           for b in position-b
           if (< a b) return nil
           if (> a b) return nil
           finally return t))

;;; Composition API

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

(defun tui--new-id ()
  "Generate a TUI ID."
  (abs (random)))

(cl-defmacro tui-define-component (name &key
                                     documentation
                                     prop-documentation
                                     get-default-props
                                     get-initial-state
                                     component-will-mount
                                     mount
                                     component-did-mount
                                     component-will-receive-props
                                     should-component-update
                                     component-will-update
                                     render
                                     component-did-update
                                     component-will-unmount)
  "Macro for defining `tui-component' types."
  (declare (indent defun)) ;; TODO: support an optional docstring as the third parameter (as an alternative to the keyword form)
  `(progn
     (cl-defstruct (,name (:include tui-component)
                          (:constructor nil)
                          (:constructor ,(intern (format "%s-create" (symbol-name name)))
                                        (&key props invisible
                                              &aux (id (tui--new-id))))))
     
     ,(if get-default-props
          `(cl-defmethod tui-get-default-props ((component ,name))
             ""
             (let ((class (tui--object-class component)))
               (cl-copy-seq
                (or (gethash class tui--default-props-table)
                    (puthash class (funcall ,get-default-props) tui--default-props-table))))))

     ,(if get-initial-state
          `(cl-defmethod tui-get-initial-state ((component ,name))
             ""
             (funcall ,get-initial-state)))

     ,(if component-will-mount
          `(cl-defmethod tui-component-will-mount ((component ,name))
             ""
             (funcall ,component-will-mount)))

     ,(if mount
          `(cl-defmethod tui--mount ((component ,name) start &optional end parent)
             ""
             (funcall ,mount component start end parent)))
     
     ,(if component-did-mount
          `(cl-defmethod tui-component-did-mount ((component ,name))
             ""
             (funcall ,component-did-mount)))

     ,(if component-will-receive-props
          `(cl-defmethod tui-component-will-receive-props ((component ,name) next-props)
             ""
             (funcall ,component-will-receive-props next-props)))

     ,(if should-component-update
          `(cl-defmethod tui-should-component-update ((component ,name) next-props next-state)
             ""
             (funcall ,should-component-update next-props next-state)))

     ,(if component-will-update
          `(cl-defmethod tui-component-will-update ((component ,name) next-props next-state)
             ""
             (funcall ,component-will-update next-props next-state)))

     ,(if render
          `(cl-defmethod tui-render ((component ,name))
             ""
             (funcall ,render)))

     ,(if component-did-update
          `(cl-defmethod tui-component-did-update ((component ,name) next-props next-state)
             ""
             (funcall ,component-did-update next-props next-state)))

     ,(if component-will-unmount
          `(cl-defmethod tui-component-will-unmount ((component ,name) prev-props prev-state)
             ""
             (funcall ,component-will-unmount prev-props prev-state)))

     (defun ,name (&rest args)
       ,(format "%s%s"
                documentation
                (if prop-documentation
                    (format "\n\nValid parameters include:\n%s"
                            (s-join "\n"
                                    (cl-loop for (key docstring) on prop-documentation by #'cddr
                                             collect
                                             (format "\t%S\t\t%s\n" key docstring))))))
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
           component)))))

(defun tui--make-root-node (node)
  "Internal function to initialize NODE as a proper root node."
  (let ((node (tui--normalize-node node)))
    (setf (tui-node-marker-list node) (tui-marker-list-create))
    node))

(defun tui--process-update-queue ()
  "Process the update queue.

Very basic now; simply apply updates until the queue is empty."
  (let ((tui--applying-updates t)
        (inhibit-read-only t))
    (while tui--update-queue
      (tui--apply-update (pop tui--update-queue)))
    (run-hooks tui-update-hook)))

(defun tui--apply-update (update)
  "Apply UPDATE to corresponding content tree."
  (pcase update
    (`(component-did-mount ,component)
     (tui--funcall #'tui-component-did-mount component))
    (`(mount ,child ,start ,end ,parent)
     (tui--mount child start end parent))
    (`(insert ,item ,parent ,index)
     (display-warning 'tui-diff (format "INSERT %S %S %d" (tui--object-class item) (tui--object-class parent) index) :debug tui-log-buffer-name)
     (tui-insert-node item parent index))
    (`(remove ,node)
     (display-warning 'tui-diff (format "REMOVE %S" (tui--object-class node)) :debug tui-log-buffer-name)
     (tui-remove node))
    (`(replace ,old-node ,new-node)
     (display-warning 'tui-diff (format "RELACE %S %S" (tui--object-class old-node) (tui--object-class new-node)) :debug tui-log-buffer-name)
     (tui-replace-node old-node new-node))
    (`(update-content ,node ,new-content)
     (display-warning 'tui-diff (format "UPDATE-CONTENT %S %S" (tui--object-class node) new-content) :debug tui-log-buffer-name)
     (setf (tui-node-content node) new-content)
     (-let* (((start . end) (tui-segment node)))
       (tui--insert node)
       (tui--apply-inherited-text-props start end (tui-parent node))))
    (`(update-props ,component ,updated-props)
     (-let* ((old-props (tui--get-props component))
             (invisible (plist-get updated-props :invisible)))
       (display-warning 'tui-diff (format "UPDATE-PROPS %S" (tui--object-class component)) :debug tui-log-buffer-name)
       (tui--set-props component updated-props)
       (when (not (eq (plist-get old-props :invisible) invisible))
         (if invisible
             (tui-hide-element component)
           (tui--show-element component)))))))

(defun tui-render-element (node &optional target)
  "Primary function for rendering content to a buffer.

Input CONTENT is converted to a well-formed content tree.
Returns a reference to the root node of the rendered content.

Optionally specify TARGET context for rendering NODE.  TARGET may
be a character position, marker, buffer name, buffer, or another
tui-element."
  ;; FIXME: temporary measure until there's a mechanism for cleaning up the queue after errors
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
          (tui--process-update-queue)
          ;;(tui-valid-content-tree-p node)
          node)))))

(cl-defmethod tui-rendered-p ((element tui-node))
  "Return t if ELEMENT has been rendered."
  (and (tui-segment element)
       t))

(defun tui-force-update (component)
  "Force COMPONENT to re-render."
  (let ((new-props (tui--get-props component))
        (new-state (tui--get-state component)))
    (tui--funcall 'tui-component-will-update component new-props new-state)
    (push `((component-did-update ,component ,new-props ,new-state)) tui--update-queue)
    (tui--update component)
    (unless tui--applying-updates
      (tui--process-update-queue))))

(cl-defmethod tui-index-position ((node tui-node))
  "Return the index position of NODE within its content tree."
  (reverse
   (mapcar
    (lambda (node)
      (tui-node-relative-index node))
    (gethash node tui--element-parent-table))))

(cl-defmethod tui-index-position-string ((node tui-node))
  "Return the index position of NODE within its content tree as a string (ex: '1.0.2.3')."
  (s-join "." (tui-index-position node)))

(cl-defmethod tui--split-division (node division)
  ""
  (let ((marker-list (tui-node-marker-list node)))
    (tui-marker-list-split-node marker-list division)))

(defun tui--node-height (node)
  "Return the height of NODE in its content tree.  The root element has a height of 1."
  (length (tui-ancestor-elements node)))

(provide 'tui-core)

;;; tui-core.el ends here
