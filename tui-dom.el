;;; tui-dom.el --- Tui "DOM" logic       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(require 'tui-node-types)

(cl-defmethod tui-append-child ((parent tui-element) child)
  "Add CHILD to end of the list of PARENT element's children."
  (let ((content (tui-element-content parent)))
    (tui-insert-node child parent (length content))))

(defun tui-insert-node (node parent index)
  "Insert NODE as a child of PARENT at INDEX.

In case NODE is already mounted, this function removes NODE from
existing parent and moves it to new location in PARENT.

Returns NODE."
  (cl-assert (tui-element-p parent) t "Nodes can only be inserted into elements.")
  (unless (tui-node-p node)
    (setq node (tui--normalize-node node)))
  (if (tui-node-mounted node) ;; TODO: special case with unmounted NODE, but with parent `(tui-parent node)'
      (tui-move-subtree node parent index)
    (-let* ((content (tui-element-content parent))
            (target-division (car (tui--separating-divisions parent index)))
            ((target-start target-end) (cl-rest (tui-marker-list-split-node (tui-node-marker-list parent)
                                                                            target-division 2))))
      ;; (if (eq node node-after-target)
      ;;     (display-warning 'tui (format "Node %S already at index %d" (tui--object-class node) index) :debug tui-log-buffer-name)
      (cl-assert (not (null target-division)) t "Target marker for insertion not found.")
      (setf (tui-element-content parent) (-insert-at index node content))
      (tui--update-node-index-positions (tui-child-nodes parent))
      (tui--mount node target-start target-end parent)))
  (unless tui--applying-updates
    (tui--process-update-queue))
  node)

(defun tui--separating-divisions (element index)
  "Internal function.

Return pair of divisions within ELEMENT at INDEX corresponding to be used for inserting content at INDEX."
  (let* ((content (tui-element-content element))
         (content-length (length content)))
    (cond
     ((eq content-length 0)
      (cons (tui--start-division element)
            (tui--end-division element)))
     ((eq index 0)
      (cons (tui--start-division element)
            (tui--start-division (cl-first content))))
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
  (cl-assert (tui-parent old-child) t "Target must be a child node.")
  (tui-replace-node old-child new-child))

(defun tui-remove (node)
  "Remove NODE from its current tree position."
  (tui--unmount node))

(defun tui-remove-child (node child)
  "Remove CHILD node of NODE."
  (cl-assert (eq (tui-parent child) node) t "Node is not a child of specified node.")
  (tui-remove child))

(defun tui-root-node (&optional thing)
  "Return the root node of the tree that THING is part of."
  (unless (tui-node-p thing)
    (setq thing (tui-get-node-at (point))))
  (if (tui-root-node-p thing)
      thing
    (-last-item (tui-ancestor-elements thing))))

(defun tui-root-node-p (node)
  "Return t if NODE is the root element of its content tree."
  (not (tui-parent node)))

(cl-defmethod tui-child-nodes (obj)
  "Return nil.  List of child nodes of OBJ (a non-tui object) is nil."
  nil)

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

(defun tui-descendent-nodes (node)
  "Return all descendent nodes of NODE."
  (apply #'append
         (mapcar (lambda (child)
                   (cons child
                         (tui-descendent-nodes child)))
                 (tui-child-nodes node))))

(defun tui--make-root-node (node)
  "Internal function to initialize NODE as a proper root node."
  (let ((node (tui--normalize-node node)))
    (setf (tui-node-marker-list node) (tui-marker-list-create))
    node))


(defvar tui--element-parent-table
  (make-hash-table :weakness 'key)
  "Parent element lookup table.")

(defun tui-ancestor-elements-at (&optional pos type)
  "Get a list of comp elements at POS.  The root element is last.

Filter returned elements according to TYPE.  All ancestors are returned when TYPE is nil."
  ;; CLEANUP: reconcile tui-ancestor-elements-at and tui-ancestor-elements? (confusing otherwise)
  (-when-let* ((element (tui-get-element-at pos)))
    (-filter
     (if type
         (-rpartial #'tui--object-of-class-p type)
       #'identity)
     (cons element
           (tui-ancestor-elements element)))))

(defun tui-ancestor-elements (node &optional type)
  "Return ancestor elements of NODE with the root node last.

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


(defun tui-move-subtree (node parent index)
  "Move subtree rooted at NODE to INDEX position within PARENT.

Returns NODE."
  (display-warning 'tui (format "MOVE-SUBTREE %S to position %d in %S" (tui--object-class node) index (tui--object-class parent)) :debug tui-log-buffer-name)
  (-let* ((current-parent (tui-parent node))
          (current-index (tui-node-relative-index node))
          (new-parent parent)
          (content (tui-element-content parent))
          (marker-list (tui-node-marker-list node)))
    (if (and (eq current-parent new-parent)
             (eq (nth index content) node))
        (display-warning 'tui (format "Node %S already at target location" (tui--object-class node)) :debug tui-log-buffer-name)
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
              (-insert-at (if (<= index current-index)
                              index
                            (- index 1))
                          node (tui-element-content new-parent)))
        (tui--update-node-index-positions (tui-child-nodes new-parent))
        ;; Move node markers
        (tui-marker-list-move-segment marker-list source-start source-end target-start target-end)
        ;; TODO: restore this
        ;; (tui--apply-inherited-text-props (tui-start node) (tui-end node) parent)
        ))
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

(cl-defmethod tui-index-position ((node tui-node))
  "Return the index position of NODE within its content tree."
  (cl-rest
   (reverse
    (mapcar
     (lambda (node)
       (tui-node-relative-index node))
     (gethash node tui--element-parent-table)))))

(cl-defmethod tui-index-position-string ((node tui-node))
  "Return the index position of NODE within its content tree as a string (ex: '1.0.2.3')."
  (s-join "." (tui-index-position node)))



(provide 'tui-dom)

;;; tui-dom.el ends here
