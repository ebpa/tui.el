;;; tui-marker-list.el --- Linked list structure tracking markers       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(eval-when-compile (require 'cl-lib))
(require 'dash)

;;; Types

;; TODO: split cost functions?
;; TODO: split/merge hooks?

;;; Code:

(cl-defstruct tui-marker-list
  "Object for maintaining a list of ordered markers within a tui content tree."
  marker-table
  first
  last)

(cl-defstruct
    (tui-marker-list-node
     (:constructor nil)
     (:constructor tui-marker-list-node-create (marker &optional data)))
  "Object for tracking a marker within a tui content tree."
  marker
  data
  previous
  next)

;;; Public Methods

;;;; CRUD

(defun tui-marker-list-create (&optional markers data)
  "Return a new marker list.  List will populated with supplies MARKERS and corresponding DATA."
  (save-current-buffer
    (save-excursion
      (let* ((marker-list (make-tui-marker-list))
             (markers (mapcar (lambda (marker)
                                (if (numberp marker)
                                    (save-excursion (goto-char marker)
                                                    (point-marker))
                                  marker))
                              markers))
             (table (setf (tui-marker-list-marker-table marker-list) (make-hash-table :test #'eq)))
             (markers (cl-stable-sort markers #'<)))
        (cl-loop for (marker . data) in (-zip-fill nil markers data)
                 for node = (tui-marker-list-node-create marker data)
                 do
                 (tui-marker-list-node--insert-last marker-list node)
                 (puthash marker node table))
        marker-list))))

(defun tui-marker-list-marker-data (marker-list marker)
  "Return data associated with MARKER in MARKER-LIST."
  (-when-let* ((node (tui-marker-list--marker-node marker-list marker)))
    (tui-marker-list-node-data node)))

(defun tui-marker-list-set-marker-data (marker-list marker data)
  "Set data associated with MARKER in MARKER-LIST to DATA."
  (if-let* ((node (tui-marker-list--marker-node marker-list marker)))
           (setf (tui-marker-list-node-data node) data)
           (error "Marker not found in marker list")))

(defun tui-marker-list-node--insert-first (marker-list new-node)
  "Insert NEW-NODE first in MARKER-LIST."
  ;;(cl-assert (tui-marker-list-node-p new-node))
  (let ((first-node (tui-marker-list-first marker-list)))
    (setf (tui-marker-list-first marker-list) new-node)
    (if first-node
        (progn
          (setf (tui-marker-list-node-next new-node) first-node)
          (setf (tui-marker-list-node-previous first-node) new-node))
      (setf (tui-marker-list-last marker-list) new-node)
      (setf (tui-marker-list-first marker-list) new-node))))

(defun tui-marker-list-node--insert-last (marker-list new-node)
  "Insert NEW-NODE last in MARKER-LIST."
  ;;(cl-assert (tui-marker-list-node-p new-node))
  (let ((last-node (tui-marker-list-last marker-list)))
    (setf (tui-marker-list-last marker-list) new-node)
    (if last-node
        (progn
          (setf (tui-marker-list-node-previous new-node) last-node)
          (setf (tui-marker-list-node-next last-node) new-node))
      (setf (tui-marker-list-last marker-list) new-node)
      (setf (tui-marker-list-first marker-list) new-node))))

(defun tui-marker-list-node--insert-before (marker-list new-node node)
  "Insert NEW-NODE before NODE in MARKER-LIST.  Return NEW-NODE."
  ;;(cl-assert (tui-marker-list-node-p new-node))
  ;;(cl-assert (tui-marker-list-node-p node))
  ;; TODO: check and possibly extract from its current position in the list
  (let ((previous-node (tui-marker-list-node-previous node))
        ;; (initial-length (tui-marker-list-length marker-list))
        )
    (setf (tui-marker-list-node-next new-node) node)
    (setf (tui-marker-list-node-previous node) new-node)
    (if previous-node
        (setf (tui-marker-list-node-next previous-node) new-node)
      (setf (tui-marker-list-first marker-list) new-node))
    (setf (tui-marker-list-node-previous new-node) previous-node)
    ;;(cl-assert (= (tui-marker-list-length marker-list) (+ initial-length 1)) t "List length increases by one with an insertion.")
    ;;(cl-assert (tui-marker-list-valid-p marker-list) t "List is valid after an insertion.")
    new-node))

(defun tui-marker-list-node--insert-after (marker-list new-node node)
  "Insert NEW-NODE after NODE in MARKER-LIST.  Return NEW-NODE."
  ;; (cl-assert (tui-marker-list-node-p new-node))
  ;; (cl-assert (tui-marker-list-node-p node))
  ;; (cl-assert (member* node (tui-marker-list--all-nodes marker-list) :test #'eq))
  ;; TODO: check and possibly extract from its current position in the list
  (let ((next-node (tui-marker-list-node-next node))
        ;; (initial-length (tui-marker-list-length marker-list))
        )
    (setf (tui-marker-list-node-previous new-node) node)
    (setf (tui-marker-list-node-next node) new-node)
    (if next-node
        (setf (tui-marker-list-node-previous next-node) new-node)
      (setf (tui-marker-list-last marker-list) new-node))
    (setf (tui-marker-list-node-next new-node) next-node)
    ;;(cl-assert (= (tui-marker-list-length marker-list) (+ initial-length 1)) t "List length increases by one with an insertion.")
    ;;(cl-assert (tui-marker-list-valid-p marker-list) t "List is valid after an insertion.")
    new-node))

(defun tui-marker-list-copy (marker-list)
  "Return a copy of MARKER-LIST with copies of markers from MARKER-LIST."
  ;; TODO
  ;; copy dll (don't copy element lists; only the references

  ;; replace all nodes with copies
  )

(defun tui-marker-list-same-buffer-p (marker-list marker)
  "Return t if MARKER-LIST and MARKER refer to the same buffer.
Returns nil if MARKER-LIST is empty."
  (let ((buffer (tui-marker-list-buffer marker-list)))
    (and buffer
         (markerp marker)
         (not (eq buffer (marker-buffer marker))))))

(defun tui-marker-list--resolve-new-marker (marker-list marker)
  "Internal helper function to resolve MARKER to a distinct marker within MARKER-LIST."
  (let ((buffer (tui-marker-list-buffer marker-list)))
    (unless (or (not buffer)
                (numberp marker)
                (eq buffer (marker-buffer marker)))
      (error "Marker must refer to the same buffer"))
    (when (numberp marker)
      (when (> marker (point-max))
        (error "Numeric position refers to point greater than (point-max)"))
      (setq marker (save-current-buffer
                     (save-excursion (when buffer
                                       (set-buffer buffer))
                                     (goto-char marker)
                                     (point-marker)))))
    (when (tui-marker-list--marker-node marker-list marker)
      (error "Marker is already in list"))
    marker))

(defun tui-marker-list-insert (marker-list marker &optional data)
  "Insert MARKER into MARKER-LIST.  Return new node containing MARKER.

Set marker data to DATA."
  ;; TODO: generalize this position verification/transformation
  (let* ((buffer (tui-marker-list-buffer marker-list)))
    (with-current-buffer (or buffer (current-buffer))
      (setq marker (tui-marker-list--resolve-new-marker marker-list marker))
      (let* ((node (tui-marker-list-first marker-list)))
        (while (and node
                    (> marker (tui-marker-list-node-marker node)))
          (when (equal (tui-marker-list-node-marker node) marker)
            (error "Ambiguous position; a marker at that position is already in the list.  Split that one if multiple coincident markers are needed"))
          (setq node (tui-marker-list-node-next node)))
        (let ((new-node (tui-marker-list-node-create marker data)))
          (if node
              (tui-marker-list-node--insert-before marker-list new-node node)
            (tui-marker-list-node--insert-last marker-list new-node))
          (puthash marker new-node (tui-marker-list-marker-table marker-list))
          new-node)))))

(defun tui-marker-list-remove (marker-list marker)
  "Remove MARKER from MARKER-LIST."
  (-if-let* ((node (tui-marker-list--marker-node marker-list marker)))
      (tui-marker-list-remove-node marker-list node)
    (error "Marker not found!")))

(defun tui-marker-list-next-marker (marker-list marker)
  "Return the next marker after MARKER in MARKER-LIST."
  (-if-let* ((node (tui-marker-list--get-single-node-at marker-list marker)))
      (-when-let* ((next-node (tui-marker-list-node-next node)))
        (tui-marker-list-node-marker next-node))
    (error "Marker not found")))

(defun tui-marker-list-prev-marker (marker-list marker)
  "Return the marker preceding MARKER in MARKER-LIST."
  (-if-let* ((node (tui-marker-list--get-single-node-at marker-list marker)))
      (-when-let* ((next-node (tui-marker-list-node-next node)))
        (tui-marker-list-node-marker next-node))
    (error "Marker not found")))

(defun tui-marker-list-remove-node (marker-list node)
  "Remove NODE from MARKER-LIST."
  (let ((previous (tui-marker-list-node-previous node))
        (next (tui-marker-list-node-next node)))
    (if previous
        (setf (tui-marker-list-node-next previous) next)
      (setf (tui-marker-list-first marker-list) next))
    (if next
        (setf (tui-marker-list-node-previous next) previous)
      (setf (tui-marker-list-last marker-list) previous))))

(defun tui-marker-list-markers (marker-list)
  "Return all markers in MARKER-LIST."
  (mapcar #'tui-marker-list-node-marker
          (tui-marker-list--all-nodes marker-list)))

(defun tui-marker-list--all-nodes (marker-list)
  "Return an ordered list of all nodes in MARKER-LIST."
  (let* ((node (tui-marker-list-first marker-list))
	 nodes)
    (while node
      (push node nodes)
      (setq node (tui-marker-list-node-next node)))
    (reverse nodes)))

(defun tui-marker-list-valid-p (marker-list)
  "Return t if MARKER-LIST is valid (all markers point to the same buffer and are properly ordered)."
  (let* ((markers (tui-marker-list-markers marker-list))
         (nodes (tui-marker-list--all-nodes marker-list)))
    (if (null markers)
        t ;; an empty marker list is valid
      (and
       ;; all markers point to the same buffer
       (let ((buffer (marker-buffer (cl-first markers))))
         (-all-p (lambda (marker)
                   (eq (marker-buffer marker) buffer))
                 (cl-rest markers)))
       ;; markers are ordered
       (-reduce
        (lambda (a b)
          (and a b
               (<= a b)
               b))
        markers)
       ;; each pair of adjacent nodes references each other
       (cl-loop for left in nodes
                for right in (cl-rest nodes)
                do
                (cl-assert (eq (tui-marker-list-node-next left) right) t "Node refers to the next node in the list")
                (cl-assert (eq (tui-marker-list-node-previous right) left) t "Node refers to the previous node in the list")
                finally return t)
       (not (cl-assert (null (tui-marker-list-node-previous (cl-first nodes))) t "First node has no previous node"))
       (not (cl-assert (null (tui-marker-list-node-next (-last-item nodes))) t "Last node has no next node"))
       (not (cl-assert (eq (tui-marker-list-first marker-list)
                        (cl-first nodes)) t "List contains references to the first item"))
       (not (cl-assert (eq (tui-marker-list-last marker-list)
                        (-last-item nodes)) t "List contains references to the last item"))))))

(defun tui-marker-list-buffer (marker-list)
  "Return the buffer that MARKER-LIST refers to.
Return nil if MARKER-LIST is empty."
  (-when-let* ((node (tui-marker-list-first marker-list)))
    (marker-buffer (tui-marker-list-node-marker node))))

(defun tui-marker-list-merge-nodes (marker-list left right data)
  "Merge adjacent LEFT and RIGHT nodes in MARKER-LIST and associating DATA with the merged node."
  ;; Merge into left node
  (let* ((next-node (tui-marker-list-node-next right)))
    (if next-node
        (setf (tui-marker-list-node-previous next-node) left)
      (setf (tui-marker-list-last marker-list) left))
    (setf (tui-marker-list-node-next left) next-node)
    (setf (tui-marker-list-node-data left) data)))

;;;; Other

(cl-defmethod cl-print-object ((marker-list tui-marker-list) stream)
  (princ "#<marker-list>" stream))

(cl-defmethod cl-print-object ((marker-list tui-marker-list) stream)
  (princ "#<marker-list>" stream))

(cl-defmethod cl-object-print ((marker-list tui-marker-list) stream)
  (princ "#<marker-list>" stream))

(cl-defmethod cl-print-object ((node tui-marker-list-node) stream)
  (princ "#<tui-marker-list-node>" stream))

(cl-defmethod cl-print-object ((node tui-marker-list-node) stream)
  (princ "#<tui-marker-list-node>" stream))

(cl-defmethod cl-object-print ((node tui-marker-list-node) stream)
  (princ "#<tui-marker-list-node>" stream))

(defun tui-marker-list--get-marker (position)
  "Return the buffer position of POSITION possibly represented bymarker node, marker, or number."
  (save-current-buffer
    (save-excursion
      (cond
       ((tui-marker-list-node-p position)
        (tui-marker-list-node-marker position))
       ((numberp position)
        (goto-char position) ;; FIXME: we need the buffer context of the marker list
        (point-marker))
       (t
        position)))))

(defun tui-marker-list-length (marker-list)
  "Return the length of MARKER-LIST."
  (let* ((node (tui-marker-list-first marker-list))
	 (n 0))
    (while node
      (setq node (tui-marker-list-node-next node))
      (setq n (+ n 1)))
    n))

(defalias 'tui-marker-list-size 'tui-marker-list-length)

(defun tui-marker-list-markers-at (marker-list position)
  "Return a list of markers in MARKER-LIST at POSITION in order."
  (mapcar #'tui-marker-list-node-marker
          (tui-marker-list--nodes-at marker-list position)))

(defun tui-marker-list-delete-segment (marker-list start end)
  "Remove START and END markers and all markers between them from MARKER-LIST."
  ;; TODO: destroy markers in segment?
  (let* ((start-node (tui-marker-list--get-single-node-at marker-list start))
         (end-node (tui-marker-list--get-single-node-at marker-list end)))
    (tui-marker-list-delete-node-segment marker-list start-node end-node)))

(defun tui-marker-list-delete-node-segment (marker-list start end)
  "Remove START and END marker nodes and all nodes between them from MARKER-LIST."
  ;; TODO: destroy markers in segment?
  (let* ((previous (and start
                        (tui-marker-list-node-previous start)))
         (next (and end
                    (tui-marker-list-node-previous end)))
         (removed-nodes (tui-marker-list-nodes-in-range marker-list start end))
         (marker-table (tui-marker-list-marker-table marker-list)))
    (if previous
        (setf (tui-marker-list-node-next previous) next)
      (setf (tui-marker-list-first marker-list) next))
    (if next
        (setf (tui-marker-list-node-previous next) previous)
      (setf (tui-marker-list-last marker-list) previous))
    (mapc (lambda (node)
            (remhash (tui-marker-list-node-marker node) marker-table))
          removed-nodes)))

(cl-defmethod tui-marker-list-open-segment (marker-list start &optional end)
  "'Open' segment bounded by START and END markers by setting the insertion type of START marker to nil and END to t.  Return as a cons (START . END).  Markers in MARKER-LIST that have coincident positions with either START or END will also have their insertion types updated to maintain their ordering for insertion between START and END."
  (let* ((start-node (tui-marker-list--get-single-node-at marker-list start))
         (end-node (tui-marker-list--get-single-node-at marker-list end)))
    (unless (eq (tui-marker-list-node-next start-node)
                end-node)
      (error "Segment contains nodes"))
    (set-marker-insertion-type (tui-marker-list-node-marker start-node) nil)
    (mapc (lambda (node)
            (set-marker-insertion-type (tui-marker-list-node-marker node) nil))
          (tui-marker-list-preceding-coincident-nodes marker-list start-node))
    (set-marker-insertion-type (tui-marker-list-node-marker end-node) t)
    (mapc (lambda (node)
            (set-marker-insertion-type (tui-marker-list-node-marker node) t))
          (tui-marker-list-following-coincident-nodes marker-list end-node))
    (cons
     (tui-marker-list-node-marker start-node)
     (tui-marker-list-node-marker end-node))))

(defun tui-marker-list-split-marker (marker-list marker &optional number)
  "Split MARKER in MARKER-LIST.
Returns the new markers as a list retaining their list ordering.
Same as `tui-marker-list-split-node', but accepts and returns
markers."
  ;;(display-warning 'tui (format "Splitting marker %S (%S)" start (tui--object-class node)) :debug tui-log-buffer-name)
  (-let* ((node (tui-marker-list--get-single-node-at marker-list marker))
          (nodes (tui-marker-list-split-node marker-list node number)))
    (mapcar #'tui-marker-list-node-marker nodes)))

;; (defun tui-marker-list-split-marker-left (marker-list marker &optional number)
;;   "Split MARKER in marker list creating a new marker at the same position, but to the left of MARKER and returning the new marker.  Same as `tui-marker-list-split-node-left', but accepts and returns markers."
;;   (car (tui-marker-list-split-marker marker-list marker number)))

(defun tui-marker-list-split-marker-right (marker-list marker &optional number)
  "Split MARKER in MARKER-LIST creating a new marker at the same position, but to the right of MARKER and returning the new marker.  Same as `tui-marker-list-split-node-right', but accepts and returns markers."
  (cadr (tui-marker-list-split-marker marker-list marker number)))

(defun tui-marker-list-split-node (marker-list node &optional number)
  ;; CLEANUP: non-recursive implementation
  "Split NODE in MARKER-LIST.
Returns the new markers as a list retaining their list ordering.
Same as `tui-marker-list-split-marker', but accepts and returns
nodes."
  ;; OPTIMIZE: it may be possible to choose an efficient direction of split to limit the number of affected elements
  (unless number (setq number 1))
  (let* ((split-node node)
         ;; (length-before-split (tui-marker-list-length marker-list))
         new-nodes)
    (dotimes (i number)
      (-let* ((existing-marker (tui-marker-list-node-marker split-node))
              (new-marker (copy-marker existing-marker))
              (new-node (tui-marker-list-node-create new-marker)))
        (tui-marker-list-node--insert-after marker-list new-node split-node)
        (puthash new-marker new-node (tui-marker-list-marker-table marker-list))
        ;;(cl-assert (tui-marker-list--nodes-adjacent-p split-node new-node) t "Adjacent split elements should be adjacent.")
        (push new-node new-nodes)))
    ;; (cl-assert (= (+ number length-before-split) (tui-marker-list-length marker-list)) t "Split should create an additional node.")
    (cons node
          new-nodes)))
;;(cl-assert (tui-marker-list-valid-p marker-list) t "List is valid after a split.")


(defun tui-marker-list--nodes-adjacent-p (left right)
  "Return t if LEFT is immediately before RIGHT."
  (eq (tui-marker-list-node-next left) right))

;; TODO: tui-marker-list-split-node-left (/right)

(defun tui-marker-list-consolidate (marker-list)
  "Consolidate (eliminate) all redundant markers in MARKER-LIST.  All coincident markers (markers with the same buffer position) are marged."
  ;; TODO: iterate through marker list doing (pairwise/group?) merges
  )

(defun tui-marker-list-move-segment (marker-list source-start source-end target-start target-end)
  "Move a segment of markers and content in within MARKER-LIST.

Replaces target segment content with source content.  Signals an
error if target segment contains markers between TARGET-START and
TARGET-END.  Markers between SOURCE-START and SOURCE-END are
relocated to preserve their relative positions within the
segment.  SOURCE-START and SOURCE-END markers are preserved and
have nothing between them after the move."
  (save-excursion
    (with-current-buffer (tui-marker-list-buffer marker-list)
      (let* ((source-start-node (tui-marker-list--get-single-node-at marker-list source-start))
             (source-start (tui-marker-list--get-marker source-start))
             (source-end-node (tui-marker-list--get-single-node-at marker-list source-end))
             (source-end (tui-marker-list--get-marker source-end))
             (target-start-node (tui-marker-list--get-single-node-at marker-list target-start))
             (target-start (tui-marker-list--get-marker target-start))
             (target-end-node (tui-marker-list--get-single-node-at marker-list target-end))
             (target-end (tui-marker-list--get-marker target-end))
             (segment-length (- source-end source-start))
             ;; Save marker offsets
             (internal-nodes (tui-marker-list-nodes-between marker-list source-start-node source-end-node))
             (internal-marker-offsets (mapcar (lambda (node)
                                                (cons node (- (tui-marker-list-node-marker node) source-start)))
                                              internal-nodes))
             ;; Copy source segment string
             (content-string (buffer-substring source-start source-end)))
        ;; Delete source segment
        (delete-region source-start source-end)
        ;; Delete target content
        (delete-region target-start target-end)
        (tui-marker-list-open-segment marker-list target-start-node target-end-node)
        (goto-char target-start)
        (insert content-string)
        ;; Update node references
        (when internal-nodes
          (let* ((last-internal-node (-last-item internal-nodes))
                 (first-internal-node (cl-first internal-nodes))
                 (source-previous-node (tui-marker-list-node-previous first-internal-node))
                 (source-next-node (tui-marker-list-node-next last-internal-node)))
            ;; front of target
            (setf (tui-marker-list-node-next target-start-node) first-internal-node)
            (setf (tui-marker-list-node-previous first-internal-node) target-start-node)
            ;; end of target
            (setf (tui-marker-list-node-next last-internal-node) target-end-node)
            (setf (tui-marker-list-node-previous target-end-node) last-internal-node)
            ;; source
            (setf (tui-marker-list-node-next source-previous-node) source-next-node)
            (setf (tui-marker-list-node-previous source-next-node) source-previous-node)))
        ;; Move all internal markers to their offset within the target segment
        (mapcar (-lambda ((node . offset))
                  (let ((marker (tui-marker-list-node-marker node)))
                    (move-marker marker (+ target-start offset) (marker-buffer marker))))
                internal-marker-offsets)))))

(defun tui-marker-list-markers-in-range (marker-list start end)
  "Return marker nodes in MARKER-LIST within START and END (inclusive)."
  ;; TODO: allow imprecise positions
  (mapcar #'tui-marker-list-node-marker
          (tui-marker-list-nodes-in-range marker-list start end)))

(defun tui-marker-list-markers-between (marker-list start end)
  "Return marker nodes in MARKER-LIST between START and END (exclusive)."
  ;; TODO: allow imprecise positions
  (mapcar #'tui-marker-list-node-marker
          (tui-marker-list-nodes-between marker-list start end)))

;;; Private methods

(defun tui-marker-list-nodes-in-range (marker-list start end)
  "Return marker nodes in MARKER-LIST within START and END (inclusive)."
  ;; CLEANUP: optionally return the markers themselves?
  (let* ((start-node (tui-marker-list--get-single-node-at marker-list start))
         (end-node (tui-marker-list--get-single-node-at marker-list end))
         (stop-node (tui-marker-list-node-next end-node))
         (this-node start-node)
         (nodes nil))
    (progn
      (while (not (eq this-node stop-node))
        (let ((next-node (tui-marker-list-node-next this-node)))
          (when (and (null next-node)
                     stop-node)
            (error "Reached end of list before finding end marker"))
          (push this-node nodes)
          (setq this-node next-node)))
      (reverse nodes))))

(defun tui-marker-list-nodes-between (marker-list start end)
  "Return marker nodes in MARKER-LIST between START and END (exclusive)."
  (let* ((nodes (tui-marker-list-nodes-in-range marker-list start end)))
    (when (> (length nodes) 2)
      (-slice nodes 1 -1))))

(defun tui-marker-list--get-single-node-at (marker-list position)
  "Return a single marker node from MARKER-LIST at POSITION or signal an error if there are multiple coincident markers."
  (or
   ;; it's already a tui-marker-list-node
   (and (tui-marker-list-node-p position)
        position)
   ;; look up marker directly
   (gethash position (tui-marker-list-marker-table marker-list))
   ;; find an equivalent marker
   (let ((nodes (tui-marker-list--nodes-at marker-list position)))
     (if (eq (length nodes) 1)
         (car nodes)
       (error "There are coincident markers at the given position.  Use a marker from the marker list")))))

(defun tui-marker-list--nodes-at (marker-list position)
  "Return a list of marker nodes in MARKER-LIST at POSITION in order."
  (let* ((node (tui-marker-list-first marker-list))
         node-position coincident-nodes)
    (while (and node
                (setq node-position (tui-marker-list-node-marker node))
                (<= node-position position))
      (when (= node-position position)
        (push node coincident-nodes))
      (setq node (tui-marker-list-node-next node)))
    coincident-nodes))

;;;; Node methods

(defun tui-marker-list--marker-node (marker-list marker)
  "Return the linked list entry in MARKER-LIST for MARKER."
  (gethash marker (tui-marker-list-marker-table marker-list)))

(defun tui-marker-list-preceding-coincident-nodes (marker-list node)
  "Return a list of marker nodes in MARKER-LIST logically preceding NODE with the same buffer position as NODE.  Marker nodes returned preserve their list ordering."
  (let* ((marker (tui-marker-list-node-marker node))
         (previous-node (tui-marker-list-node-previous node))
         coincident-marker-nodes)
    (while (and previous-node
                (equal (tui-marker-list-node-marker previous-node) marker))
      (push previous-node coincident-marker-nodes)
      (setq previous-node (tui-marker-list-node-previous previous-node)))
    coincident-marker-nodes))

(defun tui-marker-list-following-coincident-nodes (marker-list node)
  "Return a list of marker nodes in MARKER-LIST logically preceding NODE, but with the same buffer position as NODE.  Marker nodes returned preserve their list ordering."
  (let* ((marker (tui-marker-list-node-marker node))
         (next-node (tui-marker-list-node-next node))
         coincident-marker-nodes)
    (while (and next-node
                (equal (tui-marker-list-node-marker next-node) marker))
      (push next-node coincident-marker-nodes)
      (setq next-node (tui-marker-list-node-next next-node)))
    coincident-marker-nodes))

(provide 'tui-marker-list)

(provide 'tui-marker-list)

;;; tui-marker-list.el ends here
