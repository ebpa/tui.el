;;; tui-reconciler.el --- Logic for diffing content trees       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(eval-when-compile (require 'cl-lib))
(require 'tui-node-types)

;;; Code:

(defun tui--diff (old-node new-node)
  "Compute a \"diff\" between OLD-NODE and NEW-NODE and return a list of patches that should be applied to the content tree."
  (cl-assert (or (null old-node) (tui--object-of-class-p old-node 'tui-node)) t "Diff must be applied to `tui-node's.")
  (cl-assert (or (null new-node) (tui--object-of-class-p old-node 'tui-node)) t "Diff must be applied to `tui-node's.")
  (cond
   ((and new-node
         (not old-node))
    (list (list 'insert old-node)))
   ((and old-node
         (not new-node))
    (list (list 'remove old-node)))
   ;; CLEANUP: should this be the only condition/test?
   ;; TODO: check that keys aren't different?
   ((eq (tui--object-class old-node)
        (tui--object-class new-node))
    (cond
     ((tui-element-p old-node)
      (let ((old-props (tui--get-props old-node))
            (new-props (tui--get-props new-node)))
        ;; update component if any properties have changed
        (when (tui--plist-changes old-props new-props)
          (list (list 'update-props old-node new-props)))))
     ((tui-text-node-p old-node)
      (when (not (equal-including-properties (tui-node-content old-node)
                                             (tui-node-content new-node)))
        (list (list 'update-content old-node (tui-node-update-count old-node) (tui-node-content new-node)))))))
   (t
    (list (list 'replace old-node new-node)))))

(defun tui--diff-list (old-list new-list parent-element &optional index-offset)
  "Internal function for generating a diff between the content trees.  OLD-LIST is the existing content (child nodes) of ELEMENT and NEW-LIST is the potentially updated content.  INDEX-OFFSET tracks index position for recursive calculation of the DIFF."
  (unless index-offset (setq index-offset 0))
  (let ((old-keys (tui--keyed-elements old-list))
        (new-keys (tui--keyed-elements new-list))
        diff)
    (while (or old-list new-list)
      (-let* ((old-item (cl-first old-list))
              (old-class (tui--object-class old-item))
              (old-key (tui--get-key old-item))
              ((old-key-new-index . old-key-new-item) (gethash old-key new-keys))
              (new-item (cl-first new-list))
              (new-class (tui--object-class new-item))
              (new-key (tui--get-key new-item))
              ((new-key-old-index . new-key-old-item) (gethash new-key old-keys)))
        (cond
         ;; same-reconcile
         ((and (eq old-class new-class)
               (eq old-key new-key)) ;; FIXME: ensure the same test predicate as the hash table is used here
          (setq diff
                (append diff
                        (tui--diff old-item new-item)))
          (pop old-list)
          (pop new-list)
          (setq index-offset (+ 1 index-offset)))
         ;; remove
         ((and old-item
               (or (not new-item)
                   (if old-key
                       (or (not old-key-new-index) ;; old item has key that is *not* preserved
                           (not (eq old-class (tui--object-class old-key-new-item))))
                     (not (eq old-class new-class)))))
          (setq diff (append diff
                             (list (list 'remove old-item))))
          (pop old-list))
         ;; reorder (insert)
         ((and new-key ;; new item has a key that is not currently first
               new-key-old-item)
          (setq diff (append diff
                             (list (list 'insert new-key-old-item parent-element index-offset))))
          (setq old-list (cons new-key-old-item
                               (-remove (lambda (elt)
                                          (eq (tui--get-key elt) new-key)) ;; FIXME
                                        old-list))))
         ;; insert
         (new-item
          (setq diff (append diff
                             (list (list 'insert new-item parent-element index-offset))))
          (pop old-list)
          (pop new-list)
          (setq index-offset (+ 1 index-offset))))))
    diff))

(defun tui--get-key (element)
  "Return the :key of ELEMENT if defined and nil otherwise."
  (when (tui-element-p element)
    (plist-get (tui--get-props element) :key)))

(defun tui--keyed-elements (nodes)
  "Return a hash table of `tui-element's in NODES with :key property defined as key-element pairs."
  (let ((map (make-hash-table))) ;; TODO: support strings as keys (but not use equal?)
    (-map-indexed
     (lambda (index node)
       (-when-let* ((key (tui--get-key node)))
         (puthash key (cons index node) map)))
     nodes)
    map))

(defun tui--reconcile (old-node new-node)
  "Reconcile element trees of OLD-NODE and NEW-NODE.

Returns the outcome of that reconciliation process."
  (let ((diff (tui--diff old-node new-node)))
    (setf tui--update-queue
          (append diff
                  tui--update-queue))
    (unless tui--applying-updates
      (tui--process-update-queue))))

(defun tui--reconcile-content (old-content new-content parent-element)
  "Reconcile OLD-CONTENT and NEW-CONTENT within PARENT-ELEMENT."
  (cl-assert (tui--list-content-p old-content) t "Content tree is a list")
  (cl-assert (tui--list-content-p new-content) t "Content tree is a list")
  (let ((diff (tui--diff-list old-content new-content parent-element)))
    (setf tui--update-queue
          (append diff
                  tui--update-queue))
    (unless tui--applying-updates
      (tui--process-update-queue))))

;; (cl-defmethod tui--reorder-element ((element tui-element) from-index to-index)
;;   "Reorder items of ELEMENT- moving FROM-INDEX to TO-INDEX."
;;   (tui-insert-node (nth from-index (tui-child-nodes element)) element to-index))

(provide 'tui-reconciler)

;;; tui-reconciler.el ends here
