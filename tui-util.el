;;; tui-util.el --- Utility functions for tui

;;; Commentary:
;; 

(eval-when-compile (require 'cl-lib))

;;; Code:

(defun tui--plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0.

\(copied from `use-package-plist-delete')"
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun tui--plist-merge (a b)
  "Merge plists A and B."
  (let* ((merged (cl-copy-seq a)))
    (cl-loop for (key val) on b by 'cddr
             do (setq merged (plist-put merged key val)))
    merged))

(defun tui-map-elements (fn element)
  ;; CLEANUP; rename to tui-map-subtree?
  "Apply FN to all elements in the subtree of ELEMENT."
  (funcall fn element)
  (when (tui-element-p element)
    (mapc (lambda (item)
            (tui-map-elements fn item))
          (tui-node-content element))))

(defun tui--type (node)
  "Return the NODE's type as a symbol."
  (when (tui-node-p node)
    (aref node 0)))

(defun tui--symbol< (a b)
  "Compare the symbol names A and B."
  (string< (symbol-name a)
           (symbol-name b)))

(defun tui--plist-to-sorted-alist (plist)
  "Convert PLIST to a sorted ALIST."
  (sort (cl-loop for (prop value) on plist by #'cddr
                 collect (cons prop value))
        (lambda (a b)
          (tui--symbol< (car a)
                     (car b)))))

(defun tui--plist-changes (old-plist new-plist)
  "Return a plist of differences between plists OLD-PLIST and NEW-PLIST."
  (let* ((old-list (tui--plist-to-sorted-alist old-plist))
         (new-list (tui--plist-to-sorted-alist new-plist))
         (-compare-fn (lambda (a b)
                        (and (equal (symbol-name (car a))
                                    (symbol-name (car b)))
                             (tui-equal (cdr a)
                                     (cdr b)))))
         (difference (-difference new-list old-list)))
    (cl-loop for (key . value) in difference
          append (list key value))))

(defun tui-put-text-property (start end key value &optional object replace-behavior)
  "Same as `tui-put-text-properties', but only set a single key-value pair."
  (tui-put-text-properties start end (list key value) object replace-behavior))

;; TODO: use (if (eq key 'face) (add-face-text-property start end value)
(defun tui-put-text-properties (start end properties &optional object replace-behavior)
  "Apply text properties to region between START and END.

Like `put-text-property, but accepts a list of PROPERTIES and has
controllable REPLACE-BEHAVIOR.  Unlike `put-text-property` the
default behavior is to not replace existing property values.

When REPLACE-BEHAVIOR is t existing values for properties are
replaced with the new value from PROPERTIES.  When
REPLACE-BEHAVIOR is nil, existing values are not replaced; the
value from PROPERTIES is not applied.  When REPLACE-BEHAVIOR is
'push, a new value is added to the front of a list of existing
values.  When REPLACE-BEHAVIOR is 'append, a new value is added
in the manner of 'push, but at the end of the list.

If the optional OBJECT is a buffer, START and END are buffer
positions.  If OBJECT is a string, START and END are 0-based
indices into it.  When OBJECT is nil, properties are applied to
the current buffer."
  (when (and (not object)
             (markerp start))
    (setq object (marker-buffer start)))
  (let (prop-end prop-start)
    ;; (message "-safe-propertize\n")
    (cl-loop for (key value) on properties by #'cddr
             do
             (setq prop-start nil
                   prop-end nil)
             (if (eq replace-behavior t)
                 (progn
                   ;;(message "putting property %S at %S-%S" key start end)
                   (put-text-property start end key value object))
               (while (and (< (setq prop-start (if prop-start
                                                   (next-single-property-change prop-end key object end)
                                                 start))
                              end)
                           (setq prop-end (next-single-property-change prop-start key object end)))
                 (let ((existing-value (get-text-property prop-start key object)))
                   (when (or replace-behavior
                             (not existing-value))
                     ;;(message "putting property %S at %S-%S" key prop-start prop-end)
                     (put-text-property
                      prop-start
                      prop-end
                      key
                      (pcase replace-behavior
                        ('push
                         (append (list value)
                                 (if (listp existing-value)
                                     existing-value
                                   (list existing-value))))
                        ('append
                         (append (if (listp existing-value)
                                     existing-value
                                   (list existing-value))
                                 (list value)))
                        (_
                         value))
                      object))))))))

(defun tui-valid-element-p (element &optional invisible-context)
  "Return t if ELEMENT is a valid `tui-element'.
Optional argument INVISIBLE-CONTEXT track whether the this node is within an invisible section of the content tree."
  (and (not (cl-assert (tui-element-p element) t "Element should be a tui-element."))
       (or (not (tui-node-mounted element))
           (tui--object-of-class-p element 'tui-buffer) ;; CLEANUP: is this exclusion necessary?
           (-let* (((start . end) (tui-segment element))
                   (children (tui-child-nodes element))
                   (-compare-fn #'eq))
             (and (not (cl-assert (or (not start)
                                      (and (markerp start)
                                           (marker-buffer end)
                                           (marker-position end))) t "When set, start marker should be a marker object that points somewhere."))
                  (not (cl-assert (or (not end)
                                      (and (markerp end)
                                           (marker-buffer start)
                                           (marker-position start))) t "When set, end marker should be a marker object that points somewhere."))
                  (not (cl-assert (listp children) t "Children should be represented by a list"))
                  ;; all children are adjacent with consolidated markers
                  (or invisible-context
                      (tui-invisible-p element)
                      (-all-p
                       (lambda (child)
                         (not (cl-assert (and (>= (tui-start child) start)
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

(provide 'tui-util)

;;; tui-util.el ends here
