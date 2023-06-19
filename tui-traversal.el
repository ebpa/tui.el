(eval-when-compile
  (require 'cl-lib))
(require 'dash)

(defun tui-map-subtree (fn node)
  "Apply FN to all elements in the subtree of NODE."
  (let* ((nodes (list node)))
    (while nodes
      (let* ((node (pop nodes)))
        (cond
         ((tui-element-p node)
          (funcall fn node)
          (push (tui-child-nodes node) nodes))
         ;; CLEANUP: better handling of list nodes
         ((listp node)
          (setq nodes (append node nodes))))))))

(cl-defun tui-first-subtree-node (predicate node)
  "Return the first node in the subtree of NODE that satisfies PREDICATE.

See also: `tui-last-subtree-node'."
  (let* ((nodes (list node))
         current-node)
    (while (and (setq current-node (pop nodes))
                (not (funcall predicate current-node)))
      (when (tui-element-p current-node)
        (setq nodes
              (append (tui-child-nodes current-node)
                      nodes))))
    current-node))

(cl-defun tui-last-subtree-node (predicate node)
  "Return the last node in the subtree of NODE that satisfies PREDICATE.

See also: `tui-first-subtree-node'."
  (or (-some (lambda (child)
               (tui-last-subtree-node predicate child))
             (reverse (tui-child-nodes node)))
      (when (funcall predicate node)
        node)))

(provide 'tui-traversal)
