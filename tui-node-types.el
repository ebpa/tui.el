;;; tui-node-types.el --- Core Tui Node Types

;;; Commentary:
;; 

;;; Code:

;;;; Node

(cl-defstruct tui-node
  ""
  start ;; "Start of segment"
  end ;; "End of segment"
  content
  mounted
  relative-index ;; "Index position of node within parent (cached value)"
  marker-list ;; "Reference to the containing content tree's ordered marker list."
  id)

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
  invisible) ;; "Indicates whether the content element should be ignored when rendering."

;;;; Component

(cl-defstruct (tui-component (:include tui-element))
  ;; "The base class for all components."
  state)

(provide 'tui-node-types)

;;; tui-node-types.el ends here
