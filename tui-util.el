;;; tui-util.el --- Utility functions for tui       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(eval-when-compile (require 'cl-lib))
(require 'tui-node-types)

;;; Code:

(defun tui--plist-delete (plist &rest properties)
  "Delete PROPERTIES from PLIST.
This is in contrast to merely setting it to 0.

\(copied from `use-package-plist-delete')"
  (let ((property (car properties))
        (rest-properties (cdr properties))
        p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    (if rest-properties
        (apply #'tui--plist-delete p rest-properties)
      p)))

(defun tui--plist-merge (a b &rest rest)
  "Merge plists A, B, and REST into a new list.

Values for keys in successive list will override those preceding it.  Ex: If a property key is found in plists A and B, the returned list will contain the value from B."
  (let* ((merged (cl-copy-seq a)))
    (cl-loop for (key val) on b by 'cddr
             do (setq merged (plist-put merged key val)))
    (if rest
        (apply #'tui--plist-merge merged rest)
      merged)))

(defun tui--plist-equal (a b)
  "Helper to check wither plists A and B are equal."
  ;; CLEANUP: be consistent about ignoring degeneracies.  Currently only degeneracies in B are ignored.
  (let* ((b b))
    (cl-loop for (key-a value-a) on a by #'cddr
             always (equal
                     (plist-get b key-a)
                     value-a)
             do
             (setq b (tui--plist-delete b key-a))
             finally return (null b))))

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
                        (and
                         ;; TODO: confirm #'eq is adequate and use that instead of the test immediately below
                         ;; (eq (car a) (car b))
                         (equal (symbol-name (car a))
                                (symbol-name (car b)))
                         (tui-equal (cdr a)
                                    (cdr b)))))
         (difference (-difference new-list old-list)))
    ;;(display-warning 'tui (format "(differences: %S)" (mapcar #'car difference)) :debug tui-log-buffer-name)
    (cl-loop for (key . value) in difference
             append (list key value))))

(defun tui--text-prop-changes (old-props new-props)
  "Return :text-props* values that are different between old-props and new-props."
  (cl-loop for (key value) on (tui--plist-changes old-props new-props) by #'cddr
           if (member key '(:text-props
                            :text-props-push
                            :text-props-append
                            :text-props-replace
                            :text-props-safe))
           collect key))

(defun tui-put-text-property (start end key value &optional object replace-behavior)
  "Same as `tui-put-text-properties', but only set a single key-value pair."
  (tui-put-text-properties start end (list key value) object replace-behavior))

(defun tui-put-text-properties (start end properties &optional object replace-behavior)
  "Apply text properties to region between START and END.

Like `put-text-property, but PROPERTIES is a list of properties
and has controllable REPLACE-BEHAVIOR.  Unlike
`put-text-property` the default behavior is to not replace
existing property values.

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
  "Return t if NODE belongs to a valid content tree (call `tui-valid-element-p' on the root element)."
  (tui-valid-element-p (tui-root-node node)))

(defun tui--clean-plist (plist)
  "Remove degeneracies from plist."
  (let ((keys (make-hash-table :test #'equal))
        (miss (make-symbol "miss"))
        new-plist)
    (cl-loop for (key value) on plist by #'cddr
             do
             (when (eq (gethash key table miss) miss)
               (puthash key key keys)
               (push value new-plist)
               (push key new-plist)))
    new-plist))

(defun tui--target-row-offset (num-columns current-column-index steps-forward)
  "Helper function to calculate the row offset for moving STEPS-FORWARD on a grid consisting of NUM-COLUMNS assuming a current position of CURRENT-COLUMN-INDEX."
  (let ((target-index (+ steps-forward current-column-index)))
    (if (>= target-index 0)
        (/ target-index num-columns)
      (- -1 (/ (abs target-index) num-columns)))))

(defun tui--target-column-index (num-columns current-column-index steps-forward)
  "Helper function to calculate the target column index for moving STEPS-FORWARD on a grid consisting of NUM-COLUMNS assuming a current position of CURRENT-COLUMN-INDEX."
  (let ((target-index (+ steps-forward current-column-index)))
    (if (>= target-index 0)
        (% target-index num-columns)
      (+ num-columns (% target-index num-columns)))))

(defmacro tui-let (bindings &rest body)
  "Convenience form for binding state and prop values of BINDINGS for evaluation of BODY.

For use in any context where `tui-get-props' and `tui-get-state' are defined.
See: `tui-let*'."
  (declare (debug ((&rest symbolp)
                   body))
           (indent 1))
  `(tui-let* (,bindings tui-this-component)
     ,@body))

(defmacro tui-let* (bindings &rest body)
  "Convenience form for binding state and prop values from a component reference for the execution of BODY.

BINDINGS should be a list of the form (&props PROP-A PROP-B ... &state STATE-VAR-A ...)."
  (declare (debug ((&rest symbolp)
                   body))
           (indent 1))
  (-let* (((symbol-args this-ref) bindings)
          (this-sym (make-symbol "this"))
          (prop-sym (make-symbol "prop"))
          (state-sym (make-symbol "state"))
          prop-vars state-vars)
    (while (member (car symbol-args) '(&props &state))
      (let* ((var-count (or (-find-index (lambda (item)
                                           (member item '(&props &state)))
                                         (cl-rest symbol-args))
                            (length (cl-rest symbol-args)))))
        (pcase (pop symbol-args)
          ('&props
           (setq prop-vars (append prop-vars
                                   (-take var-count symbol-args))))
          ('&state
           (setq state-vars (append state-vars
                                    (-take var-count symbol-args)))))
        (setq symbol-args (nthcdr var-count symbol-args))))
    `(let* ,(append `((,this-sym ,this-ref))
                    (when prop-vars
                      `((,prop-sym (tui-get-props ,this-sym))))
                    (when state-vars
                      `((,state-sym (tui-get-state ,this-sym))))
                    (mapcar (lambda (var)
                              `(,var (plist-get ,prop-sym ,(intern (concat ":" (symbol-name var))))))
                            prop-vars)
                    (mapcar (lambda (var)
                              `(,var (plist-get ,state-sym ,(intern (concat ":" (symbol-name var))))))
                            state-vars))
       ,@body)))

(defun tui-viewport-height ()
  "Return the height of the containing viewport (in rows)."
  (window-text-height))

(defun tui-viewport-width ()
  "Return the width of the containing viewport (in columns)."
  (window-text-width))

(defun tui--check-key-value-documentation (documentation)
  "Internal function to check the form of DOCUMENTATION."
  (cl-loop for (prop-keyword docstring) on documentation by #'cddr
           if (not (keywordp prop-keyword))
           do (warn "Malformed documentation list")))

(defun tui--cl-generic-remove-method (name qualifiers specializers)
  "cl-generic appears to lack an equivalent for common lisp's remove-method, so this should be sufficient for now to help clean up component definitions."
  (let* ((generic (cl-generic-ensure-function name))
         (mt (cl--generic-method-table generic))
         (me (cl--generic-member-method specializers qualifiers mt)))
    (unless (null me)
      (setf (cl--generic-method-table generic)
            (-filter (lambda (x) (not (eq x (car me)))) mt)))))

(cl-defmethod tui-run-with-timer ((component tui-component) secs repeat cancel-on-error function &rest args)
  "Wrapper to `run-with-timer' that automatically cancels a timer when the associated component is unmounted.

When optional argument CANCEL-ON-ERROR is truthy cancel the timer if FUNCTION throws an error."
  (lexical-let* ((timer (list nil)))
    (setq timer
          (apply #'run-with-timer secs repeat
                 (lambda (&rest args)
                   (if (not (tui-mounted-p component))
                       (cancel-timer timer)
                     (if cancel-on-error
                         (apply function args)
                       (condition-case-unless-debug err
                           (apply function args)
                         (error
                          (message "%s" (error-message-string err))
                          (cancel-timer timer))))))
                 args))))

(defun tui--new-id ()
  "Generate a TUI ID."
  (abs (random)))

(cl-defmacro tui--component-defstruct (name)
  "Internal helper macro for defining component structs."
  `(progn
     (cl-defstruct (,name (:include tui-component)
                          (:constructor nil)
                          (:constructor ,(intern (format "%s-create" (symbol-name name)))
                                        (&key props invisible
                                              &aux (id (tui--new-id))))))
     ;; FIXME: this is a rather hacky way of suppressing function creation for component slots
     (mapc (-lambda ((slot ignore))
             (unless (eq slot 'cl-tag-slot)
               (setf (symbol-function (intern (concat (symbol-name ',name) "-" (symbol-name slot)))) nil)
               (setf (symbol-function (intern (concat (symbol-name ',name) "-" (symbol-name slot) "--cmacro"))) nil)))
           (cl-struct-slot-info 'tui-component))
     ',name))

(defun tui-unintern (type)
  "Remove all definitions for component TYPE.

Return t if a component definition exists and was successfully removed and return nil otherwise."
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

(defun tui-all-component-types ()
  "Return a list of symbols for all tui components that have been defined."
  (let* (types)
    (do-symbols (symbol)
      (when (and (symbol-function symbol)
                 (when (cl--find-class symbol)
                   (member 'tui-component
                           (mapcar
                            #'cl--struct-class-name
                            (cl--struct-all-parents (cl--struct-get-class symbol))))))
        (push symbol types)))
    types))

(defun tui-all-builtin-component-types ()
  "Return a list of all ``built-in'' component types (``tui-'')."
  (--filter
   (s-matches-p "^tui" (symbol-name it))
   (tui-all-component-types)))

;; (tui-read-component-type String -> Symbol)
(cl-defun tui-read-component-type (&optional (prompt "Component type: "))
  "Return a component type.

Optionally override PROMPT string."
  (intern (completing-read prompt (tui-all-component-types))))

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

(defun tui--plist-keys (plist)
  "Return the keys of PLIST."
  (cl-loop for (key value) on plist by #'cddr
           collect key))

(defun tui-describe-element (element)
  "Show information about ELEMENT."
  (interactive (list (tui-read-element-at-point)))
  (list
   (tui-heading (symbol-name element))
   (tui-line "instances: " "[not indexed]")))

(defun tui-element-summary-show (element)
  "Render RENDER-FN-SYMBOL to a dedicated buffer and return that buffer."
  (interactive (list (tui-read-element-at-point)))
  (lexical-let* ((buffer-name (format "*tui-element-summary: %s*" (cl-prin1-to-string element))))
    (tui-render-with-buffer buffer-name
      (tui-element-summary :element element))))

(defun tui-node-label (node)
  "Return a terse (human) label string for NODE."
  (format "%s (%s)" (tui--type node) (tui-node-id node)))
;; TODO: eliminate?
(defalias 'tui-element-label 'tui-node-label)

(cl-defun tui-read-element-at-point (&optional (prompt "Element: "))
  "Return a user-selected element at point.

Optionally override PROMPT string."
  (let* ((elements (tui-ancestor-elements-at (point)))
         (options (--map
                   (cons (tui-element-label it) it)
                   elements)))
    (assoc-default (completing-read prompt options)
                   options)))

(defun tui--abbreviate-string (length string)
  ""
  (if (<= (length string) length)
      string
    (propertize
     (s-truncate length string "…")
     'help-echo string)))

(defmacro tui-with-content-reported-error (body)
  "Helper for trapping and reporting errors inline as an element."
  `(condition-case err
       ,body
     (t (tui-span
         :text-props-push `(help-echo ,(prin1-to-string err))
         "(error)"))))

(defun tui-util--shr-render-html-to-string (html-string)
  "Render HTML-STRING to a string."
  (let* ((html-dom (with-temp-buffer
                     (insert html-string)
                     (libxml-parse-html-region (point-min) (point-max)))))
    (with-temp-buffer
      (shr-insert-document html-dom)
      (buffer-string))))

(defun tui--easy-going-apply (fn &rest arguments)
  "Make an ``easy-going'' funcall to FN with args- tolerating FN definitions with arity of four fewer than the length of arguments.  The last value of ARGUMENTS is treated as a list of args (the same way as `apply')."
  (-let* ((args (apply #'apply #'list arguments))
          (max-arity (cdr (func-arity fn))))
    (apply fn (if (eq max-arity 'many)
                  args
                (seq-take args max-arity)))))

;; (tui--easy-going-apply (lambda (x y z) (list x y z)) 1 2 '(3 4 5))

(provide 'tui-util)
;;; tui-util.el ends here
