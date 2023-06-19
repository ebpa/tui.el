;;; tui-util.el --- Utility functions for tui       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(eval-when-compile
  (require 'cl-lib))
(require 'dash)
(require 's)
(require 'tui-core)
(require 'tui-dom)
(require 'tui-node-types)

;;; Code:

(defun tui-valid-element-p (element &optional invisible-context)
  "Return t if ELEMENT is a valid `tui-element'.

Optional argument INVISIBLE-CONTEXT track whether the this node
is within an invisible section of the content tree."
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
  "Return t if NODE belongs to a valid content tree."
  (tui-valid-element-p (tui-root-node node)))

(defun tui--target-row-offset (num-columns current-column-index steps-forward)
  "Helper function to calculate the row offset for movement within a grid.

Calculate the row offset of moving STEPS-FORWARD on a grid
consisting of NUM-COLUMNS assuming a current position of
CURRENT-COLUMN-INDEX."
  (let ((target-index (+ steps-forward current-column-index)))
    (if (>= target-index 0)
        (/ target-index num-columns)
      (- -1 (/ (abs target-index) num-columns)))))

(defun tui--target-column-index (num-columns current-column-index steps-forward)
  "Helper function to calculate the target column index for movement within a grid.

Calculate the target column index for moving STEPS-FORWARD on a
grid consisting of NUM-COLUMNS assuming a current position of
CURRENT-COLUMN-INDEX."
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

(cl-defmethod tui-run-with-timer ((component tui-component) secs repeat cancel-on-error function &rest args)
  "`run-with-timer' for as long as COMPONENT is mounted.

The timer lifecycle is tied to the lifecycle of the component, so
the timer is canceled when the associated component is unmounted.

When optional argument CANCEL-ON-ERROR is truthy cancel the timer
if FUNCTION throws an error."
  (let* ((timer (list nil)))
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

(defun tui-component-symbol-p (sym)
  "Return t if SYM refers to a tui-component."
  (and (symbolp sym)
       (symbol-function sym)
       (when (cl--find-class sym)
         (member 'tui-component
                 (mapcar
                  #'cl--struct-class-name
                  (cl--struct-all-parents (cl--struct-get-class sym)))))))

(defun tui-all-component-types ()
  "Return a list of symbols for all tui components that have been defined."
  (let* (types)
    (cl-do-symbols (symbol)
      (when (tui-component-symbol-p symbol)
          (push symbol types)))
    types))

(defun tui-builtin-component-type-p (type)
  ""
  (when (symbolp type)
    (setq type (symbol-name type)))
  (s-matches-p "^tui" type))

(defun tui-all-builtin-component-types ()
  "Return a list of all ``built-in'' component types (``tui-'')."
  (-filter #'tui-builtin-component-type-p (tui-all-component-types)))

;; (tui-read-component-type String -> Symbol)
(cl-defun tui-read-component-type (&optional (prompt "Component type: "))
  "Return a component type.

Optionally override PROMPT string."
  (intern (completing-read prompt (tui-all-component-types))))

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
  "Abbreviate STRING to LENGTH with ellipsis ``…''."
  (declare (wip TODO "maintain the overall length?"))
  (if (<= (length string) length)
      string
    (propertize
     (s-truncate length string "…")
     'help-echo string)))

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
