;; -*- lexical-binding: t -*-

(require 'tui)
(require 'cl-lib)

(cl-defstruct (tui-hooks--dependencies-reference (:constructor tui-hooks--dependencies-reference-create)
                                                 (:copier nil))
  current
  dependencies)

(cl-defstruct (tui-hooks--effect-reference (:constructor tui-hooks--effect-reference-create)
                                           (:copier nil)
                                           (:include tui-hooks--dependencies-reference)))

(cl-defstruct (tui-hooks--cursor (:constructor tui-hooks--cursor-create)
                                 (:copier nil))
  component
  index)

;; (tui-hooks--replace-and-pad-if-needed 0 "my-ref" '())
;; (tui-hooks--replace-and-pad-if-needed 1 "my-ref" '())
;; (tui-hooks--replace-and-pad-if-needed 0 "my-ref" '("old-ref"))
;; (tui-hooks--replace-and-pad-if-needed 1 "my-ref" '("front-ref" "to-be-replaced" "old-ref"))
(defun tui-hooks--replace-and-pad-if-needed (idx ref ls)
  (declare (pure t))
  "Replace ref in list for position idx, padding if needed"
  (let ((idx-relative-to-head (- idx (length ls))))
    (if (wholenump idx-relative-to-head)
        (append (list ref)
                (make-list idx-relative-to-head nil)
                ls)
      (append
       (cl-subseq ls 0 (- -1 idx-relative-to-head))
       (list ref)
       (cl-subseq ls (- idx-relative-to-head))))))

(defun tui-hooks--ref-or-ref-producer (ref-or-ref-producer)
  (if (and (functionp ref-or-ref-producer)
           (eq 0 (cdr (func-arity ref-or-ref-producer))))
      (funcall ref-or-ref-producer)
    ref-or-ref-producer))

;; restart hook state before render
(cl-defmethod tui-render :before ((component tui-component))
  (tui--set-state component
                  (list :tui-hooks--cursor (tui-hooks--cursor-create
                                            :component component
                                            :index 0))
                  t))

;; teardown for effects
(cl-defmethod tui-component-will-unmount :after ((component tui-component))
  (let* ((references (plist-get (tui-get-state component)
                                :tui-hooks--references)))
    (cl-loop for ref in references
             when (tui-hooks--effect-reference-p ref)
             do (let ((maybe-teardown-func (tui-hooks--effect-reference-current ref)))
                  (if (functionp maybe-teardown-func)
                      (funcall maybe-teardown-func))))))

(defun tui-hooks--cursor-advance (component)
  "Statefully return the current tui-hooks--state for the component.
Useful hooks _must_ call this a _consistent_ number of times. This is a stable cursor that can be
used to get and set references at any point during the lifetime of the component"
  (let* ((cursor (or (plist-get (tui-get-state component) :tui-hooks--cursor)
                     (tui-hooks--cursor-create
                      :component component
                      :index 0)))
         (next-cursor (tui-hooks--cursor-create
                       :component component
                       :index (1+ (tui-hooks--cursor-index cursor)))))
    (tui--set-state component
                    (list :tui-hooks--cursor next-cursor) t)
    cursor))

(defun tui-hooks--cursor-get (cursor)
  "Return the current reference"
  (let* ((cursor-idx (tui-hooks--cursor-index cursor))
         (component (tui-hooks--cursor-component cursor))
         (references (plist-get (tui-get-state component) :tui-hooks--references))
         ;; we assign these in reverse order
         ;; 0 is first added, so last in list
         ;; 1 is next-to-last
         (idx (- (length references) 1 cursor-idx)))
    (when (wholenump idx)
      (nth idx references))))

(defun tui-hooks--cursor-set (cursor reference &optional no-update)
  "Set the reference at cursor position. Can be called throughout the lifetime of the component"
  (let* ((cursor-idx (tui-hooks--cursor-index cursor))
         (component (tui-hooks--cursor-component cursor))
         (update-state
          (lambda ()
            (tui--set-state
             component
             (lambda (prev-component-state)
               (let* ((prev-references (plist-get prev-component-state :tui-hooks--references))
                      (updated-references
                       (tui-hooks--replace-and-pad-if-needed cursor-idx reference prev-references)))
                 (list :tui-hooks--references updated-references)))
             no-update))))
    (if no-update
        (funcall update-state)
      ;; we wrap the set state call in a run-at-time to post it
      ;; so that the reconciler has the opportunity to diff
      (run-at-time 0 nil update-state))))

;; public API below

(defun tui-use-effect (component dependencies effect)
  "Executes the effect, deferring clean up until dependencies change or the component is unmounted"
  (let* ((hook-state (tui-hooks--cursor-advance component))
         (prev-state (tui-hooks--cursor-get hook-state))
         (invoke-and-update
          (lambda ()
            (tui-hooks--cursor-set
             hook-state
             (tui-hooks--effect-reference-create
              :current (funcall effect)
              :dependencies dependencies)
             t))))
    (if (not prev-state)
        (funcall invoke-and-update)
      (cl-assert (cl-typep prev-state 'tui-hooks--effect-reference))
      (let ((prev-dependencies
             (tui-hooks--effect-reference-dependencies prev-state)))
        (unless (equal dependencies prev-dependencies)
          (let ((prev-effect-teardown
                 (tui-hooks--effect-reference-current prev-state)))
            (if (functionp prev-effect-teardown)
                (funcall prev-effect-teardown)))
          (funcall invoke-and-update))))))

(defun tui-use-state (component state)
  (let* ((cursor (tui-hooks--cursor-advance component))
         (curr-state (or
                      (tui-hooks--cursor-get cursor)
                      (progn
                        (tui-hooks--cursor-set cursor state t)
                        state)))
         (state-updater (tui-use-callback
                         component
                         cursor
                         (lambda (next-state-or-updater)
                           (let ((next-state
                                  (if (functionp next-state-or-updater)
                                      (funcall next-state-or-updater (tui-hooks--cursor-get cursor))
                                    next-state-or-updater)))
                             (tui-hooks--cursor-set cursor next-state))))))
    (list curr-state state-updater)))

(defun tui-use-ref (component ref-or-ref-producer)
  (let* ((hook-state (tui-hooks--cursor-advance component))
         (curr-state (tui-hooks--cursor-get hook-state)))
    (or curr-state
        (let ((ref (tui-hooks--ref-or-ref-producer ref-or-ref-producer)))
          (tui-hooks--cursor-set curr-state ref)
          ref))))

(defun tui-use-memo (component dependencies ref-or-ref-producer)
  (let* ((hook-state (tui-hooks--cursor-advance component))
         (curr-reference (tui-hooks--cursor-get hook-state)))
    (if (or (not curr-reference)
            (not (equal (tui-hooks--dependencies-reference-dependencies curr-reference)
                        dependencies)))
        (let ((next-reference (tui-hooks--ref-or-ref-producer ref-or-ref-producer)))
          ;; don't update: no need for a re-render we're returning the new value synchronously
          (tui-hooks--cursor-set hook-state
                         (tui-hooks--dependencies-reference-create
                          :current next-reference
                          :dependencies dependencies)
                         t)
          next-reference)
      (tui-hooks--dependencies-reference-current curr-reference))))

(defun tui-use-callback (component dependencies callback)
  (tui-use-memo component dependencies (lambda () callback)))

(provide 'tui-hooks)
