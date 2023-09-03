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
                  (list :tui-hooks--cursor 0)
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

(defun tui-hooks--advance (component)
  "Statefully return the current tui-hooks--state for the component.
Hooks _must_ call this _exactly_ once. This is a stable cursor that can be
used to get and set references at any point during the lifetime of the component"
  (let* ((cursor (or (plist-get (tui-get-state component) :tui-hooks--cursor)
                     (tui-hooks--cursor-create
                      :component component
                      :index 0)))
         (next-cursor (tui-hooks--cursor-create
                       :component component
                       :index (tui-hooks-cursor-index cursor))))
    (tui--set-state component
                    (list :tui-hooks--cursor next-cursor) t)
    cursor))

(defun tui-hooks--get (cursor)
  "Return the current reference"
  (let* ((cursor-idx (tui-hooks--cursor-index cursor))
         (references (plist-get (tui-get-state component) :tui-hooks--references))
         ;; we assign these in reverse order
         ;; 0 is first added, so last in list
         ;; 1 is next-to-last
         (idx (- (length references) 1 cursor-idx)))
    (when (wholenump idx)
      (nth idx references))))

(defun tui-hooks--set (cursor reference &optional no-update)
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
                       (tui-hooks--replace-and-pad-if-needed idx reference prev-references)))
                 (list :tui-hooks--references updated-references)))
             no-update))))
    (if no-update
        (funcall update-state)
      ;; we wrap the set state call in a run-at-time to post it
      ;; so that the reconciler has the opportunity to diff
      (run-at-time 0 nil update-state))))


;; public API below

;; in the future we could change this to be a macro that infers dependencies
(defun tui-use-effect (component dependencies effect)
  "Executes the effect, deferring clean up until dependencies change or the component is unmounted"
  (let* ((hook-state (tui-hooks--advance component))
         (prev-state (tui-hooks--get hook-state))
         (invoke-and-update
          (lambda ()
            (tui-hooks--set
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
  (let* ((hook-state (tui-hooks--advance component))
         (curr-state (or
                      (tui-hooks--get hook-state)
                      (progn
                        (tui-hooks--set hook-state state t)
                        state)))
         (state-updater (tui-use-callback
                         component
                         hook-state
                         (lambda (next-state-or-updater)
                           (let ((next-state
                                  (if (functionp next-state-or-updater)
                                      (funcall next-state-or-updater (tui-hooks--get hook-state))
                                    next-state-or-updater)))
                             (tui-hooks--set hook-state next-state))))))
    (list curr-state state-updater)))

(defun tui-use-ref (component ref-or-ref-producer)
  (let* ((hook-state (tui-hooks--advance component))
         (curr-state (tui-hooks--get hook-state)))
    (or curr-state
        (let ((ref (tui-hooks--ref-or-ref-producer ref-or-ref-producer)))
          (tui-hooks--set curr-state ref)
          ref))))

(defun tui-use-memo (component dependencies ref-or-ref-producer)
  (let* ((hook-state (tui-hooks--advance component))
         (curr-reference (tui-hooks--get hook-state)))
    (if (or (not curr-reference)
            (not (equal (tui-hooks--dependencies-reference-dependencies curr-reference)
                        dependencies)))
        (let ((next-reference (tui-hooks--ref-or-ref-producer ref-or-ref-producer)))
          ;; don't update: no need for a re-render we're returning the new value synchronously
          (tui-hooks--set hook-state
                         (tui-hooks--dependencies-reference-create
                          :current next-reference
                          :dependencies dependencies)
                         t)
          next-reference)
      curr-reference)))

(defun tui-use-callback (component dependencies callback)
  (tui-use-memo component dependencies (lambda () callback)))

(provide 'tui-hooks)
