;; -*- lexical-binding: t -*-

(require 'tui)
(require 'cl-lib)

(cl-defstruct (tui-hooks--state-reference (:constructor tui-hooks--state-reference-create)
                                          (:copier nil))
  current)

(cl-defstruct (tui-hooks--dependencies-reference (:constructor tui-hooks--dependencies-reference-create)
                                                 (:copier nil)
                                                 (:include tui-hooks--state-reference))
  dependencies)

(cl-defstruct (tui-hooks--effect-reference (:constructor tui-hooks--effect-reference-create)
                                           (:copier nil)
                                           (:include tui-hooks--dependencies-reference)))

(cl-defstruct (tui-hooks--state (:constructor tui-hooks--state-create)
                                (:copier nil))
  component
  reference-index
  references)

;; in the future we could change this to be a macro that infers dependencies
(defun tui-use-effect (component effect dependencies)
  "Executes the effect, deferring clean up until dependencies change or the component is unmounted"
  (let* ((hook-state (tui-hooks-advance component))
         (prev-state (tui-hooks-get hook-state))
         (invoke-and-update
          (lambda ()
            (tui-hooks-set
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
  (let* ((hook-state (tui-hooks-advance component))
         (curr-state (or
                      (tui-hooks-get hook-state)
                      state)))
    (list curr-state
          (lambda (next-state-or-updater)
            (let ((next-state
                   (if (functionp next-state-or-updater)
                       (funcall next-state-or-updater curr-state)
                     next-state-or-updater)))
              (tui-hooks-set hook-state next-state))))))

(defun tui-use-ref (component ref-or-ref-producer)
  (let* ((hook-state (tui-hooks-advance component))
         (curr-state (tui-hooks-get hook-state)))
    (or curr-state
        (let ((ref (tui-hooks--ref-or-ref-producer ref-or-ref-producer)))
          (tui-hooks-set curr-state ref)
          ref))))

(defun tui-use-memo (component ref-or-ref-producer))

(defun tui-use-callback ())

(defun tui-hooks--ref-or-ref-producer (ref-or-ref-producer)
  (if (and (functionp ref-or-ref-producer)
           (eq 0 (cdr (func-arity ref-or-ref-producer))))
      (funcall ref-or-ref-producer)
    ref-or-ref-producer))

(cl-defgeneric tui-hooks-advance (component)
  "Statefully return the current tui-hooks--state for the component.
Hooks _must_ call this _exactly_ once. This is a stable cursor that can be
used to get and set references at any point during the lifetime of the component")

(cl-defgeneric tui-hooks-get (state)
  "Return the current reference")

(cl-defgeneric tui-hooks-set (state next-reference &optional no-update)
  "Set the next reference. Can be called throughout the lifetime of the component")

(cl-defmethod tui-hooks-advance ((component tui-component))
  (let* ((hook-state (or (plist-get (tui--get-state component) :tui-hooks--state)
                         (tui-hooks--state-create
                          :component component
                          :reference-index 0
                          :references '())))
         (next-hook-state (tui-hooks--state-create
                           :component (tui-hooks--state-component hook-state)
                           :reference-index (1+ (tui-hooks--state-reference-index hook-state))
                           :references (tui-hooks--state-references hook-state))))
    (tui--set-state component
                    (list :tui-hooks--state next-hook-state) t)
    hook-state))

(cl-defmethod tui-hooks-get ((state tui-hooks--state))
  (let* ((curr-idx (tui-hooks--state-reference-index state))
         (references (tui-hooks--state-references state))
         ;; we assign these in reverse order
         ;; 0 is last
         ;; 1 is next-to-last
         (idx (- (length references) 1 curr-idx)))
    (when (wholenump idx)
      (nth idx references))))

(cl-defmethod tui-hooks-set ((state tui-hooks--state) reference &optional no-update)
  (let* ((idx (tui-hooks--state-reference-index state))
         (component (tui-hooks--state-component state))
         (update-state
          (lambda ()
            (tui--set-state
             component
             (lambda (prev-component-state)
               (let* ((prev-hook-state (plist-get prev-component-state :tui-hooks--state))
                      (prev-references (tui-hooks--state-references prev-hook-state))
                      (updated-references
                       (tui-hooks--replace-and-pad-if-needed idx reference prev-references))
                      (updated-state
                       (tui-hooks--state-create
                        :component component
                        :reference-index (tui-hooks--state-reference-index prev-hook-state)
                        :references updated-references)))
                 (list :tui-hooks--state updated-state)))
             no-update))))
    (if no-update
        (funcall update-state)
      ;; we wrap the set state call in a run-at-time to post it
      ;; so that the reconciler has the opportunity to diff
      (run-at-time 0 nil update-state))))

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

;; restart hook state before render
(cl-defmethod tui-render :before ((component tui-component))
  (tui--set-state component
                  (lambda (prev-component-state)
                    (let ((prev-hook-state (plist-get prev-component-state
                                                      :tui-hooks--state)))
                      (list :tui-hooks--state
                            (tui-hooks--state-create
                             :component component
                             :reference-index 0
                             :references (and
                                          prev-hook-state
                                          (tui-hooks--state-references prev-hook-state))))))
                  t))


;; teardown for effects
(cl-defmethod tui-component-will-unmount :after ((component tui-component))
  (let* ((hook-state (plist-get (tui-get-state component)
                                 :tui-hooks--state))
         (hook-state-references
          (and hook-state
               (tui-hooks--state-references hook-state))))
    
    (cl-loop for ref in hook-state-references
             do (if (tui-hooks--effect-reference-p ref)
                    (let ((maybe-teardown-func (tui-hooks--effect-reference-current ref)))
                      (if (functionp maybe-teardown-func)
                          (funcall maybe-teardown-func)))))))

(provide 'tui-hooks)
