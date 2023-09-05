;; -*- lexical-binding: t -*-

(require 'tui-hooks)

(defmacro tui-use-inferred-effect (component &rest effect)
  (declare (indent defun))
  (let ((dependencies
         `(list ,@(cl-loop for sym in (flatten-tree effect)
                           collect `(ignore-errors ,sym)))))
     `(tui-use-effect ,component ,dependencies (lambda () ,@effect))))

(defmacro tui-use-inferred-memo (component &rest compute)
  (declare (indent defun))
  (let ((dependencies
         `(list ,@(cl-loop for sym in (flatten-tree compute)
                           collect `(ignore-errors ,sym)))))
     `(tui-use-memo ,component ,dependencies (lambda () ,@compute))))

(defalias 'tui-use-inferred-callback 'tui-use-inferred-memo)
