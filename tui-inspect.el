;;; tui-inspect.el --- Tools for inspecting tui content       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'dash)
(require 'tui-components)

(defun tui-describe-element (element)
  "Show information about ELEMENT."
  (interactive (list (tui-read-element-at-point)))
  (list
   (tui-heading (tui-node-label element))
   (tui-line "instances: " "[not indexed]")))

(defun tui-element-summary-show (element)
  "Render RENDER-FN-SYMBOL to a dedicated buffer and return that buffer."
  (interactive (list (tui-read-element-at-point)))
  (-let* ((buffer-name (format "*tui-element-summary: %s*" (cl-prin1-to-string element))))
    (tui-render-with-buffer buffer-name
      (tui-element-summary :element element))))

(provide 'tui-inspect)
;;; tui-inspect.el ends here
