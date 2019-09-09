;;; tui-heading.el --- HTML 'h1'-like component       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(tui-define-component tui-heading
  ;; TODO: support heading levels? (h1, h2, etc.) - as subcomponents? 
  :documentation "HTML 'h1'-like component"
  :render
  (lambda ()
    (tui-let (&props children)
      (tui-line
       (tui-span
        :text-props '(face org-level-1)
        :children
        children)))))

(provide 'tui-heading)
;;; tui-heading.el ends here
