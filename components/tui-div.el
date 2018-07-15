;;; tui-div.el --- HTML 'div'-like component       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(tui-define-component tui-div
  :documentation "Component for grouping elements.  Render with a newline before and after the child content."
  :render
  (lambda ()
    (tui-let (&props children)
      (list
       "\n"
       children
       "\n"))))

(provide 'tui-div)

;;; tui-div.el ends here
