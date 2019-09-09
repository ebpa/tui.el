;;; tui-div.el --- HTML 'div'-like component       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'tui-defun)

;;; Code:

(tui-defun tui-div (children)
  "Component for grouping elements.  Render with a newline before and after the child content."
  (list
   "\n"
   children
   "\n"))

(provide 'tui-div)
;;; tui-div.el ends here
