;;; tui-ul.el --- HTML 'ul'-like component       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

;; TODO: additional capability with tui-li children?
(tui-define-component tui-ul
  :documentation "Unordered list component"
  :render
  (lambda ()
    (tui-let (&props children)
      (mapcar
       (lambda (child)
         (tui-line
          " • " child))
       children))))

(provide 'tui-ul)

;;; tui-ul.el ends here
