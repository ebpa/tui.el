;;; tui-ol.el --- HTML 'ol'-like component       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

;; TODO: additional capability with tui-li children?
(tui-define-component tui-ol
  :documentation "Ordered list component"
  :render
  (lambda ()
    (tui-let (&props children)
      (-map-indexed
       (lambda (index child)
         (tui-line
          (format "%d. " (+ 1 index))
          child))
       children))))

(provide 'tui-ol)

;;; tui-ol.el ends here
