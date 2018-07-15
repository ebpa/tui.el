;;; tui-section.el --- HTML 'section'-like element       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(tui-define-component tui-section
  :documentation "HTML 'section'-like element.  Includes a newline after the content."
  :render
  (lambda ()
    (tui-let (&props children)
      (list
       children
       "\n"))))

(provide 'tui-section)

;;; tui-section.el ends here
