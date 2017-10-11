;;; tui-section.el --- HTML 'section'-like element

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(tui-define-component tui-section
  :documentation "HTML 'section'-like element.  Includes a newline after the content."
  :render
  (lambda ()
    (list
     (plist-get (tui-get-props) :children)
     "\n")))

(provide 'tui-section)

;;; tui-section.el ends here
