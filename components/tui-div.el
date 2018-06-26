;;; tui-div.el --- HTML 'div'-like component       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(tui-define-component tui-div
  :documentation "Component for grouping elements"
  :render
  (lambda ()
    (plist-get (tui-get-props) :children)))

(provide 'tui-div)

;;; tui-div.el ends here
