;;; tui-span.el --- HTML 'span'-like compnent       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(tui-define-component tui-span
  :documentation "HTML 'span'-like compnent for grouping content."
  :render
  (lambda ()
    (plist-get (tui-get-props) :children)))

(provide 'tui-span)

;;; tui-span.el ends here
