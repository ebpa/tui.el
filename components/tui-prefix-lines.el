;;; tui-prefix-lines.el --- Prefix content lines with a string

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(tui-define-component tui-prefix-lines
  :documentation
  "Prefix all lines rendered by :content child elements with :prefix."
  :prop-documentation
  (
   :prefix "String to display at the beginning of each line."
   )
  :render
  (lambda ()
    (tui-let (&props prefix children)
      (tui-div
       :text-props-safe `(line-prefix ,prefix
                                      wrap-prefix ,prefix)
       :children children))))

(provide 'tui-prefix-lines)

;;; tui-prefix-lines.el ends here
