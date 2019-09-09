;;; tui-prefix-lines.el --- Prefix content lines with a string

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(tui-defun-2 tui-prefix-lines (prefix children)
  "Prefix all lines rendered by child elements with PREFIX."
  (tui-span
   :text-props-merge `(line-prefix ,prefix
                                   wrap-prefix ,prefix)
   :children children))

(provide 'tui-prefix-lines)
;;; tui-prefix-lines.el ends here
