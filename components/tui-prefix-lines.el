;;; tui-prefix-lines.el --- Prefix content lines with a string

;;; Commentary:
;; 

(require 'tui-defun)

;;; Code:

(tui-defun-2 tui-prefix-lines (prefix children)
  "Prefix all lines rendered by child elements with PREFIX."
  (tui-span
   :text-props `(line-prefix ,prefix
                                   wrap-prefix ,prefix)
   :children children))

(provide 'tui-prefix-lines)
;;; tui-prefix-lines.el ends here
