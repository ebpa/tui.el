;;; tui-line.el --- Adds a newline after its content

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(tui-define-component tui-line
  :documentation "Render :children elements with a following \\n."
  :render
  (lambda ()
    (let ((content (plist-get (tui-get-props) :children)))
      (list content
            "\n"))))

(provide 'tui-line)

;;; tui-line.el ends here
