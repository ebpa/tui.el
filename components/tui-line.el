;;; tui-line.el --- Adds a newline after its content       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(tui-define-component tui-line
  :documentation "Render with a newline after the child content."
  :render
  (lambda ()
    (tui-let (&props children)
      (list children
            "\n"))))

(provide 'tui-line)

;;; tui-line.el ends here
