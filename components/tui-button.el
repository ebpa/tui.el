;;; tui-button.el --- Basic button       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(tui-define-component tui-button
  ;; TODO: or mouse-2/multiple?
  :documentation "A basic button control"
  :prop-documentation
  (:action "Function to bind to [mouse-1].")
  :render
  (lambda ()
    (let* ((props (tui-get-props))
           (children (plist-get props :children))
           (action (plist-get props :action))
           (map (make-sparse-keymap)))
      (define-key map [mouse-1] action)
      (tui-span
       :text-props-replace `(mouse-face highlight
                                        keymap ,map)
       children))))

(provide 'tui-button)

;;; tui-button.el ends here
