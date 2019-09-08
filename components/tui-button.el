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
    (tui-let (&props children action)
      (let* ((map (make-sparse-keymap)))
        (define-key map [mouse-1] action)
        (tui-span
         :text-props-replace `(mouse-face highlight
                                          face button
                                          keymap ,map)
         children)))))

(tui-define-demo tui-button "Basic click action"
  (tui-button
   :action (lambda ()
             (interactive)
             (message "Click!"))
   "Click Me"))

(provide 'tui-button)

;;; tui-button.el ends here
