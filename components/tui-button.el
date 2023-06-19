;;; tui-button.el --- Basic button       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'tui-core)
(require 'tui-defun)

;;; Code:

(tui-defun-2 tui-button (children action (face 'custom-button))
  "A basic button control.

ACTION is a function to bind to [mouse-1]."
  (declare (wip TODO "or mouse-2/multiple?"))
  (let* ((map (make-sparse-keymap))
         ;; Interactive command necessary for keymap binding
         (interactive-action (if (interactive-form action)
                                 action
                               `(lambda ()
                                  (interactive)
                                  (funcall ,action)))))
    (define-key map [mouse-1] interactive-action)
    (tui-span
     :text-props-replace `(font-lock-ignore t
                                    font-lock-face ,face
                                    face ,face
                                    mouse-face highlight
                                    keymap ,map)
     children)))

(with-eval-after-load 'tui-demo
  (tui-define-demo tui-button "Basic click action"
    (tui-button
     :action (lambda ()
               (interactive)
               (message "Click!"))
     "Click Me")))

(provide 'tui-button)
;;; tui-button.el ends here
