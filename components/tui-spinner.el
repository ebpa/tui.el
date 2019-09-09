;;; tui-spinner.el --- Spinner component       -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(tui-define-component tui-spinner
  :prop-documentation
  (:type "One of `spinner-types'"
         :fps "the number of desired frames per second"
         :delay "seconds to wait after starting the spinner before actually displaying it")
  :component-did-mount
  (lambda ()
    (tui-run-with-timer component 1 1 t `(lambda ()
                                        (tui-force-update ,component))))
  :component-will-unmount
  (lambda ()
    (tui-let (&state spinner)
      (spinner-stop spinner)))
  :get-initial-state
  (lambda ()
    (require 'spinner)
    (tui-let (&props type fps delay)
      (let* ((spinner (make-spinner type nil fps delay)))
        (spinner-start spinner)
        (list :spinner spinner))))
  :render
  (lambda ()
    (tui-let (&state spinner)
      (spinner-print spinner))))

(provide 'tui-spinner)
;;; tui-spinner.el ends here
