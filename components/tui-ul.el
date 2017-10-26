;;; tui-ul.el --- HTML 'ul'-like component

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

;; TODO: additional capability with tui-li children?
(tui-define-component tui-ul
  :documentation "Unordered list component"
  :render
  (lambda ()
    (let* ((props (tui-get-props))
           (children (plist-get props :children)))
      (mapcar
       (lambda (child)
         (tui-line
          " • " child))
       children))))

(provide 'tui-ul)

;;; tui-ul.el ends here
