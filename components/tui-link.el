;;; tui-link.el --- A basic clickable link component

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(defvar tui-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [space] 'tui-link-follow-link)
    (define-key map [return] 'tui-link-follow-link)
    (define-key map [mouse-1] 'tui-link-follow-link-click)
    map))

(tui-define-component tui-link
  :documentation
  "A basic link control"
  :prop-documentation
  (:target "Marker, function, or filename.")
  :render
  (lambda ()
    (tui-div
     :text-props `(font-lock-ignore t
                            face org-link
                            tui-link ,component
                            keymap ,tui-link-keymap)
     :children
     (plist-get (tui-get-props) :children))))

;; TODO: reconcile this with tui-link-follow-link ?
(defun tui-link-follow-link-click (event)
  "Handle click EVENT for following link."
  (interactive "e")
  (tui-link-follow-link (posn-point (event-end event))))

(defun tui-link-follow-link (&optional pos)
  "Follow link at POS or current point."
  (interactive)
  (unless pos (setq pos (point)))
  (-when-let* ((component (get-text-property pos 'tui-link))
               (target (plist-get (tui--get-props component) :target)))
    (cond
     ((markerp target)
      (switch-to-buffer (marker-buffer target))
      (goto-char target)
      (when (eq major-mode 'org-mode)
        (org-show-entry)))
     ((functionp target)
      (funcall target))
     ((eq (car target) 'file)
      (find-file (cdr target))))))

(provide 'tui-link)

;;; tui-link.el ends here
