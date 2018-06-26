;;; tui-link.el --- A basic clickable link component       -*- lexical-binding: t; -*-

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
    (let* ((props (tui-get-props))
           (face (if (plist-member props :face)
                     (plist-get props :face)
                   'org-link))
           (text-props (append `(keymap ,tui-link-keymap
                                    font-lock-ignore t)
                           (when face
                             `(face ,face)))))
      (tui-span
       :text-props text-props
       :children
       (plist-get (tui-get-props) :children)))))

;; TODO: reconcile this with tui-link-follow-link ?
(defun tui-link-follow-link-click (event)
  "Handle click EVENT for following link."
  (interactive "e")
  (tui-link-follow-link (posn-point (event-end event))))

(defun tui-link-follow-link (&optional pos)
  "Follow link at POS or current point."
  (interactive)
  (unless pos (setq pos (point)))
  (-when-let* ((component (tui-get-element-at pos 'tui-link))
               (target (plist-get (tui--get-props component) :target)))
    (cond
     ((and (stringp target)
           (s-starts-with-p "http" target))
      (browse-url target))
     ((markerp target)
      (switch-to-buffer (marker-buffer target))
      (goto-char target)
      (when (eq major-mode 'org-mode)
        (org-show-entry)))
     ((functionp target)
      (funcall target))
     ((and (listp target)
           (eq (car target) 'file))
      (find-file (cdr target))))))

(provide 'tui-link)

;;; tui-link.el ends here
