;;; tui-link.el --- A basic clickable link component       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'dash)
(require 'tui-defun)

;;; Code:

(defun tui-link-keymap ()
  ""
  (let ((map (make-sparse-keymap)))
    (define-key map "w" 'tui-link-copy-target)
    (define-key map [space] 'tui-link-follow-link)
    (define-key map [return] 'tui-link-follow-link)
    (define-key map [mouse-1] 'tui-link-follow-link-click)
    map))

(tui-defun-2 tui-link ((face 'button) target children &this this)
  "A basic link control.  TARGET may be a Marker, function, or filename."
  (declare (wip TODO "handle file:/// targets"))
  (let* ((text-props (append
                      `(keymap ,(tui-link-keymap)
                               font-lock-ignore t
                               tui-link-target ,target
                               help-echo ,(prin1-to-string target))
                      (when face
                        `(font-lock-face ,face
                                 face ,face)))))
    (tui-span
     :text-props-replace text-props
     :children (or children
                   (format "%s" target)))))

(defun tui-link-follow-link-click (event)
  "Handle click EVENT for following link."
  (interactive "e")
  (tui-link-follow-link (posn-point (event-end event))))

(defun tui-link-follow-link (&optional pos)
  "Follow link at POS or current point."
  (interactive)
  (unless pos (setq pos (point)))
  (-when-let* ((target (get-text-property pos 'tui-link-target)))
    (cond
     ((and (stringp target)
           (s-starts-with-p "chrome://" target))
      (eww-browse-with-external-browser target))
     ((and (stringp target)
           (s-starts-with-p "http" target))
      (browse-url-xdg-open target))
     ((and (stringp target)
           (f-exists-p target))
      (find-file target))
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

(defun tui-link-copy-target (&optional pos)
  ""
  (interactive)
  (unless pos (setq pos (point)))
  (let* ((url (get-text-property pos 'tui-link-target)))
    (kill-new url)
    (message "Copied <%s>!" url)))

(provide 'tui-link)
;;; tui-link.el ends here
