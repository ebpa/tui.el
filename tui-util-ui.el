(require 'tui-util)

(defun tui-element-label (element)
  ""
  (prin1-to-string (tui--type element)))

(defun tui--prin1-to-string (value)
  ""
  (let* ((print-level nil)
         (print-length nil))
    (with-temp-buffer
      (insert (edebug-safe-prin1-to-string value))
      (goto-char (point-min))
      (ignore-errors
        (lispy-alt-multiline t))
      (buffer-substring (point-min) (point-max)))))

(tui-defun tui--plist-summary (plist)
  ""
  (let* ((edebug-print-length 200)
	 (edebug-print-level 50))
    (cl-loop for (key value) on plist by 'cddr
             collect
             (tui-line (prin1-to-string (keyword->symbol key)) ": " (tui--prin1-to-string value)))))


(tui-defun tui-element-summary (element)
  "Information summary about ELEMENT."
  (let* ((props (tui-element-props element))
         (state (tui-component-state element)))
    (list
     (tui-heading (tui-element-label element))
     (tui-line "Mount point: " (edebug-safe-prin1-to-string (tui-element-start element)))
     (tui-line "Source definition: ")
     (tui-line (format "Properties (%d): "
                       (length (tui--plist-keys props))))
     (tui-prefix-lines
      :prefix "  "
      (tui--plist-summary :plist props))
     (tui-line
      (format "State (%d): "
              (length (tui--plist-keys state))))
     (tui-prefix-lines
      :prefix "  "
      (tui--plist-summary :plist state)))))

(provide 'tui-util-ui)
