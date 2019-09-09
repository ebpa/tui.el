;;; tui-tabstops.el ---       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(defvar tui-cycle-tabstops t
  "Whether tabstops should loop around (i.e. go back to the beginning once you've reached the end).")

(defun tui-tabstop-p (node)
  "Return t if NODE is a tabstop (has a truthy :tui-tabstop property)."
  (and (tui-element-p node)
       (plist-get (tui--get-props node) :tui-tabstop)))

(cl-defun tui-next-tabstop (&optional pos)
  "Return a marker indicating the first tabstop following POS.
Returns nil if a tabstop could not be found.

See also: `tui-previous-tabstop'."
  (-when-let* ((element (or (tui-next-element pos #'tui-tabstop-p)
                            (and tui-cycle-tabstops
                                 (tui-first-subtree-node #'tui-tabstop-p (tui-root-node))))))
    (tui-start element)))

(cl-defun tui-previous-tabstop (&optional pos)
  "Return a marker indicating the first tabstop preceding POS.
Returns nil if a tabstop could not be found.

See also: `tui-next-tabstop'."
  (-when-let* ((element (or (tui-previous-element pos #'tui-tabstop-p)
                            (and tui-cycle-tabstops
                                 (tui-last-subtree-node #'tui-tabstop-p (tui-root-node))))))
    (tui-start element)))

(cl-defun tui-forward-tabstop (&optional (num 1))
  "Move forward NUM tabstops.  Negative NUM values will move backward.

See also: `tui-backward-tabstop'."
  (interactive "p")
  (dotimes (n num)
    (-when-let* ((tabstop (tui-next-tabstop (point))))
      (goto-char tabstop))))

(cl-defun tui-backward-tabstop (&optional (num 1))
  "Move forward NUM tabstops.  Negative NUM values will move forward.

See also: `tui-forward-tabstop'."
  (interactive "p")
  (dotimes (n num)
    (-when-let* ((tabstop (tui-previous-tabstop (point))))
      (goto-char tabstop))))

(eval-after-load "avy"
  (defun tui-avy-jump-tabstop ()
    ""
    (interactive)
    ;; TODO: see avy--process
    ))

(provide 'tui-tabstops)
;;; tui-tabstops.el ends here
