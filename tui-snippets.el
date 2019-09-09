;;; tui-snippets.el --- Useful snippets       -*- lexical-binding: t; -*-



;;; Commentary:
;;

;;; Code:

(defvar tui-snippets-root (file-name-directory (or load-file-name
                                                (buffer-file-name))))

(defun tui-snippets-initialize ()
  "Load tui snippets for use with yasnippet."
  (let ((snip-dir (expand-file-name "snippets" tui-snippets-root)))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snip-dir t))
    (yas-load-directory snip-dir)))

(eval-after-load "yasnippet"
  '(tui-snippets-initialize))

(provide 'tui-snippets)
;;; tui-snippets.el ends here
