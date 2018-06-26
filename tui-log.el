;;; tui-log.el --- Tui logging       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(setq log-warning-minimum-level :warning)
(setq warning-minimum-level :warning)

(defun tui-toggle-debug-log ()
  "Switch logging level between :warning<->:debug"
  (interactive)
  (if (eq log-warning-minimum-level :debug)
      (progn
        (setq log-warning-minimum-level :warning)
        (setq warning-minimum-level :warning))
    (setq log-warning-minimum-level :debug)
    (setq warning-minimum-level :debug)))

(defvar tui-log-buffer-name "*Tui Log*")

(provide 'tui-log)

;;; tui-log.el ends here
