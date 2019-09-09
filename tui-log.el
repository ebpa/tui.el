;;; tui-log.el --- Tui logging       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(require 'warnings)

(setq warning-minimum-log-level :warning)
(setq warning-minimum-level :warning)

(defun tui-toggle-debug-log ()
  "Switch logging level between :warning<->:debug."
  (interactive)
  (if (eq warning-minimum-log-level :debug)
      (progn
        (setq warning-minimum-log-level :warning)
        (setq warning-minimum-level :warning))
    (setq warning-minimum-log-level :debug)
    (setq warning-minimum-level :debug)))

(defvar tui-log-buffer-name "*Tui Log*")

(provide 'tui-log)
;;; tui-log.el ends here
