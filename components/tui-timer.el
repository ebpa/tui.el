;;; tui-demo-timer.el --- A basic timer       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(require 'tui-defun)
(require 'tui-util)

(tui-defun-2 tui-demo-timer (&state (start-time (current-time))
                                    (timer (tui-run-with-timer
                                            component 1 1 t
                                            (lambda ()
                                              (tui-force-update component)))))
  "A basic timer"
  (let* ((start-time-seconds (truncate (time-to-seconds start-time)))
         (current-time-seconds (truncate (time-to-seconds)))
         (elapsed-seconds (- current-time-seconds start-time-seconds))
         (days (truncate (/ elapsed-seconds 86400)))
         (hours (truncate (/ (% elapsed-seconds 86400) 3600)))
         (minutes (truncate (/ (% elapsed-seconds 3600) 60)))
         (seconds (truncate (% elapsed-seconds 60))))
    (tui-span
     (when (> days 0)
       (format "%d days " days))
     (when (> hours 0)
       (format "%02d" hours))
     (format "%02d:%02d" minutes seconds))))

(provide 'tui-timer)
;;; tui-demo-timer.el ends here
