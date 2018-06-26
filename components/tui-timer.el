;;; tui-demo-timer.el --- A basic timer       -*- lexical-binding: t; -*-


;;; Commentary:
;; 

;;; Code:

(tui-define-component tui-timer
  :documentation "A basic timer"
  :get-initial-state
  (lambda ()
    (list :start-time (or (plist-get (tui-get-props) :start-time)
                          (current-time))
          :timer nil))
  :render
  (lambda ()
    (tui-let (&state start-time)
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
         (format "%02d:%02d" minutes seconds)))))
  :component-did-mount
  (lambda ()
    (tui-run-with-timer component 1 1 `(lambda ()
                                     (tui-force-update ,component)))))

(provide 'tui-timer)

;;; tui-demo-timer.el ends here
