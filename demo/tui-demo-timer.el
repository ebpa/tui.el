;;; tui-demo-timer.el --- A basic timer


;;; Commentary:
;; 

;;; Code:

(tui-define-component tui-demo-timer
  :documentation "A basic timer"
  :get-initial-state
  (lambda ()
    (list :start-time (truncate (time-to-seconds))
          :timer nil))
  :render
  (lambda ()
    (let* ((props (tui-get-props))
           (state (tui-get-state))
           (start-time (plist-get state :start-time))
           (current-time (truncate (time-to-seconds)))
           (elapsed-seconds (- current-time start-time))
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
  :component-did-mount
  (lambda ()
    (let* ((timer (add-timeout 1 `(lambda (ignore)
                                    (tui-force-update ,component))
                               nil 1)))
      (tui--set-state component (plist-put (tui--get-state component) :timer timer))))
  :component-will-unmount
  (lambda (prev-props prev-state)
    (let ((timer (plist-get (tui--get-state component) :timer)))
      (cancel-timer timer))))

(provide 'tui-demo-timer)

;;; tui-demo-timer.el ends here
