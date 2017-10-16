;;; tui-demo-timer.el --- A basic timer


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
    (let* ((timer (add-timeout 1 `(lambda (ignore)
                                    (tui-force-update ,component))
                               nil 1)))
      (push timer tui-timer--timers)
      (tui--set-state component (plist-put (tui--get-state component) :timer timer))))
  :component-will-unmount
  (lambda ()
    (tui-let (&state timer)
      (cancel-timer timer)
      (setq tui-timer--timers (delq timer tui-timer--timers)))))

(defvar tui-timer--timers nil "List of active timers")

(defun tui-timer--cancel-all-timers ()
  ""
  (interactive)
  (mapc #'cancel-timer tui-timer--timers)
  (setq tui-timer--timers nil))

(provide 'tui-timer)

;;; tui-demo-timer.el ends here
