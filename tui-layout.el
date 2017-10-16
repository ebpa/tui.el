;;; tui-layout.el --- Layout/display logic
;;; Measurement

;;; Commentary:
;; 

;;; Code:

(cl-defmethod tui-height ((element tui-element))
  "Returns the total width of COMPONENT (not just visible characters)."
  (-let* (((start . end) (tui-segment element)))
    (when (and start end)
      (- (line-number-at-pos end)
         (line-number-at-pos start)))))

(cl-defmethod tui-width ((element tui-element))
  "Returns the total width of COMPONENT (not just visible characters)."
  (-let* (((start . end) (tui-segment element)))
    (when (and start end)
      (- end start))))

(cl-defmethod tui-pixel-width ((component tui-component))
  "Returns the total width of COMPONENT in pixels."
  (-let* (((start . end) (tui-segment component)))
    (when (and start end)
      (tui-segment-pixel-width start end))))

(cl-defmethod tui-visible-width ((component tui-component))
  "Returns the visible width (i.e. the number of characters within the segment that are *not* invisible)."
  (-let* (((start . end) (tui-segment component)))
    (tui-segment-visible-width start end)))

(defun tui-segment-visible-width (start end)
  "Return the number of visible characters between START and END."
  (let ((count 0))
    (with-current-buffer (marker-buffer start)
      (cl-loop for pos from (marker-position start)
               while (< pos (marker-position end))
               do
               (when (not (invisible-p pos))
                 (cl-incf count)))
      count)))

(defun tui-region-pixel-width ()
  "Display and return the width (in pixels) of the current region."
  (interactive)
  (let ((width (tui-segment-pixel-width (region-beginning) (region-end))))
    (message "%S" width)
    width))

(defun tui-region-pixel-height ()
  "Display and return the height (in pixels) of the current region."
  (interactive)
  (let ((height (tui-segment-pixel-height (region-beginning) (region-end))))
    (message "%S" height)
    height))

;; CLEANUP: improve name
(defun tui-char-pixel-size (str)
  "Return (WIDTH . HEIGHT) of string STR rendered at point."
  (let ((pos (point)))
    (insert str)
    (tui-segment-pixel-width pos (point))
    (delete-region pos (point))))

(defun tui-segment-pixel-width (start end)
  "Calculate and return the width (in pixels) of the segment between START and END."
  ;; (* (- end start)
  ;;    (window-font-width))
  ;; TODO: address line spanning
  (save-current-buffer
    (save-excursion
      (when (markerp start)
        (switch-to-buffer (marker-buffer start))
        (set-buffer (marker-buffer start)))
      ;; (save-current-buffer
      ;;   (when (markerp start)
      ;;     (set-window-buffer
      ;;     (set-buffer ))
      ;; TODO: improve calculation (the following wasn't fully working)
      (-let* ((start-x (tui--window-x-pixel-position start))
              (end-x (tui--window-x-pixel-position end)))
        (unless (tui-spans-lines-p start end)
          (message "Segment spans multiple lines; measured width is not accurate."))
        (when (and start-x end-x)
          (- end-x start-x))))))

(defun tui-segment-pixel-height (start end)
  "Calculate and return the height (in pixels) of the segment between START and END."
  (save-current-buffer
    (save-excursion
      (when (markerp start)
        (switch-to-buffer (marker-buffer start))
        (set-buffer (marker-buffer start)))
      ;; (save-current-buffer
      ;;   (when (markerp start)
      ;;     (set-window-buffer
      ;;     (set-buffer ))
      ;; TODO: improve calculation (the following wasn't fully working)
      (-let* ((start-y (tui--window-y-pixel-position start))
              (end-y (tui--window-y-pixel-position end)))
        (unless (tui-spans-lines-p start end)
          (message "Segment spans multiple lines; measured width is not accurate."))
        (when (and start-y end-y)
          (- end-y start-y))))))

(defun tui--window-x-pixel-position (pos)
  "Calculate the distance of POS relative to the left edge of the screen in pixels."
  (car (progn (goto-char pos)
              ;; (unless (pos-visible-in-window-p pos)
              ;;   (redisplay))
              (or (window-absolute-pixel-position pos)
                  (progn (redisplay)
                         (window-absolute-pixel-position pos))))))

(defun tui--window-y-pixel-position (pos)
  "Calculate the distance of POS relative to the top edge of the screen in pixels."
  (cdr (progn (goto-char pos)
              ;; (unless (pos-visible-in-window-p pos)
              ;;   (redisplay))
              (or (window-absolute-pixel-position pos)
                  (progn (redisplay)
                         (window-absolute-pixel-position pos))))))

(defun tui-spans-lines-p (start end)
  "Return t if START and END buffer positions span multiple lines."
  (not (eq (line-number-at-pos start) (line-number-at-pos end))))

(defun tui--display-position (pos)
  "Goto POS and ensure that it is visible in the window."
  (goto-char pos)
  (redisplay))

(defun tui--pixel-width-to-char-width (pixels)
  "Convert PIXELS to an approximate number of characters based on `window-font-width'."
  (when (listp pixels)
    (setq pixels (car pixels)))
  (round (/ (* 1.0 pixels)
            (window-font-width))))

(defun tui--char-width-to-pixel-width (columns)
  "Convert COLUMNS to an approximate number of pixels based on `window-font-width'."
  (when (listp columns)
    (setq columns (car columns)))
  (* columns (window-font-width)))

(defun tui--add-widths (a b)
  "Add widths A and B.

A and B do not need to use the same units.  When a number, the
unit is assumed have a unit of characters.  When a list of length
1, the unit is assumed to be pixels."
  (cond
   ((and (numberp a)
         (numberp b))
    (+ a b))
   ((and (listp a)
         (listp b))
    (list (+ (car a)
             (car b))))
   ((or (eq a 0)
        (and (listp a)
             (eq 0 (car a))))
    b)
   ((or (eq b 0)
        (and (listp b)
             (eq 0 (car b))))
    a)
   ((and (listp a)
         (numberp b))
    (list (+ (car a)
             (tui--char-width-to-pixel-width b))))
   ((and (listp b)
         (numberp a))
    (list (+ (car b)
             (tui--char-width-to-pixel-width a))))
   (t
    (error "Unexpected (presumably incompatible) pixel values"))))


(defun tui--width-difference (a b)
  "Return nil if A or B is nil."
  (when (and a b)
    (cond
     ((and (numberp a)
           (numberp b))
      (- a b))
     ((and (listp a)
           (listp b))
      (list (- (car a)
               (car b))))
     ((or (eq a 0)
          (and (listp a)
               (eq 0 (car a))))
      b)
     ((or (eq b 0)
          (and (listp b)
               (eq 0 (car b))))
      a)
     
     (t
      (error "Unexpected (presumably incompatible) pixel values")))))

(defun tui--overflow-length (start end pixel-width)
  "Return the number of characters that END has exceeded PIXEL-WIDTH distance from START."
  ;; CLEANUP: confusing arguments
  (let ((check-position end)
        (pixel-boundary (+ (car (window-absolute-pixel-position start)) pixel-width)))
    (while (and (> check-position start)
                (> (car (window-absolute-pixel-position check-position)) pixel-boundary))
      (setq check-position (- check-position 1)))
    (- end check-position)))


(cl-defmethod tui-goto-start ((node tui-node) &optional other-window)
  "Move point to the beginning of NODE."
  (let ((start (tui-start node)))
    (funcall
     (if other-window
         #'switch-to-buffer-other-window
       #'switch-to-buffer)
     (marker-buffer start))
    (goto-char (marker-position start))))

(cl-defmethod tui-goto-end ((node tui-node) &optional other-window)
  "Move point to the end of NODE."
  (let ((end (tui-end node)))
    (funcall
     (if other-window
         #'switch-to-buffer-other-window
       #'switch-to-buffer)
     (marker-buffer end))
    (goto-char (marker-position end))))

(defun tui--goto (marker)
  "Move point to MARKER."
  (when (markerp marker)
    (set-buffer (marker-buffer marker)))
  (goto-char marker))

(provide 'tui-layout)

;;; tui-layout.el ends here
