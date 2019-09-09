;;; tui-layout.el --- Layout / Visibility Helpers       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'tui-dom)

(defun tui-get-node-at (&optional pos type)
  "Get the `tui-node' at POS.  Search ancestors for a node of type TYPE when TYPE is non-nil."
  (unless pos (setq pos (point)))
  (if type
      (cl-first (tui-ancestor-elements-at pos type))
    (get-text-property pos 'tui-node)))

(defun tui-get-element-at (&optional pos type)
  "Get the `tui-element' at POS.  Search ancestors for element of type TYPE when TYPE is non-nil."
  (unless pos (setq pos (point)))
  (if type
      (cl-first (tui-ancestor-elements-at pos type))
    (tui-parent (get-text-property pos 'tui-node))))

(cl-defmethod tui--get-string ((node tui-node))
  "Return the string representation of NODE's content tree.  Return nil if NODE is not mounted."
  (-when-let* ((start (tui-start node))
               (end (tui-end node)))
    (with-current-buffer (marker-buffer start)
      (buffer-substring start end))))

(cl-defmethod tui--get-string (content)
  "Return the string representation of CONTENT."
  (if (tui--list-content-p content)
      (mapconcat #'tui--get-string content)
    (cond
     ((null content)
      "")
     ((stringp content)
      content)
     ((tui--image-p content)
      (propertize "[image]" 'display content))
     ((numberp content)
      (format "%s" content))
     (t
      tui-error-placeholder-string))))

(defun tui-invisible-p (element)
  "Return t if ELEMENT has been marked invisible."
  (and (tui-element-p element)
       (tui-element-invisible element)))

;; (defun tui-visible-p (element)
;;   "Return t if ELEMENT is not marked invisible.  This does not
;; indicate that ELEMENT is in fact visible.  A parent element may
;; be marked invisible which would cause ELEMENT to not be visible
;; to the user even if ELEMENT is not marked invisible."
;;   (and (tui-element-p element)
;;        (not (oref element :invisible))))

;;; Segment-related

(defvar tui--marker-node-table
  (make-hash-table :test #'eq) ;; TODO restore :weakness 'key)
  "Keep track of elements that start and end at markers.  Values are formatted as cons cells (START-ELEMENTS . END-ELEMENTS).")

(cl-defmethod tui-segment ((node tui-node))
  "Return markers denoting the start and end of NODE formatted as a cons cell (START . END)."
  (cons (tui-marker-list-node-marker (tui-node-start node))
        (tui-marker-list-node-marker (tui-node-end node))))

(cl-defmethod tui-segment--nodes ((node tui-node))
  "Return marker nodes denoting the start and end of NODE formatted as a cons cell (START . END)."
  (cons (tui-node-start node) (tui-node-end node)))

(defmacro tui--with-open-node (node &rest body)
  "\"Open\" segment of NODE and execute BODY.  Ensure that markers are consolidated following evaluation of BODY."
  (declare (indent defun))
  `;; (save-current-buffer
  ;;   (save-excursion
  (-let* ((node ,node))
    (tui--open-segment node)
    (tui--goto (tui-start node))
    (progn . ,body)))

(cl-defmethod tui--open-segment ((node tui-node))
  "Return the segment for NODE formatted as a cons cell (START . END)."
  (-let* ((start (tui-node-start node))
          (end (tui-node-end node)))
    (cl-assert (tui-marker-list-node-p start) "Start of segment must be a tui-marker-list-node.")
    (cl-assert (tui-marker-list-node-p end) "End of segment must be a tui-marker-list-node.")
    (tui-marker-list-open-segment (tui-node-marker-list node) start end)))

(defun tui-start (node)
  "Return a marker denoting the start of NODE.  Returns nil if NODE is not mounted."
  ;; TODO: defensively return copies of markers?
  (when (tui-node-mounted node)
    (tui-marker-list-node-marker (tui-node-start node))))

(defun tui-end (node)
  "Return a marker denoting the end of NODE.  Returns nil if NODE is not mounted."
  ;; TODO: defensively return copies of markers?
  (when (tui-node-mounted node)
    (tui-marker-list-node-marker (tui-node-end node))))

(defun tui--start-division (node)
  "Return division denoting the start of NODE."
  (tui-node-start node))

(defun tui--end-division (node)
  "Return division denoting the end of NODE."
  (tui-node-end node))

;;;; Markers and segments

(cl-defmethod tui-markers ((node tui-node))
  "Return a list of unique markers associated with NODE."
  (-let* (((start . end) (tui-segment node)))
    (list (tui-marker-list-node-start start)
          (tui-marker-list-node-end end))))

;; (cl-defmethod tui-markers ((element tui-element))
;;   "Return a list of unique markers within the subtree of ELEMENT."
;;   (-let* (((start . end) (tui-segment element)))
;;     (tui-marker-list-range (tui-element-marker-list element) start end)))

;; (cl-defmethod tui-child-markers ((element tui-element))
;;   "Return markers of NODE's children."
;;   (apply #'append
;;          (mapcar
;;           (lambda (child)
;;             (list (tui-start child) (tui-end child)))
;;           (tui-child-nodes element))))

;;;; Internal Segment-related

;; (cl-defmethod tui--set-segment ((node tui-node) start-node end-node)
;;   "Set NODE buffer segment to START-NODE and END-NODE."
;;   ;; (when (and start-node end-node)
;;   ;;   (cl-assert (<= start-node end-node) t "Segment should be ordered."))
;;   (tui--set-start node start-node)
;;   (tui--set-end node end-node))

(defun tui--elements-starting-at (marker)
  "Return a list of nodes starting at MARKER."
  (car (gethash marker tui--marker-node-table)))

(defun tui--incident-node-p (node pos)
  "Return t if NODE starts or ends at POS."
  (or (= (tui-start node) pos)
      (= (tui-end node) pos)))

(cl-defmethod tui--set-markers ((node tui-node) marker)
  "Internal function to set all segment endpoints in the subtree of NODE to MARKER."
  (setf (tui-node-start node) marker)
  (setf (tui-node-end node) marker))

(cl-defmethod tui--set-markers ((element tui-element) marker)
  "Internal function to set all segment endpoints in the subtree of NODE to MARKER."
  (mapc (lambda (child)
          (tui--set-markers child marker))
        (tui-child-nodes element))
  (cl-call-next-method))

;;; Measurement / size calculation

(cl-defmethod tui-line-height ((element tui-element))
  "Returns the total height (in lines) of ELEMENT (not just visible characters)."
  (-let* (((start . end) (tui-segment element)))
    (when (and start end)
      (- (line-number-at-pos end)
         (line-number-at-pos start)))))
(defalias 'tui-height 'tui-line-height)

(cl-defun tui-region-width (start end &optional no-wrap)
  "Returns the total width (in columns) of region."
  ;; TODO: invisible characters?
  (when (<= start end)
    (save-current-buffer
      (save-excursion
        (when (markerp start)
          (set-buffer (marker-buffer start)))
        (if no-wrap
            (apply #'max
                   (mapcar #'length
                           (s-split "\n" (buffer-substring start end) t)))
          (tui--goto start)
          (-let* ((min-x (current-column))
                  (max-x min-x))
            (while (< (point) end)
              (end-of-visual-line)
              (when (<= (point) end)
                (setq max-x 
                      (max max-x (current-column))))
              (forward-char)
              (when (<= (point) end)
                (setq min-x 
                      (min min-x (current-column)))))
            (- max-x min-x)))))))

(cl-defmethod tui-column-width ((element tui-element) &optional no-wrap)
  ;; TODO: Reimplement as column-width (and alias tui-column-width to it) include "rectangular" in docstring
  "Returns the total width (in columns) of ELEMENT (not just visible characters)."
  (-let* (((start . end) (tui-segment element)))
    (tui-region-column-width start end no-wrap)))
(defalias 'tui-width 'tui-column-width)

(cl-defmethod tui-pixel-width ((element tui-element))
  "Returns the total width of COMPONENT in pixels."
  (-let* (((start . end) (tui-segment element)))
    (when (and start end)
      (tui-segment-pixel-width start end))))

(cl-defmethod tui-visible-width ((element tui-element))
  ;; CLEANUP: Rename as "length"?
  "Returns the visible width (i.e. the number of characters within the segment that are *not* invisible)."
  (-let* (((start . end) (tui-segment element)))
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

(defun tui-char-pixel-size (str)
  ;; CLEANUP: improve name
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

(cl-defmethod tui-length ((node tui-node))
  "Return the length (number of characters) of NODE including invisible characters."
  (-let* (((start . end) (tui-segment node)))
    (- end start)))

(cl-defmethod tui-string-width ((node tui-node))
  "Return the display length (number of characters) of NODE including invisible characters."
  (string-width (tui--get-string node)))

(defun tui--node-height (node)
  "Return the height of NODE in its content tree.  The root element has a height of 1."
  (length (tui-ancestor-elements node)))

;;; Positioning

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

(defun tui--overflow-length (start end pixel-width)
  "Return the number of characters that START - END segment has exceeded PIXEL-WIDTH.

Returns a negative or zero number of there is no overflow."
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


;;; Visibility

(defun tui-hide-element (element)
  "Hide ELEMENT and its subtree."
  (interactive (list (tui-read-element-at-point "Hide element: ")))
  ;; OPTIMIZE: can preserve (cache) the content of the segment to potentially avoid re-render when made visible
  (display-warning 'tui-diff (format "HIDE %S" (tui--object-class element)) :debug tui-log-buffer-name)
  (setf (tui-element-invisible element) t)
  (-when-let* ((mounted (tui-element-mounted element))
               (inhibit-read-only t)
               ((start . end) (tui-segment element))
               (marker-tree (tui-element-marker-list element)))
    (delete-region start end))
  ;;(tui-valid-content-tree-p element)
  element)

(defun tui--show-element (element)
  "Show ELEMENT and its subtree."
  (setf (tui-element-invisible element) nil)
  (when (tui-element-mounted element)
    (tui--insert element))
  ;;(tui-valid-content-tree-p element)
  element)

(defmacro tui-preserve-point (node &rest body)
  "Evaluate BODY preserving the point position relative to start of ancestor NODE."
  (declare (indent defun))
  (let ((node-var (make-symbol "node"))
        (offset-var (make-symbol "offset")))
    `(let* ((,node-var ,node)
            (,offset-var (tui--node-internal-offset ,node-var)))
       ,@body
       (-when-let* ((start (tui-start ,node-var)))
         (goto-char (+ start ,offset-var))))))

(defun tui--node-internal-offset (node)
  "Return the relative offset between the start of ancestor NODE and point.
Returns nil if NODE is not mounted or is not an ancestor element."
  (let* ((start (tui-start node))
         (end (tui-end node)))
    (when (and start
               (eq (marker-buffer start)
                   (current-buffer))
               (>= (point) start)
               (<= (point) end))
      (- (point) start))))

(cl-defun tui-next-element (&optional pos (predicate #'identity))
  "Return the nearest `tui-element' that starts after POS or point.
Returns nil if there are no elements following POS in the content tree.

See also `tui-previous-element'."
  (unless pos (setq pos (point)))
  (let* ((target-node (tui-get-node-at pos))
         (current-node target-node)
         current-node-index parent following-siblings next-element intermediate-element)
    (while (and (setq parent (tui-parent current-node))
                (setq current-node-index (tui-node-relative-index current-node))
                (progn (setq following-siblings (-slice (tui-child-nodes parent) (+ 1 current-node-index)))
                       (not (setq next-element
                                  (some (-partial #'tui-first-subtree-node predicate)
                                        following-siblings)))))
      (setq current-node parent))
    (or next-element
        intermediate-element)))

(cl-defun tui-previous-element (&optional pos (predicate #'identity))
  "Return the nearest `tui-element' that ends before POS.
Returns nil if there are no elements preceding POS in the content tree.

See also `tui-next-element'."
  (unless pos (setq pos (point)))
  (let* ((target-node (tui-get-node-at pos))
         (current-node target-node)
         current-node-index parent preceding-siblings previous-element intermediate-element)
    (while (and (setq parent (tui-parent current-node))
                (setq current-node-index (tui-node-relative-index current-node))
                (progn (setq preceding-siblings (reverse (-take current-node-index (tui-child-nodes parent))))
                       (not (setq previous-element
                                  (-some (-partial #'tui-last-subtree-node predicate)
                                         preceding-siblings)))))
      (setq current-node parent))
    (or previous-element
        intermediate-element)))

(provide 'tui-layout)

;;; tui-layout.el ends here
