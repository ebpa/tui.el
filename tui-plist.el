(eval-when-compile
  (require 'cl-lib))
(require 'dash)

(defun tui--plist-delete (plist &rest properties)
  "Delete PROPERTIES from PLIST.
This is in contrast to merely setting it to 0.

\(copied from `use-package-plist-delete')"
  (let ((property (car properties))
        (rest-properties (cdr properties))
        p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    (if rest-properties
        (apply #'tui--plist-delete p rest-properties)
      p)))

(defun tui--plist-merge (a b &rest rest)
  "Merge plists A, B, and REST into a new list.

Values for keys in successive list will override those preceding
it.  Ex: If a property key is found in plists A and B, the
returned list will contain the value from B."
  (let* ((merged (cl-copy-seq a)))
    (cl-loop for (key val) on b by 'cddr
             do (setq merged (plist-put merged key val)))
    (if rest
        (apply #'tui--plist-merge merged rest)
      merged)))

(defun tui--plist-equal (a b)
  "Helper to check wither plists A and B are equal."
  (declare (wip CLEANUP "be consistent about ignoring degeneracies.  Currently only degeneracies in B are ignored."))
  (let* ((b b))
    (cl-loop for (key-a value-a) on a by #'cddr
             always (equal
                     (plist-get b key-a)
                     value-a)
             do
             (setq b (tui--plist-delete b key-a))
             finally return (null b))))

(defun tui--symbol< (a b)
  "Compare the symbol names A and B."
  (string< (symbol-name a)
           (symbol-name b)))

(defun tui--plist-to-sorted-alist (plist)
  "Convert PLIST to a sorted ALIST."
  (sort (cl-loop for (prop value) on plist by #'cddr
                 collect (cons prop value))
        (lambda (a b)
          (tui--symbol< (car a)
                        (car b)))))

(defun tui--plist-changes (old-plist new-plist)
  "Return a plist of differences between plists OLD-PLIST and NEW-PLIST."
  (let* ((old-list (tui--plist-to-sorted-alist old-plist))
         (new-list (tui--plist-to-sorted-alist new-plist))
         (-compare-fn (lambda (a b)
                        (and
                         ;; TODO: confirm #'eq is adequate and use that instead of the test immediately below
                         ;; (eq (car a) (car b))
                         (equal (symbol-name (car a))
                                (symbol-name (car b)))
                         (tui-equal (cdr a)
                                    (cdr b)))))
         (difference (-difference new-list old-list)))
    ;;(display-warning 'tui (format "(differences: %S)" (mapcar #'car difference)) :debug tui-log-buffer-name)
    (cl-loop for (key . value) in difference
             append (list key value))))

(defun tui--clean-plist (plist)
  "Remove degeneracies from plist."
  (let ((keys (make-hash-table :test #'equal))
        (miss (make-symbol "miss"))
        new-plist)
    (cl-loop for (key value) on plist by #'cddr
             do
             (when (eq (gethash key table miss) miss)
               (puthash key key keys)
               (push value new-plist)
               (push key new-plist)))
    new-plist))

(defun tui--plist-keys (plist)
  "Return the keys of PLIST."
  (cl-loop for (key value) on plist by #'cddr
           collect key))

(provide 'tui-plist)
