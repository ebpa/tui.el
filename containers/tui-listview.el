(require 'tui-core)

(tui-define-component tui-listview
  :documentation ""
  :prop-documentation
  (:collection "`collection' object containing items to display."
               :iten-render-fn "Function used to render each item in the list.  Called with item as the sole argument.")
  :render
  (lambda ()
    (require 'collection nil t)
    (unless (featurep 'collection)
      (error "tui-listview requires collection"))
    (tui-let (&props item-render-fn collection)
      (mapcar
       (lambda (item)
         (tui-listview-item
          :key item
          (if item-render-fn
              (funcall item-render-fn item)
            item)))
       (collection-to-list collection))))
  :component-did-mount
  (lambda ()
    (push `(lambda (&rest ignore)
             (message "%S" (aref ,component 0))
             (tui-force-update ,component))
          (collection-collection-changed-fns
           (tui-listview--collection component))))
  :component-will-unmount
  (lambda ()
    (let* ((collection (tui-listview--collection component)))
      (setf (collection-collection-changed-fns collection)
            (remove `(lambda (&rest ignore) (tui-force-update ,component))
                    (collection-collection-changed-fns collection))))))

(defvar tui-listview-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-<down>") #'tui-listview-move-item-down)
    (define-key map (kbd "S-<up>") #'tui-listview-move-item-up)
    (define-key map (kbd "C-k") #'tui-listview-remove-item)
    map)
  "")

(tui-define-component tui-listview-item
  :documentation ""
  :prop-documentation
  ()
  :render
  (lambda ()
    (tui-div
     :text-props `(keymap ,tui-listview-keymap)
     (plist-get (tui-get-props) :children))))

(defun tui-listview--collection (listview)
  "Return the list represented by LISTVIEW."
  (plist-get (tui--get-props listview) :collection))

(defun tui-listview-move-item-down (&optional listview item)
  "Move ITEM down in the list."
  (interactive)
  (-when-let* ((listview (or listview
                             (tui-get-element-at (point) 'tui-listview)))
               (item (or item
                         (tui-get-element-at (point) 'tui-listview-item)))
               (item-index (position item (tui-child-nodes listview)))
               (collection (tui-listview--collection listview))
               (target-index (+ item-index 1)))
    (unless (= item-index (- (collection-size collection) 1))
      (tui-preserve-point item
        (collection-move collection item-index target-index)))))

(defun tui-listview-move-item-up (&optional listview item)
  "Move ITEM up in the list."
  (interactive)
  (-when-let* ((listview (or listview
                             (tui-get-element-at (point) 'tui-listview)))
               (item (or item
                         (tui-get-element-at (point) 'tui-listview-item)))
               (item-index (position item (tui-child-nodes listview)))
               (collection (tui-listview--collection listview))
               (target-index (- item-index 1)))
    (unless (= item-index 0)
      (tui-preserve-point item
        (collection-move collection item-index target-index)))))

(defun tui-listview-remove-item (&optional listview item)
  "Remove ITEM from LIST."
  (interactive)
  (-when-let* ((listview (or listview
                             (tui-get-element-at (point) 'tui-listview)))
               (item (or item
                         (tui-get-element-at (point) 'tui-listview-item)))
               (item-index (position item (tui-child-nodes listview)))
               (collection (tui-listview--collection listview)))
    (collection-remove-at collection item-index)))

(defun tui-demo-listview ()
  "Show a demo of `tui-listview'."
  (interactive)
  (let* ((sample-list (collection-create '("foo" "bar" "baz" "bat"))))
    (push (lambda (event-args)
            (message "List changed! %S" event-args))
          (collection-collection-changed-fns sample-list))
    (tui-show-component-demo
     (tui-listview
      :item-render-fn (lambda (value)
                        (list value "\n"))
      :collection sample-list))))

(provide 'tui-listview)
