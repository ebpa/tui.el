;;; tui-dev.el --- Developer helper functions       -*- lexical-binding: t; -*-

;;; Commentary:
;; 

(require 'tui-core)

;;; Code:

(defvar tui-dev-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'tui-show-basic-element-outline)
    map)
  "Developer commands for tui.")

(define-minor-mode tui-dev-mode
  "Handy functions for tui development."
  nil nil tui-dev-keymap)

(defun tui-blink-component (component)
  "Blink COMPONENT to help visually locate it."
  ;; TODO: (run-at-time "1 sec" nil )
  
  ;; add and then remove box face
  )

(tui-defun tui-element-outline (element)
  "Structure outline of ELEMENT and its children."
  ;; TODO: use tree component
  (tui-element-outline-string element))

(cl-defun tui-element-outline-string (element &optional (depth 0))
  "Return a string describing the structure of ELEMENT.  Indent string according to DEPTH."
  (interactive)
  (let ((prefix (make-string (* 2 depth) ? )))
    (if (null element)
        (format "%s nil" prefix)
      (unless (tui--object-of-class-p element 'tui-node)
        (error "Expecting a tui-node object"))
      (-let* ((content (tui-node-content element))
              ((start . end) (tui-segment element)))
        (cond
         ((tui-element-p element)
          (progn
            (format "%s%S (element %S) (%S,%S)\n%s"
                    prefix
                    (tui--type element)
                    (tui-element-id element)
                    start
                    end
                    (mapconcat
                     (lambda (content-item)
                       (tui-element-outline-string content-item (+ 1 depth)))
                     content
                     "\n"))))
         ((tui--object-of-class-p element 'tui-text-node)
          (format "%s%S (text-node) (%S,%S)"
                  prefix
                  content
                  start
                  end))
         (t
          (format "%s%S %S (%S,%S)"
                  prefix
                  (tui--type element)
                  content
                  start
                  end)))))))

(cl-defun tui-show-basic-element-outline ((node (tui-root-node (point))))
  "Show an outline representation of NODE for debugging purposes."
  (interactive)
  (let ((buffer (get-buffer-create "*Tui Tree*")))
    (save-current-buffer
      (with-current-buffer buffer
        (erase-buffer)
        (insert (tui-element-outline-string (tui-root-node node)))
        (switch-to-buffer-other-window buffer)))))

(defun tui-show-element-outline (&optional node)
  "Show a an interactive outline representation of NODE."
  (interactive)
  (tui-render-with-buffer "*tui-show-element-outline*"
   (tui-element-outline-string node)))

(cl-defun tui-read-type (&optional (prompt "Type: ") (options (tui-all-component-types)))
  "Return a user-selected type as a symbol.

Optionally override PROMPT string.
Optionally limit types to OPTIONS."
  (intern (completing-read "Type: " options nil t)))

(defun tui-find-definition (type)
  "Find the definition of tui component TYPE or an element at point."
  (interactive (list (tui-read-type
                      "Type: "
                      (or (mapcar (lambda (element)
                                    (tui--object-class element))
                                  (tui-ancestor-elements-at (point)))
                          (tui-all-component-types)))))
  (tui-find-component type))

(defun tui-find-component (type)
  "Find the definition of tui component TYPE."
  (interactive (list (tui-read-type)))
  (let* ((type-name (symbol-name type)))
    (find-function (intern (s-chop-prefix "cl-struct-" type-name)))))

(put 'tui-wip 'invisible t)

(defun tui-toggle-wip ()
  "Hide/show ``work in progress'' UI elements."
  (interactive)
  (if (get 'tui-wip 'invisible)
      (put 'tui-wip 'invisible nil)
    (put 'tui-wip 'invisible t)))

(defun tui-dev-reset ()
  "Try resetting some things in case the tui engine is behaving badly."
  (interactive)
  ;; In case of user tampering:
  (setq tui--applying-updates nil)
  ;; Disregard unprocessed updates
  (setq tui--update-queue nil)
  (mapcar #'tui--mark-buffer-clean (tui--updated-buffers)))

(tui-defun tui-dev-content-tree-short-summary (tree)
  "Single-line summary describing content TREE."
  (list
   (tui-node-label tree)
   (if (tui-mounted-p tree)
       (list " at "
             (tui-link
              :target (tui-start tree)
              (prin1-to-string (tui-start tree))))
     " not mounted")))

(cl-defun tui-dev-list-content-trees (&optional (buffer (current-buffer)))
  "Return a list of all content trees in BUFFER."
  (interactive)
  (tui-render-with-buffer "*tui-dev-list-content-trees*"
    (--map
     (tui-line (tui-dev-content-tree-short-summary :tree it))
     (tui-buffer-content-trees))))

(cl-defun tui-read-buffer-content-tree (&optional (buffer (current-buffer)) (prompt "Content Tree: "))
  "Return a user-selected content tree within BUFFER.

Optionally override PROMPT string."
  (let* ((options (--map
                   (cons (tui-render-to-string (tui-dev-content-tree-short-summary :tree it)) it)
                   (tui-buffer-content-trees buffer))))
    (assoc-default (completing-read prompt options)
                   options)))

(provide 'tui-dev)
;;; tui-dev.el ends here
