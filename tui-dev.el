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

(tui-define-component tui-element-outline
  :render
  (lambda ()
    ;; TODO
    ))

(cl-defun tui-element-outline-string (element &optional (depth 0))
  ""
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

(defun tui-show-basic-element-outline (&optional node)
  "Show an outline representation of NODE for debugging purposes."
  (interactive)
  (unless node (setq node (tui-root-node (point))))
  (let ((buffer (get-buffer-create "*Tui Tree*")))
    (save-current-buffer
      (with-current-buffer buffer
        (erase-buffer)
        (insert (tui-element-outline-string (tui-root-node node)))
        (switch-to-buffer-other-window buffer)))))


;; TODO: tree view
;; TODO: links to navigate directly to items
(defun tui-show-element-outline (&optional node)
  "Show a an interactive outline representation of NODE."
  (interactive)
  (tui-popup
   (tui-element-outline
    :element node)))

(defun tui-find-definition (&optional type)
  "Find the definition of TYPE or an element at POINT."
  (interactive)
  (unless type (setq type (completing-read "Type: " (mapcar (lambda (element)
                                                              (tui--object-class element))
                                                            (tui-ancestor-elements-at (point)))
                                           nil t)))
  (when type
    (find-function (intern (s-chop-prefix "cl-struct-" type)))))

(put 'tui-wip 'invisible t)

(defun tui-toggle-wip ()
  "Hide/show ``work in progress'' UI elements."
  (interactive)
  (if (get 'tui-wip 'invisible)
      (put 'tui-wip 'invisible nil)
    (put 'tui-wip 'invisible t)))

(provide 'tui-dev)

;;; tui-dev.el ends here
