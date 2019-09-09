;;; tui-demo.el --- Demo-related logic  -*- lexical-binding: t; -*-


;;; Commentary:
;; 

(eval-when-compile (require 'cl))
(require 'tui-core)
(require 'tui-defun)

;;; Code:

(define-derived-mode tui-demo-mode special-mode "Tui Demo"
  "Major mode for viewing tui component previews."
  (setq-local buffer-read-only t)
  (setq-local revert-buffer-function
              (lambda (ignore-auto noconfirm) (tui-force-update (tui-root-node)))))

(put 'tui-demo-mode 'mode-class 'special)

(defun tui-show-component-demo (component demo-name)
  "Display demo content with DEMO-NAME for COMPONENT in a dedicated buffer."
  (interactive (let* ((component (tui-read-component-type)))
                 (list component
                       (completing-read "Demo: " (mapcar #'car (get component 'tui-demos))))))
  (-when-let* ((demo-fn (assoc-default demo-name
                                       (get component 'tui-demos))))
    (tui-show-demo-content
     (list
      (tui-heading (symbol-name component))
      (tui-line demo-name)
      (funcall demo-fn)))))

(defun tui-show-demo-content (content)
  "Display demo CONTENT in a dedicated buffer."
  (with-current-buffer (tui-render-with-buffer "*Tui Demo*" content)
    (tui-demo-mode)))

(tui-define-component my/greeting
  :get-default-props
  (lambda ()
    (list :name "World"))
  :render
  (lambda (this)
    (tui-let* ((&props name) this)
      (format "Hello, %s!\n" name))))

(defun my/basic-question ()
  "What do you want to work on today?")

(tui-define-component my/message
  :documentation
  "Message containing other components"
  :render
  (lambda ()
    (tui-let (&props name)
      (list (my/greeting :name name)
            "\n------------\n"
            (my/basic-question)
            "\nmake "
            (tui-demo-basic-counter :start-value 3)
            " widgets!"))))

;; (defvar tui-test-keymap
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [down-mouse-1] #'tui-confirmation)
;;     (define-key map [mouse-1] #'tui-confirmation)
;;     map))

(tui-defun-2 tui-demo-basic-counter (start-value &this counter &state (or start-value 0))
  "Basic counter control"
  (let* ((keymap (make-sparse-keymap)))
    (define-key keymap [down-mouse-1] (lambda ()
                                        (interactive)
                                        (tui-set-state (-lambda ((&plist :count count))
                                                         (list :count (+ 1 count))))))
    ;; (cl-flet ((incr-counter () (interactive) (tui-set-state `(:count ,(+ 1 count))))
    ;;           (decr-counter () (interactive) (tui-set-state `(:count ,(- count 1)))))
    (list count
          " > "
          (propertize "⏶"
                      'keymap keymap
                      ;;`(keymap (down-mouse-1 . ,(lambda () (interactive) (tui-set-state 5)))))
                      ;; `(keymap (down-mouse-1 . ,))
                      )
          (propertize "⏷"
                      ;; 'keymap
                      ;; `(keymap (down-mouse-1 . ,#'decr-counter))
                      ))))

(defmacro tui-define-demo (component description &rest body)
  "Define a demonstration of the use of COMPONENT.  DESCRIPTION is used as a label and BODY returns the content to be rendered."
  (declare (indent 2))
  `(let ((render-fn (lambda ()
                      ,@body))
         (existing-demos (get ',component 'tui-demos)))
     (put ',component 'tui-demos (cons (cons ,description render-fn)
                                       (cl-remove ,description existing-demos :test #'equal :key #'car)))))

;; (tui-show-all-component-demos :: void)
(defun tui-show-all-component-demos ()
  "Display all defined component demos grouped by component in a dedicated buffer."
  (interactive)
  (tui-render-with-buffer "*Tui Demos (all)*"
    (mapcar
     (lambda (component)
       (tui-expander
        :header (tui-heading (symbol-name component))
        :expanded-glyph "⏶"
        :collapsed-glyph "⏷"
        (tui-component-demos :component component)))
     (--filter
      (get it 'tui-demos)
      (tui-all-component-types)))))

;; (tui-show-component-demos String -> void)
(defun tui-show-component-demos (component)
  "Show all defined demos for COMPONENT in a single buffer."
  (interactive (list (tui-read-component-type)))
  (tui-render-with-buffer (format "*Tui %s Demos*" component)
    (tui-component-demos
     :component (intern component))))

;; (tui-component-demos :: Symbol -> List)
(tui-define-component tui-component-demos
  :documentation "Render all defined demos for COMPONENT."
  :render
  (lambda ()
    (tui-let (&props component)
      (-let* ((demos (get component 'tui-demos)))
        (if (not demos)
            (tui-div
             (format "No demos have been defined for %s.  Define one using "
                     component)
             (tui-link
              :target (-partial #'describe-function 'tui-define-demo)
              "tui-define-demo")
             ".")
          (mapcar
           (-lambda ((description . render-fn))
             (tui-div
              (tui-heading description)
              (funcall render-fn)))
           demos))))))

(provide 'tui-demo)

;;; tui-demo.el ends here
