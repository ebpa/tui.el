(require 'cl-lib)
(require 'dash)
(require 'tui-core)

(defmacro tui-defun (name props docstring &rest body)
  "Foundational macro to enable algebraic effects for tui components."
  (declare (indent defun))
  `(tui-define-component ,name
     :documentation ,docstring
     :render
     (lambda (_)
       (tui-let (&props ,@props)
         ,@body))))

(cl-defmacro tui-defun-1 (name (this) docstring &rest body)
  ""
  `(tui-defun-2 ,name (&this ,this)
     ,docstring
     ,@body))

(defun tui-defun-2--group-arguments (arguments)
  "Separate ARGUMENTS into (THIS PROPS STATE)."
  (cl-loop with group = '&props
           for arg in arguments
           if (member arg '(&this &state))
           do (setq group arg)
           else if (eq group '&props)
           collect arg into props
           else if (eq group '&this)
           collect arg into this
           else if (eq group '&state)
           collect arg into state
           end
           finally return (list this props state)))
;; (tui-defun-2--group-arguments '(foo &this this &state bar))
;; ((this) (foo) (bar))

(defun tui--symbol-to-keyword (symbol)
  (intern (concat ":" (symbol-name symbol))))

(cl-defmacro tui--with-prop-state-bindings ((this-sym prop-symbols state-symbols) &rest body)
  "Bind PROP-SYMBOLS and STATE-SYMBOLS in their respective orders with values retrieved from component THIS-SYM and evalueate BODY.  Return the last value of BODY."
  (declare (indent defun))
  (let* ((props-sym (make-symbol "props"))
         (state-sym (make-symbol "state")))
    `(let* (,@(when prop-symbols
                `((,props-sym (tui-component-props ,this-sym))
                  ,@(--map
                     (list it `(plist-get ,props-sym ,(tui--symbol-to-keyword it)))
                     prop-symbols)))
            ,@(when state-symbols
                `((,state-sym (tui-component-state ,this-sym))
                  ,@(--map
                     (list it `(plist-get ,state-sym ,(tui--symbol-to-keyword it)))
                     state-symbols))))
       ,@body)))

(defmacro tui-defun-2 (name arguments docstring &rest body)
  "Syntactic sugar for tersely defining a tui component with NAME.  ARGUMENTS should be a list of the form (PROP-1 PROP-2 ... &this THIS &state STATE-1 STATE-2 ...).

BODY is evaluated on each render.
Documentation string DOCSTRING."
  (declare (indent defun))
  (cl-flet ((normalize-varlists (varlist) (--map (if (consp it) it (list it nil)) varlist)))
    (-let* (((this props state) (tui-defun-2--group-arguments arguments))
            (this-sym (or (car this) (make-symbol "this")))
            (prop-alist (normalize-varlists props))
            (prop-keys (mapcar #'car prop-alist))
            (state-alist (normalize-varlists state))
            (state-keys (mapcar #'car state-alist)))
      `(tui-define-component ,name
         :documentation ,docstring
         ,@(when (-non-nil (-filter #'cadr prop-alist))
             (list :get-default-props
                   `(lambda ()
                      (-let* ,prop-alist
                        (list
                         ,@(apply
                            #'append
                            (--map
                             (list (tui--symbol-to-keyword it) it)
                             prop-keys)))))))
         ,@(when (-non-nil (-filter #'cadr state-alist))
             (list :get-initial-state
                   `(lambda (,this-sym)
                      (tui--with-prop-state-bindings (,this-sym ,prop-keys nil)
                        (-let* ,state-alist
                          (list
                           ,@(apply
                              #'append
                              (--map
                               (list (tui--symbol-to-keyword it) it)
                               state-keys))))))))
         :render
         (lambda (,this-sym)
           (tui--with-prop-state-bindings (,this-sym ,prop-keys ,state-keys)
             ,@body))))))

(provide 'tui-defun)
