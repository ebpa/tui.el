;; -*- lexical-binding: t; -*-
(require 'tui-test-helper "test/tui-test-helper.el")
(require 'buttercup)
(require 's)
(require 'tui)

;; CLEANUP: improve testing focus

(describe "tui-define-component"
  (it "accepts a documentation string"
    (expect (tui-define-component tui-test-component-a
              :documentation "Documentation!"
              :render
              (lambda ()
                nil))))
  (it "accepts a prop-documentation string"
    (expect (tui-define-component tui-test-component-b
              :prop-documentation "Property documentation!"
              :render
              (lambda ()
                nil))))
  (it "requires a render method"
    (expect (tui-define-component tui-test-component-c
              :render
              (lambda ()
                nil)))
    ;; TODO: Raise an error when no method is supplied? (or is it not required?)
    ;; (expect (tui-define-component tui-test-component-c))
    ))

(describe "Re-renders"
  (it "should preserve point when possible")
  (it "should preserve mark whenever possible")
  (it "should preserve region whenever possible"))

(describe "tui-component"
  (describe "lifecycle"
    (describe "lifecycle methods"
      (describe "tui-get-default-props"
        (it "gets called before tui-get-initial-state"))
      (describe "tui-get-initial-state"
        (it "is reflected in the initial component state"))
      (describe "tui--mount"
        (it "calls an overridden tui--mount when defined"))
      (describe "tui-component-did-mount"
        (it "is called on mount")
        (it "component is marked as mounted already")
        (it "is called with a dynamically-scoped `component' reference"
          (cl-flet ((my/test-component-did-mount () (expect (tui--type component) :to-equal 'my/test-component-did-mount)))
            (tui-define-component my/test-component-did-mount-component
              :component-did-mount
              'my/test-component-did-mount
              :render
              (lambda ()
                "Test!"))

            (spy-on 'my/test-component-did-mount)

            (tui-with-rendered-element (my/test-component-did-mount-component)
              (expect 'my/test-component-did-mount :to-have-been-called-times 1)))))
      (describe "tui-get-derived-state-from-props"

        :var (my/get-derived-state-from-props my/test-component)

        (before-each
          (setf (symbol-function 'my/get-derived-state-from-props)
                (lambda (props state)
                  (let* ((a (plist-get props :a))
                         (b (plist-get state :b)))
                    (when (and a b)
                      (list :c (* a b))))))

          (tui-define-component my/test-component
            :get-default-props
            (lambda ()
              (list :a 7))
            :get-initial-state
            (lambda ()
              (list :b 6))
            :get-derived-state-from-props
            #'my/get-derived-state-from-props
            :render
            (lambda ()
              "test")))

        (after-each
          (tui-unintern 'my/test-component))

        (it "receives component props and state"
          (spy-on 'my/get-derived-state-from-props)
          (tui-with-rendered-element (my/test-component)
            ;; CLEANUP: Check using something like plist-contains
            (-let* ((call-args (spy-calls-args-for 'my/get-derived-state-from-props 0))
                    (((&plist :a a)
                      (&plist :b b))
                     call-args))
              (expect call-args)
              (expect a :to-be 7)
              (expect b :to-be 6))))
        (it "has its return value merged into state"
          (tui-with-rendered-element (my/test-component)
            (expect (plist-get (tui--get-state tui-element) :c) :to-be 42)))
        (it "is called before the initial render call"
          (let* (render-called-p get-derived-state-called-first-p)
            (cl-defmethod tui-render ((component my/test-component))
              (setq render-called-p t)
              (cl-call-next-method))

            (spy-on 'my/get-derived-state-from-props
                    :and-call-fake
                    (lambda (props state)
                      (setq get-derived-state-called-first-p
                            (not render-called-p))))

            (tui-with-rendered-element (my/test-component)
              (expect get-derived-state-called-first-p :to-be t))))
        (it "is called when component properties are updated"
          (tui-with-rendered-element (my/test-component)
            (spy-on 'my/get-derived-state-from-props)
            (expect 'my/get-derived-state-from-props :not :to-have-been-called)
            (tui--set-props tui-element '(:a 70))
            (expect 'my/get-derived-state-from-props :to-have-been-called)))
        (it "is not called when component state is updated"
          (tui-with-rendered-element (my/test-component)
            (spy-on 'my/get-derived-state-from-props)
            (tui--set-state tui-element '(:b 0))
            (expect 'my/get-derived-state-from-props :not :to-have-been-called)))
        (describe "tui-should-component-update"
          (it "inhibits an update with a nil return value")
          (it "is called following a prop update")
          (it "is called following a state update"))
        (describe "tui-component-did-update")
        (describe "tui-will-unmount"
          (it "is called when a component is removed")))

      (describe "unmounting"
        (it "all elements get unmounted when buffer is about to be destroyed")
        (it "unmounting can be skipped on buffer destruction"
          ;; TODO: configuration value
          ))))

  (describe "ref callback"
    :var (my/ref-callback my/test-component component-did-mount-called-p)

    (before-each
      (setq component-did-mount-called-p nil)
      (setf (symbol-function 'my/ref-callback)
            (lambda (component)
              nil))

      (tui-define-component my/test-component
        :component-did-mount
        (lambda ()
          (setq component-did-mount-called-p t))
        :render
        (lambda ()
          "test")))

    (after-each
      (tui-unintern 'my/test-component))

    (it "is called once before the :component-did-mount method"
      (let* (ref-callback-called-first-p)
        (spy-on 'my/ref-callback :and-call-fake
                (lambda (component)
                  (setq ref-callback-called-first-p
                        (not component-did-mount-called-p))
                  nil))

        (tui-with-rendered-element (my/test-component :ref #'my/ref-callback)
          (expect component-did-mount-called-p)
          (expect ref-callback-called-first-p))))
    (it "is called with nil when unmounting"
      (spy-on 'my/ref-callback)

      (tui-with-rendered-element (my/test-component :ref #'my/ref-callback)
        (spy-calls-reset 'my/ref-callback))
      (expect 'my/ref-callback :to-have-been-called-with nil))
    (it "is called with nil before update when :ref argument is changed")))

(describe "tui-element"
  
  (it "leaves adjacent non- comp content intact"
    (with-temp-buffer
      (insert "foobaz")
      (let ((element (tui-render-element "bar" 4)))
        (expect (buffer-string) :to-equal "foobarbaz")
        (tui-remove element)
        (expect (buffer-string) :to-equal "foobaz"))))
  
  (it "stores output segment on render"
    (tui-with-rendered-element
      "Hello world!"
      (-let* (((start . end) (tui-segment tui-element)))
        (expect (markerp start))
        (expect (markerp end)))))

  (describe "rendering targets"
    (it "is able to render element at a provided point"
      (with-temp-buffer
        (insert "foobaz")
        (tui-render-element "bar" 4)
        (expect (buffer-string) :to-equal "foobarbaz")))
    
    (it "signals an error when a rendering target is not a node, but within an existing content tree"
      ;; TODO: implement
      )

    (it "can render an element within an existing element"
      (let ((span (tui-span "foo")))
        (tui-with-rendered-element
          span
          (tui-render-element "bar" span)
          (expect (buffer-string) :to-equal "foobar"))))))



(describe "text properties"
  (it "basic text properties are applied"
    (tui-with-rendered-element
      (tui-span
       :text-props '(foo "bar")
       "woo")
      (expect (get-text-property 1 'foo) :to-equal "bar")))

  (it "text properties are updated even if they are the change between renders")

  (describe "tui-put-text-properties"
    (describe "'replace"))

  (describe "text property inheritance"))

(describe "empty elements")

(describe "visibility"

  (describe "hiding elements"
    (it "hides a basic element"
      (tui-with-rendered-element
        (tui-span "foo")
        (tui-hide-element tui-element)
        (expect (buffer-string) :to-equal "")))

    (it "can hide a basic compontent"
      (tui-with-rendered-element
        (tui-span "foo")
        (tui-hide-element tui-element)
        (expect (not (s-contains-p "foo" (buffer-string))))))

    (it "hides an entire element subtree")

    (it "only hides subtree of the target element"
      (let ((preceding-content (tui-line "Preceding content"))
            (hide-me (tui-span "Hide me!"))
            (following-content "(but not me!)"))
        (tui-with-rendered-element
          (tui-span
           preceding-content
           hide-me
           following-content)
          (tui-hide-element hide-me)
          (expect (tui-invisible-p hide-me))
          (expect (not (tui-invisible-p preceding-content)))
          (expect (not (tui-invisible-p following-content)))
          (expect (not (tui-invisible-p tui-element)))
          (expect (s-contains-p "Preceding content" (buffer-string)))
          (expect (not (s-contains-p "Hide me!" (buffer-string))))
          (expect (s-contains-p "(but not me!)" (buffer-string))))))
    
    (it "is idempotent (hiding a hidden element does nothing)"
      ))

  (describe ":invisible elements"
    (it "is not rendered when :invisible is truthy"
      (tui-with-rendered-element
        (tui-span :invisible t
                  :children "foo")
        (expect (buffer-string) :to-equal ""))))

  (it "hides only invisibile elements when intermixed with visible elements")
  (it "hides a previously visible element when :invisible is changed to t")
  (it "does not unmount element when :invisible property is changed to t")

  (it "prop-controlled visibilities are unchangable?"))

(describe "content tree operations"
  (describe "tui-insert-node"
    (it "can insert an element into an empty list"
      (tui-with-rendered-element
        (tui-span)
        (tui-insert-node (tui-span "foo") tui-element 0)
        (expect (buffer-string) :to-equal "foo")))
    
    (it "can insert an element at the beginning of a list"
      (let ((bar (tui--normalize-node "bar")))
        (tui-with-rendered-element
          (tui-span bar)
          (tui-insert-node (tui-span "foo") tui-element 0)
          (expect (buffer-substring (point-min) (point-max)) :to-equal "foobar")
          (expect (tui-node-relative-index bar) :to-be 1))))
    
    (it "can insert an element at the end of a list"
      (let ((foo (tui--normalize-node "foo")))
        (tui-with-rendered-element
          (tui-span foo)
          (tui-insert-node (tui-span "bar") tui-element 1)
          (expect (buffer-substring (point-min) (point-max)) :to-equal "foobar")
          (expect (tui-node-relative-index foo) :to-be 0))))
    (it "can insert an element in the middle of a list"
      (let* ((foo (tui--normalize-node "foo"))
             (bar (tui-span "bar"))
             (baz (tui--normalize-node "baz")))
        (tui-with-rendered-element
          (tui-span foo baz)
          (tui-insert-node bar tui-element 1)
          (expect (buffer-substring (point-min) (point-max)) :to-equal "foobarbaz")
          (expect (tui-node-relative-index foo) :to-be 0)
          (expect (tui-node-relative-index bar) :to-be 1)
          (expect (tui-node-relative-index baz) :to-be 2))))

    (it "can insert an element already in the list"
      (let ((foo (tui--normalize-node "foo"))
            (bar (tui--normalize-node "bar"))
            (baz (tui--normalize-node "baz"))
            (-compare-fn #'eq))
        (tui-with-rendered-element
          (tui-span foo bar baz)
          (tui-insert-node baz tui-element 0)
          (expect (buffer-string) :to-equal "bazfoobar")
          (expect (tui-node-relative-index baz) :to-be 0)
          (expect (tui-node-relative-index foo) :to-be 1)
          (expect (tui-node-relative-index bar) :to-be 2))))
    
    (it "can insert an element already in the list at the same position"
      (let ((foo (tui--normalize-node "foo"))
            (bar (tui--normalize-node "bar"))
            (baz (tui--normalize-node "baz")))
        (tui-with-rendered-element
          (tui-span foo bar baz)
          (tui-insert-node bar tui-element 1)
          (expect (tui-valid-element-p tui-element))
          (expect (buffer-string) :to-equal "foobarbaz"))))

    (it "can reinsert the sole child of an element at the same position"
      (let ((foo (tui--normalize-node "foo")))
        (tui-with-rendered-element
          (tui-span foo)
          (tui-insert-node foo tui-element 0)
          (expect (tui-valid-element-p tui-element))
          (expect (buffer-string) :to-equal "foo"))))

    (it "can reinsert the sole child of an element into another element"
      (let* ((foo (tui--normalize-node "foo"))
             (bar (tui-span foo))
             (baz (tui-span)))
        (tui-with-rendered-element
          (tui-span bar "-" baz)
          (tui-insert-node foo baz 0)
          (expect (tui-valid-element-p tui-element))
          (expect (buffer-string) :to-equal "-foo"))))))

(describe "tui-lowest-common-ancestor"
  (it "identifies shared parent"
    (let* ((a (tui-span "A"))
           (b (tui-span "B"))
           (c (tui-span "C" a b)))
      (tui-with-rendered-element c
        (expect (tui-lowest-common-ancestor a b) :to-be c)))))

(describe "tui-remove"
  (it "throws an error when target node is not mounted")
  (it "can remove the first child of an element"
    (let ((a (tui--normalize-node "a"))
          (b (tui--normalize-node "b"))
          (c (tui--normalize-node "c")))
      (tui-with-rendered-element
        (tui-span a b c)
        (tui-remove a)
        (expect (tui-node-relative-index b) :to-be 0)
        (expect (tui-node-relative-index c) :to-be 1))))
  (it "can remove the middle of an element"
    (let ((a (tui--normalize-node "a"))
          (b (tui--normalize-node "b"))
          (c (tui--normalize-node "c")))
      (tui-with-rendered-element
        (tui-span a b c)
        (tui-remove b)
        (expect (tui-node-relative-index a) :to-be 0)
        (expect (tui-node-relative-index c) :to-be 1))))
  (it "can remove end last child of an element"
    (let ((a (tui--normalize-node "a"))
          (b (tui--normalize-node "b"))
          (c (tui--normalize-node "c")))
      (tui-with-rendered-element
        (tui-span a b c)
        (tui-remove c)
        (expect (tui-node-relative-index a) :to-be 0)
        (expect (tui-node-relative-index b) :to-be 1))))
  (it "can remove the only child of an element"
    (let ((a (tui--normalize-node "a")))
      (tui-with-rendered-element
        (tui-span a)
        (tui-remove a)
        (expect (tui-child-nodes tui-element) :to-be nil)))))

(describe "tui-force-update"
  (it "re-renders target component"
    (let* ((component-b (tui-span "Blah"))
           (component-a (tui-span component-b)))
      (spy-on 'tui-render :and-call-through)
      (tui-with-rendered-element component-a
        (expect 'tui-render :to-have-been-called-with component-a)
        (spy-calls-reset 'tui-render)
        (tui-force-update component-a)
        (expect 'tui-render :to-have-been-called-with component-a))))

  (it "calls should-component-update on all children"))

(describe "core utility functions"
  (describe "tui--set-props"
    (it "preserves existing properties"
      (tui-define-component tui-set-props-test-component
        :get-default-props
        (lambda ()
          (list :a 1234))
        :render
        (lambda ()
          "test"))
      (let* ((test-component (tui-set-props-test-component)))
        (tui-with-rendered-element test-component
          (tui--set-props test-component '(:b 100))
          (expect (plist-get
                   (tui--get-props test-component)
                   :a)
                  :to-equal 1234))))))

(describe "tui--set-state"
  (it "causes an update when NO-UPDATE is nil")
  (it "does not update when NO-UPDATE is truthy"))

(describe "tui-render-with-buffer"
  (it "accepts a single content item"
    (with-current-buffer
        (tui-render-with-buffer "*test1*"
          "foo")
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "foo")))
  (it "accepts multiple content items"
    (with-current-buffer
        (tui-render-with-buffer "*test2*"
          "a" "b" "c")
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "abc"))))

(describe "tui-component")
