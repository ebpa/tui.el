(require 'tui-test-helper "test/tui-test-helper.el")

(require 's)
(require 'tui)


;; CLEANUP: improve testing focus

(describe "tui-define-component")

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
      (let ((div (tui-div "foo")))
        (tui-with-rendered-element
          div
          (tui-render-element "bar" div)
          (expect (buffer-string) :to-equal "foobar")))))

  (describe "lifecycle"
    (describe "lifecycle methods"
      (describe "tui-get-default-props"
        (it "gets called before tui-get-initial-state"))
      (describe "tui-get-initial-state")
      (describe "tui-component-will-mount")
      (describe "tui--mount"
        (it "calls an overridden tui--mount when defined"))
      (describe "tui-component-did-mount"
        (it "is called on mount")
        (it "component is marked as mounted already"))
      (describe "tui-component-will-receive-props"
        (it "supplies new props"))
      (describe "tui-should-component-update"
        (it "inhibits an update with a nil return value"))
      (describe "tui-component-will-update"
        (it "is called before tui-component-did-update"))
      (describe "tui-component-did-update")
      (describe "tui-will-unmount"
        (it "is called when a component is removed")))

    (describe "unmounting"
      (it "all elements get unmounted when buffer is about to be destroyed")
      (it "unmounting can be skipped on buffer destruction"
        ;; TODO: configuration value
        ))))

(describe "text properties"
  (it "basic text properties are applied"
    (tui-with-rendered-element
      (tui-div
       :text-props '(foo "bar")
       "woo")
      (expect (get-text-property 1 'foo) :to-equal "bar")))

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
            (hide-me (tui-div "Hide me!"))
            (following-content "(but not me!)"))
        (tui-with-rendered-element
          (tui-div
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
        (tui-div :invisible t
              :children "foo")
        (expect (buffer-string) :to-equal ""))))

  (it "hides only invisibile elements when intermixed with visible elements")
  (it "hides a previously visible element when :invisible is changed to t")
  (it "does not unmount element when :invisible property is changed to t")

  (it "prop-controlled visibilities are unchangable?"))

(describe "content tree operations"
  (describe "insert"
    (it "can insert an element into an empty list"
      (tui-with-rendered-element
        (tui-div)
        (tui-insert-node (tui-div "foo") tui-element 0)
        (expect (buffer-string) :to-equal "foo")))
    
    (it "can insert an element at the beginning of a list"
      (let ((bar (tui--normalize-node "bar")))
        (tui-with-rendered-element
          (tui-div bar)
          (tui-insert-node (tui-div "foo") tui-element 0)
          (expect (buffer-substring (point-min) (point-max)) :to-equal "foobar")
          (expect (tui-node-relative-index bar) :to-be 1))))
    
    (it "can insert an element at the end of a list"
      (let ((foo (tui--normalize-node "foo")))
        (tui-with-rendered-element
          (tui-div foo)
          (tui-insert-node (tui-div "bar") tui-element 1)
          (expect (buffer-substring (point-min) (point-max)) :to-equal "foobar")
          (expect (tui-node-relative-index foo) :to-be 0))))
    (it "can insert an element in the middle of a list"
      (let* ((foo (tui--normalize-node "foo"))
             (bar (tui-div "bar"))
             (baz (tui--normalize-node "baz")))
        (tui-with-rendered-element
          (tui-div foo baz)
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
          (tui-div foo bar baz)
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
          (tui-div foo bar baz)
          (tui-insert-node bar tui-element 1)
          (expect (tui-valid-element-p tui-element))
          (expect (buffer-string) :to-equal "foobarbaz"))))

    (it "can reinsert the sole child of an element at the same position"
      (let ((foo (tui--normalize-node "foo")))
        (tui-with-rendered-element
          (tui-div foo)
          (tui-insert-node foo tui-element 0)
          (expect (tui-valid-element-p tui-element))
          (expect (buffer-string) :to-equal "foo"))))

    (it "can reinsert the sole child of an element into another element"
      (let* ((foo (tui--normalize-node "foo"))
             (bar (tui-div foo))
             (baz (tui-span)))
        (tui-with-rendered-element
          (tui-div bar "-" baz)
          (tui-insert-node foo baz 0)
          (expect (tui-valid-element-p tui-element))
          (expect (buffer-string) :to-equal "-foo"))))))

(describe "tui-lowest-common-ancestor"
  (it "identifies shared parent"
    (let* ((a (tui-span "A"))
           (b (tui-span "B"))
           (c (tui-div "C" a b)))
      (tui-with-rendered-element c
        (expect (tui-lowest-common-ancestor a b) :to-be c)))))

(describe "tui-remove"
  (it "throws an error when target node is not mounted")
  (it "can remove the first child of an element"
    (let ((a (tui--normalize-node "a"))
          (b (tui--normalize-node "b"))
          (c (tui--normalize-node "c")))
      (tui-with-rendered-element
        (tui-div a b c)
        (tui-remove a)
        (expect (tui-node-relative-index b) :to-be 0)
        (expect (tui-node-relative-index c) :to-be 1))))
  (it "can remove the middle of an element"
    (let ((a (tui--normalize-node "a"))
          (b (tui--normalize-node "b"))
          (c (tui--normalize-node "c")))
      (tui-with-rendered-element
        (tui-div a b c)
        (tui-remove b)
        (expect (tui-node-relative-index a) :to-be 0)
        (expect (tui-node-relative-index c) :to-be 1))))
  (it "can remove end last child of an element"
    (let ((a (tui--normalize-node "a"))
          (b (tui--normalize-node "b"))
          (c (tui--normalize-node "c")))
      (tui-with-rendered-element
        (tui-div a b c)
        (tui-remove c)
        (expect (tui-node-relative-index a) :to-be 0)
        (expect (tui-node-relative-index b) :to-be 1))))
  (it "can remove the only child of an element"
    (let ((a (tui--normalize-node "a")))
      (tui-with-rendered-element
        (tui-div a)
        (tui-remove a)
        (expect (tui-child-nodes tui-element) :to-be nil)))))

(describe "tui-component")
