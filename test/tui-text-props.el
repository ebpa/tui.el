(require 'tui-test-helper "test/tui-test-helper.el")
(require 'tui)
(require 'tui-text-props)

(describe "tui--extend-text-props")

(describe "tui--apply-grouped-text-props"
  (it "merges empty sets"
    (expect (tui--apply-grouped-text-props nil '(nil nil nil nil)) :to-be nil))
  (it "merges values with expected inheritance"
    ;; TODO: need a plist-equal function to make this more robust...
    (expect (tui--apply-grouped-text-props
             '(:a 1 :b "foo" :c "baz")
             '((:b "bar")(:a 0)(:a 2)(:c "bat" :d "blah")))
            :to-equal
            '(:d "blah" :c "baz" :b "bar" :a (0 1 2)))))

(describe "inheritance of rendered elements"
  (it "applies basic inheritance"
    (let* ((child (tui-span "foo")))
      (tui-with-rendered-element
        (tui-div
         :text-props '(:a 1)
         child "bar")
        (expect (get-text-property (tui-start tui-element) :a) :to-equal 1))))
  (it "applies safe inheritance properties"
    (let* ((child (tui-span :text-props-safe '(:a 0) "foo")))
      (tui-with-rendered-element
        (tui-div
         :text-props '(:a 1)
         child "bar")
        (expect (get-text-property (tui-start tui-element) :a) :to-equal 1))))
  (it "applies replace inheritance properties"
    (let* ((child (tui-span :text-props-replace '(:a 0) "foo")))
      (tui-with-rendered-element
        (tui-div
         :text-props '(:a 1)
         child "bar")
        (expect (get-text-property (tui-start tui-element) :a) :to-equal 0))))
  (it "applies push inheritance properties"
    (let* ((child (tui-span :text-props-push '(:a 0) "foo")))
      (tui-with-rendered-element
        (tui-div
         :text-props '(:a 1)
         child "bar")
        (expect (get-text-property (tui-start tui-element) :a) :to-equal '(0 1)))))
  (it "applies append inheritance properties"
    (let* ((child (tui-span :text-props-append '(:a 2) "foo")))
      (tui-with-rendered-element
        (tui-div
         :text-props '(:a 1)
         child "bar")
        (expect (get-text-property (tui-start tui-element) :a) :to-equal '(1 2)))))
  (it "merges values with expected inheritance"))
