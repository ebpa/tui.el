(require 'buttercup)
(require 'tui-test-helper "test/tui-test-helper.el")
(require 'tui)
(require 'tui-text-props)

(describe "tui--extend-text-props"
  (it "merges empty sets"
    (expect (tui--extend-text-props nil '(nil nil nil nil)) :to-be nil))
  (it "merges values with expected inheritance"
    ;; TODO: need a plist-equal function to make this more robust...
    (expect (tui--plist-equal
             (tui--extend-text-props
              '(:a 1 :b "foo" :c "baz")
              '((:b "bar")(:a 0)(:a 2)(:c "bat" :d "blah")))
             '(:d "blah" :c "baz" :b "bar" :a (0 1 2))))))

(describe "tui--get-grouped-text-props"
  (it "should return a list of length 4"
    (expect (length (tui--get-grouped-text-props (tui-span :text-props-push '(:a 0) "foo"))) :to-be 4))
  (it "should return a properly grouped values"
    (-let* ((span (tui-span
                   :text-props-push '(:a 1)
                   :text-props-replace '(:b 2)
                   :text-props-append '(:c 3)
                   :text-props-safe '(:d 4)
                   "foo"))
            ((replace push append safe) (tui--get-grouped-text-props span)))
      (expect push :to-equal '(:a 1))
      (expect replace :to-equal '(:b 2))
      (expect append :to-equal '(:c 3))
      (expect safe :to-equal '(:d 4)))))

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
        (expect (get-text-property (tui-start child) :a) :to-equal 0))))
  (it "applies push inheritance properties"
    (let* ((child (tui-span :text-props-push '(:a 0) "foo"))
           (div (tui-div
                 :text-props '(:a 1)
                 child "bar")))
      (tui-with-rendered-element
        div
        (expect (get-text-property (tui-start child) :a) :to-equal '(0 1)))))
  (it "applies append inheritance properties"
    (let* ((child (tui-span :text-props-append '(:a 2) "foo")))
      (tui-with-rendered-element
        (tui-div
         :text-props '(:a 1)
         child "bar")
        (expect (get-text-property (tui-start child) :a) :to-equal '(1 2)))))
  (it "merges values with expected inheritance"))
