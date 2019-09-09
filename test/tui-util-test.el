(require 'buttercup)
(require 'tui-test-helper "test/tui-test-helper.el")
(require 'cl)
(require 'tui-core)

(describe "tui-put-text-properties"
  (describe "inheritance rules"
    (describe "replace"
      (it "replaces existing properties regardless of their policies"))
    (describe "push")
    (describe "append")
    (describe "safe"
      (it "applies properties")
      (it "does not affect existing properties"))))

(describe "tui--plist-delete"
  (it "doesn't alter the list if a key doesn't exist"
    (let* ((foo '(:a 1 :b 2 :c 3))
           (bar (tui--plist-delete foo :d)))
      (expect foo :to-equal bar)))
  (it "deletes a single property"
    (let* ((foo '(:a 1 :b 2 :c 3))
           (bar (tui--plist-delete foo :b)))
      (expect (plist-get bar :a) :to-equal 1)
      (expect (plist-get bar :b) :to-equal nil)
      (expect (plist-member bar :b) :to-equal nil)
      (expect (plist-get bar :c) :to-equal 3)))
  (it "deletes multiple properties"
    (let* ((foo '(:a 1 :b 2 :c 3))
           (bar (tui--plist-delete foo :b :a)))
      (expect (plist-get bar :a) :to-equal nil)
      (expect (plist-member bar :a) :to-equal nil)
      (expect (plist-get bar :b) :to-equal nil)
      (expect (plist-member bar :b) :to-equal nil)
      (expect (plist-get bar :c) :to-equal 3))))

(describe "tui--plist-equal"
  (it "judges equality independent of order"
    (expect (tui--plist-equal '(:a 1 :b 2) '(:b 2 :a 1)) :to-be t)
    (expect (tui--plist-equal '(:a 1 :b 2) '(:b 2 :a 1 :c 3)) :to-be nil)
    (expect (tui--plist-equal '(:a 1 :b 2 :c 3) '(:b 2 :a 1)) :to-be nil)))

(describe "tui-unintern"
  (before-each
    (tui-define-component test-component
      :get-initial-state
      (lambda ()
        (list :foo "bar"))
      :render
      (lambda ()
        "test component"))
    (tui-unintern 'test-component))
  (it "has an interactive form"
    (expect (interactive-form 'tui-unintern)))
  (it "removes component constructor"
    (expect (symbol-function 'test-component) :to-be nil))
  (xit "removes the struct definition"
    (expect (cl--find-class 'test-component) :to-be nil))
  (it "removes generic methods"
    (expect
     (-none-p
      (lambda (generic)
        (equal 'test-component (car (cl--generic-method-specializers generic))))
      (cl--generic-method-table (cl-generic-ensure-function 'tui-get-initial-state))))))

(describe "tui-equal"
  
  (it "behaves like equal for non-lisp, non-tui types"
    (expect (tui-equal nil nil) :to-be t)
    (expect (tui-equal "foo" "foo") :to-be t)
    (expect (tui-equal "2" 2) :to-be nil)
    (expect (tui-equal "foo" nil) :to-be nil))
  (it "fresh objects without children are identical"
    (expect (tui-equal (tui-div) (tui-div)))))

(describe "tui--plist-changes"
  (it "compares empty lists"
    (expect (tui--plist-changes nil nil) :to-be nil))
  (it "identifies differences"
    (expect (tui--plist-changes '(:a 1 :b 2 :c 3)
                             '(:a 1 :b 4 :c 3))
            :to-equal '(:b 4))
    (expect (tui--plist-changes '(:a 1 :b 2) '(:a 2 :b 2 :c 2))
            :to-equal '(:a 2 :c 2)))
  ;; TODO: reconsider this behavior?
  (it "uses tui-equal to compare items"
    (spy-on 'tui-equal)
    (expect (tui--plist-changes '(:a (1 2 3)) '(:a (1 2 3)))
            :to-equal '(:a (1 2 3)))
    (expect 'tui-equal :to-have-been-called)))

(describe "tui--type"
  (it "returns nil for an unknown object type"
    (expect (tui--type nil) :to-be nil)
    (expect (tui--type '(1)) :to-be nil)
    (expect (tui--type [1]) :to-be nil)))
