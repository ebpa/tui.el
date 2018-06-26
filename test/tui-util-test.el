(require 'buttercup)
(require 'tui-test-helper "test/tui-test-helper.el")

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
