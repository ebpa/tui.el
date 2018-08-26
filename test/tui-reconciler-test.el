(require 'tui-test-helper "test/tui-test-helper.el")
(require 'tui)
;; (require 'tui-div)
(require 'tui-heading "components/tui-heading.el")
(require 'buttercup)

(describe "tui-diff"
  (it "recognizes equivalent empty elements"
    (expect (tui--diff (tui-div) (tui-div)) :to-be nil))
  (it "recognizes equivalent elements with basic content"
    (expect (tui--diff (tui-div 1) (tui-div 1)) :to-be nil)
    (expect (tui--diff (tui-div "foo") (tui-div "foo")) :to-be nil))
  (it "identifies basic content differences"
    (let* ((operations (tui--diff (tui-div "foo") (tui-div "bar"))))
      (expect (length operations) :to-be 1)
      (expect (caar operations) :to-be 'update-props)))
  (it "identifies a nested content difference"
    (let* ((operations (tui--diff (tui-div (tui-span "foo")) (tui-div (tui-span "bar")))))
      (expect (length operations) :to-be 1)
      (expect (caar operations) :to-be 'update-props)
      (expect (tui--type (cadar operations)) :to-be 'tui-div)))
  (it "only updates an element for changed children if one of its children would not be reference-equal")
  (it "diffs a mounted element with a fresh element"
    (let* ((div (tui-div (tui-span 1))))
      (tui-with-rendered-element div
        (tui--diff div (tui-div (tui-span 1)))))))

(describe "tui-reconciler"
  (it "is called on a basic property change"
    (spy-on 'tui--reconcile-content)
    (let ((div (tui-div "foo")))
      (tui-with-rendered-element div
        (tui--set-props div '(:children "bar"))
        (expect 'tui--reconcile-content :to-have-been-called))))
  
  (it "yields basic patches"
    (let ((patches (tui--diff (tui-create-element 'tui-div nil "foo")
                           (tui-create-element 'tui-heading nil "bar"))))
      (expect (length patches) :to-equal 1)
      (expect (car (cl-first patches))
              :to-equal 'replace))
    (let ((patches (tui--diff (tui-heading "foo")
                           (tui-heading "bar"))))
      (expect (length patches) :to-equal 1)
      (expect (car (cl-first patches))
              :to-equal 'update-props))))

(describe "tui-patch"
  (describe "insert operation"
    (it "does a basic insert")
    (it "can fully rotate a list forward")
    (it "can fully rotate a list backwards"))

  (describe "remove operation"
    (it "can remove the last remaining element of a list"
      )
    (it "can remove an element from the beginning of a list")
    (it "can remove an alement from the end of a list")
    (it "can remove an element from the middle of a list")

    (it "can remove an only child element"))

  (describe "replace operation"
    (it "can replace an element at the beginning of a list")
    (it "can replace an element at the end of a list")
    (it "can replace an element in the middle of a list")

    (it "can remove replace an only child element"))

  (describe "change-props operation")

  (describe "reorder operation"))

(provide 'tui-diff-test)
