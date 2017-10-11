(require 'buttercup)
(require 'tui)
;; (require 'tui-div)
(require 'tui-heading "components/tui-heading.el")

(describe "tui--plist-changes"
  (it "identifies a changed value"
    (expect (tui--plist-changes '(:a 1 :b 2 :c 3)
                             '(:a 1 :b 4 :c 3))
            :to-equal '(:b 4))))

(describe "tui-diff"
  (it "is called on a basic property change"
    (spy-on 'tui--reconcile-content)
    (let ((div (tui-div "foo")))
      (tui-with-rendered-element div
        (tui--set-props div '(:content "bar"))
        (expect 'tui--reconcile-content :to-have-been-called))))
  
  (it "yields basic patches"
    (let ((patches (tui--diff (tui-create-element 'tui-div nil "foo")
                           (tui-create-element 'tui-heading nil "bar"))))
      (expect (length patches) :to-equal 1)
      (expect (car (first patches))
              :to-equal 'replace))
    (let ((patches (tui--diff (tui-heading "foo")
                           (tui-heading "bar"))))
      (expect (length patches) :to-equal 1)
      (expect (car (first patches))
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
