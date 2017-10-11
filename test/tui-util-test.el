(require 'buttercup)

(describe "tui-put-text-properties"
  (describe "inheritance rules"
    (describe "replace"
      (it "replaces existing properties regardless of their policies"))
    (describe "push")
    (describe "append")
    (describe "safe"
      (it "applies properties")
      (it "does not affect existing properties"))))
