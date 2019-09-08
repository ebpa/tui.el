(require 'buttercup)

(require 'tui-test-helper "test/tui-test-helper.el")
(require 'tui-div "components/tui-div.el")

(describe "tui-div"
  (it "renders w/leading and following newlines"
    (expect (tui-render-to-string (tui-div "foo"))
            :to-equal "\nfoo\n")))
