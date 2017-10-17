(require 'tui-test-helper "test/tui-test-helper.el")
(require 'tui-fixed-width "components/tui-fixed-width.el")

(describe "tui-fixed-width"
  (describe "character-width specification"
    (it "pads a short element to a longer fixed width"
      (tui-with-rendered-element
        (tui-fixed-width
         :width '10
         "Hello!")
        (expect (- (point-max) (point-min)) :to-equal 10)))
    (it "renders an element with the samed fixed width"
      (expect (tui-with-rendered-element
                (tui-fixed-width
                 :width 13
                 "Hello, world!")
                (- (point-max) (point-min)))
              :to-equal 13))
    (it "truncates a long element to a shorter fixed width"
      (expect (tui-with-rendered-element
                (tui-fixed-width
                 :width 3
                 "Hey there world!")
                (- (point-max) (point-min)))
              :to-equal 3)))
  
  (describe "precise pixel specification"
    (xit "builds the desired width for content that includes a symbol"
      (expect (tui-with-rendered-element
                (tui-fixed-width
                 :width '(20)
                 (list
                  #("" 0 1 (font-lock-ignore t display (raise -0.24) face (:family "github-octicons" :height 1.2)))
                  " foo bar baz"))
                (tui-segment-pixel-width (point-min) (point-max)))
              :to-equal 20)))

  (describe "nested fixed-width elements")

  (describe "searching hidden values")

  (describe "customizable ellipsis behavior")

  (describe "expand/collapse (disabling truncation)")

  (describe "see full value on hover"))

;; (tui-render
;;  (list
;;   (tui-fixed-width
;;    :width '(54)
;;    "wooooooooooooooooooooooooo!")
;;   "|"
;;   (tui-fixed-width
;;    :width 10
;;    :children "foo")
;;   "|"
;;   (tui-fixed-width
;;    :width 10
;;    :children "blooh!")
;;   "|"
;;   (tui-fixed-width
;;    :width 10
;;    :children "bar")))
