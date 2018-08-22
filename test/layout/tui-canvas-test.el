(require 'tui-canvas "layout/tui-canvas.el")
(require 'buttercup)

(describe "tui-canvas"
  (it "renders an blank component"
    (tui-with-rendered-element (tui-canvas)
      (expect (buffer-string) :to-equal "")))
  (it "renders components horizontally"
    (let* ((canvas (tui-canvas
                    (tui-span
                     :x 1 :y 0
                     "foo")
                    (tui-span
                     :x 7 :y 0
                     "bar")
                    (tui-span
                     :x 14 :y 0
                     "baz"))))
      (tui-with-rendered-element canvas
        (expect (buffer-substring-no-properties (point-min) (point-max)) :to-equal " foo   bar    baz"))))
  (it "renders components vertically"
    (let* ((canvas (tui-canvas
                    (tui-span
                     :x 0 :y 0
                     "foo")
                    (tui-span
                     :x 0 :y 2
                     "bar")
                    (tui-span
                     :x 0 :y 5
                     "baz"))))
      (tui-with-rendered-element canvas
        (expect (buffer-string) :to-equal "foo\n\nbar\n\n\nbaz"))))
  (it "renders multiline components horizontally"
    (let ((rectangle (s-join "\n"
                             (list "+---+"
                                   "|   |"
                                   "+---+")))
          (canvas (tui-canvas
                   (tui-span
                    :x 0 :y 0
                    rectangle)
                   (tui-span
                    :x 6 :y 0
                    rectangle))))
      (tui-with-rendered-element canvas
        (expect (buffer-string) :to-equal
                (s-join "\n"
                        (list "+---+ +---+"
                              "|   | |   |"
                              "+---+ +---+"))))))
  (it "renders overlapping components"
    (let ((rectangle (s-join "\n"
                             (list "+---+"
                                   "|   |"
                                   "+---+")))
          (canvas (tui-canvas
                   (tui-span
                    :x 0 :y 0
                    rectangle)
                   (tui-span
                    :x 2 :y 1
                    rectangle))))
      (tui-with-rendered-element canvas
        (expect (buffer-string) :to-equal
                (s-join "\n"
                        (list "+---+"
                              "| +---+"
                              "+-|   |"
                              "  +---+")))))))


