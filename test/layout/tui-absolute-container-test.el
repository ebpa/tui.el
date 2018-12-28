(require 'tui-absolute-container "layout/tui-absolute-container.el")
(require 'buttercup)
(require 'tui-buttercup-matchers "test/tui-buttercup-matchers.el")

(describe "tui-absolute-container"
  (it "renders an blank component"
    (tui-with-rendered-element (tui-absolute-container)
      (expect (buffer-string) :to-equal "\n\n")))
  (it "renders components horizontally"
    (let* ((container (tui-absolute-container
                       (tui-span
                        :x 1 :y 0
                        "foo")
                       (tui-span
                        :x 7 :y 0
                        "bar")
                       (tui-span
                        :x 14 :y 0
                        "baz"))))
      (tui-with-rendered-element container
        (expect (buffer-substring-no-properties (point-min) (point-max)) :to-equal "\n foo   bar    baz\n"))))
  (it "renders components vertically"
    (let* ((container (tui-absolute-container
                       (tui-span
                        :x 0 :y 0
                        "foo")
                       (tui-span
                        :x 0 :y 2
                        "bar")
                       (tui-span
                        :x 0 :y 5
                        "baz"))))
      (tui-with-rendered-element container
        (expect (buffer-string) :to-equal "\nfoo\n\nbar\n\n\nbaz\n"))))
  (it "renders multiline components horizontally"
    (let* ((rectangle (s-join "\n"
                              (list "+---+"
                                    "|   |"
                                    "+---+")))
           (container (tui-absolute-container
                       (tui-span
                        :x 0 :y 0
                        rectangle)
                       (tui-span
                        :x 6 :y 0
                        rectangle))))
      (tui-with-rendered-element container
        (expect (buffer-string) :to-equal
                (concat "\n"
                        (s-join "\n"
                                (list "+---+ +---+"
                                      "|   | |   |"
                                      "+---+ +---+"))
                        "\n")))))
  (it "renders overlapping components"
    (let* ((rectangle (s-join "\n"
                              (list "+---+"
                                    "|   |"
                                    "+---+")))
           (container (tui-absolute-container
                       (tui-span
                        :x 0 :y 0
                        rectangle)
                       (tui-span
                        :x 2 :y 1
                        rectangle))))
      (tui-with-rendered-element container
        (expect (buffer-string) :tui/to-equal
                (concat "\n"
                        (s-join "\n"
                                (list "+---+"
                                      "| +---+"
                                      "+-|   |"
                                      "  +---+"))
                        "\n"))))))
