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
      (expect (tui-render-to-string container) :to-equal "\n foo   bar    baz\n")))
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
      (expect (tui-render-to-string container) :to-equal "\nfoo\n\nbar\n\n\nbaz\n")))
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
      (expect (tui-render-to-string container) :to-equal
              (concat "\n"
                      (s-join "\n"
                              (list "+---+ +---+"
                                    "|   | |   |"
                                    "+---+ +---+"))
                      "\n"))))
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
      (expect (tui-render-to-string container) :to-equal
              (concat "\n"
                      (s-join "\n"
                              (list "+---+"
                                    "| +---+"
                                    "+-|   |"
                                    "  +---+"))
                      "\n"))))
  (it "can be nested within other tui-absolute-container elements"
    (expect (tui-render-to-string (tui-absolute-container
                                (tui-span
                                 :x 2
                                 :y 2
                                 " "
                                 (tui-absolute-container
                                  (tui-span
                                   :x 1
                                   :y 1
                                   "x")))))
            :to-equal "


   
  
   x
  
"))
  (it "updates to reflect nested tui-absolute-container elements"
    ))
