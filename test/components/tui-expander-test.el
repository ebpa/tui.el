(require 'tui-test-helper "test/tui-test-helper.el")
(require 's)
(require 'tui-buffer "components/tui-buffer.el")
(require 'tui-expander "components/tui-expander.el")

(describe "tui-expander"
  (xit "is initially expanded by default"
    (let ((expander (tui-expander
                     :header "header"
                     :children "content")))
      (tui-with-rendered-element expander
        (expect (s-contains-p "header" (buffer-string)))
        (expect (s-contains-p "content" (buffer-string))))))

  (xit "hides content when collapsed"
    (let ((expander (tui-expander
                     :header "header"
                     :children "content")))
      (tui-with-rendered-element expander
        (tui-expander--collapse expander)
        ;;(buffer-substring (point-min) (point-max))))
        (expect (s-contains-p "header" (buffer-string)))
        (expect (not (s-contains-p "content" (buffer-string)))))))
  
  (xit "has idempotent expand and collapse functions"
    (let ((expander (tui-expander
                     :header "header"
                     :children "content")))
      (tui-with-rendered-element expander
        (expect (s-contains-p "content" (buffer-string)))
        (tui-expander--collapse expander)
        (expect (not (s-contains-p "content" (buffer-string))))
        (tui-expander--collapse expander)
        (expect (not (s-contains-p "content" (buffer-string))))
        (tui-expander--expand expander)
        (expect (s-contains-p "content" (buffer-string)))
        (tui-expander--expand expander)
        (expect (s-contains-p "content" (buffer-string))))))
  
  (xit "toggles expansion"
    (tui-with-rendered-element
      (tui-expander
       :header "header"
       :children "content")
      (expect (s-contains-p "content" (buffer-string)))
      (tui-expander-toggle-expansion tui-element)
      (expect (not (s-contains-p "content" (buffer-string))))
      (tui-expander-toggle-expansion tui-element)
      (expect (s-contains-p "content" (buffer-string))))))

(describe "nested expanders"
  (xit "inner expander expands and collapses independently of outer expander."
    (let* ((inner-expander (tui-expander :header (tui-line "inner-header")
                                      :children (tui-line "inner-content")))
           (outer-expander (tui-expander :header (tui-line "outer-header")
                                      :children
                                      (list 
                                       (tui-line
                                        "outer-content")
                                       inner-expander))))
      (tui-with-rendered-element outer-expander
        (expect (s-contains-p "outer-header" (buffer-string)))
        (expect (s-contains-p "outer-content" (buffer-string)))
        (expect (s-contains-p "inner-header" (buffer-string)))
        (expect (s-contains-p "inner-content" (buffer-string)))
        (tui-expander--collapse inner-expander)
        (expect (s-contains-p "outer-header" (buffer-string)))
        (expect (s-contains-p "outer-content" (buffer-string)))
        (expect (s-contains-p "inner-header" (buffer-string)))
        (expect (not (s-contains-p "inner-content" (buffer-string))))
        (tui-expander--expand inner-expander)
        (expect (s-contains-p "outer-header" (buffer-string)))
        (expect (s-contains-p "outer-content" (buffer-string)))
        (expect (s-contains-p "inner-header" (buffer-string)))
        (expect (s-contains-p "inner-content" (buffer-string)))
        ;;(tui-expander--collapse inner-expander)
        (tui-expander--collapse outer-expander)
        (expect (s-contains-p "outer-header" (buffer-string)))
        (expect (not (s-contains-p "outer-content" (buffer-string))))
        (expect (not (s-contains-p "inner-header" (buffer-string))))
        (expect (not (s-contains-p "inner-content" (buffer-string))))
        (tui-expander--collapse inner-expander)
        (expect (s-contains-p "outer-header" (buffer-string)))
        (expect (not (s-contains-p "outer-content" (buffer-string))))
        (expect (not (s-contains-p "inner-header" (buffer-string))))
        (expect (not (s-contains-p "inner-content" (buffer-string))))
        (tui-expander--expand outer-expander)
        (expect (s-contains-p "outer-header" (buffer-string)))
        (expect (s-contains-p "outer-content" (buffer-string)))
        (expect (s-contains-p "inner-header" (buffer-string)))
        (expect (not (s-contains-p "inner-content" (buffer-string))))
        ;; (tui-expander--expand outer-expander)
        ;; (expect (s-contains-p "outer-header" (buffer-string)))
        ;; (expect (s-contains-p "outer-content" (buffer-string)))
        ;; (expect (s-contains-p "inner-header" (buffer-string)))
        ;; (expect (s-contains-p "inner-content" (buffer-string)))
        )))

  (xit "should preserve child elements across collapse/expand")

  (xit "should preserve the expansion state of a nested expander"))
