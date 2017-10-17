(require 'tui-test-helper "test/tui-test-helper.el")
(require 'tui-shared-size)
(require 'tui-fixed-width "components/tui-fixed-width.el")
(require 'tui-line "components/tui-line.el")

(describe "tui-shared-size"
  (it "can resize a single element"
    (let* ((shared-size (tui-shared-size-create))
           (content "The quick brown fox")
           (fixed-width-content (tui-fixed-width
                                 :width shared-size
                                 :content content)))
      (tui-with-rendered-element
        (tui-line fixed-width-content)
        (tui-recalculate-size shared-size)
        (expect (tui-visible-width fixed-width-content) :to-equal (tui-size shared-size)))))

  (it "can resize multiple elements")

  (it "can resize multiple adjacent elements")

  (it "works with other adjacent tui-shared-size elements"
    (let* ((content-elements '(("foo" "foobar" "a")
                               ("wasd" "b" "bazbat")))
           (column-sizes (list (tui-shared-size-create)
                               (tui-shared-size-create)
                               (tui-shared-size-create)))
           (content-components
            (list
             (list
              (tui-fixed-width :width (nth 0 column-sizes) :children (nth 0 (nth 0 content-elements)))
              (tui-fixed-width :width (nth 1 column-sizes) :children (nth 1 (nth 0 content-elements)))
              (tui-fixed-width :width (nth 2 column-sizes) :children (nth 2 (nth 0 content-elements))))
             (list
              (tui-fixed-width :width (nth 0 column-sizes) :children (nth 0 (nth 1 content-elements)))
              (tui-fixed-width :width (nth 1 column-sizes) :children (nth 1 (nth 1 content-elements)))
              (tui-fixed-width :width (nth 2 column-sizes) :children (nth 2 (nth 1 content-elements))))))
           (content (list (apply #'tui-line (nth 0 content-components))
                          (apply #'tui-line (nth 1 content-components)))))
      (tui-with-rendered-element
        content
        (mapc #'tui-recalculate-size column-sizes)
        ;;(buffer-substring-no-properties (point-min) (point-max))))
        ;;(oref (cl-first column-sizes) :element-sizes)))
        ;; (message "width: %S" (tui-visible-width (nth 0 (nth 0 content-components))))
        ;; (message "width: %S" (tui-visible-width (nth 1 (nth 0 content-components))))
        ;; (message "width: %S" (tui-visible-width (nth 2 (nth 0 content-components))))
        (expect (tui-visible-width (nth 0 (nth 0 content-components)))
                :to-equal (tui-visible-width (nth 0 (nth 1 content-components))))
        (expect (tui-visible-width (nth 1 (nth 0 content-components)))
                :to-equal (tui-visible-width (nth 1 (nth 1 content-components))))
        (expect (tui-visible-width (nth 2 (nth 0 content-components)))
                :to-equal (tui-visible-width (nth 2 (nth 1 content-components))))))))


;; (let ((my/width (tui-shared-size-create :element-sizes '(1 2 3))))
;;   (push 10 (oref my/width :element-widths))
;;   (tui-width my/width))

;; (progn
;;   (erase-buffer)
;;   (setq-local content-elements '(("foo" "foobar" "a")
;;                                  ("wasd" "b" "bazbat")))
;;   (setq-local column-sizes (list (tui-shared-size-create)
;;                                  (tui-shared-size-create)
;;                                  (tui-shared-size-create)))
;;   (setq-local content-components (list (list (tui-fixed-width :width (nth 0 column-sizes) :content (nth 0 (nth 0 content-elements)))
;;                                              (tui-fixed-width :width (nth 1 column-sizes) :content (nth 1 (nth 0 content-elements)))
;;                                              (tui-fixed-width :width (nth 2 column-sizes) :content (nth 2 (nth 0 content-elements))))
;;                                        (list (tui-fixed-width :width (nth 0 column-sizes) :content (nth 0 (nth 1 content-elements)))
;;                                              (tui-fixed-width :width (nth 1 column-sizes) :content (nth 1 (nth 1 content-elements)))
;;                                              (tui-fixed-width :width (nth 2 column-sizes) :content (nth 2 (nth 1 content-elements))))))
;;   (setq-local content (list (apply #'tui-line (nth 0 content-components))
;;                             (apply #'tui-line (nth 1 content-components))))

;;   (tui-render-element content)
;;   (mapc #'tui-recalculate-size column-sizes))

(provide 'tui-shared-size-test)
