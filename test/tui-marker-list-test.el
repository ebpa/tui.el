(require 'tui-test-helper "test/tui-test-helper.el")
(require 'tui-marker-list)

(describe "tui-marker-list-create"
  (it "returns a marker list"
    (expect (tui-marker-list-p (tui-marker-list-create))))
  (it "accepts an initial list of markers"
    (with-temp-buffer
      (let* ((a (point-marker))
             (b (progn (insert "foo")
                       (point-marker)))
             (c (progn (insert "bar")
                       (point-marker)))
             (marker-list (tui-marker-list-create (list a b c))))
        (expect (tui-marker-list-p marker-list))
        (expect (tui-marker-list-length marker-list) :to-be 3)
        (expect (tui-marker-list-markers marker-list) :to-equal (list a b c)))))
  (it "the initial list may contain coincident markers"
    (with-temp-buffer
      (let* ((a (point-marker))
             (b (point-marker))
             (c (point-marker))
             (marker-list (tui-marker-list-create (list a b c))))
        (expect (tui-marker-list-p marker-list))
        (expect (tui-marker-list-length marker-list) :to-be 3)
        (expect (tui-marker-list-markers marker-list) :to-equal (list a b c))))))

(describe "tui-marker-list-insert"
  (it "inserts markers"
    (with-temp-buffer
      (let ((marker-list (tui-marker-list-create))
            (a (point-marker)))
        (tui-marker-list-insert marker-list a)
        (expect (tui-marker-list-markers marker-list) :to-equal (list a)))))
  (it "returns a marker node"
    (with-temp-buffer
      (let ((marker-list (tui-marker-list-create))
            (a (point-marker)))
        (expect (tui-marker-list-node-p (tui-marker-list-insert marker-list a))))))
  (xit "rejects markers from other buffers"
    (with-temp-buffer
      (let* ((marker-list (tui-marker-list-create))
             (alt-buffer (get-buffer-create "another-buffer"))
             (alt-buffer-marker (with-current-buffer alt-buffer
                                  (point-marker))))
        (tui-marker-list-insert marker-list 1)
        ;; TODO: improper spec for buttercup?
        (expect (tui-marker-list-insert marker-list alt-buffer-marker) :to-throw 'error))))
  (it "accepts buffer positions"
    (with-temp-buffer
      (insert "foo")
      (let* ((marker-list (tui-marker-list-create))
             (a (tui-marker-list-insert marker-list 1))
             (b (tui-marker-list-insert marker-list 3)))
        (expect (marker-position (tui-marker-list-node-marker a)) :to-be 1)
        (expect (marker-position (tui-marker-list-node-marker b)) :to-be 3))))
  (it "applies buffer positions to the marker-list buffer"
    (let* ((marker-list (tui-marker-list-create))
           marker-a marker-b)
      (with-temp-buffer
        (insert "foo")
        (setq marker-a (tui-marker-list-node-marker (tui-marker-list-insert marker-list 1)))
        (with-temp-buffer 
          (setq marker-b (tui-marker-list-node-marker (tui-marker-list-insert marker-list 3))))
        (expect (marker-buffer marker-a) :to-be (marker-buffer marker-b)))))
  (xit "rejects invalid buffer positions"
    (with-temp-buffer
      (insert "foo")
      (let ((marker-list (tui-marker-list-create)))
        ;; TODO: improper spec for buttercup?
        (expect (tui-marker-list-insert marker-list 5) :to-throw 'error))))
  (it "rejects coincident markers")
  (it "accepts markers from different buffers after the list has been cleared"))

(describe "tui-marker-list-remove"
  (it "can remove the first marker"
    (with-temp-buffer
      (let* ((a (point-marker))
             (b (point-marker))
             (c (point-marker))
             (marker-list (tui-marker-list-create (list a b c))))
        (tui-marker-list-remove marker-list a)
        (expect (tui-marker-list-p marker-list))
        (expect (tui-marker-list-length marker-list) :to-be 2)
        (expect (tui-marker-list-markers marker-list) :to-equal (list b c)))))
  (it "can remove an interior marker"
    (with-temp-buffer
      (let* ((a (point-marker))
             (b (point-marker))
             (c (point-marker))
             (marker-list (tui-marker-list-create (list a b c))))
        (tui-marker-list-remove marker-list b)
        (expect (tui-marker-list-p marker-list))
        (expect (tui-marker-list-length marker-list) :to-be 2)
        (expect (tui-marker-list-markers marker-list) :to-equal (list a c)))))
  (it "can remove the last marker"
    (with-temp-buffer
      (let* ((a (point-marker))
             (b (point-marker))
             (c (point-marker))
             (marker-list (tui-marker-list-create (list a b c))))
        (tui-marker-list-remove marker-list c)
        (expect (tui-marker-list-p marker-list))
        (expect (tui-marker-list-length marker-list) :to-be 2)
        (expect (tui-marker-list-markers marker-list) :to-equal (list a b)))))
  (it "can remove the only marker in a list"
    (with-temp-buffer
      (let* ((a (point-marker))
             (marker-list (tui-marker-list-create (list a))))
        (tui-marker-list-remove marker-list a)
        (expect (tui-marker-list-p marker-list))
        (expect (tui-marker-list-length marker-list) :to-be 0)
        (expect (tui-marker-list-markers marker-list) :to-equal nil)))))

(describe "tui-marker-list-split-marker"
  (it "split markers are distinct, but have the same position"
    (with-temp-buffer
      (insert "foobar")
      (-let* ((marker-list (tui-marker-list-create))
              (node (tui-marker-list-insert marker-list 3))
              ((left right) (tui-marker-list-split-marker marker-list 3)))
        (expect left :not :to-be right)
        (expect left :to-equal right)
        (expect (tui-marker-list-valid-p marker-list)))))
  (it "performs multiple splits"
    (with-temp-buffer
      (-let* ((a (point-marker))
              (b (point-marker))
              (marker-list (tui-marker-list-create (list a b)))
              (splits (tui-marker-list-split-marker marker-list a 4)))
        (expect (length splits) :to-be 5)
        (expect (length (tui-marker-list--all-nodes marker-list)) :to-be 6)
        (expect (tui-marker-list-valid-p marker-list)))))
  (it "performs multiples splits on nodes"
    (with-temp-buffer
      (-let* ((a (point-marker))
              (b (point-marker))
              (marker-list (tui-marker-list-create))
              (node-a (tui-marker-list-insert marker-list a))
              (node-b (tui-marker-list-insert marker-list b))
              (splits (tui-marker-list-split-node marker-list node-a 4)))
        (expect (length splits) :to-be 5)
        (expect (length (tui-marker-list--all-nodes marker-list)) :to-be 6)
        (expect (tui-marker-list-valid-p marker-list))))))

(describe "tui-marker-list-open-segment"
  (it "sets the insertion type for identified nodes"
    (with-temp-buffer
      (let* ((a (point-marker))
             (b (point-marker))
             (c (point-marker))
             (d (progn (insert "foo")
                       (point-marker)))
             (marker-list (tui-marker-list-create (list a b c d))))
        (tui-marker-list-open-segment marker-list a b)
        (expect (mapcar #'marker-insertion-type (tui-marker-list-markers marker-list)) :to-equal (list nil t t nil)))))
  (it "sets the insertion type for all adjacent coincident nodes"
    (with-temp-buffer
      (let* ((a (point-marker))
             (b (point-marker))
             (c (point-marker))
             (d (point-marker))
             (marker-list (tui-marker-list-create (list a b c d))))
        (tui-marker-list-open-segment marker-list a b)
        (expect (mapcar #'marker-insertion-type (tui-marker-list-markers marker-list)) :to-equal (list nil t t t))))))

(describe "tui-marker-list-next"
  (it "returns the next marker"
    (with-temp-buffer
      (let* ((a (progn (insert "foo")
                       (point-marker)))
             (b (progn (insert "bar")
                       (point-marker)))
             (marker-list (tui-marker-list-create (list a b))))
        (expect (tui-marker-list-next-marker marker-list a) :to-equal b)))))

(describe "tui-marker-list-previous"
  (it "returns the previous marker"
    (with-temp-buffer
      (let* ((a (progn (insert "foo")
                       (point-marker)))
             (b (progn (insert "bar")
                       (point-marker)))
             (marker-list (tui-marker-list-create (list a b))))
        (expect (tui-marker-list-prev-marker marker-list a) :to-equal b)))))

(describe "tui-marker-list-move-segment"
  (it "can move a segment that doesn't contain any markers"
    (with-temp-buffer
      (-let* ((a (point-marker))
              (b (progn (insert "bar")
                        (point-marker)))
              (c (progn (insert "foo")
                        (point-marker)))
              (marker-list (tui-marker-list-create (list a b c)))
              ((target-start target-end) (tui-marker-list-split-marker marker-list a)))
        (tui-marker-list-move-segment marker-list b c target-start target-end)
        (expect (buffer-string) :to-equal "foobar")
        (expect (tui-marker-list-markers marker-list) :to-equal (list target-start target-end b c)))))
  (it "can move a segment that contains multiple markers"
    (with-temp-buffer
      (-let* ((a (prog1 (point-marker)
                   (insert "foo")))
              (b (prog1 (point-marker)
                   (insert "bar ")))
              (c (prog1 (point-marker)
                   (insert "(1")))
              (d (prog1 (point-marker)
                   (insert " 2 ")))
              (e (prog1 (point-marker)
                   (insert "3)")))
              (f (prog1 (point-marker)
                   (insert ".")))
              (g (point-marker))
              (marker-list (tui-marker-list-create (list a b c d e f g)))
              ((target-start target-end) (tui-marker-list-split-marker marker-list b)))
        (tui-marker-list-move-segment marker-list c f target-start target-end)
        (expect (buffer-string) :to-equal "foo(1 2 3)bar .")
        (expect (tui-marker-list-markers marker-list) :to-equal (list a target-start d e target-end c f g))))))

(describe "tui-marker-list-nodes-in-range"
  (it "returns adjacent nodes"
    (with-temp-buffer
      (-let* ((a (prog1 (point-marker)
                   (insert "foo")))
              (b (point-marker))
              (marker-list (tui-marker-list-create (list a b)))
              (nodes (tui-marker-list-markers-in-range marker-list a b)))
        (expect (cl-first nodes) :to-be a)
        (expect (cl-second nodes) :to-be b))))
  (it "returns non-adjacent nodes"
    (with-temp-buffer
      (-let* ((a (prog1 (point-marker)
                   (insert "foo")))
              (b (prog1 (point-marker)
                   (insert "bar ")))
              (c (point-marker))
              (marker-list (tui-marker-list-create (list a b c)))
              (nodes (tui-marker-list-markers-in-range marker-list a c)))
        (expect (nth 0 nodes) :to-be a)
        (expect (nth 1 nodes) :to-be b)
        (expect (nth 2 nodes) :to-be c)))))

(describe "tui-marker-list-nodes-between"
  (it "find no nodes between adjacent nodes"
    (with-temp-buffer
      (-let* ((a (prog1 (point-marker)
                   (insert "foo")))
              (b (point-marker))
              (marker-list (tui-marker-list-create (list a b))))
        (expect (car (tui-marker-list-markers-between marker-list a b)) :to-be nil))))
  (it "find nodes between non-adjacent nodes"
    (with-temp-buffer
      (-let* ((a (prog1 (point-marker)
                   (insert "foo")))
              (b (prog1 (point-marker)
                   (insert "bar ")))
              (c (point-marker))
              (marker-list (tui-marker-list-create (list a b c))))
        (expect (car (tui-marker-list-markers-between marker-list a c)) :to-be b)))))
