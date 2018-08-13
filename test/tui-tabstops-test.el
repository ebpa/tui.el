(require 'buttercup)
(require 'tui)

(describe "tabstops"
  (describe "tui-forward-tabstop"
    (it "does nothing (and doesn't error) if grid is empty"))
  (it "loop (back to the beginning)")
  (it "preserve editing state"
    ;; editing
    ;; tab
    ;; expect editing

    ;; (not editing)
    ;; tab
    ;; expect (not editing)
    )
  (it "create a new record when editing the last record of the last row and recordCreate is t")
  (it "loops properly when grid only has a single item")
  (it "tabs through footer cells")
  (it "tabs through header cells?"))

(describe "tui--target-row-offset"
  (progn
    (expect (tui--target-row-offset 5 2 -2) :to-equal 0)
    (expect (tui--target-row-offset 5 2 -3) :to-equal -1)
    (expect (tui--target-row-offset 5 2 -1) :to-equal 0)
    (expect (tui--target-row-offset 5 0 -1) :to-equal -1)
    (expect (tui--target-row-offset 5 0 0) :to-equal 0)
    (expect (tui--target-row-offset 5 0 1) :to-equal 0)))

(describe "tui--target-column-index"
  (progn
    (expect (tui--target-column-index 5 2 -2) :to-equal 0)
    (expect (tui--target-column-index 5 2 -3) :to-equal 4)
    (expect (tui--target-column-index 5 2 -1) :to-equal 1)
    (expect (tui--target-column-index 5 0 -1) :to-equal 4)
    (expect (tui--target-column-index 5 0 0) :to-equal 0)
    (expect (tui--target-column-index 5 0 1) :to-equal 1)))
