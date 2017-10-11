;;; tui-tic-tac-toe.el --- A demo implementation of Tic-Tac-Toe
;;; Code:

(eval-when-compile (require 'cl-lib))

;;; Commentary:
;; 

(require 'tui)

;; Design (for monospaced fonts)

;;  ⌜ ⌝ ⌜ ⌝ ⌜ ⌝
;;   X   O   X    Next player: X
;;  ⌞ ⌟ ⌞ ⌟ ⌞ ⌟
;;  ⌜ ⌝ ⌜ ⌝ ⌜ ⌝   1. Go to Game start
;;   X   O   X    2. Go to Move #1
;;  ⌞ ⌟ ⌞ ⌟ ⌞ ⌟   3. Go to Move #2
;;  ⌜ ⌝ ⌜ ⌝ ⌜ ⌝
;;   X   O   X
;;  ⌞ ⌟ ⌞ ⌟ ⌞ ⌟


(defun tui-tic-tac-toe--square (squares i)
  "Build a tic-tac-toe cell from SQUARES for cell I."
  (tui-absolute
   :width 3
   :height 3
   :x (* (% i 3) 4)
   :y (* (/ i 3) 3)
   (tui-button
    :action `(lambda (event)
               (interactive "e")
               (let ((game (tui-get-element-at (posn-point (event-start event)) 'tui-tic-tac-toe-game)))
                 (tui-tic-tac-toe--handle-click game ,i)))
    "⌜ ⌝\n"
    (format " %s \n" (or (nth i squares) " "))
    "⌞ ⌟\n")))

(tui-define-component tui-tic-tac-toe-board
  :render
  (lambda ()
    (let ((squares (plist-get (tui-get-props) :squares)))
      ;; (when (-any-p #'identity squares)
      ;;   (edebug))
      (list
       (tui-tic-tac-toe--square squares 0)
       (tui-tic-tac-toe--square squares 1)
       (tui-tic-tac-toe--square squares 2)
       (tui-tic-tac-toe--square squares 3)
       (tui-tic-tac-toe--square squares 4)
       (tui-tic-tac-toe--square squares 5)
       (tui-tic-tac-toe--square squares 6)
       (tui-tic-tac-toe--square squares 7)
       (tui-tic-tac-toe--square squares 8)))))

(tui-define-component tui-tic-tac-toe-game
  :documentation "A demo implementation of Tic-Tac-Toe.

Basedon on Dan Abramov's Tic-Tac-Toe tutorial for React at https://codepen.io/gaearon/pen/gWWZgR?editors=0010."
  :get-initial-state
  (lambda ()
    (list :history (list (make-list 9 nil))
          :x-is-next t))
  :render
  (lambda ()
    (let* ((state (tui-get-state))
           (history (plist-get state :history))
           (step-number (- (length history) 1))
           (current (nth step-number history))
           (winner (tui-tic-tac-toe-calculate-winner current))
           (x-is-next (plist-get state :x-is-next)))
      (list
       (tui-absolute
        :x 2
        :y 3
        :width 11
        :height 9
        (tui-tic-tac-toe-board
         :squares current))
       (tui-absolute
        :width 20
        :height 12
        :x 17
        :y 2
        (tui-div
         (if winner
             (list "Winner: " winner)
           (list "Next player: "
                 (if x-is-next "X" "O")
                 "        ")))
        "\n\n"
        (tui-ol
         :children
         (-map-indexed
          (-lambda (move squares)
            (let ((desc (if (> move 0)
                            (format "Go to move #%d" move)
                          "Go to game start")))
              (tui-button
               :action `(lambda (event)
                          (interactive "e")
                          (let ((game (tui-get-element-at
                                       (posn-point (event-start event))
                                       'tui-tic-tac-toe-game)))
                            (tui-tic-tac-toe-jump-to game ,move)))
               desc)))
          history)))))))

(defun tui-tic-tac-toe--handle-click (game i)
  "Handle mouse click for GAME board cell I."
  (let* ((state (tui--get-state game))
         (history (plist-get state :history))
         (current (-last-item history))
         (squares (cl-copy-list current)))
    (unless (or (tui-tic-tac-toe-calculate-winner squares)
                (nth i squares))
      (setf (nth i squares)
            (if (plist-get state :x-is-next) "X" "O"))
      (tui--set-state
       game
       (list :history (append history
                              (list squares))
             :x-is-next (not (plist-get state :x-is-next)))))))

(defun tui-tic-tac-toe-jump-to (game turn-number)
  "Roll back GAME state to TURN-NUMBER."
  (let* ((state (cl-copy-list (tui--get-state game)))
         (history (plist-get state :history)))
    (tui--set-state game
                 (list :history (cl-subseq history 0 (+ 1 turn-number))
                       :x-is-next (= (% turn-number 2) 0)))))

(defun tui-tic-tac-toe-insert-game ()
  "Render Tic-Tac-Toe game at point."
  (interactive)
  (tui-render-element
   (tui-tic-tac-toe-game)))

(defun tui-tic-tac-toe-calculate-winner (squares)
  "Calculate whether the state of SQUARES yields a winner."
  (let ((lines '((0 1 2)
                 (3 4 5)
                 (6 7 8)
                 (0 3 6)
                 (1 4 7)
                 (2 5 8)
                 (0 4 8)
                 (2 4 6))))
    (cl-loop for (a b c) in lines
             if (and (nth a squares)
                     (equal (nth a squares)
                            (nth b squares))
                     (equal (nth a squares)
                            (nth c squares)))
             return (nth a squares))))

(provide 'tui-tic-tac-toe)

;;; tui-tic-tac-toe.el ends here
