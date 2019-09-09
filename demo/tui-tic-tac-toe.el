;;; tui-tic-tac-toe.el --- A demo implementation of Tic-Tac-Toe  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'tui-absolute-container)
(require 'tui-buffer)
(require 'tui-button)
(require 'tui-div)
(require 'tui-ol)

;; Design reference (for monospaced fonts)

;;  ⌜ ⌝ ⌜ ⌝ ⌜ ⌝
;;   X   O   X    Next player: X
;;  ⌞ ⌟ ⌞ ⌟ ⌞ ⌟
;;  ⌜ ⌝ ⌜ ⌝ ⌜ ⌝   1. Go to Game start
;;   X   O   X    2. Go to Move #1
;;  ⌞ ⌟ ⌞ ⌟ ⌞ ⌟   3. Go to Move #2
;;  ⌜ ⌝ ⌜ ⌝ ⌜ ⌝
;;   X   O   X
;;  ⌞ ⌟ ⌞ ⌟ ⌞ ⌟


(tui-defun-2 tui-tic-tac-toe-cell (value i)
  "Build a tic-tac-toe cell containing VALUE for cell I with appropriate absolute x,y positioning."
  (tui-button
   :max-width 3
   :max-height 3
   :face nil
   :action `(lambda (event)
              (interactive "e")
              (-when-let* ((game (tui-get-element-at (posn-point (event-start event)) 'tui-tic-tac-toe-game)))
                (tui-tic-tac-toe--handle-click game ,i)))
   (concat
    "⌜ ⌝\n"
    (format " %s \n" (or value " "))
    "⌞ ⌟\n")))

(tui-defun-2 tui-tic-tac-toe-board (squares &this this)
  "Render Tic-Tac-Toe board."
  (tui-absolute-container
   :children
   (cl-loop for i from 0 to 8
            collect
            (tui-tic-tac-toe-cell
             :x (* (% i 3) 4)
             :y (* (/ i 3) 3)
             :value (nth i squares) :i i))))

(tui-define-component tui-tic-tac-toe-game
  :documentation "A demo implementation of Tic-Tac-Toe.

Basedon on Dan Abramov's Tic-Tac-Toe tutorial for React at https://codepen.io/gaearon/pen/gWWZgR?editors=0010."
  :state-documentation
  (:history "Representation of the current board state and the board state of all previous turns in the current game."
            :x-is-next "Truthy if 'X' is the next player to play.")
  :get-initial-state
  (lambda (_)
    (list :history (list (make-list 9 nil))
          :x-is-next t))
  :render
  (lambda (this)
    (tui-let (&state history x-is-next)
      (let* ((step-number (- (length history) 1))
             (current (nth step-number history))
             (winner (tui-tic-tac-toe-calculate-winner current)))
        (tui-absolute-container
         (tui-tic-tac-toe-board
          :x 2
          :y 3
          :width 11
          :height 9
          :squares (copy-list current))
         (tui-div
          :x 17
          :y 2
          :width 20
          :height 12
          (if winner
              (tui-div (concat "Winner: " winner))
            (tui-div (concat
                      "Next player: "
                      (if x-is-next "X" "O")
                      "        ")))
          "\n"
          (tui-ol
           :children
           (-map-indexed
            (-lambda (move squares)
              (let* ((desc (if (> move 0)
                               (format "Go to move #%d" move)
                             "Go to game start")))
                (tui-button
                 :action `(lambda (event)
                            (interactive "e")
                            (-when-let* ((game (tui-get-element-at
                                                (posn-point (event-start event))
                                                'tui-tic-tac-toe-game)))
                              (tui-tic-tac-toe-jump-to game ,move)))
                 desc)))
            history))))))))

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
      (tui--save-point-row-column
       (tui--set-state
        game
        (list :history (append history
                               (list squares))
              :x-is-next (not (plist-get state :x-is-next))))))))

(defun tui-tic-tac-toe-jump-to (game turn-number)
  "Roll back GAME state to TURN-NUMBER."
  (let* ((state (cl-copy-list (tui--get-state game)))
         (history (plist-get state :history)))
    (tui--save-point-row-column
     (tui--set-state game
                     (list :history (cl-subseq history 0 (+ 1 turn-number))
                           :x-is-next (= (% turn-number 2) 0))))))

;;;###autoload
(defun tui-tic-tac-toe-insert-game ()
  "Render Tic-Tac-Toe game at point."
  (interactive)
  (tui-render-element
   (tui-tic-tac-toe-game)))

;;;###autoload
(defun tui-play-tic-tac-toe ()
  "Play a game of Tic Tac Toe."
  (interactive)
  (let* ((buffer (get-buffer-create "*Tic Tac Toe*")))
    (tui-render-element
     (tui-buffer
      :buffer buffer
      (tui-tic-tac-toe-game)))
    (switch-to-buffer buffer)))

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
