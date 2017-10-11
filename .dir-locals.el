;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (eval progn
        (require 'projectile)
        (puthash
         (projectile-project-root)
         "cask exec buttercup -L ." projectile-test-cmd-map))
  (nameless-current-name . "tui")))

