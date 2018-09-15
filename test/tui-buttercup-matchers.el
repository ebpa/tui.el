(buttercup-define-matcher :tui/to-equal (a b)
  (cl-destructuring-bind
      ((a-expr . a) (b-expr . b))
      (mapcar #'buttercup--expr-and-value (list a b))
    (if (equal a b)
        t
      (cons nil (format "Expected `%s' to equal:\n====\n%s\n====\nBut received:\n====\n%s\n====\n"
                        a-expr b a)))))

(provide 'tui-buttercup-matchers)
