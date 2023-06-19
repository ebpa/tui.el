(defmacro tui-with-content-reported-error (body)
  "Helper for trapping and reporting errors inline as an element."
  `(condition-case err
       ,body
     (t (tui-span
         :text-props-push `(help-echo ,(prin1-to-string err))
         "(error)"))))

(provide 'tui-errors)
