(defvar tui-type-to-html-tag-mapping
  '((tui-div . div)
    (tui-span . span))
  "Alist of mappings from tui types to html tags.")

(defun tui-node-to-html-sexp (node)
  "Convert NODE to HTML s-expressions."
  (if (stringp node)
      node
    (let* ((type (tui--type node)))
      (apply #'list
             (or (assoc-default type tui-type-to-html-tag-mapping)
                 type)
             nil
             (mapcar
              #'tui-node-to-html-sexp
              (plist-get (tui-element-props node) :children))))))

(defun tui--sexp-to-html-string (sexp)
  "Encode HTML SEXP as an HTML string."
  (cond
   ((null sexp)
    "")
   ((stringp sexp)
    sexp)
   ((listp sexp)
    (let* ((tag (symbol-name (car sexp))))
      (format "<%s>%s</%s>"
              tag
              (mapconcat
               #'tui--sexp-to-html-string
               (cddr sexp)
               "")
              tag)))))

(defun tui-node-to-html-string (node)
  ""
  (tui--sexp-to-html-string
   (tui-node-to-html-sexp node)))

;; (tui-node-to-html-string ;tui-node-to-html-sexp
;;  (tui-div
;;   (tui-span "Hello, world!")))
;; "<div><span>Hello, world!</span></div>"

;; (div nil (span nil "Hello, world!"))

;; (tui-element-content
;;  (tui-span "Hello, world!"))

;; (div nil nil)

(defun my/lib-libxml-parse-html-string (str)
  "Convenience function for parsing an HTML string STR."
  (with-temp-buffer
    (insert str)
    (libxml-parse-html-region (point-min) (point-max))))

;; (my/lib-libxml-parse-html-string "<div>Hello, world!</div>")
;; (html nil (body nil (div nil "Hello, world!")))

(provide 'tui-html)
