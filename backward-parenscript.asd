(defsystem backward-parenscript
  :depends-on (parse-js optima fare-quasiquote-optima parenscript)
  :components ((:file "packages")
               (:file "main")))
