(in-package #:backward-parenscript)

(defun js-name-to-symbol (string)
  (intern
   (with-output-to-string (s)
     (loop
        for c across string
          do
          (when (upper-case-p c)
            (princ #\- s))
           (princ (char-upcase c) s)))))

(defgeneric convert-to-parenscript (j))

(defvar *last-parse* nil)

(defmethod convert-to-parenscript ((javascript-pathname pathname))
  (with-open-file (s (probe-file javascript-pathname))
    (convert-js-parse-tree-to-javascript 
     (setf *last-parse* (parse-js s)))))

(defmethod convert-to-parenscript ((javascript string))
  (convert-js-parse-tree-to-javascript 
   (setf *last-parse* (parse-js javascript))))

(defvar *last-conversion* nil)

(defun convert-js-parse-tree-to-javascript (parse)
  (labels
      ((TBD (&rest args)
         (declare (ignore args))
         (error "no rest of the wiki'd"))
       (op (op)
         (ecase op
           (:+ '+)
           (:- '-)
           (:* '*)
           (:< '<)
           ;;(:-- '--)
           ;;(:++ '++)
           ;;(:! '!)
           ;;(:~ '~)
           (:typeof 'typeof)
           (:void 'void)
           (:delete 'delete)))
       (binary (form &optional ps-op?)
         (ematch form
           (`(,_ ,op ,left ,right)
             `(,(if ps-op? ps-op? (op op)) ,(r left) ,(r right)))))
       (unary (form)
         (ematch form
           (`(,_ ,op ,arg)
             `(,(op op) ,(r arg)))))
       (r! (forms) (mapcar #'r forms))
       (r!-progn (forms)
         (help-let-forms
          (let ((f (r! forms)))
            (if (cdr f) `(progn ,@f) (car f)))))
       (r (x)
         (ematch x
           (`(:name ,txt)           (js-name-to-symbol txt))
           (`(:num ,num)            num)
           (`(:string ,s)           s)
           (`(:stat ,form)          (r form))

           (`(:call ,func ,args)    (tidy `(,(r func) ,@(r! args))))
           (`(:return ,form)        `(return ,(r form)))
           (`(:binary ,_ ,_ ,_)     (binary x))
           (`(:assign :- ,_ ,_)     (binary x 'decf))

           (`(:assign :+ ,_ ,_)     (binary x 'incf))
           (`(:assign := ,_ ,_)     (binary x 'setf))
           (`(:unary-prefix ,_ ,_)  (unary x))
           (`(:array ,elements)     `(list ,@(r! elements)))

           (`(:toplevel ,forms)     (r!-progn forms))
           (`(:seq ,forms)          (r!-progn forms))
           (`(:block ,forms)        (r!-progn forms))
           (`(:sub ,array ,index)   `(aref ,(r array) ,(r index)))

           (`(:dot ,x ,(guard slot (stringp slot)))
             (tidy
              `(@ ,(r x) ,(js-name-to-symbol slot))))

           (`(:function nil ,args ,forms)
             `(lambda ,(mapcar #'js-name-to-symbol args) ,@(r!-progn forms)))
           (`(:defun ,name ,args ,forms)
             `(defun ,(js-name-to-symbol name) ,(mapcar #'js-name-to-symbol args)
                ,@(r!-progn forms)))

           (`(:object ,field-alist)
             `(create ,@(loop
                           for (key . value) in field-alist
                           nconc `(,(js-name-to-symbol key) ,(r value)))))
           (`(:var ,bindings)
             `(let ,(loop 
                       for (var . init) in bindings
                       collect `(,(js-name-to-symbol var) ,(r init)))
                :helpme))

           (`(:if ,q ,a ,b)         `(if ,(r q) ,(r a) ,(r b)))
           (`(:if ,q ,a)            `(if ,(r q) ,(r a)))

           ;; That's all so far, pending:
           (`(:atom ,atom) (TBD atom))
           (`(:regexp ,expr ,flags) (TBD expr flags))
           (`(:unary-postfix ,op ,place) (TBD op place))
           (`(:conditional ,test ,then ,else) (TBD test then else))
           (`(:new ,func ,args) (TBD func args))
           (`(:label ,name ,form) (TBD name form))
           (`(:with ,obj ,body) (TBD obj body))
           (`(:return ,value) (TBD value))
           ('(:debugger) (TBD))
           (`(:try ,body ,catch ,finally)  (TBD body catch finally))
           (`(:throw ,expr) (TBD expr))
           (`(:break ,label) (TBD label))
           (`(:continue ,label) (TBD label))
           (`(:while ,cond ,body) (TBD cond body))
           (`(:do ,cond ,body) (TBD cond body))
           (`(:for ,init ,cond ,step ,body) (TBD init cond step body))
           (`(:for-in ,init ,lhs ,obj ,body) (TBD init lhs obj body))
           (`(:switch ,val ,@cases) (TBD val cases))))
       (tidy (x)
         "clean up various @/chain forms"
         (match x
           ;; merge nested cases
           (`(@ (@ ,@a) ,@b)         `(@ ,@(append a b)))
           (`(chain (@ ,@a) ,@b)     `(chain ,@(append a b)))
           (`(chain (chain ,@a) ,@b) `(chain ,@(append a b)))
           ;; revise calls on chain
           (`((chain ,@a) ,@args)
             `(chain ,@(loop 
                         for (i . remainder) on a
                           collect (if remainder
                                       i
                                       `(,i ,@args)))))
           ;; Introduce chain if working on a complex object.
           (`(@ ,(guard call (listp call)) ,@b)
             (tidy
              `(chain ,call ,@b)))
           ;; otherwise untouched.
           (_ x))))
    (setf *last-conversion* (r parse))))

(defun help-let-forms (form)
  (match form
    (`(progn ,@forms)
      (labels ((r (forms)
                 (ematch forms
                   (`((let ,binds :helpme) ,@more-forms)
                     `((let ,binds ,@(r more-forms))))
                   (`(,form ,@more-forms)
                    `(,form ,@(r more-forms)))
                   (nil nil))))
        (let ((rewrite (r forms)))
          (if (cdr rewrite)
              `(progn ,@rewrite)
              (car rewrite)))))
    (_ form)))


