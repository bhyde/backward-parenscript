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

(defvar *context-is-top-level* t)

(defun convert-js-parse-tree-to-javascript (parse)
  (labels
      ((TBD (&rest args)
         (declare (ignore args))
         (error "no rest of the wiki'd"))
       (cant (x)
         (error "Parenscript has no construct for ~a." x))
       (op (op)
         (ecase op
           (:+ '+)
           (:- '-)
           (:* '*)
           (:< '<)
           (:-- '--)
           (:++ '++)
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
       (wrap-forms-in-progn (forms)
         (help-let-forms
          (help-progn
           `(progn ,@forms))))
       (form-to-forms (form)
         (match form
           (`(progn ,@forms)   forms)
           (x                  (list x))))
       (r! (forms) (mapcar #'r forms))
       (r!-progn (forms)
         (wrap-forms-in-progn (r! forms)))
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

           (`(:assign t ,_ ,_)      (binary x 'setf))
           (`(:assign :+ ,_ ,_)     (binary x 'incf))
           (`(:assign := ,_ ,_)     (binary x 'setf))
           (`(:unary-prefix ,_ ,_)  (unary x))

           (`(:array ,elements)     `(list ,@(r! elements)))
           (`(:toplevel ,forms)     (let ((*context-is-top-level* t))
                                      (r!-progn forms)))
           (`(:seq ,forms)          (r!-progn forms))
           (`(:block ,forms)        (r!-progn forms))

           (`(:sub ,array ,index)   `(aref ,(r array) ,(r index)))
           (`(:regexp ,expr "")     `(regex ,expr))
           (`(:regexp ,expr ,flags) `(regex ,(format nil "/~A/~A" expr flags)))
           ('(:debugger)            'debugger)

           (`(:new ,func ,args)     `(new ,(tidy `(,(r func) ,@(r! args)))))
           (`(:atom :true)          `t)
           (`(:atom ,atom) (TBD atom))

           (`(:dot ,x ,(guard slot (stringp slot)))
             (tidy
              `(@ ,(r x) ,(js-name-to-symbol slot))))

           (`(:function nil ,args ,forms)
             `(lambda ,(mapcar #'js-name-to-symbol args) 
                ,@(form-to-forms
                   (let ((*context-is-top-level* nil))
                     (r!-progn forms)))))
           (`(:defun ,name ,args ,forms)
             `(defun ,(js-name-to-symbol name) ,(mapcar #'js-name-to-symbol args)
                ,@(form-to-forms
                   (let ((*context-is-top-level* nil))
                     (r!-progn forms)))))

           (`(:object ,field-alist)
             `(create ,@(loop
                           for (key . value) in field-alist
                           nconc `(,(js-name-to-symbol key) ,(r value)))))
           (`(:var ,bindings)
             (let ((bindings (loop 
                                for (var . init) in bindings
                                collect 
                                  `(,(js-name-to-symbol var) ,(r init)))))
               (if *context-is-top-level*
                   (wrap-forms-in-progn
                    (loop for (var binding) in bindings
                       collect `(var ,var ,binding)))
                   `(let ,bindings :helpme))))

           (`(:throw ,expr)                   `(throw ,(r expr)))
           (`(:try ,body ,catch nil)          `(try ,(r body)
                                                    (:catch ,(r catch))))
           (`(:try ,body ,catch ,finally)     `(try ,(r body) 
                                                    (:catch ,(r catch))
                                                    (:finally ,(r finally))))


           (`(:if ,q ,a ,b)                   `(if ,(r q) ,(r a) ,(r b)))
           (`(:if ,q ,a)                      `(if ,(r q) ,(r a)))
           (`(:conditional ,test ,then ,else) `(if ,test ,then ,else))
           (`(:while ,cond (:block ,forms))   `(while ,(r cond)
                                                 ,(r!-progn forms)))

           (`(:for-in (:var ((,name))) (:name ,_) ,obj ,body)
             `(for-in (,(js-name-to-symbol name) ,(r obj))
                      ,@(form-to-forms (r body))))

           (`(:label ,_ ,_)                   (cant "label")) 
           (`(:continue nil)                  '(continue))
           (`(:continue ,label)               `(continue ,(js-name-to-symbol label)))
           (`(:break nil)                     '(break))
           (`(:break ,label)                  `(break ,(js-name-to-symbol label)))
           (`(:do ,_ ,_)                      (cant "do"))

           ;; That's all so far, pending:
           (`(:unary-postfix ,op ,place) (TBD op place))
           (`(:with ,obj ,body)             (TBD obj body))


           (`(:for (:var ,bindings) ,cond ,step ,body)
             (let ((ps-bindings (loop for (name . init) in bindings
                                   collect `(,(js-name-to-symbol name) ,(r init)))))
               `(for ,ps-bindings (,(r cond)) (,(r step))
                     ,@(form-to-forms (r body)))))
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
    (setf *last-conversion* (r parse)))))

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

(defun help-progn (form)
  "Eliminate unnecessary progn"
  ;; (progn 1 (progn 2 (f (progn 3)) 4) 5 (progn (progn 6) 7))
  ;; -> (progn 1 2 (f (progn 3)) 4 5 6 7)
  (match form
      (`(progn ,x) (help-progn x))
      (`(progn ,@forms)           
        (labels ((r (forms)
                   (match forms
                     (`((progn ,@a) ,@b)  `(,@(r a) ,@(r b)))
                     (`(,a ,@b)           `(,a ,@(r b)))
                     (nil                 nil))))
          `(progn ,@(r forms))))
      (x x)))


