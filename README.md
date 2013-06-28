A fun boondoggle, toying with converting javascript into parenscript.

This works by parsing the javascript with
[parse-js](https://github.com/marijnh/parse-js) and then [walking the
resulting 
s-expression](https://github.com/bhyde/backward-parenscript/blob/master/main.lisp#L68)
using [optima](https://github.com/m2ym/optima#readme) to convert it
into parenscript.

It is quite incomplete, but I think the code is pretty.

```common-lisp
backward-parenscript> (convert-to-parenscript "a[1].b.c.d(1).e(1).f")
(chain (aref a 1) b c (d 1) (e 1) f)
backward-parenscript> (ps* *)
"a[1].b.c.d(1).e(1).f;"
backward-parenscript> 
```



```common-lisp
backward-parenscript> (convert-to-parenscript "if (a<b) {f(a)} else {f(b)};")
(progn (if (< a b) (f a) (f b)) nil)
backward-parenscript> (ps* *)
"if (a < b) {
    f(a);
} else {
    f(b);
};"
backward-parenscript> 
```

```common-lisp
backward-parenscript> (convert-to-parenscript "function f(x) { var z = 1+x; g(z);}")
(defun f (x) (let ((z (+ 1 x))) (g z)))
backward-parenscript> (ps* *)
"function f(x) {
    var z = 1 + x;
    return g(z);
};"
```
Here's an example a bug.  The handling of returns from functions needs some lov'n.

```common-lisp
backward-parenscript> (convert-to-parenscript "function f(x) { return 1 + x;};")
(progn (defun f (x) (return (+ 1 x))) nil)
backward-parenscript> (ps* *)
; Warning: Returning from unknown block nilblock
; While executing: parenscript::return-exp, in process repl-thread(13).
"function f(x) {
    return 1 + x;
};"
backward-parenscript> 
```

