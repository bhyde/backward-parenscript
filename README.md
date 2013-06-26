A fun boondoggle, toying with converting javascript into parenscript.

This works by parsing the javascript with
[parse-js](https://github.com/marijnh/parse-js) and then walking the
resulting s-expression using
[optima](https://github.com/m2ym/optima#readme) to convert it into
parenscript.

It is quite incomplete, but I think the code is pretty.