# 🧠 Slip: A Clean Lisp Dialect

_Slide past boilerplate. Think clearly. Write less._

**Slip** is a thin layer of macros atop Common Lisp that smooth out the
syntax for common tasks. It enables succinct, readable code with minimal
overhead.

---

## ✨ Features

### `def` — like `defun`, but simpler

Supports optional and keyword arguments using familiar syntax:

```lisp
(def greet (name (title "Dr.") (:loud nil))
  (format t "~a ~a~%" title name)
  (when loud (format t "LOUD MODE~%")))
```

Here, forms like `(title "Dr.")` beome `&optional (title "Dr.")` and
forms like `(:loud nil)` beome `&key (loud nil)`


### `->` — short anonymous functions

```lisp
(mapcar (-> (x) (* x x)) '(1 2 3))  ; ⇒ (1 4 9)
```


### `let+` — extended let with inline function bindings

Binds values like `let`, but auto-lifts `(-> ...)` forms to local
functions:

```lisp
(let+ ((x 10)
       (inc (-> (y) (+ y 1))))
  (inc x))  ; ⇒ 11
```


### `map+` — `mapcar` with `nil` removed

```lisp
(map+ (-> (x) (when (evenp x) (* x x))) '(1 2 3 4))  ; ⇒ (4 16)
```


### `prog+` — safe block with error capture (on SBCL)
Avoids pages and pages of error output.

```lisp
(prog+
  (format t "Hello~%")
  (/ 1 0))  ; prints error instead of crashing (SBCL only)
```


### `?` — nested property access

```lisp
(? config :user :email)  ; expands to nested getf
```


### `$` — shorthand for `(getf self :foo)`

Inside a method, use:

```lisp
(def fred (self)
  (format t "~a~%" $name)) ; exxpand to (getf self $name)
```

### `say` — sh


### `say` — short `format` with optional `:out` keyword

```lisp
(say "Hello ~a~%" 'world)
(say "Oops: ~a~%" 'error :out *error-output*)
```

## 🛠️ Editor Support

For Vim/Neovim, add this modeline to the top of your file:

```lisp
;; <!-- vim: set lispwords+=let+,map+,def,prog+ : -->
```

For Emacs, configure indentation:

```lisp
(put 'def 'common-lisp-indent-function 'defun)
(put 'let+ 'common-lisp-indent-function 'let)
(put 'map+ 'common-lisp-indent-function '(&body))
(put 'prog+ 'common-lisp-indent-function 'progn)
```


## 📦 Installation

Just copy the macros into your Common Lisp project. Slip works with
any ANSI-compliant CL implementation. No dependencies.


## 🔍 License

MIT © 2025 Tim Menzies
