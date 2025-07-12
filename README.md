<p>
<img width=200 align=right src="https://www.lisperati.com/lisplogo_flag2_256.png">
<a href="https://github.com/timm/slip/blob/main/LICENSE.md"><img 
  src="https://img.shields.io/badge/license-MIT-brightgreen.svg?xstyle=for-the-badge" 
  alt="License"></a>
<img 
  src="https://img.shields.io/badge/language-Lisp-purple.svg?xstyle=for-the-badge" 
  alt="Language">
<a href="https://github.com/timm/slip"><img 
  src="https://img.shields.io/badge/src-code-orange.svg?xstyle=for-the-badge" 
  alt="Source Code"></a>
</p>

# 🧠 Slip: A Clean Lisp Dialect

_Slip_ = Common Lisp macros for clearer, shorter code.

**Slip** is a thin layer of macros atop Common Lisp that smooth out the
syntax for common tasks. It enables succinct, readable code with minimal
overhead.

## 📦 Installation

Copy the file slip.lisp. Add this to the top of your code:

```lisp
(load "slip")
```

SLIP has been tested on SBCL and CLISP and it should work on any
any ANSI-compliant CL implementation. No dependencies.


# Quick Reference


---

## 🔧 Macros

- `def`: like `defun`, supports `(x default)` and `(:key val)`
  ```lisp
  (def hi (name (title "Dr.")) (format t "~a ~a~%" title name))
  ```

- `->`: short lambda
  ```lisp
  (mapcar (-> (x) (* x x)) '(1 2 3))  ; ⇒ (1 4 9)
  ```

- `let+`: `let` + local `(-> ...)` functions
  ```lisp
  (let+ ((x 1) (inc (-> (y) (+ y 1)))) (inc x))  ; ⇒ 2
  ```

- `map+`: mapcar + remove nil
  ```lisp
  (map+ (-> (x) (when (evenp x) x)) '(1 2 3 4))  ; ⇒ (2 4)
  ```

- `prog+`: safe `progn` (prints errors in SBCL)
  ```lisp
  (prog+ (/ 1 0))  ; prints ❌ not crash
  ```

- `?`: nested getf
  ```lisp
  (? x :a :b)  ; → (getf (getf x :a) :b)
  ```

- `$foo`: → `(getf self :foo)`
  ```lisp
  (format t "~a" $name)
  ```

- `say`: short format with `:out` stream
  ```lisp
  (say "Hi ~a" 'you)
  (say "Err: ~a" 'oops :out *error-output*)
  ```

---

## 🛠 Vim

```lisp
;; <!-- vim: set lispwords+=let+,map+,def,prog+ : -->
```

## 🧠 Emacs

```lisp
(put 'def 'common-lisp-indent-function 'defun)
```

---

MIT © 2025 Tim Menzies
