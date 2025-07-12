;; <!-- vim: set lispwords+=let+,map+,def,prog+ : -->
(load "slip")

(def hello (name (title "Dr.") (:loud nil))
  (fmt "~a ~a~%" title name)
  (when loud (format t "LOUD MODE ~s ~%" loud)))

(def x () (/ 0 a))

(prog+
  (let+ ((x 5)
         (square (-> (y) (* y y))))
    (x)
    (print (square x))))
