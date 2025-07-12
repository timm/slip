;; <!-- vim: set lispwords+=let+,map+,def,prog+ : -->
(load "slip")

;(print (macroexpand-1 '
(def hi (name (title "Dr.") (:loud nil) (:sad))
  (say "name ~a title ~a sad ~a~%"  name title sad)
  (when loud (format t "LOUD MODE ~s ~%" loud)))

(DEFUN HI1 (&OPTIONAL NAME (TITLE "Dr.") &KEY (LOUD NIL) (SAD NIL)) 
       (format t "name ~a title ~a sad ~a~%" NAME TITLE SAD)
       (WHEN LOUD (FORMAT T "LOUD MODE ~s ~%" LOUD)))

(def x () (/ 0 a))

(prog+
  (let+ ((x 5)
         (square (-> (y) (* y y))))
    (hi "timm")
    (hi1 "timm")
    (print (square x))))
