;; <!-- vim: set lispwords+=let+,map+,def,prog+ : -->
(load "slip")

(defstructs
  (col ()    (at 0) (txt "") (n 0))
  (num (col) (mu 0) (m2 0) (sd 0) (w 1))
  (sym (col)  has)
  (cols ()    x y all klass)
  (data ()    rows cols)

(def data (names (rows) (i '(:row () :cols (cols (car rows)))))
  (dolist (row (cdr rows) i)
    (add i row))))

(def cols (names (i '(:all () :x () :y () : (x) (y) (klass)) 
   (setf (? 

