;; <!-- vim: set lispwords+=let+,map+,def,prog+ : -->
(load "slip")

(defstructs
  (col ()    (at 0) (txt "") (n 0))
  (num (col) (mu 0) (m2 0) (sd 0) (w 1) :make %num)
  (sym (col)  has                       :make %sym)
  (cols ()    x y all klass             :make %cols)
  (data ()    rows cols                 :make %data))

(def data (names (rows) (i '(:row () :cols (cols (car rows)))))
  (dolist (row (cdr rows) i)
    (add i row))))

(def cols (names (i '(:all () :x () :y () : (x) (y) (klass)) 
   (setf (? 

