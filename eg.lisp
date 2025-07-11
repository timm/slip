(defpackage :slip
  (:use :cl)
  (:shadow :let)
  (:export :def :let :-> :map+))

(in-package :slip)

;;; --- def: smart function def with optional & key args
(defmacro def (name args &body body)
  (let ((opts '()) (keys '()))
    (dolist (a args)
      (cond
        ((and (consp a) (keywordp (car a)))
         (push `(,(intern (symbol-name (car a)) :cl) ,(cadr a)) keys))
        ((consp a) (push a opts))
        (t (push a opts))))
    `(defun ,name
       (,@(when opts `(&optional ,@(nreverse opts)))
        ,@(when keys `(&key ,@(nreverse keys))))
       ,@body)))

;;; --- ->: short anonymous lambda
(defmacro -> (args &body body)
  `(lambda ,(if (symbolp args) (list args) args) ,@body))

;;; --- let: enhanced let with -> function detection
(defmacro let (bindings &body body)
  (let ((vars '()) (fns '()))
    (dolist (b bindings)
      (destructuring-bind (name expr) b
        (if (and (consp expr) (eq (first expr) '->))
            (push `(,name ,(second expr) ,@(cddr expr)) fns)
            (push `(,name ,expr) vars))))
    (cond
      ((and vars fns) `(labels ,fns (cl:let ,vars ,@body)))
      (fns `(labels ,fns ,@body))
      (t `(cl:let ,vars ,@body)))))

;;; --- map+: mapcar + remove nil
(defmacro map+ (fn list)
  `(remove nil (mapcar ,fn ,list)))


(in-package :slip)

(def hello (name (title "Dr.") (:loud nil))
  (format t "~a ~a~%" title name)
  (when loud (format t "LOUD MODE~%")))

(let ((x 5)
      (square (-> (y) (* y y))))
  (square x)) ;; => 25

