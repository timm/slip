;; <!-- vim: set lispwords+=let+,map+,def,prog+ : -->

;; `def` → defun with support for optional and keyword arguments.
;; Args may be symbols (required), (x default) for optionals,
;; and (:key default) for keyword arguments.
(defmacro def (name args &body body)
  (let ((opts '()) (keys '()))
    (dolist (a args)
      (cond
        ((and (consp a) (keywordp (car a)))
         (push `(,(intern (symbol-name (car a))) ,(cadr a)) keys))
        ((consp a) (push a opts))
        (t (push a opts))))
    `(defun ,name
,(append
         (when opts `(&optional ,@(nreverse opts)))
         (when keys `(&key ,@(nreverse keys))))
       ,@body)))

;; `->` → shorthand for (lambda ...) with optional arg destructuring.
(defmacro -> (args &body body)
  `(lambda ,(if (symbolp args) (list args) args) ,@body))

;; `let+` → smart let that lifts any (-> ...) to labels block.
(defmacro let+ (bindings &body body)
  (let ((vars '()) (fns '()))
    (dolist (b bindings)
      (destructuring-bind (name expr) b
        (if (and (consp expr) (eq (first expr) '->))
            (push `(,name ,(second expr) ,@(cddr expr)) fns)
            (push `(,name ,expr) vars))))
    (cond
      ((and vars fns) `(labels ,fns (let ,vars ,@body)))
      (fns `(labels ,fns ,@body))
      (t `(let ,vars ,@body)))))

;; `map+` → (mapcar ...) with nils removed from the result.
(defmacro map+ (fn list)
  `(remove nil (mapcar ,fn ,list)))

;; `prog+` → safe progn that prints errors in SBCL; raw otherwise.
(defmacro prog+ (&body body)
  #-sbcl `(progn ,@body)
  #+sbcl `(handler-case
            (progn ,@body)
            (error (e)
                   (format t "❌ Error: ~A~%" e))))

;; `?` → nested getf chain: (? x :a :b) ⇒ (getf (getf x :a) :b)
(defmacro ? (x &rest keys)
  (reduce (lambda (acc key) `(getf ,acc ',key)) keys :initial-value x))

;; `$foo` → shorthand for (getf self :foo)
(set-macro-character 
  #\$
  (lambda (stream char)
    `(getf self 
           ,(intern 
              (concatenate 'string ":" 
                           (string (read stream t nil t))))))
  t)

;; `say` → compact (format ...) with optional :out stream
(defmacro say (&rest a)
  `(format ,(or (getf a :out) 't)
           ,@(loop for (k v) on a unless (eq k :out) 
                   collect k and collect v)))

