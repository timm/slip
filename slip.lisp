;; <!-- vim: set lispwords+=let+,map+,def,prog+,->,format ts=2 sw=2 sts=2 et : -->

(defun chr (s i)
  (char (string s) (if (minusp i) (+ (length s) i) i)))

;; `def` → defun with support for optional and keyword arguments.
;; Args may be symbols (required), (x default) for optionals,
;; and (:key default) for keyword arguments.
(defmacro def (name args &body body)
  (let (pos opts keys)
    (dolist (a args)
      (cond ((and (consp a) (keywordp (car a)))
             (push `(,(intern (symbol-name (car a))) ,(cadr a)) keys))
            ((consp a) (push a opts))
            (t (push a pos))))
    `(defun ,name
       ,(append
          (when pos `(,@(nreverse pos)))
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
  (reduce (lambda (acc key) 
            `(getf ,acc ',key)) keys :initial-value x))

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
(defmacro say (fmt &rest args)
  `(format t ,fmt ,@args))

(defmacro str (fmt &rest args)
  `(format nil ,fmt ,@args))

(defparameter *float-places* 3)

(defun show (x)
  (if (floatp x)
      (let* ((s (str "~,vf" *float-places* x))
             (clean (string-right-trim "." (string-right-trim "0" s))))
        (if (equal clean "") "0" clean))
      x))

(defmacro defstructs (&rest defs)
  `(progn ,(mapcar #'_defstruct defs)))

(defun _defstruct (spec)
  (destructuring-bind (name (&optional isa) &rest rest) spec
    (let+ ((ctor (second (member :make rest)))
           (raw  (or (subseq rest 0 (position :make rest)) rest))
           (slots (map+ (-> (s) (if (consp s) (car s) s)) raw)))
      `(progn
         (defstruct ,(append (list name)
                             (when isa `((:include ,isa)))
                             (when ctor `((:constructor ,ctor))))
           ,@raw)
         (defmethod slots ((x ,name)) ',slots)
         (defmethod print-object ((x ,name) str)
           (format str "(~a ~{~^ ~a~})" ',name
             (map+ (-> (s)
                     (unless (eql (chr s 0) #\_)
                       (str ":~a ~s" s (show (slot-value x s)))))
               (slots x))))))))
