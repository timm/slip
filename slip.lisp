;; <!-- vim: set lispwords+=let+,map+,def,prog+,->,format ts=2 sw=2 sts=2 et : -->

(defun chr (s i)
  (char (string s) (if (minusp i) (+ (length s) i) i)))

;; `def` → defun with support for optional and keyword arguments.
;; Args may be symbols (required), (x default) for optionals,
;; and (:key default) for keyword arguments.
(defmacro def (name args &body body)
  (let ((opts '()) (keys '()))
    (dolist (a args)
      (cond
        ((and (consp a) (keywordp (car a)))
         (push `(,(intern (symbol-name (car a))) ,(cadr a)) keys))
        ((consp a)
         (push a opts))
        (t (push a opts))))
    `,(if (consp name)
          `(defmethod ,@name
             ,(append
               (when opts `(&optional ,@(nreverse opts)))
               (when keys `(&key ,@(nreverse keys))))
             ,@body)
          `(defun ,name
             ,(append
               (when opts `(&optional ,@(nreverse opts)))
               (when keys `(&key ,@(nreverse keys))))
             ,@body))))

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
(defparameter *float-places* 3)

(defun chr (x i)
  (char (string x) (mod i (length (string x)))))

(defmacro -> (args &body body)
  `(lambda ,(if (symbolp args) (list args) args) ,@body))

(defmacro map+ (f xs)
  `(remove nil (mapcar ,f ,xs)))

(defmacro str (fmt &rest args)
  `(format nil ,fmt ,@args))

(defun show (x)
  (if (floatp x)
      (let* ((s (str "~,vf" *float-places* x))
             (s (string-right-trim "." (string-right-trim "0" s))))
        (if (equal s "") "0" s))
      (str "~a" x)))

(unless (find-class 'slip nil)
  (defstruct slip))

(defmethod print-object ((x slip) str)
  (format str "(~a ~{~a~^ ~})" (type-of x)
    (map+ (-> (s)
           (unless (eql (chr s 0) #\_)
             (str ":~a ~a" s (show (slot-value x s)))))
          (slots x))))

(defmacro defstructs (&rest defs)
  `(progn ,@(mapcar #'_defstruct defs)))

(defun _defstruct (spec)
  (destructuring-bind (name (&optional isa) &rest rest) spec
    (let* ((isa (or isa 'slip))
           (ctor (second (member :make rest)))
           (raw  (or (subseq rest 0 (position :make rest)) rest))
           (slots (mapcar (lambda (s) (if (consp s) (car s) s)) raw)))
      `(progn
         (defstruct ,(append (list name `(:include ,isa))
                             (when ctor `((:constructor ,ctor))))
           ,@raw)
         (defmethod slots ((x ,name)) ',slots)))))
