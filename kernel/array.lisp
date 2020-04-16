(in-package :d3l)

#|
Listen unterscheiden in
 ---- statisch/dynamisch
 ---- geordnet/ungeordnet
|#

(deftype d3-statische-ungeordnete-menge (element-type)
  (declare (ignore element-type))
  `(simple-vector *))

#-:fast
(defmacro d3-mit-allen-elementen-sum ((name ort &optional ergebnis) &body body)
  (let ((index (gensym))
        (ort-eval (gensym)))
    `(let ((,ort-eval ,ort))
       (if (null ,ort-eval)
           ,ERGEBNIS
       (dotimes (,index (the fixnum (length (the simple-vector ,ort-eval))) ,ergebnis)
         (declare (fixnum ,index))
         (let ((,name (svref ,ort-eval ,index)))
           ,@body))))))

#+:fast
(defmacro d3-mit-allen-elementen-sum ((name ort &optional ergebnis) &body body)
  (let ((index (gensym))
        (ort-eval (gensym)))
    `(let ((,ort-eval ,ort))
       (locally
         (declare (optimize (speed 3)(safety 0)))
         (if (null ,ort-eval)
           ,ERGEBNIS
           (dotimes (,index (the fixnum (length (the simple-vector ,ort-eval))) ,ergebnis)
             (declare (fixnum ,index))
             (let ((,name (svref ,ort-eval ,index)))
               ,@body)))))))

(defun d3-statische-ungeordnete-menge-erzeugen (&rest elemente)
  (declare (dynamic-extent elemente))
  (make-array (length elemente)
              :initial-contents elemente))

(defun d3-statische-ungeordnete-menge-aus-liste-erzeugen (elemente)
  (make-array (length elemente)
              :initial-contents elemente))

(defun d3-statische-ungeordnete-menge->liste (was)
  (map 'list #'identity was)
  )

#|
(defun test ()
  (d3-mit-allen-elementen-sum
    (ich (d3-statische-ungeordnete-menge-erzeugen 1 2 3 4) 99)
    (print ich)))

(test )
|#