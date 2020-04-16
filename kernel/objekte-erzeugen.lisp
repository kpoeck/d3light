(in-package :d3l)

#|
Erzeugung der Objekte
Alle Diagnosen
|#


(defvar *d3-nummer->objektpool* nil)

(defun d3-nummer->objektpool-initialisieren (groesse)
  (setq *d3-nummer->objektpool* (make-array groesse :initial-element nil)))

(defun size-of-kb ()
  (length *D3-NUMMER->OBJEKTPOOL*))

#+:fast
(defun d3-nummer->objekt (nummer)
  (declare 
   (optimize (safety 0)(speed 3))
   #+no (:explain :types)
   )
  (svref (the simple-vector *d3-nummer->objektpool*)(the fixnum  nummer)))

#-:fast
(defun d3-nummer->objekt (nummer)
  (aref *d3-nummer->objektpool* nummer))

(defun d3-nummern->objekte (nummern)
  (mapcar #'d3-nummer->objekt nummern))

(defun d3-nummer->objekt-eintragen (nummer objekt)
  (setf (aref *d3-nummer->objektpool* nummer) objekt))

(defun d3-objekt->nummer (objekt)
  (nummer objekt)
  )

#-:fast
(defmacro d3-mit-allen-objekten ((wert) &body body)
  (let ((lauf (gensym)))
    `(when (arrayp  *d3-nummer->objektpool*)
       (dotimes (,lauf (length *d3-nummer->objektpool*))
         (unless (zerop ,lauf)
           (let ((,wert (aref *d3-nummer->objektpool* ,lauf))
                 )
             (unless (null ,wert)
               (let ()
                 ,@body))))))))

#+:fast
(defmacro d3-mit-allen-objekten ((wert) &body body)
  (let ((lauf (gensym))
        (referenz (gensym))
        )
    `(let ((,referenz *d3-nummer->objektpool*))
       (locally (declare (OPTIMIZE (speed 3)(safety 0)))
       (when (arrayp ,referenz)
       (dotimes (,lauf (length ,referenz))
         (unless (zerop ,lauf)
           (let ((,wert (svref (the simple-array ,referenz)(the fixnum ,lauf)))
                 )
             (unless (null ,wert)
               (let ()
                 ,@body))))))))))

(defmacro d3-mit-allen-objekten&ids ((id wert) &body body)
  (let ((lauf (gensym)))
    `(when (arrayp  *d3-nummer->objektpool*)
       (dotimes (,lauf (length *d3-nummer->objektpool*))
         (unless (zerop ,lauf)
           (let ((,wert (aref *d3-nummer->objektpool* ,lauf))
                 (,id ,lauf))
             (unless (null ,wert)
               (let ()
             ,@body))))))))

(defun d3-symbolstring->nummer (string)
  (d3-mit-allen-objekten&ids (id objekt)
    (when (string-equal (d3-symbol-string objekt) string)
      (return id)))
  )
  
(defmacro d3-mit-allen-diagnosen ((name) &body body)
   `(d3-mit-allen-objekten (,name)
       (when (typep ,name 'd3-diagnose)
         ,@body)))


(defmacro d3-mit-allen-fragen ((name) &body body)
  `(d3-mit-allen-objekten (,name)
       (when (typep ,name 'd3-frage)
         ,@body)))

(defmacro d3-mit-allen-frageklassen ((name) &body body)
  `(d3-mit-allen-objekten (,name)
       (when (typep ,name 'D3-Frageklasse)
         ,@body)))

(defmacro d3-mit-allen-regeln ((name) &body body)
  `(d3-mit-allen-objekten (,name)
       (when (typep ,name 'd3-regel)
         ,@body)))

#|
(d3-mit-allen-objekten (b)
  (print `(,b)))
|#


(defun d3-erzeuge-objekt (klasse &rest init-list)
  (declare (dynamic-extent init-list))
  (let ((objekt (apply #'make-instance klasse init-list)))
    (d3-nummer->objekt-eintragen (nummer objekt) objekt)
    objekt))

(defun d3-erzeuge-objekt-ohne-nummer (klasse &rest init-list)
  (declare (dynamic-extent init-list))
  (apply #'make-instance klasse init-list))

(defun d3-erzeuge-$=-einzelkondition (negiert objekt antwortalternative)
  (make-instance 'd3-$=-einzelkondition
    :Negiert-P negiert
    :objekt objekt
    :antwortalternative antwortalternative))

(defun d3-erzeuge-$=-num-einzelkondition (negiert objekt wert)
  (make-instance 'd3-$=-num-einzelkondition
    :Negiert-P negiert
    :objekt objekt
    :wert wert))

(defun d3-erzeuge-$intervall-num-einzelkondition (negiert objekt unten oben)
  (make-instance 'd3-$intervall-num-einzelkondition
    :Negiert-P negiert
    :objekt objekt
    :unten unten
    :oben oben
    )
  )

(defun d3-erzeuge-$or-einzelkondition (negiert objekt antwortalternativen)
  (make-instance 'd3-$or-einzelkondition
    :Negiert-P negiert
    :objekt objekt
    :antwortalternativen antwortalternativen))

(defun d3-erzeuge-$and-einzelkondition (negiert objekt antwortalternativen)
  (make-instance 'd3-$and-einzelkondition
    :Negiert-P negiert
    :objekt objekt
    :antwortalternativen antwortalternativen))

(defun d3-erzeuge-$bekannt-einzelkondition (objekt)
  (make-instance 'd3-$bekannt-einzelkondition
    :objekt objekt))

(defun d3-erzeuge-$unbekannt-einzelkondition (objekt)
  (make-instance 'd3-$unbekannt-einzelkondition
    :objekt objekt))

(defun d3-erzeuge-$pc-einzelkondition (diagnose)
  (make-instance 'd3-$pc-einzelkondition
    :objekt diagnose))

(defun d3-erzeuge-$pcnon-einzelkondition (diagnose)
  (make-instance 'd3-$pcnon-einzelkondition
    :objekt diagnose))