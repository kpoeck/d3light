(in-package :d3l)

#|
Listen unterscheiden in
 ---- statisch/dynamisch
 ---- geordnet/ungeordnet
|#

(deftype d3-statische-ungeordnete-menge (element-type)
  (declare (ignore element-type))
  'list)

(defmacro d3-mit-allen-elementen-sum ((name ort &optional ergebnis) &body body)
  `(dolist (,name ,ort ,ergebnis)
     ,@body))

(defun d3-statische-ungeordnete-menge-erzeugen (&rest elemente)
  elemente)

(defun d3-statische-ungeordnete-menge-aus-liste-erzeugen (elemente)
  elemente)

(defun d3-statische-ungeordnete-menge->liste (was)
  was
  )