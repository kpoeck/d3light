(in-package :d3l)

(defmethod Regel-Ausfuehren ((ich d3-regel-kategorisch))
  (setf (d-gefeuert (dynamic-instance ich)) t)
  ;melde es der Diagnose
  (debug-format t ">Kat: ~A ~a ~a~%" (bewertung ich) (aktionsobjekt ich)(d3-objekt->nummer ich))
  (diagnose-kategorisch-empfangen (d3-nummer->objekt (aktionsobjekt ich))(bewertung ich)))

(defmethod Regel-Zurueckziehen ((ich d3-regel-kategorisch))
  (setf (d-gefeuert (dynamic-instance ich)) nil)
  ;melde es der Diagnose
  (debug-format t "- >Kat: ~A ~a ~a~%" (bewertung ich) (aktionsobjekt ich)(d3-objekt->nummer ich))
  (diagnose-kategorisch-vergessen (d3-nummer->objekt (aktionsobjekt ich))(bewertung ich)))


#|
Preisfrage: Was mache ich wenn sowohl positive als auch negative regeln feuern?
Ersmal ignorieren, dass macht Frank auch
|#

(defmethod diagnose-kategorisch-empfangen ((ich d3-diagnose-kategorisch) was)
   (let ((vorher (diagnose-status ich)))
     (ecase was
       (:notwendig (Error "Kann ich noch nicht"))
       (:sicher  (incf (d-anzahl-positive (dynamic-instance ich))))
       (:ausgeschlossen (incf (d-anzahl-negative (dynamic-instance ich)))))
     (let ((nachher (diagnose-status ich)))
       (unless (eq vorher nachher)
         (d3-objekt-schliessen ich))))
  )

(defmethod Diagnose-Kategorisch-Vergessen ((ich d3-diagnose-kategorisch) was)
  (let ((vorher (diagnose-status ich)))
    (ecase was
      (:notwendig (Error "Kann ich noch nicht"))
      (:sicher  (decf (d-anzahl-positive (dynamic-instance ich))))
      (:ausgeschlossen (decf (d-anzahl-negative (dynamic-instance ich)))))
    (let ((nachher (diagnose-status ich)))
      (unless (eq vorher nachher)
        (d3-objekt-schliessen ich))))
  )

(defmethod diagnose-status ((ich d3-diagnose-kategorisch))
  (cond ((plusp (d-anzahl-negative (dynamic-instance ich)) ) :kategorisch-Ausgeschlossen)
        ((plusp (d-anzahl-positive (dynamic-instance ich))) :kategorisch-Etabliert)
        (t nil)))

