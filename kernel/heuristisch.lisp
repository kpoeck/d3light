(in-package :d3l)

(defmethod Regel-Ausfuehren ((ich d3-regel-heuristisch))
  (setf (d-gefeuert (dynamic-instance ich)) t)
  ;melde es der Diagnose
  (debug-format t ">FB: ~A ~a ~a~%" (bewertung ich) (aktionsobjekt ich)(d3-objekt->nummer ich))
  (diagnose-punkte-empfangen (d3-nummer->objekt (aktionsobjekt ich))(bewertung ich)))

(defmethod Regel-Zurueckziehen ((ich d3-regel-heuristisch))
  (setf (d-gefeuert (dynamic-instance ich)) nil)
  ;melde es der Diagnose
  (debug-format t "- >FB: ~A ~a ~a~%" (bewertung ich) (aktionsobjekt ich)(d3-objekt->nummer ich))
  (diagnose-punkte-vergessen (d3-nummer->objekt (aktionsobjekt ich))(bewertung ich)))

(defmethod diagnose-punkte-empfangen ((ich d3-diagnose-heuristisch) punkte)
  (let ((vorher (diagnose-status ich)))
    (incf (d-punkte (dynamic-instance ich)) punkte)
    (let ((nachher (diagnose-status ich)))
      (debug-format t ">Diagnose ~a ~A ~a ~a~%" (d3-objekt->nummer ich) (d-punkte (dynamic-instance ich)) vorher nachher)
      (unless (eq vorher nachher)
        ;(break "Diagnose ~a etabliert" (name ich))
        (d3-objekt-schliessen ich))
      )
    )
  )

(defmethod diagnose-punkte-vergessen ((ich d3-diagnose-heuristisch) punkte)
  (let ((vorher (diagnose-status ich)))
    (decf (d-punkte (dynamic-instance ich)) punkte)
    (let ((nachher (diagnose-status ich)))
      (debug-format t ">Diagnose ~a ~A ~a ~a~%" (d3-objekt->nummer ich) (d-punkte (dynamic-instance ich)) vorher nachher)
      (unless (eq vorher nachher)
        ;(break "Diagnose ~a etabliert" (name ich))
        (d3-objekt-schliessen ich))
      )
    )
  )

(defmethod d-tatsaechliche-praedisposition ((ich D3-DIAGNOSE-HEURISTISCH))
  (let ((statisches-apriori (statisches-apriori ich)))
    (if (zerop statisches-apriori)
      0
      (schema->nummer
       statisches-apriori
       #+no '(-40 -20 -10 -5 1.0s0 1.1s0 1.2s0 1.4s0 1.8s0)
       #-no '(-40 -20 -10 -5 1.0 1.1 1.2 1.4 1.8) ;(gl=prae-faktoren global)
       '(-39 -19 -9 -4 5 10 20 40) ;(gl=prae-schwellen global)
       )
      )
    )
  )

#+(and no allegro)
(defmethod d-tatsaechliche-punkte ((ich d3-diagnose-heuristisch))
  (declare (:explain :calls :boxing :types :variables)
           (optimize (speed 3)(safety 0)))
  (let ((punkte (the fixnum (d-punkte (dynamic-instance ich)))))
    (cond ((zerop punkte)
           0)
          ((minusp punkte) punkte)
          (T
           (let ((praedisposition (d-tatsaechliche-praedisposition ich)))
             (cond ((zerop praedisposition)
                    punkte)
                   ((minusp praedisposition)
                    (the fixnum (+ punkte
                       (the fixnum praedisposition))))
                   (T (locally
                        (declare (type (integer 0 999) punkte)
                                 (type (single-float  0.0 1.8) praedisposition))
                        (let ((fp (single-float punkte)))
                          (declare (type (single-float 0.0 2.0) fp))
                        (the single-float (* fp PRAEDISPOSITION)))))))))))

(defmethod d-tatsaechliche-punkte ((ich d3-diagnose-heuristisch))
  (let ((punkte (d-punkte (dynamic-instance ich))))
    (cond ((zerop punkte)
           0)
          ((minusp punkte) punkte)
          (T
           (let ((praedisposition (d-tatsaechliche-praedisposition ich)))
             (cond ((zerop praedisposition)
                    punkte)
                   ((minusp praedisposition)
                    (+ punkte PRAEDISPOSITION))
                   (T (* punkte PRAEDISPOSITION))))))))

(defmethod diagnose-status ((ich d3-diagnose-heuristisch))
  (or (call-next-method)
      (schema->nummer 
       (d-tatsaechliche-punkte ich)
       '(:Ausgeschlossen :unklar :Etwas-Verdaechtigt :Verdaechtigt :Etabliert)
       '(-41 10 30 42) ;(gl=schwellen global)
       )))