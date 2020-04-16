(in-package :d3l)

(defclass d3-dynamic-values (d3-zeus)
  (
   )
  )

;For Symptoms;
(defclass d3-values (d3-dynamic-values)
  (
   )
  )

(defclass d3-num-value (d3-values)
  (
   (d-wert :accessor  d-wert 
           :type d3-num
           )
   )
  )

(defmethod d3-ruecksetzen :AFTER ((ich d3-num-value))
  (setf (d-wert ich) :kein-wert)
  )


(defclass d3-text-value (d3-values)
  (
   (d-wert :accessor  d-wert 
           :type d3-text
           )
   )
  )

(defmethod d3-ruecksetzen :after ((ich d3-text-value))
  (setf (d-wert ich) :kein-wert)
  )

(defclass d3-mcocjn-value (d3-values)
  (
   )
  )

(defclass d3-yn-value (d3-mcocjn-value)
  (
   (d-wert :accessor  d-wert 
           :type D3-YES-NO
           )
   )
  )

(defmethod d3-ruecksetzen :AFTER ((ich d3-yn-value))
  (setf (d-wert ich) :kein-wert)
  )

(defclass d3-mcoc-value (d3-mcocjn-value)
  (
   )
  )

(defclass d3-oc-value (d3-mcoc-value)
  (
   (d-wert :accessor  d-wert 
           :type d3-oc
           )
   )
  )

(defmethod d3-ruecksetzen :AFTER ((ich d3-oc-value))
  (setf (d-wert ich) :kein-wert)
  )

;Lets See
(defclass d3-oc-value-and-points (D3-OC-VALUE)
  (
   (d-si-punkte :accessor d-si-punkte :type fixnum)
   )
  )

(defmethod d3-ruecksetzen :AFTER ((ich d3-oc-value-and-points))
  (setf (d-si-punkte ich) 0)
  )

(defclass d3-mc-value (D3-MCOC-VALUE)
  (
   (d-wert :accessor  d-wert 
           :type d3-mc
           )
   )
  )

(defmethod d3-ruecksetzen :AFTER ((ich d3-mc-value))
  (setf (d-wert ich) :kein-wert)
  )

;For Diagnoses

(defclass d3-diagnosis-kategorical (d3-dynamic-values)
  (
   (d-dynamisches-apriori :accessor d-dynamisches-apriori
                          :type (or null real)
                          )
   (d-anzahl-positive :accessor d-anzahl-positive :type fixnum)
   (d-anzahl-negative :accessor d-anzahl-negative :type fixnum)
   )
  )

(defmethod d3-ruecksetzen :AFTER ((ich d3-diagnosis-kategorical))
  (setf (d-dynamisches-apriori ich) 0)
  (setf (d-anzahl-positive ich) 0)
  (setf (d-anzahl-negative ich) 0)
  )

(defclass d3-diagnosis-heuristical (d3-diagnosis-kategorical)
  (
   (d-punkte :accessor d-punkte :type (or null real))
   )
  )

(defmethod d3-ruecksetzen :AFTER ((ich d3-diagnosis-heuristical))
  (setf (d-punkte ich) 0)
  )

;Rules
(defclass d3-dynamic-rule (d3-dynamic-values)
  (
   (d-gefeuert :accessor d-gefeuert :type (member t nil))
   )
  )

(defmethod d3-ruecksetzen :AFTER ((ich d3-dynamic-rule))
  (setf (d-gefeuert ich) nil)
  )


;Delegators
(defmethod set-answer-value ((me d3-dataabstraction-with-value) value)
  (setf (d-wert (dynamic-instance me)) value))

(defmethod get-answer-value ((me d3-dataabstraction-with-value))
  (d-wert (dynamic-instance me)))

(defmethod set-answer-value ((me d3-question-with-answer) value)
  (setf (d-wert (dynamic-instance me)) value))

(defmethod get-answer-value ((me d3-question-with-answer))
  (d-wert (dynamic-instance me)))

(defmethod d-dynamisches-apriori ((me d3-diagnose-kategorisch))
  (d-dynamisches-apriori (dynamic-instance me)))

(defmethod d-anzahl-positive ((me d3-diagnose-kategorisch))
  (d-anzahl-positive (dynamic-instance me)))

(defmethod d-anzahl-negative ((me d3-diagnose-kategorisch))
  (d-anzahl-negative (dynamic-instance me)))

(defmethod d-punkte ((me d3-diagnose-heuristisch))
  (d-punkte (dynamic-instance me)))