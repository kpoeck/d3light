(in-package :d3l)

#|
Typen und Konstantan fuer D3-L
|#

(deftype d3-kein-wert-oder-unbekannt ()
  '(member :kein-wert :unbekannt))

#|
(typep 3 'd3-kein-wert-oder-unbekannt)
(typep :unbekannt 'd3-kein-wert-oder-unbekannt)(typep :kein-wert 'd3-kein-wert-oder-unbekannt)
|#

(deftype d3-nein () '(eql :nein))

(deftype d3-ja () '(eql :ja))

(deftype d3-antwort () 'fixnum)

(defun d3-listen-antwortp (list)  (if (listp list)
    (every #'(lambda(was)
               (typep was 'd3-antwort))
           list)    nil))

(deftype d3-antworten () '(and list (satisfies d3-listen-antwortp)))


(deftype d3-yes-no () 
  '(or d3-kein-wert-oder-unbekannt
       d3-ja d3-nein))

(deftype d3-oc ()
  '(or d3-kein-wert-oder-unbekannt
       d3-antwort))

(deftype d3-mc ()
  '(or d3-kein-wert-oder-unbekannt
                     d3-nein
       d3-antworten))

(deftype d3-num ()
  '(or d3-kein-wert-oder-unbekannt integer float))

(deftype d3-text ()
  '(or d3-kein-wert-oder-unbekannt string))

#|
(typep "saj" 'D3-YES-NO)
(typep 3 'D3-YES-NO)
(typep :ja 'D3-YES-NO)
(typep :ja 'd3-oc)
(typep 3 'd3-oc)
(typep 3 'D3-MC)
(typep '(1 2)  'D3-MC)(typep :kein-wert 'D3-MC)
|#


(deftype d3-points () '(integer -80 80))
(deftype d3-categories () '(member :notwendig :sicher :ausgeschlossen))

#-ecl1
(deftype d3-pos-index () `(integer 0  ,array-rank-limit))

#-ecl1
(deftype d3-possible-points () '(member -80 -50 -40 -20 -10 -5 -2 2 5 10 20 40 50 80))
#-ecl1
(deftype d3-possible-praes () '(member -40 -20 -10 -5 1.1 1.2 1.4 1.8))

#|

(typep 40 'd3-points)
(typep 90 'd3-points)

(typep :hugo 'D3-CATEGORIES)
(typep :SICHER 'D3-CATEGORIES)

(typep 23 'D3-POS-INDEX)
(typep most-positive-fixnum 'D3-POS-INDEX)
|#

#-ecl1
(defmacro d3-antwort= (a b)
  `(= (the d3-antwort ,a) (the d3-antwort ,b)))

#-ecl1
(defmacro d3-num-antwort= (a b)
  `(= (the real ,a) (the real ,b)))

#-ecl1
(defmacro d3-antwort-vorhanden-p (wert wertliste)
  (let ((variable (gensym)))
    `(dolist (,variable ,wertliste nil)
       (when (d3-antwort= ,wert ,variable)
         (return t)))))

#|
(time 
 (dotimes (x 1000)
   (d3-antwort-vorhanden-p 3 '(4 5 6 3))))

(time 
 (dotimes (x 1000)
   (find 3 '(4 5 6 3) )))

(typep 3 'd3-antworten)
(typep '(3) 'd3-antworten)

|#

#+no
(deftype d3-praedikate ()
  `(member 
    $Pc $Or  $Isvalue
    $= $After  $Before $Eval
    $<
    $>
    $<=
    $>=
    $Ge
    $Le
    $In
    $In1
    $In-Oder
    $And
    $Alle
    $Ex
    $Isvalue
    $Unbekannt
    $Id
    $Quotient
    $Plus
    $Minus
    $Times
    $Mult
    &Anzahl))

#+no
(deftype d3-isvalue ()
  `(member true false unknown))

#+no
(deftype d3-konditionstypen ()
  `(member :und :oder :n-aus-m))

(deftype d3-costs ()
  '(or null real))

