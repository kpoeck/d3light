#|
Designentscheidung, es wird kein Speicher in der Problemloesung gebraucht!!!!
|#

(in-package :d3l)


(defun D3-Fall-Ruecksetzen ()
  (d3-mit-allen-objekten
    (objekt)
    (D3-Ruecksetzen objekt))
  (setq *anzahl-betrachtete-regeln* 0)
  (setq *anzahl-geteste-regeln* 0)
  )

(defun D3-Schliessen (symptomnummer wert)
  (SET-ANSWER-VALUE (d3-nummer->objekt symptomnummer) wert)
  (d3-objekt-schliessen (d3-nummer->objekt symptomnummer))
  )

(defun D3-schliessen-aus-objekt (symptom wert)
  (SET-ANSWER-VALUE symptom wert)
  (d3-objekt-schliessen symptom)
  )

(defmethod d3-objekt-schliessen ((ich d3-benamtes-objekt))
  (d3-mit-allen-elementen-sum (regelnummer (datenabstraktion-bedeutung ich))
    (regel-testen (d3-nummer->objekt regelnummer)))
  (d3-mit-allen-elementen-sum (regelnummer (heuristische-bedeutung ich))
    (regel-testen (d3-nummer->objekt regelnummer)))
  (d3-mit-allen-elementen-sum (regelnummer (kategorische-bedeutung ich))
    (regel-testen (d3-nummer->objekt regelnummer)))
  
  #+:lokale-dialogsteuerung
  (d3-mit-allen-elementen-sum (regelnummer (lokale-dialogsteuerung-bedeutung ich))
    (regel-testen (d3-nummer->objekt regelnummer)))
  
  #+:globale-dialogsteuerung
  (d3-mit-allen-elementen-sum (regelnummer (globale-dialogsteuerung-bedeutung ich))
    (regel-testen (d3-nummer->objekt regelnummer)))
  )
 
(defmethod nicht-erfasst-p ((ich d3-symptom))
  (eq (GET-ANSWER-VALUE ich) :kein-wert))

(defmacro diagnosis-status-not-known-p (status-not-evaluated)
  (let ((status (gensym)))
    `(let ((,status ,status-not-evaluated))
       (not
        (or (eq :Ausgeschlossen ,status)
            (eq :Etabliert ,status)
            (eq :kategorisch-Ausgeschlossen ,status)
            (eq :kategorisch-Etabliert ,status)
            ))))
  )

(defmethod nicht-erfasst-p ((ich d3-diagnose))
  (diagnosis-status-not-known-p (diagnose-status ich)))

(defun schema->nummer (wert werte schema)
  (dolist (grenze schema (first werte))
    (if (< wert grenze)
      (return (first werte))
      (setq werte (rest werte)))))

#|
(schema->nummer 99 '(a b c d) '(1 2 3))

(schema->nummer 0 '(a b c d) '(1 2 3))

(schema->nummer 2.5 '(a b c d) '(1 2 3))
|#



  