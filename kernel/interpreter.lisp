(in-package :d3l)

#|
Schnittstelle nach aussen
regel-zurueckziehen,
regel-ausfuehren
|#

(defmethod regel-testen ((ich d3-regel))
  (incf *anzahl-betrachtete-regeln*)
  (if (d-gefeuert (dynamic-instance ich))
    (gefeuerte-regel-testen ich)
    (nichtgefeuerte-regel-testen ich)))

(defmethod nichtgefeuerte-regel-testen ((ich d3-regel))
  (when (regel-koennte-potentiell-feuern ich)
    (regel-probieren ich)))


(defmethod gefeuerte-regel-testen ((ich d3-regel))
  (if (regel-koennte-potentiell-feuern ich)
    ;regel koennte noch feuern
    (unless (regel-vorbedingung-gilt-p ich)
      (regel-zurueckziehen ich))
    (regel-zurueckziehen ich)))
#|
Die sollte schnell sein!!!!
|#

(defmethod regel-koennte-potentiell-feuern ((ich d3-regel))
  (kondition-koennte-potentiell-feuern (kondition ich)))

(defmethod kondition-koennte-potentiell-feuern ((ich d3-und-kondition))
  ;Alle sind bekannt
  (d3-mit-allen-elementen-sum (vorbedingungsobjekt (vorbedingungsobjekte ich) t)
    (when (nicht-erfasst-p (d3-nummer->objekt vorbedingungsobjekt))
      (return nil)))
  )

(defmethod kondition-koennte-potentiell-feuern ((ich d3-oder-kondition))
  ;mindestens einer ist bekannt
  (d3-mit-allen-elementen-sum (vorbedingungsobjekt (vorbedingungsobjekte ich) nil)
    (unless (nicht-erfasst-p (d3-nummer->objekt vorbedingungsobjekt))
      (return t)))
  )

(defmethod kondition-koennte-potentiell-feuern ((ich d3-minimum-oder-maximum-kondition))
  ;;mindestens n sind bekannt
  (let ((anzahl 0)
        (grenze (minimum ich)))
    (d3-mit-allen-elementen-sum (einzelkondition (konditionsliste ich) nil)
      (unless (nicht-erfasst-p (d3-nummer->objekt (objekt einzelkondition)))
        (incf anzahl)
        (when (>= anzahl grenze)
          (return t)))))
  )

(defmethod regel-probieren ((ich d3-regel))
  (when (regel-vorbedingung-gilt-p ich)
    (regel-ausfuehren ich)))

(defmethod regel-vorbedingung-gilt-p ((ich d3-regel))
  (incf *anzahl-geteste-regeln*)
  (regel-vorbedingung-gilt-p (kondition ich))
  )

(defmethod regel-vorbedingung-gilt-p ((ich d3-und-kondition))
  (d3-mit-allen-elementen-sum (einzelkondition (konditionsliste ich) t)
    (unless (einzelkondition-gilt-p einzelkondition)
      (return nil)))
  )

(defmethod regel-vorbedingung-gilt-p ((ich d3-oder-kondition))
  (d3-mit-allen-elementen-sum (einzelkondition (konditionsliste ich) nil)
    (when (einzelkondition-gilt-p einzelkondition)
      (return t)))
  )

(defmethod regel-vorbedingung-gilt-p ((ich d3-minimum-kondition))
  (let ((minimum (minimum ich))
        (gute-bedingungen 0)
        )
    (d3-mit-allen-elementen-sum (einzelkondition (konditionsliste ich) nil)
      (when (einzelkondition-gilt-p einzelkondition)
        (incf gute-bedingungen)
        (when (>= gute-bedingungen minimum)
          (return t))
        )
      )
    )
  )

(defmethod regel-vorbedingung-gilt-p ((ich d3-minimum-und-maximum-kondition))
  (let ((minimum (minimum ich))
        (maximum (maximum ich))
        (gute-bedingungen 0)
        )
    (d3-mit-allen-elementen-sum (einzelkondition (konditionsliste ich) (<= minimum gute-bedingungen maximum))
      (when (einzelkondition-gilt-p einzelkondition)
        (incf gute-bedingungen)
        (when (> gute-bedingungen maximum)
          (return nil))
        )
      )
    )
  )

#|
Achtung jeweils auf negierbarp achten!!!!
|#

#|
Spezifikation:
nicht-erfasst -> nil, in jedem Fall
unbekannt, nur ueber $unbekannt
:nein ist ein normaler wert

non nicht bei allen praedikaten, auch im Baum anzeigen
Fast alle nur auf symptome

|#
(defmethod einzelkondition-gilt-p ((ich d3-symptom-einzelkondition))
  (let* ((objekt (d3-nummer->objekt (objekt ich)))
         (wert (GET-ANSWER-VALUE objekt)))
    (case wert
      (:kein-wert nil)
      (:unbekannt (unbekannt-gilt-p ich))
      (:nein (nein-gilt-p ich objekt))
      (T (einzelkondition-gilt-bei-normalen-wert-non? ich objekt wert))
      )
    )
  )

(defmethod unbekannt-gilt-p ((ich d3-symptom-einzelkondition))
  nil)

(defmethod unbekannt-gilt-p ((ich d3-$unbekannt-einzelkondition))
  t)

#|
Eigentlich viel einfacher, da die meisten Regeln nicht feueren koennen
|#

;delegate the method from the symptom

(defmethod NEIN-GILT-P (EINZELKONDITION (me D3-QUESTION-WITH-ANSWER))
  (NEIN-GILT-P EINZELKONDITION (dynamic-instance me)))

(defmethod NEIN-GILT-P (EINZELKONDITION (me D3-DATAABSTRACTION-WITH-VALUE))
  (NEIN-GILT-P EINZELKONDITION (dynamic-instance me)))
                                        
(defmethod nein-gilt-p (einzelkondition (ich D3-MC-VALUE))
  #+no (break "was")
  (einzelkondition-gilt-bei-normalen-wert-non? einzelkondition ich '(0)))

(defmethod nein-gilt-p (einzelkondition (ich d3-yn-value))
  (einzelkondition-gilt-bei-normalen-wert-non? einzelkondition ich :nein))

(defmethod einzelkondition-gilt-bei-normalen-wert-non? ((ich d3-negierbare-einzelkondition)
                                                        objekt wert)
  (let ((ergebnis (einzelkondition-gilt-bei-normalen-wert ich objekt wert)))
    (if (negiert-p ich)
      (not ergebnis)
      ergebnis))
  )

(defmethod einzelkondition-gilt-bei-normalen-wert-non? ((ich d3-nicht-negierbare-einzelkondition)
                                                        objekt wert)
  (einzelkondition-gilt-bei-normalen-wert ich objekt wert)
  )

(defmethod einzelkondition-gilt-bei-normalen-wert ((ich d3-$=-einzelkondition) objekt wert)
  (wert-gleich-p objekt wert (antwortalternative ich)))

(defmethod WERT-GLEICH-P ((me D3-QUESTION-WITH-ANSWER) a b)
  (WERT-GLEICH-P (dynamic-instance me) a b))

(defmethod WERT-GLEICH-P ((me D3-DATAABSTRACTION-WITH-VALUE) a b)
  (WERT-GLEICH-P (dynamic-instance me) a b))

(defmethod wert-gleich-p ((ich d3-oc-value) mein-wert wert)
  (d3-antwort= mein-wert wert))

(defmethod wert-gleich-p ((ich d3-mc-value) mein-wert wert)
  (if (listp mein-wert)
      (d3-antwort-vorhanden-p wert mein-wert)
    (d3-antwort= mein-wert wert)
    )
  )

(defmethod einzelkondition-gilt-bei-normalen-wert ((ich d3-$=-num-einzelkondition) objekt wert)
  (declare (ignore objekt))
  (= wert (wert ich)))

(defmethod einzelkondition-gilt-bei-normalen-wert ((ich D3-$Intervall-Num-Einzelkondition) objekt wert)
  (declare (ignore objekt))
  (<= (unten ich) wert (oben ich)))

(defmethod einzelkondition-gilt-bei-normalen-wert ((ich d3-$or-einzelkondition) objekt wert)
  (wert-or-p objekt wert (antwortalternativen ich)))

(defmethod WERT-OR-P ((me D3-QUESTION-WITH-ANSWER) a b)
  (WERT-OR-P (dynamic-instance me) a b))

(defmethod WERT-OR-P ((me D3-DATAABSTRACTION-WITH-VALUE) a b)
  (WERT-OR-P (dynamic-instance me) a b))

(defmethod wert-or-p ((ich d3-oc-value) mein-wert werte)
  #+no (print `(,mein-wert ,werte))
  #+no (break "")
  (d3-antwort-vorhanden-p mein-wert werte))

(defmethod wert-or-p ((ich d3-mc-value) mein-wert werte)
  (dolist (wert mein-wert)
   (when (d3-antwort-vorhanden-p wert werte)
      (return t))))

(defmethod einzelkondition-gilt-bei-normalen-wert ((ich d3-$and-einzelkondition) objekt wert)
  (wert-and-p objekt wert (antwortalternativen ich)))

(defmethod WERT-AND-P ((me D3-QUESTION-WITH-ANSWER) a b)
  (WERT-AND-P (dynamic-instance me) a b))

(defmethod WERT-AND-P ((me D3-DATAABSTRACTION-WITH-VALUE) a b)
  (WERT-AND-P (dynamic-instance me) a b))


(defmethod wert-and-p ((ich d3-mc-value) mein-wert werte)
  (dolist (wert werte t)
      (unless (d3-antwort-vorhanden-p wert mein-wert)
        (return nil)))
  )

(defmethod einzelkondition-gilt-bei-normalen-wert ((ich d3-$bekannt-einzelkondition) objekt wert)
  (declare (ignore objekt wert))
  t)

(defmethod einzelkondition-gilt-bei-normalen-wert ((ich d3-$unbekannt-einzelkondition) objekt wert)
  (declare (ignore objekt wert))
  nil)

(defmethod einzelkondition-gilt-p ((ich d3-$pc-einzelkondition))
  (let ((status (diagnose-status (d3-nummer->objekt (objekt ich)))))
    (or (eq status :Etabliert)
        (eq status :kategorisch-Etabliert))))


(defmethod einzelkondition-gilt-p ((ich d3-$pcnon-einzelkondition))
  (let ((status (diagnose-status (d3-nummer->objekt (objekt ich)))))
    (or (eq status :Ausgeschlossen)
        (eq status :kategorisch-Ausgeschlossen)))
  )


  

