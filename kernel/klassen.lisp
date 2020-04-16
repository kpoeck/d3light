(in-package :d3l)

#|
Designentscheidungen

 : keine Funktional Abhaengigen Slots!!!
 : jeder Slot wird korrekt deklariert
|#


(defclass d3-zeus ()
  ()
  (:documentation "Davon muss alles erben")
  )

(defmethod d3-ruecksetzen ((ich d3-zeus))
  )

(defclass d3-nummer (d3-zeus)
  (
   (nummer :accessor nummer :type D3-POS-INDEX :initarg :nummer)
   (d3-symbol-string :accessor d3-symbol-string :type string :initarg :d3-symbol-string)
   #+no (d3-symbol :accessor d3-symbol :type symbol :initarg :d3-symbol)
   )
  )

(defclass d3-meta-knowledgebase (d3-nummer)
  (
   (myName :type string :accessor myName :initform "No name" :initarg :MYNAME)
   (myAuthor :type string :accessor myAuthor :initform "No author" :initarg :MYAUTHOR)
   (startDialogueWith  :accessor startDialogueWith :initform nil :initarg :STARTDIALOGUEWITH)
   )
  )

(defclass d3-benamtes-objekt (d3-nummer)
  (
   (name :accessor name :type string :initarg :name
         :documentation "Ein  Text, der das Objekt eindeutig beschreibt")
   
   )
  )

(defclass d3-frageklassenoberbegriff/blatt (d3-benamtes-objekt)
  ())

(defclass d3-frageklassenoberbegriff (d3-frageklassenoberbegriff/blatt)
  (
   (frageklassen :accessor frageklassen
                 :type (d3-statische-ungeordnete-menge (or d3-frageklassenoberbegriff d3-frageklasse))
                 :initarg :frageklassen)
   )
  )

(defclass d3-frageklasse (d3-frageklassenoberbegriff/blatt)
  (
   (Eingangsfragen :accessor Eingangsfragen
                   :type (d3-statische-ungeordnete-menge d3-frage)
                   :initarg :Eingangsfragen)
   ;;; fixme do I really need the initform nil?
   (kosten :accessor kosten :type d3-costs :initarg :kosten :initform nil)
   )
  )

(defclass d3-bedeutungsobjekt (d3-benamtes-objekt)
  (
   (kategorische-bedeutung :accessor kategorische-bedeutung
                           :initarg :kategorische-bedeutung
                           :initform (d3-statische-ungeordnete-menge-erzeugen)
                           :type (d3-statische-ungeordnete-menge d3-regel)
                           )
   (heuristische-bedeutung :accessor heuristische-bedeutung
                           :initarg :heuristische-bedeutung
                           :initform (d3-statische-ungeordnete-menge-erzeugen)
                           :type (d3-statische-ungeordnete-menge d3-regel)
                           )
   (datenabstraktion-bedeutung :accessor datenabstraktion-bedeutung
                               :initform (d3-statische-ungeordnete-menge-erzeugen)
                               :initarg :datenabstraktion-bedeutung
                               :type (d3-statische-ungeordnete-menge d3-regel)
                               )
   (apriori-bedeutung :accessor apriori-bedeutung
                      :initform (d3-statische-ungeordnete-menge-erzeugen)
                      :initarg :apriori-bedeutung
                      :type (d3-statische-ungeordnete-menge d3-regel))
   
   (Lokale-Dialogsteuerung-Bedeutung :accessor Lokale-Dialogsteuerung-Bedeutung
                                     :initform (d3-statische-ungeordnete-menge-erzeugen)
                                     :initarg :Lokale-Dialogsteuerung-Bedeutung
                                     :type (d3-statische-ungeordnete-menge d3-regel))
   (globale-Dialogsteuerung-Bedeutung :accessor globale-Dialogsteuerung-Bedeutung
                                     :initform (d3-statische-ungeordnete-menge-erzeugen)
                                     :initarg :globale-Dialogsteuerung-Bedeutung
                                     :type (d3-statische-ungeordnete-menge d3-regel))
   )
  )

(defclass d3-diagnose (d3-bedeutungsobjekt)
  (
   (statisches-apriori :accessor statisches-apriori :type real :initarg :statisches-apriori)
   (therapietext :Accessor therapietext :initarg :therapietext)
   (diagnose-nachfolger :accessor diagnose-nachfolger :type (d3-statische-ungeordnete-menge d3-diagnose)
                        :initarg :diagnose-nachfolger)
   )
  )

(defclass d3-diagnose-kategorisch (d3-diagnose)
  (
   (dynamic-instance :accessor dynamic-instance :initarg :dynamic-instance
                     ;;; dont lie about the type  :type d3-diagnosis-kategorical
                     :initform nil
                     )
   )
  )

(defmethod create&reset-dynamic-instance ((me d3-zeus))
  (let ((dyn (dynamic-instance me)))
    (if (null dyn)
        (let ((answer
               (my-dynamic-instance me)))
          (setf (dynamic-instance me) answer)
          (d3-ruecksetzen answer))
      (d3-ruecksetzen dyn)))
  nil
  )

(defmethod my-dynamic-instance ((me d3-zeus))
  (error "Must be redefined by subclass"))

(defmethod d3-ruecksetzen :after ((me D3-DIAGNOSE-KATEGORISCH))
  (CREATE&RESET-DYNAMIC-INSTANCE me)
  )

(defmethod my-dynamic-instance ((me D3-DIAGNOSE-KATEGORISCH))
  (make-instance 'd3-diagnosis-kategorical)
  )

(defclass d3-diagnose-heuristisch (d3-diagnose-kategorisch)
  (
   (klaerung :accessor klaerung :initarg :klaerung :initform nil) ;syntax liste von (nummer zahl)
   (questionset-if-established :initarg :questionset-if-established :initform nil :accessor questionset-if-established)
   )
  )

(defmethod my-dynamic-instance ((me D3-DIAGNOSE-HEURISTISCH))
  (make-instance 'd3-diagnosis-heuristical)
  )

(defclass d3-symptom (d3-bedeutungsobjekt)
  (
   )
  )

(defclass d3-frage (d3-symptom)
  (
   (fragetext :initarg :fragetext :accessor fragetext :type string)
   )
  )


#|
Fragen
|#

(defclass d3-question-with-answer (D3-FRAGE)
  (
   (dynamic-instance :accessor dynamic-instance :initarg :dynamic-instance  
                     ;;; dont lie about types :type d3-values
                     :initform nil
                     )
   )
  )

(defmethod d3-ruecksetzen :AFTER ((me d3-question-with-answer))
  (CREATE&RESET-DYNAMIC-INSTANCE me)
 )

(defmethod my-dynamic-instance ((me d3-question-with-answer))
  (make-instance 'd3-values))

(defclass d3-frage-num (d3-question-with-answer)
  (
   )
  )

(defmethod my-dynamic-instance ((me D3-FRAGE-NUM))
  (make-instance 'D3-NUM-value))


(defclass d3-frage-jnocmc (D3-QUESTION-WITH-ANSWER)
  (
   )
  )

(defclass d3-frage-jn (d3-frage-jnocmc)
  (
   )
  )

(defmethod my-dynamic-instance ((me D3-FRAGE-JN))
  (make-instance 'D3-YN-value))

(defclass d3-frage-ocmc (D3-FRAGE-JNOCMC)
  (
   (moegliche-werte :initarg :moegliche-werte
                    :accessor moegliche-werte :type d3-antworten)
   (moegliche-werte-texte :initarg :moegliche-werte-texte
                          :accessor moegliche-werte-texte 
                          ;;; lie about type :type d3-antworten
                          )
   )
  )

(defclass d3-frage-oc (d3-frage-ocmc)
  ()
  )

(defmethod my-dynamic-instance ((me D3-FRAGE-OC))
  (make-instance 'd3-oc-value))

(defclass d3-frage-mc (d3-frage-ocmc)
  (
   )
  )

(defmethod my-dynamic-instance ((me D3-FRAGE-MC))
  (make-instance 'D3-MC-value))

(defclass d3-frage-text (d3-question-with-answer)
  (
   )
  )

(defmethod my-dynamic-instance ((me D3-FRAGE-TEXT))
  (make-instance 'D3-TEXT-value))


(defclass d3-symptominterpretation (d3-symptom)
  (
   )
  )

(defclass d3-dataabstraction-with-value (D3-SYMPTOMINTERPRETATION)
  (
   (dynamic-instance :accessor dynamic-instance :initarg :dynamic-instance 
                     ;;; dont lie :type d3-values
                     :initform nil
                     )
   )
  )

(defmethod d3-ruecksetzen :AFTER ((me d3-dataabstraction-with-value))
  (CREATE&RESET-DYNAMIC-INSTANCE me)
 )

(defclass d3-symptominterpretation-oc/punkte (D3-DATAABSTRACTION-WITH-VALUE)
  (
   (moegliche-werte :initarg :moegliche-werte
                    :accessor moegliche-werte :type d3-antworten)
   (moegliche-werte-texte :initarg :moegliche-werte-texte
                          :accessor moegliche-werte-texte
                          ;;; dont lie :type d3-antworten
                          )
   
   )
  )

(defmethod possible-values ((me D3-SYMPTOMINTERPRETATION-OC/PUNKTE))
  (moegliche-werte me))


(defmethod my-dynamic-instance ((me D3-SYMPTOMINTERPRETATION-OC/PUNKTE))
  (make-instance 'd3-oc-value))

(defclass d3-symptominterpretation-oc (d3-symptominterpretation-oc/punkte)
  (
   )
  )

(defclass d3-symptominterpretation-punkte (d3-symptominterpretation-oc/punkte)
  (
   (verechnungsschema :initarg :verechnungsschema :accessor verechnungsschema)
   )
  )

(defmethod my-dynamic-instance ((me D3-SYMPTOMINTERPRETATION-PUNKTE))
  (make-instance 'd3-oc-value-and-points))

(defclass d3-symptominterpretation-num (D3-DATAABSTRACTION-WITH-VALUE)
  (
   )
  )

(defmethod my-dynamic-instance ((me D3-SYMPTOMINTERPRETATION-NUM))
  (make-instance 'd3-num-value))

;;; Fixme, why do i put here the number and not the object
(defclass d3-einzelkondition (d3-zeus)
  (
   (objekt :initarg :objekt :accessor objekt
           ;;; dont lie :type d3-benamtes-objekt
           )
   )
  )

(defclass d3-symptom-einzelkondition (d3-einzelkondition)
  ())

(defclass d3-negierbare-einzelkondition (d3-symptom-einzelkondition)
  (
   (negiert-p :initarg :negiert-p :accessor negiert-p :type (member t nil))
   )
  )

(defclass d3-nicht-negierbare-einzelkondition (d3-symptom-einzelkondition)
  (
   )
  )

(defclass d3-$=-einzelkondition (d3-negierbare-einzelkondition)
  ((antwortalternative :initarg :antwortalternative
                       :accessor antwortalternative
                       :type d3-antwort)))

(defclass d3-$=-num-einzelkondition (d3-negierbare-einzelkondition)
  ((wert :initarg :wert :accessor wert :type real)))

(defclass d3-$intervall-num-einzelkondition (d3-negierbare-einzelkondition)
  (
   (unten :initarg :unten :accessor unten :type real)
   (oben :initarg :oben :accessor oben :type real)
   )
  )

(defclass d3-$or-einzelkondition (d3-negierbare-einzelkondition)
  ((antwortalternativen :initarg :antwortalternativen
                       :accessor antwortalternativen
                       :type d3-antworten)))

(defclass d3-$and-einzelkondition (d3-negierbare-einzelkondition)
  ((antwortalternativen :initarg :antwortalternativen
                       :accessor antwortalternativen
                       :type d3-antworten)))

(defclass d3-$bekannt-einzelkondition (d3-nicht-negierbare-einzelkondition)
  ())

(defclass d3-$unbekannt-einzelkondition (d3-nicht-negierbare-einzelkondition)
  ())

(defclass d3-diagnosen-einzelkondition (d3-nicht-negierbare-einzelkondition)
  ())

(defclass d3-$pc-einzelkondition (d3-diagnosen-einzelkondition)
  ())

(defclass d3-$pcnon-einzelkondition (d3-diagnosen-einzelkondition)
  ())

(defclass d3-kondition (d3-zeus)
  (
   (konditionsliste :initarg :konditionsliste :accessor konditionsliste
                    :type (d3-statische-ungeordnete-menge d3-einzelkondition))
   (vorbedingungsobjekte :initarg :vorbedingungsobjekte :accessor vorbedingungsobjekte
                         :type (d3-statische-ungeordnete-menge d3-bedeutungsobjekt))
   )
  )

(defclass d3-und-kondition (d3-kondition)
  ()
  )

(defclass d3-oder-kondition (d3-kondition)
  ()
  )

(defclass d3-minimum-oder-maximum-kondition (d3-kondition)
  (
   (minimum :initarg :minimum :accessor minimum :type D3-POS-INDEX))
  )

(defclass d3-minimum-kondition (d3-minimum-oder-maximum-kondition)
  (   
   )
  )

(defclass d3-minimum-und-maximum-kondition (d3-minimum-oder-maximum-kondition)
  (
   (maximum :initarg :maximum :accessor maximum :type D3-POS-INDEX)
   )
  )

(defclass d3-regel (d3-nummer)
  (
   (kondition :accessor kondition :initarg :kondition :type d3-kondition)
   (dynamic-instance :accessor dynamic-instance :initarg :dynamic-instance
                     ;;; dont lie  :type d3-diagnosis-kategorical 
                     :initform nil
                     )
   #+noch-nicht (ausnahmekondition :accessor ausnahmekondition :initarg :ausnahmekondition :initform nil :type (or nil d3-kondition))
   )
  )

(defmethod d3-ruecksetzen :AFTER ((me d3-regel))
  (CREATE&RESET-DYNAMIC-INSTANCE me)
  )

(defmethod my-dynamic-instance ((me D3-REGEL))
  (make-instance 'd3-dynamic-rule))


(defclass d3-regel-objekt&wert (d3-regel)
  (
   (aktionsobjekt :accessor aktionsobjekt :initarg :aktionsobjekt 
                  ;;; dont lie :type d3-benamtes-objekt
                  )
   )
  )

(defclass D3-Lokale-Dialogsteuerung-Regel (d3-regel)
  (
   (Aktionsobjekte :accessor Aktionsobjekte :initarg :Aktionsobjekte :type (d3-statische-ungeordnete-menge d3-nummer))
   )
  )

(defclass D3-globale-Dialogsteuerung-Regel (d3-regel)
  (
   (Aktionsobjekte :accessor Aktionsobjekte :initarg :Aktionsobjekte :type (d3-statische-ungeordnete-menge d3-nummer))
   )
  )

(defclass d3-indikationsregel (D3-globale-Dialogsteuerung-Regel)
  ()
  )

(defclass d3-kontraindikationsregel (D3-globale-Dialogsteuerung-Regel)
  ()
  )

(defclass d3-regel-diagnosebewertung (d3-regel-objekt&wert)
  ())

(defclass d3-regel-kategorisch (d3-regel-diagnosebewertung)
  (
   (bewertung :initarg :bewertung :accessor bewertung :type D3-CATEGORIES)
   )
  )

(defclass d3-regel-heuristisch (d3-regel-diagnosebewertung)
  (
   (bewertung :initarg :bewertung :accessor bewertung :type d3-points)
   )
  )

(defclass d3-regel-apriori (d3-regel-objekt&wert)
  (
   (bewertung :initarg :bewertung :accessor bewertung :type d3-possible-praes)
   )
  )

(defclass d3-regel-datenabstraktion (d3-regel-objekt&wert)
  ()
  )

(defclass d3-da-regel-eval (d3-regel-datenabstraktion)
  (
   (punkte :accessor punkte :initarg :punkte :type real)
   )
  )

(defclass d3-da-regel-alternative (d3-regel-datenabstraktion)
  (
   (alternative :accessor alternative :initarg :alternative :type d3-antwort)
   )
  )

(defclass d3-da-regel-formel (d3-regel-datenabstraktion)
  (
   (formel :accessor formel :initarg :formel :type list)
   )
  )
   