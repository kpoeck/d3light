(in-package :d3l)

;;; Save on load a knowledge base

(defclass anonymous-objects-mapper (d3-zeus)
  (
   (mapper :accessor mapper :initform (make-hash-table))
   (mapper-index :accessor mapper-index :initform 0)
   )
  )

(defvar *mapper*)

(defun reset-mapper ()
  (setq *mapper* (make-instance 'anonymous-objects-mapper)))

(defmethod note-anonymous-object ((me anonymous-objects-mapper) object)
  (let ((current-index (mapper-index me)))
    (setf (gethash object (mapper me)) current-index)
    (incf (mapper-index me))
    current-index)
  )

(defmethod note-anonymous-object-with-index ((me anonymous-objects-mapper) object CURRENT-INDEX)
  (setf (gethash CURRENT-INDEX (mapper me)) OBJECT)
  )

(defmethod get-mapped-object-index ((me anonymous-objects-mapper) object)
  (gethash object (mapper me)))

(defmethod get-object-for-index ((me anonymous-objects-mapper) index)
  (gethash index (mapper me)))

(defun save-knowledge-base-to-file (filename)
  (when (probe-file filename)
    (delete-file filename))
  (with-open-file  
      (file filename :if-does-not-exist :create :direction :output)
    (SAVE-KNOWLEDGE-BASE :stream file)))

(defun save-knowledge-base (&key (stream *standard-output*))
  (reset-mapper)
  (save-size stream)
  (d3-mit-allen-objekten 
   (object)
   (store-object-to-stream object stream)))

;;;; Begin
;;;; :class ...
;;;; End

(defmethod store-object-to-stream ((me d3-zeus) stream &key index)
  (store-referenced-objects me stream)
  (store-heading me stream)
  (store-core-object me stream :index index)
  (note-store-referenced-objects me stream)
  (store-footer me stream)
  )

(defmethod store-referenced-objects ((me d3-zeus) stream)
  stream
  nil
  )

(defmethod note-store-referenced-objects ((me d3-zeus) stream)
  stream
  nil
  )

(defun SAVE-SIZE(stream)
  (format stream ":size ~a~%" (size-of-kb)))

(defmethod store-simple-referenz ((me d3-zeus) stream object name)
  (format stream ":~a ~s~%" name (list :simple-referenz (get-mapped-object-index *mapper* object)))
  )

(defmethod STORE-list-REFERENZ ((me d3-zeus) stream objects name)
  (let ((list nil))
    (d3-mit-allen-elementen-sum
     (referenz objects)
     (push (get-mapped-object-index *mapper* REFERENZ) list))
    (format stream ":~a ~s~%" name (list :list-referenz (SERIALIZE-STATIC-LIST (reverse list))))
    )
  )


(defmethod store-heading ((me d3-zeus) stream)
  (format stream ":BEGINOBJECTDEFINITION~%"))

(defmethod store-footer ((me d3-zeus) stream)
  (format stream ":EndObjectDefinition~%~%"))

(defmethod store-core-object ((me d3-zeus) stream &key index)
  (format stream ":class ~a~%" (class-name (class-of me)))
  (when index
    (format stream ":referenz ~a~%" index))
  (store-variables me stream)
  )

(defmethod store-variables ((me d3-zeus) stream)
  (dolist (pair (get-variables-to-store me))
    (let ((name (first pair))
          (reader (second pair))
          )
      #+no(print  `(,name ,reader))
      (let ((value (funcall reader me)))
        (format stream ":~a ~s~%" name value)))))

(defmethod get-variables-to-store ((me d3-zeus))
  )

(defmethod get-variables-to-store ((me d3-nummer))
  (list
   (list :NUMMER #'NUMMER)
   (list :D3-SYMBOL-STRING #'D3-SYMBOL-STRING)))

(defmethod get-variables-to-store ((me D3-META-KNOWLEDGEBASE))
   (append (call-next-method)
           (list
            (list :myname #'MYNAME)
            (list :myAuthor #'MYAUTHOR)
            (list :STARTDIALOGUEWITH #'STARTDIALOGUEWITH)
            ))
  )

(defmethod get-variables-to-store ((me D3-BENAMTES-OBJEKT))
  (append (call-next-method)
          (list
           (list :name #'name)
           )
          )
  )

(defmethod GET-VARIABLES-TO-STORE ((me D3-FRAGEKLASSENOBERBEGRIFF))
    (append
     (call-next-method)
     (list
      (list :frageklassen #'SERIALIZE-questionaires))))

(defmethod SERIALIZE-questionaires ((me D3-FRAGEKLASSENOBERBEGRIFF))
  (SERIALIZE-STATIC-LIST (FRAGEKLASSEN me))
  )

(defmethod get-variables-to-store ((me D3-FRAGEKLASSE))
  (append
   (call-next-method)
   (list
    (list :EINGANGSFRAGEN #'serialize-toplevel-questions)
    (list :kosten #'kosten))))

(defmethod SERIALIZE-TOPLEVEL-QUESTIONS ((me D3-FRAGEKLASSE))
  (SERIALIZE-STATIC-LIST (eingangsfragen me))
  )

(defmethod GET-VARIABLES-TO-STORE ((me D3-BEDEUTUNGSOBJEKT))
  (append (call-next-method)
          (list
           (list :KATEGORISCHE-BEDEUTUNG #'serialize-categorical-implications)
           (list :HEURISTISCHE-BEDEUTUNG #'serialize-heuristical-implications)
           (list :DATENABSTRAKTION-BEDEUTUNG #'SERIALIZE-dataabstractions-IMPLICATIONS)
           (list :APRIORI-BEDEUTUNG #'SERIALIZE-apriori-IMPLICATIONS)
           (list :LOKALE-DIALOGSTEUERUNG-BEDEUTUNG #'serialize-local-dialogue-control-implications)
           (list :GLOBALE-DIALOGSTEUERUNG-BEDEUTUNG #'serialize-global-dialogue-control-implications)
           )))

(defun serialize-static-list (was)
  (if (null was)
      nil
  (cons :sum (D3-STATISCHE-UNGEORDNETE-MENGE->LISTE was))))

(defmethod serialize-categorical-implications ((me D3-BEDEUTUNGSOBJEKT))
  (SERIALIZE-STATIC-LIST (kategorische-bedeutung me))
  )

(defmethod serialize-heuristical-implications ((me D3-BEDEUTUNGSOBJEKT))
  (SERIALIZE-STATIC-LIST (heuristische-bedeutung me))
  )

(defmethod SERIALIZE-dataabstractions-IMPLICATIONS ((me D3-BEDEUTUNGSOBJEKT))
  (SERIALIZE-STATIC-LIST (DATENABSTRAKTION-BEDEUTUNG me))
  )

(defmethod SERIALIZE-apriori-IMPLICATIONS ((me D3-BEDEUTUNGSOBJEKT))
  (SERIALIZE-STATIC-LIST (APRIORI-BEDEUTUNG me))
  )

(defmethod serialize-local-dialogue-control-implications ((me D3-BEDEUTUNGSOBJEKT))
  (SERIALIZE-STATIC-LIST (LOKALE-DIALOGSTEUERUNG-BEDEUTUNG me))
  )

(defmethod serialize-global-dialogue-control-implications ((me D3-BEDEUTUNGSOBJEKT))
  (SERIALIZE-STATIC-LIST (GLOBALE-DIALOGSTEUERUNG-BEDEUTUNG me))
  )

(defmethod GET-VARIABLES-TO-STORE ((me d3-diagnose))
  (append (call-next-method)
          (list (list :STATISCHES-APRIORI #'STATISCHES-APRIORI)
                (list :THERAPIETEXT #'THERAPIETEXT)
                (list :DIAGNOSE-NACHFOLGER #'serialize-diagnosis-successors))))
          
(defmethod serialize-diagnosis-successors ((me D3-DIAGNOSE))
  (SERIALIZE-STATIC-LIST (DIAGNOSE-NACHFOLGER me))
  )

(defmethod GET-VARIABLES-TO-STORE ((me D3-DIAGNOSE-KATEGORISCH))
  (append (call-next-method)
          (list
           (list :klaerung #'klaerung))))

(defmethod GET-VARIABLES-TO-STORE ((me D3-DIAGNOSE-HEURISTISCH))
  (append (call-next-method)
          (list
           (list :QUESTIONSET-IF-ESTABLISHED #'QUESTIONSET-IF-ESTABLISHED))))


(defmethod GET-VARIABLES-TO-STORE ((me d3-frage))
  (append (call-next-method)
          (list (list :fragetext #'FRAGETEXT))))


(defmethod STORE-REFERENCED-OBJECTS ((me D3-DATAABSTRACTION-WITH-VALUE) stream)
  stream
  #+no (let ((index (note-anonymous-object *mapper* (my-answer me))))
    (STORE-OBJECT-TO-STREAM (my-answer me) stream :index index)
    )
  )

(defmethod STORE-REFERENCED-OBJECTS ((me D3-FRAGE-JNOCMC) stream)
  stream
  #+no (let ((index (note-anonymous-object *mapper* (my-answer me))))
    (STORE-OBJECT-TO-STREAM (my-answer me) stream :index index)
    )
  )

(defmethod GET-VARIABLES-TO-STORE ((me d3-frage-ocmc))
  (
   append
   (call-next-method)
   (list
    (list :moegliche-werte #'moegliche-werte)
    (list :moegliche-werte-texte #'moegliche-werte-texte))))

(defmethod GET-VARIABLES-TO-STORE ((me D3-SYMPTOMINTERPRETATION-OC/PUNKTE))
  (
   append
   (call-next-method)
   (list
    (list :moegliche-werte #'moegliche-werte)
    (list :moegliche-werte-texte #'moegliche-werte-texte))))


(defmethod note-store-referenced-objects ((me D3-DATAABSTRACTION-WITH-VALUE) stream)
  stream
  #+no (store-simple-referenz me stream (my-answer me) :MY-ANSWER)
  )

(defmethod note-store-referenced-objects ((me D3-FRAGE-JNOCMC) stream)
  stream
  #+no (store-simple-referenz me stream (my-answer me) :MY-ANSWER)
  )


(defmethod GET-VARIABLES-TO-STORE ((me D3-SYMPTOMINTERPRETATION-PUNKTE))
  (append (call-next-method)
          (list (list :VERECHNUNGSSCHEMA #'VERECHNUNGSSCHEMA))))

(defmethod store-referenced-objects ((me d3-regel) stream)
  (let ((index (note-anonymous-object *mapper* (KONDITION me))))
    (STORE-OBJECT-TO-STREAM (KONDITION me) stream :index index)
    )
  )

(defmethod note-store-referenced-objects ((me D3-REGEL) stream)
  (store-simple-referenz me stream (KONDITION me) :KONDITION)
  )

(defmethod GET-VARIABLES-TO-STORE ((me d3-kondition))
  (append (call-next-method)
          (list
           (list :VORBEDINGUNGSOBJEKTE #'VORBEDINGUNGSOBJEKTE-serialisieren))))

(defmethod VORBEDINGUNGSOBJEKTE-serialisieren ((me D3-KONDITION))
  (SERIALIZE-STATIC-LIST (VORBEDINGUNGSOBJEKTE me)))

(defmethod store-referenced-objects ((me D3-KONDITION) stream)
  (d3-mit-allen-elementen-sum 
   (was (KONDITIONSLISTE me))
   (let ((index (note-anonymous-object *mapper* was)))
     (STORE-OBJECT-TO-STREAM was stream :index index))))

(defmethod NOTE-STORE-REFERENCED-OBJECTS ((me D3-KONDITION) stream)
  (STORE-list-REFERENZ me stream (KONDITIONSLISTE me) :konditionsliste))

(defmethod GET-VARIABLES-TO-STORE ((me D3-MINIMUM-ODER-MAXIMUM-KONDITION))
  (append (call-next-method)
          (list
           (list :MINIMUM #'MINIMUM))))

(defmethod GET-VARIABLES-TO-STORE ((me D3-MINIMUM-UND-MAXIMUM-KONDITION))
  (append (call-next-method)
          (list
           (list :maximum #'maximum))))

(defmethod get-variables-to-store ((me D3-EINZELKONDITION))
  (append (call-next-method)
          (list
           (list :OBJEKT #'OBJEKT))))


(defmethod get-variables-to-store ((me D3-NEGIERBARE-EINZELKONDITION))
  (append (call-next-method)
          (list
           (list :negiert-p #'negiert-p))))


(defmethod get-variables-to-store ((me D3-$=-EINZELKONDITION))
  (append (call-next-method)
          (list
           (list :antwortalternative #'ANTWORTALTERNATIVE))))


(defmethod get-variables-to-store ((me D3-$=-NUM-EINZELKONDITION))
  (append (call-next-method)
          (list
           (list :wert #'wert))))

(defmethod get-variables-to-store ((me D3-$INTERVALL-NUM-EINZELKONDITION))
  (append (call-next-method)
          (list
           (list :unten #'unten)
           (list :oben #'oben))))

(defmethod get-variables-to-store ((me D3-$OR-EINZELKONDITION))
  (append (call-next-method)
          (list
           (list :ANTWORTALTERNATIVEN #'ANTWORTALTERNATIVEN))))

(defmethod get-variables-to-store ((me D3-$AND-EINZELKONDITION))
  (append (call-next-method)
          (list
           (list :ANTWORTALTERNATIVEN #'ANTWORTALTERNATIVEN))))

(defmethod GET-VARIABLES-TO-STORE ((me D3-REGEL-OBJEKT&WERT))
  (append (call-next-method)
          (list (list :AKTIONSOBJEKT #'AKTIONSOBJEKT))))

(defmethod GET-VARIABLES-TO-STORE ((me D3-REGEL-KATEGORISCH))
  (append (call-next-method)
          (list (list :BEWERTUNG #'BEWERTUNG))))

(defmethod GET-VARIABLES-TO-STORE ((me D3-REGEL-HEURISTISCH))
  (append (call-next-method)
          (list (list :BEWERTUNG #'BEWERTUNG))))

(defmethod GET-VARIABLES-TO-STORE ((me D3-REGEL-APRIORI))
  (append (call-next-method)
          (list (list :BEWERTUNG #'BEWERTUNG))))

(defmethod GET-VARIABLES-TO-STORE ((me D3-DA-REGEL-EVAL))
  (append (call-next-method)
          (list (list :punkte #'punkte))))

(defmethod GET-VARIABLES-TO-STORE ((me D3-DA-REGEL-ALTERNATIVE))
  (append (call-next-method)
          (list (list :alternative #'alternative))))

(defmethod GET-VARIABLES-TO-STORE ((me D3-DA-REGEL-FORMEL))
  (append (call-next-method)
          (list (list :formel #'formel))))

;;; need to serialize it
(defmethod GET-VARIABLES-TO-STORE ((me D3-LOKALE-DIALOGSTEUERUNG-REGEL))
  (append (call-next-method)
          (list (list :AKTIONSOBJEKTE #'SERIALIZE-AKTIONSOBJEKTE))))

(defmethod GET-VARIABLES-TO-STORE ((me D3-GLOBALE-DIALOGSTEUERUNG-REGEL))
  (append (call-next-method)
          (list (list :AKTIONSOBJEKTE #'SERIALIZE-AKTIONSOBJEKTE))))

(defmethod SERIALIZE-AKTIONSOBJEKTE ((me D3-LOKALE-DIALOGSTEUERUNG-REGEL))
  (SERIALIZE-STATIC-LIST (AKTIONSOBJEKTE me))
  )

(defmethod SERIALIZE-AKTIONSOBJEKTE ((me D3-GLOBALE-DIALOGSTEUERUNG-REGEL))
  (SERIALIZE-STATIC-LIST (AKTIONSOBJEKTE me))
 )

(defun read-and-install-knowledge-base (filename)
  (reset-mapper)
  #+no (D3-NUMMER->OBJEKTPOOL-INITIALISIEREN 4000)
  (INSTALL-OBJECT-FROM-FILE filename)
  :ready
  )

(defun install-object-from-file (filename)
  (with-open-file (file filename :direction :input)
    (Let ((class nil)
          (parameters nil)
          (class-to-be-read nil)
          (parameterName nil)
          (lastObject nil)
          (SIZEREAD nil)
          (index 0)
          )
      (do ((token (read file nil :finished)(read file nil :finished)))
          ((eq token :finished))
        #+no (print    `(token ,token class-to-be-read ,CLASS-TO-BE-READ parametername ,PARAMETERNAME))
        (cond 
         ((eq token :size)
          (setq sizeread t))
         (sizeread
          (D3-NUMMER->OBJEKTPOOL-INITIALISIEREN (1+ TOKEN))
          (setq sizeread nil))
         ((eq token :beginObjectDefinition)
          (setq class nil parameters nil))
         ((eq token :EndObjectDefinition)
          (setq lastObject (create-object class PARAMETERS))
          (incf index)
          #+no (format t "Index ~s~%" index)
 
          #+:aclno
          (excl:gc :tenure)
          #+no (break "aber")
          )
         ((eq token :class)
          (setq class-to-be-read t))
         (class-to-be-read (setq class token class-to-be-read nil))
         ((null parameterName)
          (setq parameterName token))
         (t
          (push token parameters)
          (push parameterName parameters)
          (setq parameterName nil))
         )
        )
      lastObject
      )
    )
  )

(defun create-object (class parameters)
  #+no (print `(class ,class params ,parameters))
  (Let ((internal-number (getf parameters :referenz))
        (external-number (getf parameters :nummer))
        (object nil)
        (parameters-filtered nil)
        )
    (setq parameters-filtered (MAP-PARAMETER-LIST parameters))
    (setq object
          (apply #'make-instance class parameters-filtered))
    (when (null object)
      (print `(class ,class params ,PARAMETERS-FILTERED))
      (break "baeh"))
    (when internal-number
      (NOTE-ANONYMOUS-OBJECT-WITH-INDEX *MAPPER* OBJECT INTERNAL-NUMBER))
    (when external-number
      (D3-NUMMER->OBJEKT-EINTRAGEN external-number object))
    object
    )
  )

(defun map-parameter-list (list)
  (let ((parameter :nada)
        (parameter-value :nada)
        (result nil)
        )
    (dolist (value list)
      (if (eq :nada PARAMETER)
          (setq PARAMETER value)
        (setq parameter-value value))
      (cond
       ((eq PARAMETER :referenz))
       ((and (listp parameter-value)(eq (first parameter-value) :SIMPLE-REFERENZ))
        (push (GET-OBJECT-FOR-INDEX *mapper* (second parameter-value)) result)
        (push PARAMETER result))
       ((and (listp parameter-value)(eq (first parameter-value) :list-REFERENZ))
        (if (eq :sum (first (second PARAMETER-VALUE)))
            (Let ((references nil))
              (dolist (ref (rest (second PARAMETER-VALUE)))
                (push (GET-OBJECT-FOR-INDEX *MAPPER* ref) references))
              (push (d3-statische-ungeordnete-menge-aus-liste-erzeugen (reverse references)) result)
              (push parameter result)
              )
          (Let ((references nil))
            (dolist (ref (second PARAMETER-VALUE))
              (push (GET-OBJECT-FOR-INDEX *MAPPER* ref) references))
            (push (reverse references) result)
            (push parameter result)
            )
          )
        )
       ((and (listp parameter-value)(eq :sum (first parameter-value)))
        (push (d3-statische-ungeordnete-menge-aus-liste-erzeugen (rest PARAMETER-VALUE)) result)
        (push parameter result)
        )
       ((eq :nada parameter-value))
       (t (push parameter-value result)
          (push parameter result)))
      (when (and  (not (eq :nada PARAMETER))(not (eq :nada PARAMETER-VALUE)))
        (setq PARAMETER-VALUE :nada parameter :nada))
      
      #+no (print       `(value ,value p ,parameter pv ,parameter-value r ,result))
      
      )
    result
    )
  #+no (error "Baeh")
  )
  
  
  
  
  
  #|

(READ-AND-INSTALL-KNOWLEDGE-BASE
 "c:\\d3light\\knowledgebase\\books.kb")

(time
 (progn
   (READ-AND-INSTALL-KNOWLEDGE-BASE
    "c:\\d3light\\knowledgebase\\neuro.kb")
   42))

(d3-fall-ruecksetzen)
(SAVE-KNOWLEDGE-BASE)

(progn
  
  (SAVE-KNOWLEDGE-BASE-TO-FILE "c:\\d3light\\knowledgebase\\books11.kb")
  42)

(progn
  (SAVE-KNOWLEDGE-BASE-TO-FILE "c:\\d3light\\knowledgebase\\neuro6.kb")
  42)

(progn
  (SAVE-KNOWLEDGE-BASE-TO-FILE "c:\\d3light\\knowledgebase\\bauch.kb")
  42)

|#
                            