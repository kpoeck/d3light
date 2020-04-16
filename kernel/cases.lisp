(in-package :d3l)

;;; Define Cases for being saved

(defclass caseList (d3-zeus)
  (
   (knowledgeBase :accessor knowledgeBase :type string :initarg :knowledgeBase)
   (myCaseList :accessor myCaseList :initform nil :initarg :myCaseList)
   (current-case :accessor current-case :type aCase)
   )
  )

(defclass aCase (d3-zeus)
  (
   (currentNumber :initarg :currentNumber :accessor currentNumber)
   (start-values :accessor start-values :initform nil :initarg :start-values)
   (results :accessor results :initform nil :initarg :results)
   )
  )

(defclass aStartValue (d3-zeus)
  (
   (id :accessor id :initarg :id)
   (initialValue :accessor initialValue :initarg :initialValue)
   )
  )

(defclass aResult (d3-zeus)
  (
   (diagnosisId :accessor diagnosisId :initarg :diagnosisId)
   #+no
   (SymbolString :accessor SymbolString :initarg :SymbolString)
   (points :accessor points :initarg :points)
   #+no
   (name :accessor NAME :initarg :NAME)
   )
  )

(defmethod initialize-instance :after ((me aResult) &rest blah &key symbolstring name)
  (declare (ignore symbolstring name))
  )

(defmethod GET-VARIABLES-TO-STORE ((me caseList))
  (append (call-next-method)
          (list (list :knowledgeBase #'knowledgeBase))))

(defmethod GET-VARIABLES-TO-STORE ((me aCase))
  (append (call-next-method)
          (list (list :currentNumber #'currentNumber)
                #+no (list :start-values #'start-values)
                #+no (list :results #'results)
                )))

(defmethod GET-VARIABLES-TO-STORE ((me aStartValue))
  (append (call-next-method)
          (list (list :id #'id)
                (list :initialValue #'initialValue)
                )))

(defmethod GET-VARIABLES-TO-STORE ((me ARESULT))
  (append (call-next-method)
          (list (list :DIAGNOSISID #'DIAGNOSISID)
                #+no (list :SYMBOLSTRING #'SYMBOLSTRING)
                (list :points #'points)
                #+no (list :name #'name)
                )))

(defmethod store-referenced-objects ((me CASELIST) stream)
  (dolist (case (myCaseList me))
   (let ((index (note-anonymous-object *mapper* CASE)))
     (STORE-OBJECT-TO-STREAM CASE stream :index index))))

(defmethod NOTE-STORE-REFERENCED-OBJECTS ((me CASELIST) stream)
  (STORE-list-REFERENZ me stream (myCaseList me) :myCaseList))

(defmethod store-referenced-objects ((me ACASE) stream)
  (dolist (sv (start-values me))
   (let ((index (note-anonymous-object *mapper* sv)))
     (STORE-OBJECT-TO-STREAM sv stream :index index)))
  (dolist (result (results me))
   (let ((index (note-anonymous-object *mapper* result)))
     (STORE-OBJECT-TO-STREAM result stream :index index)))
  )

(defmethod NOTE-STORE-REFERENCED-OBJECTS ((me ACASE) stream)
  (STORE-list-REFERENZ me stream (START-VALUES me) :START-VALUES)
  (STORE-list-REFERENZ me stream (results me) :results)
)

(defmethod save-caselist-to-file ((me caselist) filename)
  (when (probe-file filename)
    (delete-file filename))
  (with-open-file  
      (file filename :if-does-not-exist :create :direction :output)
    (save-caselist me :stream file)))

(defmethod save-caselist ((me caselist) &key (stream *standard-output*))
  (reset-mapper)
  (STORE-OBJECT-TO-STREAM me stream))

(defmethod execute-all-cases ((me caselist) &key print time check)
  (d3-mit-allen-elementen-sum (case (myCaseList me))
    (execute-case case :print print :time time :check check))
  )

(defmethod execute-case ((me aCase) &key print time check)
  (if time
      (time (excecute-core-case me))
    (excecute-core-case me))
  (when print
    (D3-Diagnosen-Zeigen :Alle T))
  (when check
    (compore-case-result-current-result me))
 )

(defmethod excecute-core-case ((me aCase))
  (D3-FALL-RUECKSETZEN)
  (d3-mit-allen-elementen-sum (pair (start-values me))
    (D3-SCHLIESSEN (id pair)(initialValue pair)))
  )

(defun delta=p (a b)
  (< (abs (- a b)) .01))

(defmethod COMPORE-CASE-RESULT-CURRENT-RESULT ((me aCase))
  (d3-mit-allen-diagnosen 
    (hugo)
    (let ((tatsaechlich 
           (d-tatsaechliche-punkte hugo))
          (saved-object (find (nummer hugo) (results me) :key #'diagnosisId)))
      (if (zerop tatsaechlich)
          (when saved-object
              (print `(Error ,(nummer hugo) in Case with , (points saved-object))))
        (if saved-object
            (unless (delta=p TATSAECHLICH (points saved-object))
              
              (print `(Error ,(nummer hugo) ,TATSAECHLICH ,(points saved-object)))
              (break "Baeh"))
            (print `(Error ,(nummer hugo) not found)))))))
            
            
(defun read-and-install-cases (filename)
  (reset-mapper)
  (INSTALL-OBJECT-FROM-FILE filename)
  )


#|

Convert the cases by
create cases object (global)
create current case object
execute ALLEFAELLEAUSFUEHREN :print t
redefine D3-SCHLIESSEN to save the ijnput and then call the original
redefine D3-DIAGNOSEN-ZEIGEN save everyting

(defvar *current-caselist*)

(defun reset-caselist (kb)
  (setq *current-caselist* nil)
  (setq *current-caselist* (make-instance 'caseList :knowledgeBase kb))
  (setf (current-case *current-caselist*)
    (make-instance 'aCase
      :currentNumber 0))
  )

(defun D3-Schliessen-original (symptomnummer wert)
  (SET-ANSWER-VALUE (d3-nummer->objekt symptomnummer) wert)
  (d3-objekt-schliessen (d3-nummer->objekt symptomnummer))
  )

(defun generate-case-object (kb)
  (reset-caselist kb)
  (ALLEFAELLEAUSFUEHREN :print t)
  nil)

(defmethod note-symptom&value ((me caseList) id value)
  (push (make-instance 'aStartValue :id id :initialValue value)
        (start-values (current-case me))))

(defun d3-schliessen (SYMPTOMNUMMER wert)
  (note-symptom&value *CURRENT-CASELIST* symptomnummer wert)
  (D3-Schliessen-original  symptomnummer wert)
  )

(defun D3-DIAGNOSEN-ZEIGEN (&rest egal)
  (declare (ignore egal))
    ;store the diagnoses
  (d3-mit-allen-diagnosen 
    (hugo)
    (let ((tatsaechlich 
           (d-tatsaechliche-punkte hugo)))
      (unless (zerop tatsaechlich)
        (push (make-instance 'aResult
                :diagnosisId (nummer hugo)
                :SymbolString (d3-symbol-string hugo)
                :name (name hugo)
                :points TATSAECHLICH)
              (results (current-case *current-caselist*))))))

  ;store the symptoms
  (let ((index (currentNumber (current-case *current-caselist*))))
    (setf (start-values (current-case *current-caselist*))
      (reverse (start-values (current-case *current-caselist*))))
    (push (current-case *current-caselist*)
          (myCaseList *current-caselist*))
    (setf (current-case *current-caselist*)
      (make-instance 'aCase
        :currentNumber (1+ index)))
    )
  )

(defmethod prepare-for-store ((me caselist))
  ;cases are in inverse order
  (setf (myCaseList me)(sort (myCaseList me) #'< :key #'CURRENTNUMBER))
  ;diagnoses are not sorted
  (dolist (case (myCaseList me))
    (setf (results case)
      (sort (results case) #'> :key #'points)))
  )

(GENERATE-CASE-OBJECT "Books")
(GENERATE-CASE-OBJECT "Neuro")
(PREPARE-FOR-STORE *CURRENT-CASELIST*)

(SAVE-CASELIST *CURRENT-CASELIST*)

(time
 (save-caselist-to-file *CURRENT-CASELIST* "c:\\d3light\\cases\\books.cases"))

(time
 (save-caselist-to-file *CURRENT-CASELIST* "c:\\d3light\\cases\\books.cases2"))

(time
 (save-caselist-to-file *CURRENT-CASELIST* "c:\\d3light\\cases\\neuro.cases"))

(setq *CURRENT-CASELIST* (READ-AND-INSTALL-CASES "c:\\d3light\\cases\\books.cases"))
(EXECUTE-ALL-CASES *CURRENT-CASELIST* :check t)

|#