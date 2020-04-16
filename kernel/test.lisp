(in-package :d3l)

(defvar *current-case-list* )

(defun execute-a-test-case (kbFile caseFile)
  (let ((*package* (find-package :d3l)))
    (print "loadKB")
    (time (READ-AND-INSTALL-KNOWLEDGE-BASE kbFile))
    #+:aclno
    (excl::gc :tenure)
    (print "loadCases")
    (Let ((cases (time (READ-AND-INSTALL-CASES caseFile))))
      #+:aclno
      (excl::gc :tenure)
      (setq *current-case-list* cases)
      (print "FirstExecute and Check")
      (time (EXECUTE-ALL-CASES cases :check t))
      (dotimes (x 5)
        (print `(,x "Execute"))
        (time
         (EXECUTE-ALL-CASES cases))))))

(defun prepare-test-run-and-check (kbFile caseFile)
   (let ((*package* (find-package :d3l)))
    (time (READ-AND-INSTALL-KNOWLEDGE-BASE kbFile))
    (Let ((cases (time (READ-AND-INSTALL-CASES caseFile))))
      (setq *current-case-list* cases)
    (time (EXECUTE-ALL-CASES cases :check t)))))

(defun execute-test-run (cases)
  (EXECUTE-ALL-CASES cases)
  )

;;; now execute the global var

(defun load-the-benchmark ()
  (cl-user::load-the-stuff "d3l:wissensbasen;neurowissensbasis" :print nil)
  (cl-user::load-the-stuff "d3l:faelle;193neuros" :print nil)
  )

(defun load-book()
  (cl-user::load-the-stuff "d3l:wissensbasen;buch-Wissensbasis-neu" :print nil)
  (cl-user::load-the-stuff "d3l:faelle;18-buecher"  :print nil)
  )

(defun load-bauch ()
  (cl-user::load-the-stuff "d3l:wissensbasen;bauchschmerz" :print nil)
  (cl-user::load-the-stuff "d3l:faelle;17refu" :print nil)
  )


(defun do-the-benchmark () 
  (dotimes (i 5)
    (time
     (ALLEFAELLEAUSFUEHREN)))
  )

(defun allefaelleausfuehren ())

(defun test1 ()
  (execute-a-test-case (cl-user::TRANSLATE-FILENAME "d3l:knowledgebase;books10.kb")
	(cl-user::TRANSLATE-FILENAME "d3l:cases;books.cases"))
  )

(defun test1-prepare ()
  (prepare-test-run-and-check
   (cl-user::TRANSLATE-FILENAME "d3l:knowledgebase;books10.kb")
   (cl-user::TRANSLATE-FILENAME "d3l:cases;books.cases"))
  )

(defun test1-execute ()
  (execute-test-run *current-case-list*)
  )


(defun test2-prepare ()
  (prepare-test-run-and-check
   (cl-user::TRANSLATE-FILENAME "d3l:knowledgebase;neuro6.kb")
   (cl-user::TRANSLATE-FILENAME "d3l:cases;neuro2.cases"))
  )

(defun test2-execute ()
  (execute-test-run *current-case-list*)
  )

(defun test2 ()
  (execute-a-test-case 
   (CL-USER::TRANSLATE-FILENAME "d3l:knowledgebase;neuro6.kb" )
   (CL-USER::TRANSLATE-FILENAME "d3l:cases;neuro2.cases"))
  )

#|

(execute-a-test-case "d3l:knowledgebase;books8.kb" "d3l:cases;books.cases")

(execute-a-test-case "d3l:knowledgebase;neuro6.kb" "d3l:cases;neuro.cases")

(time
 (EXECUTE-ALL-CASES d3l::*current-case-list*))

(time (dotimes (x 1000000)
        (d3-nummer->objekt 2)))

(time
 (dotimes (x 193)
   (d3-fall-ruecksetzen)))

|#
    
