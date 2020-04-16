(in-package :d3l)

(defparameter *print-debug* nil) ; (setq *print-debug* nil) (setq *print-debug* t)

(defvar *anzahl-betrachtete-regeln*)
(defvar *anzahl-geteste-regeln*)

(defmacro debug-format (&body body)
  #+:debug-print
  `(when *print-debug*
     (format ,@body))
  #-:debug-print
  (declare (ignore body))
  )

(defun i (nummer)
  (inspect (d3-nummer->objekt nummer)))

(defun d3-diagnosen-zeigen (&key (stream t)(nuller nil)(nur-positive t)(alle nil))
  (format stream "~2%Symbol Nummer Status Tatsaechliche-punkte positive negative dynamisches-apriori tatsaechliche-praedisposition punkte~%")
  (d3-mit-allen-diagnosen 
   (hugo)
   (let ((tatsaechlich 
          (d-tatsaechliche-punkte hugo)))
     (when (or alle
               (and (or nuller (not (zerop tatsaechlich)))
                    (or (not nur-positive)(plusp tatsaechlich))))
       (format stream "~A ~a ~a ~a ~a ~A ~a ~a ~,1,,,f ~a~%"
         ;;;;; (name hugo)
         (d3-symbol-string hugo)
         (nummer hugo)
         (name hugo)
         (diagnose-status hugo)
         (d-tatsaechliche-punkte hugo)
         (d-anzahl-positive hugo)
         (d-anzahl-negative hugo)
         (d-dynamisches-apriori hugo)
         (d-tatsaechliche-praedisposition hugo)
         (d-punkte hugo)
         ))))
  )

(defun d3-regeln-zeigen ()
  (d3-mit-allen-regeln 
    (regel)
    (when (d-gefeuert (dynamic-instance regel))
      (format t "Regel ~a~%" (nummer regel))
      )))



(defun d3-anzahl-gefeuerte-regeln ()
  (let ((anzahl 0))
     (d3-mit-allen-regeln 
       (regel)
       (when (d-gefeuert (dynamic-instance regel))
         (incf anzahl)))
     anzahl))

#|
(let ((anzahl 0)
      (kondi 0)
      )
  (d3-mit-allen-regeln 
    (regel)
    (incf anzahl)
    (incf kondi (length (konditionsliste (kondition regel))))
    )
  (values anzahl kondi))
|#

(defun d3-anzahl-regeln ()
  (let ((anzahl 0))
     (d3-mit-allen-regeln 
       (regel)
       (incf anzahl))
     anzahl))
 
                        

#|
(D3-DIAGNOSEN-ZEIGEN)
(d3-anzahl-regeln)
(d3-anzahl-gefeuerte-regeln)
(i '40)
(i 'p7)
(i 'Rfb4560)
(i 'Radd4154)
(d-tatsaechliche-punkte (d3-nummer->objekt 'p405))
(regel-vorbedingung-gilt-p (d3-nummer->objekt (cl-user::d3-symbol->d3l-symbol 'cl-user::Rfb4673)))

(i (cl-user::d3-symbol->d3l-symbol 'cl-user::Rfb4673))
(i 3190)

(trace einzelkondition-gilt-p)
(trace unbekannt-gilt-p)
|#