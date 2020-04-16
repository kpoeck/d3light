(in-package :cl-user)

(eval-when
    #-gcl (:compile-toplevel :execute :load-toplevel)
    #+:gcl (compile load eval) 
  (proclaim '(optimize (speed 0) (safety 3) (space 0)(debug 3)(compilation-speed 0)))
  )

;;;Tested in LispWorks, ACL, Clisp and CMU, sbcl ,corman, clozure, clasp, abcl, ecl

(defparameter *my-root* *LOAD-TRUENAME*)

#-(or :cormanlisp gcl)
(let ((default 
        #+rmcl "**:"
        #+(or darwin unix) "**/"
        #-(or darwin unix rmcl) "**\\"
        )
      )
  (flet ((complete-name (sub)
                        (concatenate 'string (namestring (make-pathname
                                                          :name nil
                                                          :type nil
                                                          :version nil
                                                          :host (pathname-host *my-root*)
                                                          :device (pathname-device *my-root*)
                                                          :directory (butlast (pathname-directory *my-root*))))
			sub "*.*")))
    
    (setf (logical-pathname-translations "d3l")
	  `( 
	    ("**;*.*.*"              ,(pathname (complete-name default)))
	    )
	  )
    )
  )

(defun lisp-info ()
  (concatenate 'string
    (lisp-implementation-version) " " (lisp-implementation-type)))
#|
(translate-logical-pathname "d3l:wissensbasen;test.lisp")
|#

(defun translate-filename (was)
  #+(or allegro lispworks cmu sbcl scl CLOZURE-COMMON-LISP ecl rmcl abcl) was
  #+(or clisp clasp ) (merge-pathnames (translate-logical-pathname was) "*.lisp")
  #+(or cormanlisp gcl)
  ;parse the string into Host (till :)
  ;dir :beetween : and ;
  ;filename after:
  (let ((suffix nil)
        (host "")
        (dir "")
        (filename "")
        (current "")
        )
    (dotimes (index (length was))
      (let ((current-char (aref was index)))
        (cond ((char-equal current-char #\:)
               (setq host current current ""))
              ((char-equal current-char #\;)
               (setq dir current current ""))
              (T (when (char-equal current-char #\.)
                   (setq suffix t))
                 (setq current (concatenate 'string current (string current-char)))))
        )
      )
    (setq filename current)
    (concatenate 'string "c:\\data\\lisp\\d3light\\" dir "\\" filename (if suffix "" ".lisp"))
    )
  #-(or allegro lispworks cmu sbcl scl ecl clisp cormanlisp gcl CLOZURE-COMMON-LISP rmcl clasp abcl)
  (error "don't know this lisp")
  )

(defun load-the-stuff (file &key (IF-NEWER nil)(print t)(verbose t))
  VERBOSE
  IF-NEWER
  #+:allegro
  (compile-file
   file
   :verbose verbose
   :print print
   :if-newer IF-NEWER
   :load-after-compile t)

  #+:lispworks
  (compile-file
   file
   :verbose verbose
   :print print
   :load t)

  #+:cormanlisp
  (load (translate-filename file) :verbose verbose)
  
  #+:gcl
  (multiple-value-bind
        (fasl warning failure)
      (compile-file
       (translate-filename file)
      )
    warning
    failure
    (if fasl
        (load fasl :print print :verbose verbose)
      (error "Fasl not generated~s~%" file)))

  #-(or :lispworks :allegro :cormanlisp :gcl)
  (multiple-value-bind
        (fasl warning failure)
      (compile-file
       (translate-filename file)
       :verbose verbose :print print)
    warning
    failure
    (load fasl :print print :verbose verbose))

  )

#+(or :gcl :cormanlisp)
(defmacro WITH-COMPILATION-UNIT (arg &body body)
  (declare (ignore arg body))
  `(progn ,@body))

(defun compile&load-kernel (&key (if-newer nil)(arrays-p nil))
  (with-compilation-unit ()
    (load-the-stuff "d3l:kernel;paket" :if-newer if-newer)
    (load-the-stuff "d3l:kernel;typen":IF-NEWER IF-NEWER)

    (if arrays-p
        (load-the-stuff "d3l:kernel;array" :IF-NEWER IF-NEWER)
      (load-the-stuff "d3l:kernel;listen" :IF-NEWER IF-NEWER))

    (load-the-stuff "d3l:kernel;klassen" :IF-NEWER IF-NEWER)
    (load-the-stuff "d3l:kernel;dynamicclasses" :IF-NEWER IF-NEWER)

    (load-the-stuff "d3l:kernel;objekte-erzeugen" :IF-NEWER IF-NEWER)

    (load-the-stuff "d3l:kernel;debug" :IF-NEWER IF-NEWER)
    (load-the-stuff "d3l:kernel;interpreter" :IF-NEWER IF-NEWER)
    (load-the-stuff "d3l:kernel;schliessen" :IF-NEWER IF-NEWER)
    (load-the-stuff "d3l:kernel;kategorisch" :IF-NEWER IF-NEWER)
    (load-the-stuff "d3l:kernel;heuristisch" :IF-NEWER IF-NEWER)
    (load-the-stuff "d3l:kernel;datenabstraktion" :IF-NEWER IF-NEWER)

    (load-the-stuff "d3l:kernel;storeread" :IF-NEWER IF-NEWER)
    (load-the-stuff "d3l:kernel;cases" :IF-NEWER IF-NEWER)
    (load-the-stuff "d3l:kernel;test" :IF-NEWER IF-NEWER)

    (load-the-stuff "d3l:kernel;non" :IF-NEWER IF-NEWER)
    (load-the-stuff "d3l:kernel;export" :IF-NEWER IF-NEWER)
    )
  :ready
  )

(defmacro with-fast-execution (&body body)
  `(#+nada nada
           #+:excl mp:without-scheduling
           #+:lispworks mp:without-preemption
           #-(or :excl :lispworks) progn
           ,@body))

(compile&load-kernel)

 #|

(eval-when
    (:compile-toplevel :execute :load-toplevel)
  (proclaim '(optimize (speed 3) (safety 0) (space 0)(debug 0)
                       #+:lispworks (compilation-speed 0)
                       #+:lispworks (fixnum-safety 0)
                       #+:lispworks (float 0)
                       #+:lispworks (sys:interruptable 3)

                       )
            )
  )

(pushnew :fast *features*)
(compile&load-kernel)

(eval-when
    (:compile-toplevel :execute :load-toplevel)
  (proclaim '(optimize (speed 0) (safety 3) (space 0)(debug 3)
                       #+:lispworks (compilation-speed 0)
                       #+:lispworks (fixnum-safety 3)
                       #+:lispworks (float 3)
                       #+:lispworks (sys:interruptable 0)
                       )
            )
  )



(setq *FEATURES* (delete :fast *FEATURES*))
(compile&load-kernel)

(compile&load-kernel :ARRAYS-P t)

(d3l::execute-a-test-case "d3l:knowledgebase;neuro4.kb" "d3l:cases;neuro.cases")

(d3l::execute-a-test-case (cl-user::TRANSLATE-FILENAME "d3l:knowledgebase;neuro6.kb" )
                          (cl-user::TRANSLATE-FILENAME "d3l:cases;neuro.cases"))

(d3l::execute-a-test-case (cl-user::TRANSLATE-FILENAME "d3l:knowledgebase;books11.kb")
                          (cl-user::TRANSLATE-FILENAME "d3l:cases;books.cases"))





"6.2.beta [Windows] (May 11, 2002 23:01) International Allegro CL Enterprise Edition"
;(4 "Execute") 
; cpu time (non-gc) 4,560 msec user, 0 msec system
; cpu time (gc)     0 msec user, 0 msec system
; cpu time (total)  4,560 msec user, 0 msec system
; real time  4,560 msec
; space allocation:
;  142 cons cells, 19,528 other bytes, 0 static bytes
"4.2.0 LispWorks Personal Edition" "Windows"
;(4 "Execute") 
;Timing the evaluation of (EXECUTE-ALL-CASES CASES)
;user time    =      4.390
;system time  =      0.000
;Elapsed time =   0:00:04
;Allocation   = 1500864 bytes standard / 3091 bytes fixlen
"2.28 (released 2002-03-03) (built on ampy.LAIR [192.168.0.1]) CLISP""Visual C"
;(1 "Execute")
;Real time: 36.14 sec.
;Run time: 36.14 sec.
;Space: 33588 Bytes
                 
"2.28 (released 2002-03-03) (built 3226686662) (memory 3226687537) CLISP""cygwin"

;(4 "Execute")
;Real time: 20.938393 sec.
;Run time: 0.0 sec.
;Space: 33588 Bytes

"1.5 Corman Common Lisp"
;(2 "Execute") Total Execution time: 69.457408 seconds
;Time spent garbage collecting: 3.13832 seconds

"18d CMU Common Lisp" "Linux"

;(4 "Execute") 
;Evaluation took                                                      :
;  3.56 seconds of real time
;  3.47 seconds of user run time
;  0.0 seconds of system run time
;  0 page faults and
;  98272 bytes consed.


;;; New Laptop
"7.0.beta [Windows] (Jul 30, 2004 21:39)"
;(4 "Execute") 
; cpu time (non-gc) 1,272 msec user, 0 msec system
; cpu time (gc)     0 msec user, 0 msec system
; cpu time (total)  1,272 msec user, 0 msec system
; real time  1,272 msec
; space allocation:
;  26 cons cells, 10,504 other bytes, 0 static bytes

"Lispworks 4.3.7"
;;;(4 "Execute") 
;;;Timing the evaluation of (EXECUTE-ALL-CASES CASES)
;;;
;;;user time    =      1.291
;;;system time  =      0.000
;;;Elapsed time =   0:00:02
;;;Allocation   = 1641720 bytes standard / 2101 bytes conses
;;;0 Page faults

"clisp 2.33.1 (2004-05-22) (built on winsteingoldlap [192.168.1.101])"
;;;(4 "Execute")
;;;Real time: 7.459 sec.
;;;Run time: 7.451 sec.
;;;Space: 33588 Bytes

"Corman 2.5"

;;;(4 "Execute") Total Execution time: 30.582878 seconds
;;;Time spent garbage collecting: 3.712505 seconds


|#
