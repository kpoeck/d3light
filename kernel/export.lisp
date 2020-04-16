(in-package :d3l)

(eval-when
    #-:gcl (:load-toplevel :execute)
  #+:gcl (compile load eval)
  (export '(d3-symptom d3-benamtes-objekt name d3-mit-allen-fragen
                       d3-frage-oc d3-frage-mc d3-frage-jn
                       d3-frage-num d3-frage-text
                       d3-moc-value
                       moegliche-werte moegliche-werte-texte
                       eingangsfragen d3-mit-allen-frageklassen
                       d3-nummer->objekt
                       antwortalternative-als-string
                       d3-frageklasse
                       D3-schliessen-aus-objekt
                       folgefragen-abholen
                       geloeschte-folgefragen-abholen
                       
                       d3-create-knowledge-base
                       d3-fall
                       )
          (find-package :d3l))
  )