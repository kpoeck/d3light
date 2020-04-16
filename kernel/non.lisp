(in-package :d3l)

(defmethod nein-gilt-p (einzelkondition (ich D3-MC-VALUE))
  (einzelkondition-gilt-bei-non einzelkondition ich))

(defmethod einzelkondition-gilt-bei-non ((ich d3-einzelkondition) objekt)
  (declare (ignore objekt))
  nil)

(defmethod einzelkondition-gilt-bei-non ((ich D3-$BEKANNT-EINZELKONDITION)
                                                        objekt)
  (declare (ignore objekt))
  nil
  )

(defmethod EINZELKONDITION-GILT-BEI-NON ((ich d3-$=-einzelkondition) objekt)
  (declare (ignore objekt))
  (negiert-p ich))

(defmethod EINZELKONDITION-GILT-BEI-NON ((ich d3-$or-einzelkondition) objekt)
  (declare (ignore objekt))
  (negiert-p ich))

(defmethod EINZELKONDITION-GILT-BEI-NON ((ich d3-$and-einzelkondition) objekt)
  (declare (ignore objekt))
  (negiert-p ich))
