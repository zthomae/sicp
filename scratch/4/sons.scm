(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert!  (rule (grandson ?G ?S)
                (and (son ?G ?F)
                     (son ?F ?S))))

(assert!  (rule (son ?F ?S)
                (and (wife ?F ?M)
                     (son ?M ?S))))

(assert! (rule (great-modifiable grandson)))

(assert! (rule (nil ())))

(assert! (rule ((great . (?r . ?rs)) ?x ?desc)
               (and (son ?x ?y)
                    (or (and (?r ?y ?desc)
                             (great-modifiable ?r)
                             (nil ?rs))
                        (and ((?r . ?rs) ?y ?desc)
                             (not (nil ?rs)))))))
