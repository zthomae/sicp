(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (grandson ?G ?S)
               (and (son ?G ?F)
                    (son ?F ?S))))

(assert! (rule (son ?F ?S)
               (and (wife ?F ?M)
                    (son ?M ?S))))

(assert! (rule (false)
               (lisp-value > 0 1)))

(assert! (rule (ends-with-grandson ())
               (false)))
(assert! (rule (ends-with-grandson (grandson))))
(assert! (rule (ends-with-grandson (?x . ?xs))
               (ends-with-grandson ?xs)))

(assert! (rule ((great) ?elder ?younger)
               (false)))

(assert! (rule ((great . ?rel) ?elder ?younger)
               (and (ends-with-grandson ?rel)
                    (and (son ?elder ?middle)
                         (?rel ?middle ?younger)))))



(assert! (rule ((great grandson) ?elder ?younger)
               (and (son ?elder ?middle)
                    (grandson ?middle ?younger))))