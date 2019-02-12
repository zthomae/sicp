(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))

(assert! (rule (reverse () ())))
(assert! (rule (reverse (?x . ?xs) ?y)
               (and (reverse ?xs ?xs-rev)
                    (append-to-form ?xs-rev (?x) ?y))))
