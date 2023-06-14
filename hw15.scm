;;SICP 4.56 using SICP query language
;a.
(and (supervisor ?who (bitdiddle ben)) (address ?who ?where))
;b.
(and (salary (bitdiddle ben) ?test) (and (salary ?who ?amount) (lisp-value < ?amount ?test))) 
;c.
(and (supervisor ?who ?sup) (job ?sup ?job) (not (job ?sup (computer . ?type))))

;;SICP 4.57
(rule (replace? ?person1 ?person2) 
      (and 
        (job ?person1 ?job1)
        (job ?person2 ?job2)
        (or (same ?job1 ?job2) (can-do-job ?job1 ?job2)) 
        (not (same ?person1 ?person2))))

;returns that ben bitdiddle or alyssa p hacker could replace cy d fect


;;SICP 4.58
(rule (bigshot? ?person1)
      (and (supervisor ?person1 ?sup)
           (job ?sup (?a . ?rest))
           (job ?person1 (?b . ?rest2))
           (not (same ?a ?b))))
;returns that ben bitdiddle and eben scrooge are bigshots
           
;;SICP 4.65
;the (wheel ?who) query returns Oliver Warbucks four times because of the way the streams are filtered down
;through the query system. The (supervisor ?middle-manager ?person) turns into (supervisor ?middle-manager ?who)
;through the pattern matching algorithm. This call assembles a stream of all employees (free variable ?middle-manager)
;who are managed by a supervisor, indicated by the argument ?who. This stream includes three employees who are supervised by Oliver Warbucks
;This stream is then filtered through qeval again via the and call, which asks if the ?middle-manager variable
;in any of the returned stream are a supervisor themselves.

;This returns a listing of Oliver Warbucks for Scrooge Eben, since he is a manager.
;This returns three listing of Oliver Warbucks for Ben Bitdiddle, since he is a manager of three people.
