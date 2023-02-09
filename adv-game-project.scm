;;1. seems to work as expected
;;2A. calling the person object returns a procedure
;;2B. places understand the messages: 'type, 'neighbors, 'exits, 'look-in, 'appear,'enter,'gone,'exit,
;;    'new-neighbor,'add-entry-procedure,'add-exit-procedure, 'remove-entry-procedure, 
;;    'remove-exit-procedure, and 'clear-all-procs
;;2C. all calls work as expected, even without defining the op level objects
;;    the call to ask something of 'Peoples-park' does not work, since our working environment
;;    does not have a pointer to the object of 'Peoples-park'
;;2D. (ask 61A-Lab 'appear computer) should be correct, since the global environment does not know what Durer
;;    is, as we did not define it. We also do not want to just add the string 'Durer, which is not an object.
;;2E. 

;auxillary procedures for debugging
(define (name obj) (ask obj 'name))
(define (inventory obj)
  (if (person? obj)
    (map name (ask obj 'possessions))
    (map name (ask obj 'things))))

(define (whereis obj)
  (if (person? obj)
    (name (ask obj 'place))
    "Object not a person"))

(define (owner obj)
  (if (thing? obj)
    (name (ask obj 'possesor) 'place)
    "Object not a thing"))
