;;1. seems to work as expected
;;2A. calling the person object returns a procedure
;;2B. not sure yet, need to dig into the code
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

;can't figure out how to get thing locations yet, still working
(define (whereis obj)
  (if (person? obj)
    (ask obj 'place)
    obj))
