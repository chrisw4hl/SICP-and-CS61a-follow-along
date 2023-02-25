;;; small-world.scm
;;; Miniature game world for debugging the CS61A adventure game project.
;;; You can load this instead of adv-world.scm, and reload it quickly
;;; whenever you change a class.

;;; How to use this file:
;;; If, for example, your person class doesn't work, and you do something
;;; like (define Matt (instantiate person 'Matt)), and then fix your
;;; person class definition, Matt is still bound to the faulty person
;;; object from before.  However, reloading this file whenever you
;;; change something should redefine everything in your world with the
;;; currently loaded (i.e. most recent) versions of your classes.

(define 61A-Lab (instantiate place '61A-Lab))
(define Lounge (instantiate place 'Lounge))
(define locked-closet (instantiate locked-place 'locked-closet))
(define garage (instantiate garage 'garage))
(define starbucks (instantiate hotspot 'starbucks 'coffee123))
(define jail (instantiate place 'jail))
(define bagel-barn (instantiate restaurant 'bagel-barn bagel .50))
(can-go 61A-Lab 'up Lounge)
(can-go 61A-Lab 'south locked-closet)
(can-go locked-closet 'north 61A-Lab)
(can-go Lounge 'down 61A-Lab)
(can-go 61A-Lab 'east garage)
(can-go garage 'west 61A-Lab)
(can-go 61A-Lab 'west starbucks)
(can-go starbucks 'east 61A-Lab)
(can-go 61A-Lab 'north bagel-barn)
(can-go bagel-barn 'south 61A-lab)
;;;  Hopefully you'll see more of the world than this in real life
;;;  while you're doing the project!
(define Chris (instantiate person 'Chris 61A-Lab))
(define homework-box (instantiate thing 'homework-box))
(ask 61A-Lab 'appear homework-box)

(define Coke (instantiate food 'Coke 100))

(define macbook (instantiate laptop 'macbook)) 

(ask Lounge 'appear Coke)
(ask 61A-Lab 'appear macbook)
(define laba (instantiate person 'Lab-assistant 61A-Lab))

(define ferarri (instantiate thing 'ferarri))
(ask 61A-Lab 'appear ferarri)

(define slice (instantiate food 'pizza 200))
(ask 61A-Lab 'appear slice)
(define prof (instantiate thief 'prof 61A-Lab))

