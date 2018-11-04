;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2 Op-blocks world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain BLOCKS)
  (:requirements :strips)
  (:predicates (on ?x ?y)
	       (ontable ?x)
	       (clear ?x)
	       )

  (:action move-to-block
	     :parameters (?x ?to)
	     :precondition (and (clear ?x) (ontable ?x) (clear ?to))
	     :effect
	     (and 
		      (on ?x ?to)
	          (not (ontable ?x))
		      (not (clear ?to))
		      
		 )
  )

 (:action move-to-table
	     :parameters (?x ?from)
	     :precondition (and (clear ?x) (on ?x ?from) )
	     :effect
	     (and (not (on ?x ?from))
		      (clear ?from)
		      (ontable ?x)
		 )
  )
)