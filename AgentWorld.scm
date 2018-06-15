;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;                      Name: Jayant Kashyap
;;                      SCU ID: 1433401
;;                      Assignment: Agent World
;;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



;; Initialize Agent
(define (initialize-agent) "Hi ALL!")


; apply-action
(define (apply-action state action)
	(cond	((equal? action "STAY")		state)
		((equal? action "MOVE-1")	(apply-action-helper-move state 1))
		((equal? action "MOVE-2")	(apply-action-helper-move state 2))
		((equal? action "MOVE-3")	(apply-action-helper-move state 3)) 
		((equal? action "TURN-LEFT")	(apply-action-helper-turn state 'L)) 
		((equal? action "TURN-RIGHT")	(apply-action-helper-turn state 'R)) 
		((equal? action "TURN-AROUND")	(apply-action-helper-turn state 'A))))

(define (apply-action-helper-move state move)
	(cond	((= move 1)	(cond	((equal? (nth-item 3 state) 'N)	(replace-nth-item 2 state (+ (nth-item 2 state) 1)))
					((equal? (nth-item 3 state) 'E)	(replace-nth-item 1 state (+ (nth-item 1 state) 1)))
					((equal? (nth-item 3 state) 'S)	(replace-nth-item 2 state (+ (nth-item 2 state) (- 1))))
					((equal? (nth-item 3 state) 'W)	(replace-nth-item 1 state (+ (nth-item 1 state) (- 1))))))
		((= move 2)	(cond	((equal? (nth-item 3 state) 'N) (replace-nth-item 2 state (+ (nth-item 2 state) 2)))
					((equal? (nth-item 3 state) 'E)	(replace-nth-item 1 state (+ (nth-item 1 state) 2)))
					((equal? (nth-item 3 state) 'S)	(replace-nth-item 2 state (+ (nth-item 2 state) (- 2))))
					((equal? (nth-item 3 state) 'W)	(replace-nth-item 1 state (+ (nth-item 1 state) (- 2))))))
		((= move 3)	(cond	((equal? (nth-item 3 state) 'N) (replace-nth-item 2 state (+ (nth-item 2 state) 3)))
					((equal? (nth-item 3 state) 'E)	(replace-nth-item 1 state (+ (nth-item 1 state) 3)))
					((equal? (nth-item 3 state) 'S)	(replace-nth-item 2 state (+ (nth-item 2 state) (- 3))))
					((equal? (nth-item 3 state) 'W)	(replace-nth-item 1 state (+ (nth-item 1 state) (- 2))))))))

(define (apply-action-helper-turn state turn)
	(cond	((equal? turn 'L)	(cond	((equal? (nth-item 3 state) 'N)	(replace-nth-item 3 state 'W))
						((equal? (nth-item 3 state) 'E)	(replace-nth-item 3 state 'N))
						((equal? (nth-item 3 state) 'S)	(replace-nth-item 3 state 'E))
						((equal? (nth-item 3 state) 'W)	(replace-nth-item 3 state 'S))))
		((equal? turn 'R)	(cond	((equal? (nth-item 3 state) 'N)	(replace-nth-item 3 state 'E))
						((equal? (nth-item 3 state) 'E)	(replace-nth-item 3 state 'S))
						((equal? (nth-item 3 state) 'S)	(replace-nth-item 3 state 'W))
						((equal? (nth-item 3 state) 'W)	(replace-nth-item 3 state 'N))))
		((equal? turn 'A)	(cond	((equal? (nth-item 3 state) 'N)	(replace-nth-item 3 state 'S))
						((equal? (nth-item 3 state) 'E)	(replace-nth-item 3 state 'W))
						((equal? (nth-item 3 state) 'S)	(replace-nth-item 3 state 'N))
						((equal? (nth-item 3 state) 'W)	(replace-nth-item 3 state 'E))))))


; choose-action -> choose what action needs to be performed based on history of events and percepts
(define (choose-action current-energy previous-events percepts)
    (let*   ((s     (score-stay previous-events percepts))
            (r      (score-turn-right previous-events percepts))
            (l      (score-turn-left previous-events percepts))
            (a      (score-turn-around previous-events percepts))
            (eatp   (score-eat-passive previous-events percepts))
            (eata   (score-eat-aggressive current-energy previous-events percepts))
            (movep1 (score-move-passive-1 previous-events percepts))
            (movea1 (score-move-aggressive-1 current-energy previous-events percepts))
            (movep2 (score-move-passive-2 previous-events percepts))
            (movea2 (score-move-aggressive-2 current-energy previous-events percepts))
            (movep3 (score-move-passive-3 previous-events percepts))
            (movea3 (score-move-aggressive-3 current-energy previous-events percepts))
            (hscore (find-biggest (list s r l a eatp eata movep1 movea1 movep2 movea2 movep3 movea3))))
    
    (cond   ((equal? hscore s)  "STAY")
            ((equal? hscore r)  (begin  (set! right-corner bottom)
                                        (set! bottom left-corner)
                                        (set! left-corner (get-location percepts 0 1))
                                        "TURN-RIGHT"))
            ((equal? hscore l)  (begin  (set! left-corner bottom)
                                        (set! bottom right-corner)
                                        (set! right-corner (get-location percepts 0 1))
                                        "TURN-LEFT"))
            ((equal? hscore a)  (let    ((left_t left-corner))
                                (begin  (set! left-corner right-corner)
                                        (set! right-corner left_t)
                                        (set! bottom (get-location percepts 0 1))
                                        "TURN-AROUND")))
            ((equal? hscore eatp)       "EAT-PASSIVE")
            ((equal? hscore eata)       "EAT-AGGRESSIVE")
            ((equal? hscore movep1)     (begin  (set! left-corner (get-location percepts -1 1))
                                                (set! right-corner (get-location percepts 1 1))
                                                (set! bottom 'empty)
                                                "MOVE-PASSIVE-1"))
            ((equal? hscore movep2)     (begin  (set! left-corner (get-location percepts -1 2))
                                                (set! right-corner (get-location percepts 1 2))
                                                (set! bottom 'empty)
                                                "MOVE-PASSIVE-2"))
            ((equal? hscore movep3)     (begin  (set! left-corner (get-location percepts -1 3))
                                                (set! right-corner (get-location percepts 1 3))
                                                (set! bottom 'empty)
                                                "MOVE-PASSIVE-3"))
            ((equal? hscore movea1)     (begin  (set! left-corner (get-location percepts -1 1))
                                                (set! right-corner (get-location percepts 1 1))
                                                (set! bottom 'empty)
                                                "MOVE-AGGRESSIVE-1"))
            ((equal? hscore movea2)     (begin  (set! left-corner (get-location percepts -1 2))
                                                (set! right-corner (get-location percepts 1 2))
                                                (set! bottom 'empty)
                                                "MOVE-AGGRESSIVE-1"))
            ((equal? hscore movea3)     (begin  (set! left-corner (get-location percepts -1 3))
                                                (set! right-corner (get-location percepts 1 3))
                                                (set! bottom 'empty)
                                                "MOVE-AGGRESSIVE-1"))
            (#f                         "STAY"))))



;; ========================================================================================
;; +++++++++++++++++++++++++++++++++++ HELPER FUNCTIONS +++++++++++++++++++++++++++++++++++
;; ========================================================================================

; nth-item -> to get nth-item from the list
(define (nth-item a lst)
	(cond 	((null? lst)    0)
	      	((= a 1)        (car lst))
	      	(#t			    (nth-item (- a 1) (cdr lst)))))


; replace-nth-item -> to replace nth-item from the list
(define (replace-nth-item i lst r)
	(cond 	((null? lst)	(cons r lst))
	      	((= i 1)		(cons r (cdr lst)))
	      	(#t			    (cons (car lst) (replace-nth-item (- i 1) (cdr lst) r)))))

; get-n-items -> returns n items from list
(define (get-n-items a n)
    (if (> n 0)
        (cons (car a) (get-n-items (cdr a) (- n 1)))
        '()))

; get-sublist -> returns sublist from list starting at a and offset o
(define (get-sublist a s o)
    (if (> s 1)
        (get-sublist (cdr a) (- s 1) o)
        (get-n-items a o)))

; get-location -> returns location at horzontal and vertical positions
(define (get-location percept posH posV) (nth-item (+ (+ posH posV) 1) (nth-item posV percept))) 

; is-empty? -> returns boolean if the given location is empty
(define (is-empty? location)
    (if (equal? location 'empty)
        #t
        #f))

; find-biggest -> returns biggest of the list
(define (find-biggest a)
    (if     (null? (cdr a)) (car a)
            (if (> (car a) (find-biggest (cdr a)))
                (car a)
                (find-biggest (cdr a)))))




;; ========================================================================================
;; +++++++++++++++++++++++++++++++++++ ENVIRONMENT CONFIG +++++++++++++++++++++++++++++++++
;; ========================================================================================


;; BARRIER
; is-barrier? -> returns true if barrier found
(define (is-barrier? location)
    (if (equal? location 'barrier)
        #t
        #f))



;; VEGETATION
; is-vegetation? -> returns true is there is vegetation at a given location
(define (is-vegetation? location)
    (if (and (list? location) (= (length location) 3))
        #t
        #f))

; vegetation-energy -> returns vegetaion  energy from a location
(define (vegetation-energy location)
    (if (is-vegetation? location)
        (nth-item 3 location)
        0))



;; PREDATOR
; is-predator? -> returns true is there is predator at the given location
(define (is-predator? location)
    (if (and (list? location) (= (length location) 2))
        #t
        #f))


;; AGENT
; is-agent? -> returns true if there is agent at the location
(define (is-agent? location)
    (if (and (list? location) (= (length location) 4))
        #t
        #f))

; agent-energy -> returns energy of the agent at location
(define (agent-energy location) (expt 2 (nth-item 3 location)))



;; ENVIRONMENT
; path-empty? -> checks if the path is empty
(define (path-empty? percepts l h)
        (if (> l h) #t
            (let    ((curr-location (get-location percepts 0 l)))
	                (if (is-empty? curr-location) 
                        (path-empty? percepts (+ l 1) h)
	                    #f))))




;; ========================================================================================
;; +++++++++++++++++++++++++++++++++++++++ STRATEGY +++++++++++++++++++++++++++++++++++++++
;; ========================================================================================

;; direction lists
(define left-corner '())
(define right-corner '())
(define bottom '())



;; EVENTS IN THE ARCADE
; current-accessible -> returns 0 if not accessible 
(define  (current-accessible percepts x y)
    (let    ((curr-location (get-location percepts x y)))
            (cond   ((is-barrier? curr-location) 1)
	                ((is-predator? curr-location) 1)
	                ((is-vegetation? curr-location) 1)
	                ((is-agent? curr-location) 1)
	                (#t (if (= y 1)
		                0
		                (current-accessible percepts x (- y 1)))))))


; is-attacked? -> returns true if agent is  attacked
(define (is-attacked? previous-events)
        (if (null? previous-events) 
            #f
            (let    ((curr-event (car previous-events)))
	                (if (equal? (car curr-event) 'attacked-by) 
                        #t
	                    (is-attacked? (cdr previous-events))))))

; action-underattack? -> returns value wrt type of attack
(define (action-underattack? previous-events percepts moving-steps)
        (let    ((step1 (get-location percepts 0 1))
	            (step2 (get-location percepts 0 2))
	            (step3 (get-location percepts 0 3)))
                (if (is-attacked? previous-events)
	                (cond ((= moving-steps 1) (if (is-empty? step1) 1 0))
	                      ((= moving-steps 2) (if (and (is-empty? step1) (is-empty? step2)) 1 0))
	                      ((= moving-steps 3) (if (and (is-empty? step1) (is-empty? step2) (is-empty? step3)) 1 0))
	                      (#t 0))
                0)))

; run-away-underattack? -> returns 0 otherwise 
(define (run-away-underattack? previous-events origin-state action)
        (if (and (is-attacked? previous-events) (is-empty? origin-state)
	             (or (equal? action "STAY")
	                 (equal? action "TURN-LEFT")
	                 (equal? action "TURN-RIGHT")
	                 (equal? action "TURN-AROUND")
	                 (equal? action "EAT-PASSIVE")
	                 (equal? action "EAT-AGGRESSIVE")))
            1
            0))

; stay-face-predator? -> returns 1 if yes and 0 otherwise
(define (stay-face-predator? percepts)
    (if (is-predator? (get-location percepts 0 1)) 1 0))




;; AGENT STATE
; damage-level -> returns the damage level from is-attacked?
(define (damage-level previous-events)
    (if (null? previous-events) 
        0
        (let ((curr-event (car previous-events)))
	        (if (equal? (car curr-event) 'attacked-by)
	            (+ (nth-item 3 curr-event) (damage-level (cdr previous-events)))
	            (damage-level (cdr previous-events))))))
      
; has-ate? -> whether the agent eat the vegetation in last turn
(define (has-ate? previous-events)
        (if (null? previous-events) 
            #f
            (let    ((curr-event (car previous-events)))
	                (if (equal? (car curr-event) 'ate) 
                        #t
	                    (has-ate? (cdr previous-events))))))


; surrounded-by-predator? -> whether a location is surrounded by predator
(define (surrounded-by-predator? x y percepts)
    (if (= y 0)
        (or (is-predator? (get-location percepts 0 1))
	        (is-predator? left-corner)
	        (is-predator? right-corner)
	        (is-predator? bottom))
        (or (is-predator? (get-location percepts x (+ y 1)))
	        (is-predator? (get-location percepts (- x 1) y))
	        (is-predator? (get-location percepts (+ x 1) y)))))

; surrounded-by-veg-2? -> if after moving by 2 steps is the energy is greater than 30
(define (surrounded-by-veg-2? x y percepts)
    (let    ((above (get-location percepts x (+ y 1)))
	        (left (get-location percepts (- x 1) y))
	        (right (get-location percepts (+ x 1) y)))
            (if (or (and (is-vegetation? above) (> (vegetation-energy above) 30))
	            (and (is-vegetation? left) (> (vegetation-energy left) 30))
	            (and (is-vegetation? right) (> (vegetation-energy right) 30)))
	            1
	            0)))

; is-surrond-by-vegetaion-3 -> if after moving by 3 steps is energy greater than 60
(define (surrounded-by-veg-3? x y percepts)
    (let    ((above (get-location percepts x (+ y 1)))
	        (left (get-location percepts (- x 1) y))
	        (right (get-location percepts (+ x 1) y)))
            (if (or (and (is-vegetation? above) (> (vegetation-energy above) 60))
	            (and (is-vegetation? left) (> (vegetation-energy left) 60))
	            (and (is-vegetation? right) (> (vegetation-energy right) 60)))
	            1
	            0)))

; stronger? -> location or the agents all weaker than me, otherwise return #t
(define (stronger? x y percepts my-energy)
    (let    ((above (get-location percepts x (+ y 1)))
	        (left (get-location percepts (- x 1) y))
	        (right (get-location percepts (+ x 1) y)))
            (or (and (is-agent? above)(> (agent-energy above) my-energy))
	            (and (is-agent? left)(> (agent-energy left) my-energy))
	            (and (is-agent? right)(> (agent-energy right) my-energy)))))




;; EVIRONMENT STATE

; strongest-1? -> checks strength in the neighbors till 1 2 3 levels
(define (strongest-1? my-energy percepts)
        (let ((above1 (get-location percepts 0 2))
	         (above2 (get-location percepts 0 3))
	         (above3 (get-location percepts 0 4))
	         (left1 (get-location percepts -1 1))
	         (right1 (get-location percepts 1 1)))
        (and    (or (not (is-agent? above1))(> my-energy (agent-energy above1)))
	            (or (not (is-agent? above2))(> my-energy (agent-energy above2)))
	            (or (not (is-agent? above3))(> my-energy (agent-energy above3)))
                (or (not (is-agent? left1))(> my-energy (agent-energy left1)))
	            (or (not (is-agent? right1))(> my-energy (agent-energy right1))))))

; has-vegetaion-this-line? -> whether the straight line has vegetation
(define (has-vegetation-percept?? percepts)
  (or (is-vegetation? (get-location percepts 0 1))
      (is-vegetation? (get-location percepts 0 2))
      (is-vegetation? (get-location percepts 0 3))
      (is-vegetation? (get-location percepts 0 4))
      (is-vegetation? (get-location percepts 0 5))))

; has-vegetaion-left-side? -> whether the left side has vegetation
(define (has-vegetation-left-side? percepts)
  (let iter-y ((y 1))
    (if (> y 5)
	#f
	(let iter-x ((x -1))
	  (if (> (abs x) y)
	      (iter-y (+ y 1))
	      (let ((curr-location (get-location percepts x y)))
		(if (is-vegetation? curr-location)
		    #t
		    (iter-x (- x 1)))))))))

; has-vegetaion-left-side? -> whether the right side has vegetation
(define (has-vegetation-right-side? percepts)
  (let iter-y ((y 1))
    (if (> y 5)
	#f
	(let iter-x ((x 1))
	  (if (> (abs x) y)
	      (iter-y (+ y 1))
	      (let ((curr-location (get-location percepts x y)))
		(if (is-vegetation? curr-location)
		    #t
		    (iter-x (+ x 1)))))))))




;; GAINS
; left-corner-gain -> caculates the left corner gain
(define (left-corner-gain left-corner)
    (cond   ((null? left-corner) 0)
	        ((is-vegetation? left-corner) (vegetation-energy left-corner))
	        (#t 0)))

; right-corner-gain -> right corner gain
(define (right-corner-gain right-corner)
  (cond ((null? right-corner) 0)
	((is-vegetation? right-corner) (vegetation-energy right-corner))
	(#t 0)))

; bottom-gain -> returns bottom gain
(define (bottom-gain bottom)
  (cond ((null? bottom) 0)
	((is-vegetation? bottom) (vegetation-energy bottom))
	(#t 0)))




;; VEGETATION LOCATION AND STATES
; is-left-hand-vege -> returns 1 if left hand has vegetation
(define (is-left-hand-vege left-corner)
  (if (> (left-corner-gain left-corner) 0)
      1
      0))

; is-right-hand-vege -> returns 1 if left hand has vegetation
(define (is-right-hand-vege right-corner)
  (if (> (right-corner-gain right-corner) 0)
      1
      0))

; is-bottom-vege -> returns 1 if bottom has vegetation
(define (is-bottom-vege bottom)
  (if (> (bottom-gain bottom) 0)
      1
      0))

; get-max-vegetation-energy-around-me -> calculate the maximum vegetation energy around the agent
(define (get-max-vegetation-energy-around-me percepts)
  (let* ((front-location (get-location percepts 0 1))
	 (front-energy (vegetation-energy front-location))
	 (left-energy (vegetation-energy left-corner))
	 (right-energy (vegetation-energy right-corner))
	 (bottom-energy (vegetation-energy bottom)))
    (max front-energy left-energy right-energy bottom-energy)))

; is-eating-front-vege-has-gain -> returns gain
(define (is-eating-front-vege-has-gain percepts previous-events)
  (let ((front-location (get-location percepts 0 1)))
    (if (and (is-vegetation? front-location)
	     (> (+ (vegetation-energy front-location)
		   (damage-level previous-events)) 0))
	1
	0)))

; is-nothing-to-eat -> whether there is anything to eat
(define (is-nothing-to-eat percepts)
  (let ((front-location (get-location percepts 0 1)))
    (if (and (is-vegetation? front-location)
	     (> (vegetation-energy front-location) 0))
	0
	1)))

; is-escaping-mode -> whether in escaping mode
(define (is-escaping-mode percepts previous-events steps)
  (if (and (is-attacked? previous-events)
	   (path-empty? percepts 1 steps))
      1
      0))

; continuous-turn-penalty -> continuous-turn-penalty, if it sums up to 4, return 1
(define (continuous-turn-penalty percepts)
  (let ((front-location (get-location percepts 0 1)))
    (if (and (is-empty? front-location)
	     (is-empty? left-corner)
	     (is-empty? right-corner)
	     (is-empty? bottom))
	1
	0)))

; is-moving-eat-more -> evaluate whether the further vege has more energy
(define (is-moving-eat-more percepts)
  (let ((front-left (get-location percepts -1 1))
	(front-right (get-location percepts 1 1))
	(max-around-me (get-max-vegetation-energy-around-me percepts)))
    (if (or (> (vegetation-energy front-left) max-around-me)
	    (> (vegetation-energy front-right) max-around-me))
	1
	0)))

; turn-to-predator-penalty ->  calculate penalty by predator
(define (turn-to-predator-penalty new-state)
  (if (is-predator? new-state)
      1
      0))




;; DIRECTIONAL SCORES
; score-stay -> calculate the score of stay
(define (score-stay previous-events percepts)
  (let ((origin-state (get-location percepts 0 1)))
    (+ (* (run-away-underattack? previous-events origin-state "STAY") -200)
       (* (current-accessible percepts 0 1) -1000)
       (* (stay-face-predator? percepts) -300)
       (if (surrounded-by-predator? 0 0 percepts) -600 0)
       -1
       -100)))

; turn-left-score -> calculate the score of turn left
(define (score-turn-left previous-events percepts)
  (let* ((origin-state (get-location percepts 0 1))
	 (origin-left (get-location percepts -1 1)))
    (+ (if (is-barrier? origin-state) 30 0)
       (if (is-barrier? origin-left) -1000 0)
       (if (surrounded-by-predator? 0 0 percepts) -600 0)
       (* (is-left-hand-vege left-corner) 200)
       (if (is-predator? origin-state) 40 0)
       (if (has-ate? previous-events) 30 0)
       (* (continuous-turn-penalty percepts) -500)
       (* (turn-to-predator-penalty left-corner) -200)
       (* (run-away-underattack? previous-events origin-state "TURN-LEFT") -200)
       50
       -2)))

; turn-right-score -> calculate the score of turn right
(define (score-turn-right previous-events percepts)
  (let* ((origin-state (get-location percepts 0 1))
	 (origin-right (get-location percepts 1 1)))
    (+ (if (is-barrier? origin-state) 30 0)
       (if (is-barrier? origin-right) -1000 0)
       (if (surrounded-by-predator? 0 0 percepts) -600 0)
       (* (is-right-hand-vege right-corner) 200)
       (if (is-predator? origin-state) 40 0)
       (if (has-ate? previous-events) 30 0)
       (* (continuous-turn-penalty percepts) -500)
       (* (turn-to-predator-penalty right-corner) -200)
       (* (run-away-underattack? previous-events origin-state "TURN-RIGHT") -200)
       50
       -2)))

; turn-right-score -> calculate the score of turn around
(define (score-turn-around previous-events percepts)
  (let* ((origin-state (get-location percepts 0 1)))
    (+ (if (is-barrier? origin-state) 30 0)
       (if (surrounded-by-predator? 0 0 percepts) -600 0)
       (* (is-bottom-vege bottom) 200)
       (if (is-predator? origin-state) 40 0)
       (if (has-ate? previous-events) 30 0)
       (* (continuous-turn-penalty percepts) -500)
       (* (run-away-underattack? previous-events origin-state "TURN-AROUND") -200)
       50
       -2)))




;; MOVEMENT SCORES
; move-passive-1-score -> calculate the score of move passive 1
(define (score-move-passive-1 previous-events percepts)
  (let* ((origin-state (get-location percepts 0 1)))
    (+ (* (current-accessible percepts 0 1) -1000)
       (if (surrounded-by-predator? 0 1 percepts) -600 0)
       (* (action-underattack? previous-events percepts 1) 100)
       (if (has-vegetation-percept?? percepts) 80 0)
       (* (is-moving-eat-more percepts) 50)
       -10)))

; move-passive-2-score -> calculate the score of move passive 2
(define (score-move-passive-2 previous-events percepts)
  (let* ((origin-state (get-location percepts 0 1))
	 (origin-above-state (get-location percepts 0 2)))
    (+ (* (current-accessible percepts 0 2) -1000)
       (if (surrounded-by-predator? 0 2 percepts) -600 0)
       (* (action-underattack? previous-events percepts 2) 100)
       (* (is-escaping-mode percepts previous-events 2) 100)
       (* (surrounded-by-veg-2? 0 2 percepts) 100)
       (if (has-vegetation-percept?? percepts) 80 0)
       -30)))

; score-move-passive-3 -> calculate the score of move passive 3
(define (score-move-passive-3 previous-events percepts)
  (let* ((origin-state (get-location percepts 0 1))
	 (origin-above-state (get-location percepts 0 2))
	 (origin-above2-state (get-location percepts 0 3)))
    (+ (* (current-accessible percepts 0 3) -1000)
       (if (surrounded-by-predator? 0 3 percepts) -600 0)
       (* (action-underattack? previous-events percepts 3) 100)
       (* (is-escaping-mode percepts previous-events 3) 100)
       (* (surrounded-by-veg-2? 0 3 percepts) 100)
       (if (has-vegetation-percept?? percepts) 80 0)
       -60)))

; score-move-aggressive-1 -> calculate the score of move aggressive 1
(define (score-move-aggressive-1 current-energy previous-events percepts)
  (let* ((origin-state (get-location percepts 0 1)))
    (+ (* (current-accessible percepts 0 1) -1000)
       (if (surrounded-by-predator? 0 1 percepts) -600 0)
       (* (action-underattack? previous-events percepts 1) 100)
       (if (has-vegetation-percept?? percepts) 80 0)
       (* (is-moving-eat-more percepts) 50)
       -10)))

; score-move-aggressive-2 -> calculate the score of move aggressive 2
(define (score-move-aggressive-2 current-energy previous-events percepts)
  (let* ((origin-state (get-location percepts 0 1))
	 (origin-above-state (get-location percepts 0 2)))
    (+ (* (current-accessible percepts 0 2) -1000)
       (if (surrounded-by-predator? 0 2 percepts) -600 0)
       (* (action-underattack? previous-events percepts 2) 100)
       (* (is-escaping-mode percepts previous-events 2) 100)
       (* (surrounded-by-veg-2? 0 2 percepts) 100)
       (if (has-vegetation-percept?? percepts) 80 0)
       -30)))

; score-move-aggressive-3 -> calculate the score of move aggressive 3
(define (score-move-aggressive-3 current-energy previous-events percepts)
  (let* ((origin-state (get-location percepts 0 1))
	 (origin-above-state (get-location percepts 0 2))
	 (origin-above2-state (get-location percepts 0 3)))
    (+ (* (current-accessible percepts 0 3) -1000)
       (if (surrounded-by-predator? 0 3 percepts) -600 0)
       (* (action-underattack? previous-events percepts 3) 100)
       (* (is-escaping-mode percepts previous-events 3) 100)
       (* (surrounded-by-veg-3? 0 3 percepts) 100)
       (if (has-vegetation-percept?? percepts) 80 0)
       -60)))




;; EATING SCORES
; score-eat-passive-score -> calculate the score after passive eating
(define (score-eat-passive previous-events percepts)
  (let ((origin-state (get-location percepts 0 1)))
    (+ (* (is-eating-front-vege-has-gain percepts previous-events) 500)
       (* (is-nothing-to-eat percepts) -1000)
       (if (surrounded-by-predator? 0 0 percepts) -600 0)
       -5)))

; eat-aggressive-score ->  calculate the score after aggressive eating
(define (score-eat-aggressive current-energy previous-events percepts)
  (let ((origin-state (get-location percepts 0 1)))
    (+ (* (is-eating-front-vege-has-gain percepts previous-events) 500)
       (* (is-nothing-to-eat percepts) -1000)
       (if (not (stronger? 0 1 percepts current-energy)) 50 0)
       (if (surrounded-by-predator? 0 0 percepts) -600 0)
       -5)))





