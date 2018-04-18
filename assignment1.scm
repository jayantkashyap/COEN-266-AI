;; ------------ASSIGNMENT 1-------------
;  JAYANT KASHYAP
;  SCU ID: 1433401
;; -------------------------------------


;; BASIC

; find-biggest
(define (find-biggest a)
	(cond 	((null? a)  0)
		    (#t         (let 	((x (find-biggest (cdr a))))
					            (if (> (car a) x) 
						            (car a) 
						            x)))))

; count-from
(define (count-from a b)
	    (cond 	((= a b) 	(begin
		    		        (display b)
			    	        (newline)))
	          	((> a b) 	(display 0))
	      	    (#t      	(begin
	 			            (display a)
				            (newline)
				            (count-from (+ 1 a) b)))))

; nth-item
(define (nth-item a lst)
	    (cond 	((null? lst)	0)
	      	    ((= a 1)		(car lst))
	      	    (#t			    (nth-item (- a 1) (cdr lst)))))


; replace-nth-item
(define (replace-nth-item i lst r)
	    (cond 	((null? lst)	(cons r lst))
	      	    ((= i 1)		(cons r (cdr lst)))
	      	    (#t			    (cons (car lst) (replace-nth-item (- i 1) (cdr lst) r)))))

; sorted?
(define (sorted-descending? lst)
	    (cond 	((null? lst) 					#t)
	      	    ((and (not (null? (cdr lst)))   
                      (< (car lst) (cadr lst))) #f)
	      	    (#t								(and #t (sorted-descending? (cdr lst))))))

(define (sorted-ascending? lst)
	    (cond 	((null? lst) 					#t)
	      	    ((and (not (null? (cdr lst))) 
                      (> (car lst) (cadr lst))) #f)
	      	    (#t								(and #t (sorted-ascending? (cdr lst))))))

(define (sorted? lst)
	(cond 	((null? lst) 					#t)
	      	((or (sorted-descending? lst) 
                 (sorted-ascending? lst))	#t)
	      	(#t							    #f)))


; apply-action
(define (apply-action state action)
	    (cond	((equal? action "STAY")		    state)
    	    	((equal? action "MOVE-1")	    (apply-action-helper-move state 1))
	    	    ((equal? action "MOVE-2")	    (apply-action-helper-move state 2))
		        ((equal? action "MOVE-3")	    (apply-action-helper-move state 3)) 
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




;; ADVANCED

; get-location

(define (get-location percept posH posV) (nth-item (+ (+ posH posV) 1) (nth-item posV percept))) 

(define percept	'((empty empty empty) 
		         (empty (vegetation 2 45) empty empty empty)
 		         ((vegetation 3 150) empty empty empty empty empty barrier)
		         (barrier empty empty empty empty empty empty barrier barrier)
		         (barrier barrier empty (vegetation 4 200) empty empty empty (vegetation 1 125) barrier barrier barrier)))



;; EXTRA-CREDIT

; merge-ordered-lists
(define (merge-ordered-lists lst1 lst2) 
	    (cond 	((and (null? lst1) 
                      (null? lst2))	        '())
		        ((and (null? lst1) 
                      (not (null? lst2)))	lst2)
		        ((and (not (null? lst1)) 
                      (null? lst2))	        lst1)
		        (#t					        (if (< (car lst1) (car lst2))
							                    (append (list (car lst1)) (merge-ordered-lists (cdr lst1) lst2))
							                    (append (list (car lst2)) (merge-ordered-lists lst1 (cdr lst2)))))))


; merge-sort (APPROACH_1)

;(define (merge-sort lst)
;	(cond	((null? lst)		'())
;		((null? (cdr lst))	lst)
;		(#t			(merge-ordered-lists (list (car lst)) (merge-sort (cdr lst))))))

; merge-sort (APPROACH_2)

(define (merge-sort lst)
	    (cond	((null? lst)		'())
		        ((null? (cdr lst))	lst)
		(#t			                (let 	((x (split lst))) 
						                    (merge-ordered-lists (merge-sort (car x)) (merge-sort (cadr x)))))))

(define (helper-length lst)
	    (cond	((null? lst)	0)
		(#t		                (+ 1 (helper-length (cdr lst))))))

(define (split lst)
	(cond 	((null? lst)	'())
		((null? (cdr lst))	lst)
		(#t		            (let	((x (helper-length lst)))
					                    (if 	(even-helper? x)	
						                        (split-helper lst (/ x 2)) 
						                        (split-helper lst (/ (+ x 1) 2)))))))

(define (even-helper? num)
	    (cond 	((= 0 num)	#t)
		        ((= 1 num)	#f)
		        (#t		    (and (even-helper? (- num 2)) #t))))

(define (split-helper lst i)
	    (cond	((and (null? lst) (= i 0))	'())
		((= i 0)			                (cons '() (list lst)))
		(#t				                    (let 	((x (split-helper (cdr lst) (- i 1))))
							                        (cons (append (list(car lst)) (car x)) (cdr x))))))
