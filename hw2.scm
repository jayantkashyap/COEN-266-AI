;; ----------------------------ASSIGNMENT 2-------------------------------
;  JAYANT KASHYAP
;  SCU ID: 1433401
;; -----------------------------------------------------------------------



;; ++++++++++++++++++++++++++++++BASIC++++++++++++++++++++++++++++++++++++


;; ------------------------------SWAP-------------------------------------

; helper-length
(define (helper-length lst)
	    (cond	((null? lst)	0)
		(#t		                (+ 1 (helper-length (cdr lst))))))

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

; swap-elements
(define (swap-elements loc1 loc2 lst)
        (cond   ((null? lst)    '())
                (#t             (let ((x(nth-item loc1 lst)) (y (nth-item loc2 lst)))
                                        (replace-nth-item loc2 (replace-nth-item loc1 lst y) x)))))

; push-swap-pair
(define (push-swap-pair current swap-lst) (append (cadr current) swap-lst))

; pop-swap-pair
(define (pop-swap-pair swap-lst) (cdr swap-lst))



;; -----------------------------ADJACENCY--------------------------------

; get-state-adjacency-list
(define (get-state-adjacency-list adjacency-map state)
        (cond   ((null? adjacency-map)                 '())
                ((equal? state (caar adjacency-map))    (car adjacency-map))
                (#t                                     (get-state-adjacency-list (cdr adjacency-map) state))))

; is-adjacent?-helper
(define (is-adjacent?-helper lst state)
        (cond   ((null? lst) #f)
                ((equal? (car lst) state)   #t)
                (#t                         (is-adjacent?-helper (cdr lst) state))))

; is-adjacent?
(define (is-adjacent? state1 state2)
        (let    ((lst   (get-state-adjacency-list adjacency-map state1)))
                        (if (is-adjacent?-helper lst state2) #t #f)))



;; -----------------------------GOAL STATE--------------------------------

; is-goal-state? (APPROACH_1)
;(define (is-goal-state? searchstate)
;        (cond    ((null? searchstate)                   #f)
;                 ((= (length-helper searchstate) 2)     (if  (is-adjacent? (car searchstate) (cdr searchstate))
;                                                             #t
;                                                             #f))
;                 (#t                                    (and (is-adjacent?  (car searchstate) (cadr searchstate))
;                                                             (is-goal-state? (cdr searchstate))))))

; is-goal-state? (APPROACH_2)
(define (is-goal-state? searchstate) (is-goal-state?-helper (car searchstate) 1 (helper-length (car searchstate))))

; is-goal-state?-helper
(define (is-goal-state?-helper searchstate s e)
        (cond   ((= s e)        #t)
                ((= (- e s) 1)  (if (is-adjacent? (nth-item s searchstate) (nth-item e searchstate))
                                    #t
                                    #f))
                (#t             (and (is-adjacent? (nth-item s searchstate) (nth-item (+ s 1) searchstate)) 
                                     (is-goal-state?-helper searchstate (+ s 1) e)))))



;; -----------------------------CHILDREN----------------------------------

; get-children
(define (get-children lst) 
        (cond   ((null? (car lst))   '())
                ((null? (cdar lst))  lst)
                (#t                  (get-children-helper-permute (car lst) 1 (helper-length (car lst))))))

; get-children-permute-helper
(define (get-children-helper-permute lst s e)
        (cond   ((null? lst)    '())
                ((= s (- e 1))  (permute-helper lst s s e))
                (#t             (append   (permute-helper lst s s e)
                                          (get-children-helper-permute lst (+ s 1) e)))))

; permute-helper
(define (permute-helper lst c s e)
        (cond   ((null? lst)        '())
                ((= s (- e 1))      (cons
                                        (cons   (swap-elements c e lst) 
                                                (list (list (list c (+ s 1)))))
                                        '()))
                (#t                 (cons 
                                        (cons   (swap-elements c (+ s 1) lst) 
                                                (list (list (list c (+ s 1)))))
                                        (permute-helper lst c (+ s 1) e)))))



;; -------------------------------DFS-------------------------------------

(define (dfs frontier)
        (cond   ((null? frontier)                   #f)
                ((is-goal-state? (car frontier))    (car frontier))
                (#t                                 (dfs (append (get-children (car frontier)) (cdr frontier))))))



;; ------------------------------DLS--------------------------------------

;(define (dls frontier limit cut-off swap-lst)
;        (cond   ((null? frontier)                       #f)
;                ((is-goal-state? (car frontier))        (cons (caar frontier) (list (append (reverse swap-lst) (cadar frontier)))))
;                ((and (= limit 0) (not (= cut-off 0)))  (dls (cdr frontier) 0 (- cut-off 1) swap-lst))
;                ((and (= limit 0) (= cut-off 0))        (dls frontier (+ limit 1) 0 (pop-swap-pair swap-lst)))
;                (#t                                     (let ((lst (get-children (car frontier))))
;                                                            (dls (append lst (cdr frontier)) (- limit 1) (helper-length lst) (push-swap-pair (car frontier) swap-lst))))))

(define (dls frontier limit cut-off-lst swap-lst)
        (cond   ((null? frontier)                                   #f)
                ((is-goal-state? (car frontier))                    (cons (caar frontier) (list (append (reverse swap-lst) (cadar frontier)))))
                ((and (= limit 0) 
                      (not (= (car cut-off-lst) 0)))                (dls (cdr frontier) 0 (cons (- (car cut-off-lst) 1) (cdr cut-off-lst)) swap-lst))
                ((= (car cut-off-lst) 0)                            (dls frontier (+ limit 1) (cons (- (cadr cut-off-lst) 1) (cddr cut-off-lst)) (pop-swap-pair swap-lst)))
                (#t                                                 (let    ((lst (get-children (car frontier))))
                                                                            (dls (append lst (cdr frontier)) (- limit 1) (cons (length lst) cut-off-lst) (push-swap-pair (car frontier) swap-lst))))))


;; ----------------------------ID-DFS--------------------------------------

(define (id-dfs start-state)
        (cond   ((null? start-state)        #f)
                ((null? (cdr start-state))  (cons start-state '(())))
                (#t                         (let ((len (length start-state)))
                                                 (id-dfs-helper (list (cons start-state '(()))) 0 (- len 1) len)))))

(define (id-dfs-helper frontier s e len)
        (cond   ((null? frontier)   #f)
                ((= e s)            #f)
                (#t                 (or (dls frontier s '(1) '()) (id-dfs-helper frontier (+ s 1) e len)))))





;; -----------------------------MAP------------------------------------------

(define adjacency-map '(
  (Alabama Mississippi Tennessee Georgia Florida)
  (Alaska)
  (Arkansas Texas Oklahoma Missouri Tennessee Mississippi Louisiana)
  (Arizona California Nevada Utah New-Mexico)
  (California Arizona Nevada Oregon)
  (Colorado New-Mexico Utah Wyoming Nebraska Kansas Oklahoma)
  (Connecticut New-York Massachusetts Rhode-Island)
  (Delaware Maryland Pennsylvania New-Jersey)
  (Florida Alabama Georgia)
  (Georgia Florida Alabama Tennessee North-Carolina South-Carolina)
  (Hawaii)
  (Idaho Oregon Washington Montana Wyoming Utah Nevada)
  (Indiana Illinois Michigan Ohio Kentucky)
  (Illinois Missouri Iowa Wisconsin Indiana Kentucky)
  (Iowa Missouri Illinois Wisconsin Minnesota South-Dakota Nebraska)
  (Kansas Colorado Nebraska Missouri Oklahoma)
  (Kentucky Missouri Illinois Indiana Ohio West-Virginia Virginia Tennessee)
  (Louisiana Texas Arkansas Mississippi)
  (Maine New-Hampshire)
  (Maryland Virginia West-Virginia Pennsylvania Delaware)
  (Massachusetts Rhode-Island Connecticut New-York Vermont New-Hampshire)
  (Michigan Wisconsin Indiana Ohio)
  (Minnesota North-Dakota South-Dakota Iowa Wisconsin)
  (Mississippi Louisiana Arkansas Tennessee Alabama)
  (Missouri Oklahoma Kansas Nebraska Iowa Illinois Kentucky Tennessee Arkansas)
  (Montana Idaho Wyoming South-Dakota North-Dakota)
  (Nebraska Colorado Kansas Missouri Iowa South-Dakota Wyoming)
  (Nevada California Arizona Utah Idaho Oregon)
  (New-Hampshire Maine Vermont Massachusetts)
  (New-Jersey Delaware Pennsylvania New-York)
  (New-Mexico Texas Oklahoma Colorado Arizona)
  (New-York Pennsylvania New-Jersey Connecticut Massachusetts Vermont)
  (North-Carolina South-Carolina Georgia Tennessee Virginia)
  (North-Dakota Montana South-Dakota Minnesota)
  (Ohio Michigan Indiana Kentucky West-Virginia Pennsylvania)
  (Oklahoma Texas New-Mexico Colorado Kansas Missouri Arkansas)
  (Oregon Washington Idaho Nevada California)
  (Pennsylvania Ohio West-Virginia Maryland Delaware New-Jersey New-York)
  (Rhode-Island Connecticut Massachusetts)
  (South-Carolina Georgia North-Carolina)
  (South-Dakota Nebraska Iowa Minnesota North-Dakota Montana Wyoming)
  (Tennessee Arkansas Missouri Kentucky Virginia North-Carolina Georgia Alabama Mississippi)
  (Texas New-Mexico Oklahoma Arkansas Louisiana)
  (Utah Nevada Idaho Wyoming Colorado Arizona)
  (Vermont New-York Massachusetts New-Hampshire)
  (Virginia North-Carolina Tennessee Kentucky West-Virginia Maryland)
  (Washington Oregon Idaho)
  (West-Virginia Virginia Kentucky Ohio Pennsylvania Maryland)
  (Wisconsin Minnesota Iowa Illinois Michigan)
  (Wyoming Idaho Montana South-Dakota Nebraska Colorado Utah)
))
