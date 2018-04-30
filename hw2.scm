;; ----------------------------ASSIGNMENT 2-------------------------------
;  JAYANT KASHYAP
;  SCU ID: 1433401
;; -----------------------------------------------------------------------



;; ++++++++++++++++++++++++++++++BASIC++++++++++++++++++++++++++++++++++++


;; ------------------------------SWAP-------------------------------------

; nth-item -> returns nth item from the list
(define (nth-item a lst)
	    (cond 	((null? lst)	0)
	      	    ((= a 1)		(car lst))
	      	    (#t			    (nth-item (- a 1) (cdr lst)))))


; replace-nth-item  -> replaces nth item form the list with some other value
(define (replace-nth-item i lst r)
	    (cond 	((null? lst)	(cons r lst))
	      	    ((= i 1)		(cons r (cdr lst)))
	      	    (#t			    (cons (car lst) (replace-nth-item (- i 1) (cdr lst) r)))))


; swap-elements -> swap elements at loc1 and loc2 in a given list
(define (swap-elements loc1 loc2 lst)
        (cond   ((null? lst)    '())
                (#t             (let ((x(nth-item loc1 lst)) (y (nth-item loc2 lst)))
                                        (replace-nth-item loc2 (replace-nth-item loc1 lst y) x)))))


; push-swap-pair -> pushes a swap pair in the swap-lst. The swap list is appended at the end of the goal state to represent the path of the swaps
(define (push-swap-pair current swap-lst) (append (cadr current) swap-lst))


; pop-swap-pair -> pops the swap-lst, i.e., removes the head of the swap-lst as it is not the part of the path of the swaps
(define (pop-swap-pair swap-lst) (cdr swap-lst))



;; -----------------------------ADJACENCY--------------------------------

; get-state-adjacency-list -> returns adjacency list based on the map
(define (get-state-adjacency-list adjacency-map state)
        (cond   ((null? adjacency-map)                 '())
                ((equal? state (caar adjacency-map))    (car adjacency-map))
                (#t                                     (get-state-adjacency-list (cdr adjacency-map) state))))


; is-adjacent?-helper -> returns boolean #true if the state is adjacent to the adjacency list   
(define (is-adjacent?-helper lst state)
        (cond   ((null? lst)                #f)
                ((equal? (car lst) state)   #t)
                (#t                         (is-adjacent?-helper (cdr lst) state))))


; is-adjacent? -> returns boolean #true if the states are adjacent to each other
(define (is-adjacent? state1 state2)
        (let    ((lst   (get-state-adjacency-list adjacency-map state1)))
                        (if (is-adjacent?-helper lst state2) #t #f)))



;; -----------------------------GOAL STATE--------------------------------


; is-goal-state? -> returns boolean #true if the list of states is a goal state, i.e., each of the state pairs are adjacent to each other
(define (is-goal-state? searchstate) (is-goal-state?-helper (car searchstate) 1 (length (car searchstate))))


; is-goal-state?-helper -> helper function for the goal state finder
(define (is-goal-state?-helper searchstate s e)
        (cond   ((= s e)        #t)
                ((= (- e s) 1)  (if (is-adjacent? (nth-item s searchstate) (nth-item e searchstate))
                                    #t
                                    #f))
                (#t             (and (is-adjacent? (nth-item s searchstate) (nth-item (+ s 1) searchstate)) 
                                     (is-goal-state?-helper searchstate (+ s 1) e)))))



;; -----------------------------CHILDREN----------------------------------

; get-children -> returns all the possible permutations of the list as children alongwith their respective swaps
(define (get-children lst) 
        (cond   ((null? (car lst))   '())
                ((null? (cdar lst))  lst)
                (#t                  (get-children-helper-permute (car lst) 1 (length (car lst))))))


; get-children-permute-helper 
(define (get-children-helper-permute lst s e)
        (cond   ((null? lst)    '())
                ((= s (- e 1))  (permute-helper lst s s e))
                (#t             (append   (permute-helper lst s s e)
                                          (get-children-helper-permute lst (+ s 1) e)))))


; permute-helper -> simple permutation algorithm
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

; dfs -> depth first search
(define (dfs frontier)
        (cond   ((null? frontier)                   #f)
                ((is-goal-state? (car frontier))    (car frontier)) ; if goal state, return the front of the frontier
                (#t                                 (dfs (append (get-children (car frontier)) (cdr frontier)))))) ; get-children of the head of the frontier and append at the front of the frontier




;; ------------------------------DLS--------------------------------------

; dls -> depth limited search
(define (dls frontier limit cut-off-lst swap-lst) ; cut-off-lst -> stores the branching factor of the node || swap-lst -> stores the possible swaps in the list
        (cond   ((null? frontier)                                   #f)
                ((is-goal-state? (car frontier))                    (cons (caar frontier) (list (append (reverse swap-lst) (cadar frontier))))) ; if goal state, return the front of the frontier alongwith the respective swap list
                ((and (= limit 0) 
                      (not (= (car cut-off-lst) 0)))                (dls (cdr frontier) 0 (cons (- (car cut-off-lst) 1) (cdr cut-off-lst)) swap-lst)) ; if at the leaves of the tree check if the cut-off is zero or not
                ((= (car cut-off-lst) 0)                            (dls frontier (+ limit 1) (cons (- (cadr cut-off-lst) 1) (cddr cut-off-lst)) (pop-swap-pair swap-lst))) ; if cut-off is zero increase the limit as no child is the goal state
                (#t                                                 (let    ((lst (get-children (car frontier))))
                                                                            (dls (append lst (cdr frontier)) (- limit 1) (cons (length lst) cut-off-lst) (push-swap-pair (car frontier) swap-lst)))))) ; for each node generating child, push its swap to swap-lst to maintain the path of the traversal



;; ----------------------------ID-DFS--------------------------------------

; id-dfs -> iterative deepening -dfs
(define (id-dfs start-state)
        (cond   ((null? start-state)        #f)
                ((null? (cdr start-state))  (if (null? (get-state-adjacency-list adjacency-map (car start-state))) ; if the state is not present in the adjacency-map 
                                                #f
                                                (cons start-state '(()))))
                (#t                         (let ((len (length start-state)))
                                                 (id-dfs-helper (list (cons start-state '(()))) 0 (- len 1) len)))))

; id-dfs-helper
(define (id-dfs-helper frontier s e len)
        (cond   ((null? frontier)   #f)
                ((= e s)            #f)
                (#t                 (or (dls frontier s '(1) '()) (id-dfs-helper frontier (+ s 1) e len))))) ; increase the limit size (s) of each time till (n-1)





;; ++++++++++++++++++++++++++++++++ADVANCED++++++++++++++++++++++++++++++++++


;; --------------------------------------------------------------------------
;  
;  In A* search algorithm, the heuristic here is the count of non-adjacency
;  in the list of states. As we move closer to the goal state the count of
;  non-adjacency decreases. This will always result in solution thus it is 
;  complete. Also the heuristic is admissible as this heuristic is not over
;  -estimating the cost for reaching the goal. In reach the goal the number
;  of non-adjacency decreases.
;
;  Here I am taking the cost of the transition (g) as 1 for each parent to 
;  child. After each node the least cost (h-cost) child is used to generate 
;  further children and thus the path is generated until whole tree is pruned.
;  The sum of the transition cost (g) and the (h-cost) is used to decide 
;  which node is to be pruned.
;
;; --------------------------------------------------------------------------


; not-adjacent-heuristic-count -> counts the non-adjacency of the list of states
(define (not-adjacent-heuristic-count lst)
        (cond   ((null? lst)        0)
                ((null? (cdr lst))  0)
                (#t                 (if (not (is-adjacent? (car lst) (cadr lst)))
                                        (+ (not-adjacent-heuristic-count (cdr lst)) 1)
                                        (not-adjacent-heuristic-count (cdr lst))))))


; h-cost -> appends the h-cost to the end of the list
(define (h-cost lst)
        (cond   ((null? lst)    '())
                (#t             (cons (append (car lst) (list (not-adjacent-heuristic-count (caar lst)))) (h-cost (cdr lst))))))


; get-least-cost-child -> returns the least cost child
(define (get-least-cost-child lst child)
        (cond   ((null? lst)        (cons (car child) (list (cadr child))))
                ((null? child)      (get-least-cost-child (cdr lst) (append (car lst) child)))
                (#t                 (if (< (nth-item 3 (car lst)) (nth-item 3 child)) 
                                        (get-least-cost-child (cdr lst) (car lst))
                                        (get-least-cost-child (cdr lst) child)))))


; a-star -> A* Search
(define (a-star start-state)
        (cond   ((null? start-state)        #f)
                ((null? (cdr start-state))  (if (null? (get-state-adjacency-list adjacency-map (car start-state)))
                                                #f
                                                (cons start-state '(()))))
                (#t                         (let ((len (length start-state)))
                                                 (a-star-helper (list (cons start-state '(()))) 0 (- len 1) len)))))


; a-star-helper 
(define (a-star-helper open-lst s e len)
        (cond   ((null? open-lst)   #f)
                ((= e s)            #f)
                (#t                 (or (a-star-helperl open-lst open-lst s '(1) '()) (a-star-helper open-lst  (+ s 1) e len)))))



; contains-goal-state? -> returns boolean #true if the least contains the goal state
(define (contains-goal-state? lst)
        (cond   ((null? lst)                #f)
                ((is-goal-state? (car lst)) #t)
                (#t                         (or #f (contains-goal-state? (cdr lst))))))


; goal-state -> returns the goal-state from the list 
(define (goal-state lst)
        (cond   ((null? (cdr lst))           (car lst))
                ((is-goal-state? (car lst))  (car lst))
                (#t                          (goal-state (cdr lst)))))


; a-star-helper -> open-lst: that stores the nodes to visit || closed-lst: stores the visited nodes and maintains the same node is not added twice
(define (a-star-helperl open-lst closed-lst limit cut-off-lst swap-lst)
        (cond   ((null? open-lst)                                   #f)
                ((is-goal-state? (car open-lst))                    (if     (contains-wildcard? (caar open-lst))
                                                                            (wildcard-support (cons (caar open-lst) (list (append (reverse swap-lst) (cadar open-lst)))))
                                                                            (cons (caar open-lst) (list (append (reverse swap-lst) (cadar open-lst))))))
                ((and (= limit 0) 
                      (not (= (car cut-off-lst) 0)))                (a-star-helperl (cdr open-lst) closed-lst 0 (cons (- (car cut-off-lst) 1) (cdr cut-off-lst)) swap-lst)) 
                ((= (car cut-off-lst) 0)                            (a-star-helperl open-lst closed-lst (+ limit 1) (cons (- (cadr cut-off-lst) 1) (cddr cut-off-lst)) (pop-swap-pair swap-lst)))
                (#t                                                 (let    ((lst (get-children (car open-lst))))
                                                                            (a-star-helperl (append lst (cdr open-lst)) (cons (get-least-cost-child lst '()) closed-lst) (- limit 1) (cons (length lst) cut-off-lst) (push-swap-pair (car open-lst) swap-lst)))))) ; adds the visited node to the closed-lst, least-cost-child is used to preceed further





;; +++++++++++++++++++++++++++EXTRA CREDIT++++++++++++++++++++++++++++++++++

; wildcard-support -> provides wildcard support to a-star search
(define (wildcard-support lst)
        (cond   ((null? lst)                        '())
                ((and (null? (cdr lst))
                      (equal? (car lst) 'Wildcard)) (append (list (replace-nth-item 1 lst (cadr (get-state-adjacency-list adjacency-map 'Wildcard)))) (list '()))) ; if its just the Wildcard in the list
                ((null? (cdr lst))                  (append (list lst) (list '())))
                (#t                                 (wildcard-support-helper lst (get-state-adjacency-list adjacency-map 'Wildcard) (wildcard-location (car lst) 0))))) ; otherwise


; wildcard-support-helper
(define (wildcard-support-helper lst w-lst loc)
        (cond   ((null? lst)                                                '())
                ((and   (not (equal? (nth-item loc (car lst)) 'Wildcard)) 
                        (is-goal-state? lst))                               (cons (replace-nth-item loc (car lst) (list 'Wildcard (nth-item loc (car lst)))) (list (nth-item 2 lst)))) ; check for the goal state by replacing wildcard with state name 
                (#t                                                         (wildcard-support-helper (cons (replace-nth-item loc (car lst) (car w-lst)) (list (nth-item 2 lst))) (cdr w-lst) loc))))


; wildcard-location -> returns location of 'Wildcard
(define (wildcard-location lst count)
        (cond   ((null? lst)                    #f)
                ((equal? (car lst) 'Wildcard)   (+ count 1))
                (#t                             (wildcard-location (cdr lst) (+ count 1)))))


; contains-wildcard -> returns boolean #true if the Wildcard exixts
(define (contains-wildcard? lst) (if (equal? (member 'Wildcard lst) #f)
                                    #f
                                    #t))



;; -----------------------------MAP------------------------------------------

(define adjacency-map '(
  (Alabama Mississippi Tennessee Georgia Florida Wildcard)
  (Alaska Wildcard)
  (Arkansas Texas Oklahoma Missouri Tennessee Mississippi Louisiana Wildcard)
  (Arizona California Nevada Utah New-Mexico Wildcard)
  (California Arizona Nevada Oregon Wildcard)
  (Colorado New-Mexico Utah Wyoming Nebraska Kansas Oklahoma Wildcard)
  (Connecticut New-York Massachusetts Rhode-Island Wildcard)
  (Delaware Maryland Pennsylvania New-Jersey Wildcard)
  (Florida Alabama Georgia Wildcard)
  (Georgia Florida Alabama Tennessee North-Carolina South-Carolina Wildcard)
  (Hawaii Wildcard)
  (Idaho Oregon Washington Montana Wyoming Utah Nevada Wildcard)
  (Indiana Illinois Michigan Ohio Kentucky Wildcard)
  (Illinois Missouri Iowa Wisconsin Indiana Kentucky Wildcard)
  (Iowa Missouri Illinois Wisconsin Minnesota South-Dakota Nebraska Wildcard)
  (Kansas Colorado Nebraska Missouri Oklahoma Wildcard)
  (Kentucky Missouri Illinois Indiana Ohio West-Virginia Virginia Tennessee Wildcard)
  (Louisiana Texas Arkansas Mississippi Wildcard)
  (Maine New-Hampshire Wildcard)
  (Maryland Virginia West-Virginia Pennsylvania Delaware Wildcard)
  (Massachusetts Rhode-Island Connecticut New-York Vermont New-Hampshire Wildcard)
  (Michigan Wisconsin Indiana Ohio Wildcard)
  (Minnesota North-Dakota South-Dakota Iowa Wisconsin Wildcard)
  (Mississippi Louisiana Arkansas Tennessee Alabama Wildcard)
  (Missouri Oklahoma Kansas Nebraska Iowa Illinois Kentucky Tennessee Arkansas Wildcard)
  (Montana Idaho Wyoming South-Dakota North-Dakota Wildcard)
  (Nebraska Colorado Kansas Missouri Iowa South-Dakota Wyoming Wildcard)
  (Nevada California Arizona Utah Idaho Oregon Wildcard)
  (New-Hampshire Maine Vermont Massachusetts Wildcard)
  (New-Jersey Delaware Pennsylvania New-York Wildcard)
  (New-Mexico Texas Oklahoma Colorado Arizona Wildcard)
  (New-York Pennsylvania New-Jersey Connecticut Massachusetts Vermont Wildcard)
  (North-Carolina South-Carolina Georgia Tennessee Virginia Wildcard)
  (North-Dakota Montana South-Dakota Minnesota Wildcard)
  (Ohio Michigan Indiana Kentucky West-Virginia Pennsylvania Wildcard)
  (Oklahoma Texas New-Mexico Colorado Kansas Missouri Arkansas Wildcard)
  (Oregon Washington Idaho Nevada California Wildcard)
  (Pennsylvania Ohio West-Virginia Maryland Delaware New-Jersey New-York Wildcard)
  (Rhode-Island Connecticut Massachusetts Wildcard)
  (South-Carolina Georgia North-Carolina Wildcard)
  (South-Dakota Nebraska Iowa Minnesota North-Dakota Montana Wyoming Wildcard)
  (Tennessee Arkansas Missouri Kentucky Virginia North-Carolina Georgia Alabama Mississippi Wildcard)
  (Texas New-Mexico Oklahoma Arkansas Louisiana Wildcard)
  (Utah Nevada Idaho Wyoming Colorado Arizona Wildcard)
  (Vermont New-York Massachusetts New-Hampshire Wildcard)
  (Virginia North-Carolina Tennessee Kentucky West-Virginia Maryland Wildcard)
  (Washington Oregon Idaho Wildcard)
  (West-Virginia Virginia Kentucky Ohio Pennsylvania Maryland Wildcard)
  (Wisconsin Minnesota Iowa Illinois Michigan Wildcard)
  (Wyoming Idaho Montana South-Dakota Nebraska Colorado Utah Wildcard)
  
  (Wildcard Alabama Alaska Arkansas Arizona California Colorado Connecticut Delaware Florida Georgia Hawaii Idaho Indiana Illinois Iowa Kansas Kentucky Louisiana Maine Maryland Massachusetts Michigan Minnesota Mississippi Missouri Montana Nebraska Nevada New-Hampshire New-Jersey New-Mexico New-York North-Dakota North-Carolina Ohio Oklahoma Oregon Pennsylvania Rhode-Island South-Carolina South-Dakota Tennessee Texas Utah Vermont Virginia Washington West-Virginia Wisconsin Wyoming)
))
