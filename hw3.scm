;; ----------------------------ASSIGNMENT 2-------------------------------
;  JAYANT KASHYAP
;  SCU ID: 1433401
;; -----------------------------------------------------------------------



;; ++++++++++++++++++++++++++++++BASIC++++++++++++++++++++++++++++++++++++

; is-complement? -> returns boolean for if the literals are complement or not
(define (is-complement? l1 l2)
        (cond   ((list? l1) (if (equal? (cadr l1) l2)
                                #t
                                #f))
                ((list? l2) (if (equal? (cadr l2) l1)
                                #t
                                #f))
                (#t         #f)))


; contains-complement? -> returns boolean if complement exists
(define (contains-complement? lst l)
        (cond   ((null? lst)                    #f)
                ((is-complement? (car lst) l)   #t)
                (#t                             (contains-complement? (cdr lst) l))))


; get-complement -> returns complement of specified literal
(define (get-complement lst)
        (cond   ((list? lst)    (cadr lst))
                (#t             (list 'NOT lst))))


; eliminate -> eliminates complement of specified literal
(define (eliminate lst l)
        (cond   ((null? lst)            '())
                ((equal? (car lst) l)   (cdr lst))
                (#t                     (cons (car lst) (eliminate (cdr lst) l)))))


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



(define (factoring lst l) #t)


; resolve -> resolves the given propositions
(define (resolve lst1 lst2)
        (cond   ((null? lst1)                                   #f)
                ((null? lst2)                                   #f)
                ((and (= (length lst1) 1) (= (length lst2) 1))  (if (is-complement? (car lst1) (car lst2))
                                                                    'CONTRADICTION
                                                                    (if (equal? (car lst1) (car lst2))
                                                                        lst1
                                                                        #f)))
                (#t                                             (let ((lst (resolve-helper lst1 lst2)))
                                                                     (cond  ((equal? lst #f)                                    #f)
                                                                            ((= (+ (length lst1) (length lst2)) (length lst))   #f)
                                                                            (#t                                                 lst))))))


; resolve-helper -> helper to resolve function
(define (resolve-helper lst1 lst2)
        (cond   ((null? lst1)                               #f)
                ((null? (cdr lst2))                         (if (contains-complement? lst1 (car lst2))
                                                                (let  ((lst1 (eliminate lst1 (get-complement (car lst2)))))
                                                                      (cond  ((null? lst1) #f)
                                                                             (#t           lst1)))
                                                                (append lst1 (list (car lst2)))))
                ((contains-complement? lst1 (car lst2))     (resolve-helper (eliminate lst1 (get-complement (car lst2))) (cdr lst2)))
                (#t                                         (resolve-helper (append lst1 (list (car lst2))) (cdr lst2)))))





;; ++++++++++++++++++++++++++++++INTERMEDIATE+++++++++++++++++++++++++++++++


; KB -> Knowledge Base
(define KB '())


; contains? -> returns boolean #true if proposition exists in list of propositions
(define (contains? lst p)
        (cond   ((null? lst)            #f)
                ((equal? (car lst) p)   #t)
                (#t                     (contains? (cdr lst) p))))


; tell -> stores proposition to knowledge base
(define (tell proposition)
        (cond   ((null? proposition)          #f)
                ((contains? KB proposition)   'OK)
                (#t                           (begin    (set! KB (cons proposition KB))
                                                        'OK))))


; ask -> deduce proposition from knowledge base
(define (ask proposition)
        (cond   ((null? proposition)    #f)
                (#t                     (begin (resolver KB)
                                               (if  (contains? KB proposition)
                                                    (begin (resolver-prop KB proposition)
                                                           #t)
                                                    'UNKNOWN)))))


; resolves? -> returns boolean #false if two propositions resolves otherwise returns resolution
(define (resolves? lst1 lst2)
        (let    ((res (resolve lst1 lst2)))
                (cond   ((equal? res #f)                #f)
                        ((equal? res 'CONTRADICTION)    #f)
                        (#t                             res))))


; resolver-prop -> resolves proposition to knowledge base and stores in it
(define (resolver-prop kblst proposition)
        (cond   ((null? (cddr kblst))   (let   ((res   (resolves? (car kblst) proposition)))
                                                (cond  ((and (not (equal? res #f)) (not (contains? KB res)))   (set! KB (cons res KB))))))
                (#t                     (let   ((res   (resolves? (car kblst) proposition)))
                                                (begin (cond   ((and (not (equal? res #f)) (not (contains? KB res)))   (set! KB (cons res KB))))
                                                       (resolver-prop (cdr kblst) proposition))))))


; resolver -> resolves the current knowledge base
(define (resolver kblst)
        (cond   ((null? (cddr kblst))   (let   ((res   (resolves? (car kblst) (cadr kblst))))
                                                (cond  ((and (not (equal? res #f)) (not (contains? KB res)))   (set! KB (cons res KB))))))
                (#t                     (let   ((res   (resolves? (car kblst) (cadr kblst))))
                                                (begin (cond   ((and (not (equal? res #f)) (not (contains? KB res)))   (set! KB (cons res KB))))
                                                       (resolver (cdr kblst)))))))





;; +++++++++++++++++++++++++ADVANCED++++++++++++++++++++++++++++++

; conjunction -> defines conjunction 
(define (conjunction p q)
        (list (list p) (list q)))

; disjunction -> defines disjunction
(define (disjunction p q) (list p q))

; negation -> defines negation
(define (negation p)
        (cond ((not (list? p))        (list 'NOT p))
              ((equal? (car p) 'NOT)  (cdr p))
              (#t                     (list 'NOT p))))

; implication -> defines implication
(define (implication p q) (disjunction (negation p) q))

; biconditional -> defines biconditional
(define (biconditional p q) (conjunction (implication p q) (implication q p)))


; De Morgan's Law

; negation of conjunction
(define (noc lst nlst)
        (cond   ((not (equal? (car lst) 'NOT))  (begin (display lst) (newline) (cond ((= (length lst) 2)   (nod lst nlst))
                                                      ((= (length lst) 1)   (car lst)))))
                (#t                             (list (noc (nth-item 2 lst) nlst)
                                                      (noc (nth-item 3 lst) nlst)))))


; negation of disjunction
(define (nod lst nlst)
        (cond   ((not (equal? (car lst) 'NOT))  (cond ((= (length lst) 2)   (noc lst nlst))
                                                      ((= (length lst) 1)   (car lst))))
                (#t                             (list (noc (nth-item 2 lst) nlst)
                                                      (noc (nth-item 3 lst) nlst)))))
