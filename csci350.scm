(define (reverse-general L) 
  (cond 
   ((null? L) L)
   ((list? L) (append (reverse-general (cdr L)) (list (reverse-general (car L)))))
   (else L) 
  )
)

(define (sum-up-numbers-simple L)
  (cond
   ((null? L) 0)
   ((list? L) (cond
               ((number? (car L)) (+ (sum-up-numbers-simple (cdr L)) (car L)))
               (else (+ (sum-up-numbers-simple (cdr L)) 0))
              )
   )
   (else 0)
   )
)

(define (sum-up-numbers-general L)
  (cond
   ((null? L) 0)
   ((list? L) (cond
               ((number? (car L)) (+ (sum-up-numbers-general (cdr L)) (car L)))
               ((list? (car L)) (+ (sum-up-numbers-general (cdr L)) (sum-up-numbers-general (car L))))
               (else (+ (sum-up-numbers-general (cdr L)) 0))
              )
   )
   (else 0)
   )
)

(define (min-simple L)
  (cond 
    ((null? (cdr L)) (cond
        ((number? (car L)) (car L))
        (else 10000000000000000000)
        ))
       ((number? (car L)) (cond
        ((< (car L) (min-simple (cdr L))) (car L))
        (else (min-simple (cdr L)))
        ))
    (else (min-simple (cdr L))))
)
