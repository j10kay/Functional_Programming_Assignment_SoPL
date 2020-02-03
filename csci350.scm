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
