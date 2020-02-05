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

(define (min-simple L currentMin) ;currentMin default value is #f
  (cond 
   ((null? L) currentMin)
    ((number? (car L)) (cond
        ((not(number? currentMin)) (min-simple (cdr L) (car L)))
        ((< (car L) currentMin) (min-simple (cdr L) (car L)))
        (else (min-simple (cdr L) currentMin))
        ))
    (else (min-simple (cdr L) currentMin)))
)

(define (min-above-num L currentMin otherMin) ;currentMin default value is #f
    (cond
        ((null? L) currentMin) 
        ((not(number? (car L))) (min-above-num (cdr L) currentMin otherMin))
        (else
