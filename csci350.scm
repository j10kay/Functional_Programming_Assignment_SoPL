(define (reverse-general L) 
  (cond 
   ((null? L) L)
   ((list? L) (append (reverse-general (cdr L)) (list (reverse-general (car L)))))
   (else L) 
  )
)
