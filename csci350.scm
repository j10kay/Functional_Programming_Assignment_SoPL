(define (reverse-general L) 
  (cond 
   ((not (list? L)) 0)
   ((null? L) L)
   (else L) 
  )
)
