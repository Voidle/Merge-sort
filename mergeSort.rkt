(define (mergeFunc l1 l2)
   (if (null? l1) l2
    (if (null? l2) l1
     (cond ((< (car l1) (car l2))
      (cons (car l1) (mergeFunc (cdr l1) l2))
     )
     (else
      (cons (car l2) (mergeFunc l1 (cdr l2)))
     )
     )
    )
   )
  )

(define (leftSplit l)
   (cond ((null? l)
          '()
         )
         ((null? (cdr l))
          l
         )
         (else
          (cons (car l) (leftSplit (cdr (cdr l))))
         )
   )
 )
 (define (rightSplit l)
   (cond ((null? l)
          '()
         )
         ((null? (cdr l))
          l
         )
         (else
          (cons (car (cdr l)) (rightSplit (cdr (cdr l))))
         )
   )
 )
 (define (sortFunc l)
   (cond ((null? l)
          '()
         )
         ((null? (cdr l))
          l
         )
         
         (else
          (mergeFunc (sortFunc (leftSplit l)) (sortFunc (rightSplit l)))
         )
   )
  )
