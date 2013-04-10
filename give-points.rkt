(define (update-points)
  
  (define-struct posn (x y))
  
  
  (define (get-left pos dir)
    (define (helper l p dir)
      (if(equal? 'left dir)(if (equal? 0 ((get-pixel viewport ) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (+ (posn-y p) 5)) dir)
                               l)
         (if(equal? 'down dir)(if(equal? 0 ((get-pixel viewport) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (+ (posn-x p) 5) (posn-y p)) dir)
                                 l)
            (if(equal? 'right dir)(if(equal? 0 ((get-pixel viewport) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (- (posn-y p) 5)) dir)
                                     l)
               (if(equal? 'up dir)(if(equal? 0 ((get-pixel viewport) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (- (posn-x p) 5) (posn-y p)) dir)
                                     l)
                  (error "undefined direction"))))))
    (helper '() (make-posn (posn-x p) (posn-y p)) dir))
  (define (get-down pos dir)
    (define (helper l p dir)
      (if(equal? 'left dir)(if (equal? 0 ((get-pixel viewport ) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (+ 5 (posn-x p)) (posn-y p)) dir)
                               l)
         (if(equal? 'down dir)(if(equal? 0 ((get-pixel viewport) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (- (posn-y p) 5)) dir)
                                 l)
            (if(equal? 'right dir)(if(equal? 0 ((get-pixel viewport) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (- (posn-x p) 5) (posn-y p)) dir)
                                     l)
               (if(equal? 'up dir)(if(equal? 0 ((get-pixel viewport) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (+ (posn-y p) 5)) dir)
                                     l)
                  (error "undefined direction"))))))
    (helper '() (make-posn (posn-x p) (posn-y p)) dir))
  
  (define (get-right pos dir)
    (define (helper l p dir)
      (if(equal? 'left dir)(if (equal? 0 ((get-pixel viewport ) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (- (posn-y p) 5)) dir)
                               l)
         (if(equal? 'down dir)(if(equal? 0 ((get-pixel viewport) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (- (posn-x p) 5) (posn-y p)) dir)
                                 l)
            (if(equal? 'right dir)(if(equal? 0 ((get-pixel viewport) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (+ (posn-y p) 5)) dir)
                                     l)
               (if(equal? 'up dir)(if(equal? 0 ((get-pixel viewport) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (+ (posn-x p) 5) (posn-y p)) dir)
                                     l)
                  (error "undefined direction"))))))
    (helper '() (make-posn (posn-x p) (posn-y p)) dir))
  
  (define (get-front pos dir)
    (define (helper l p dir)
      (if(equal? 'left dir)(if (equal? 0 ((get-pixel viewport ) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (- (posn-x p) 5) (posn-y p)) dir)
                               l)
         (if(equal? 'down dir)(if(equal? 0 ((get-pixel viewport) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (+ (posn-y p) 5)) dir)
                                 l)
            (if(equal? 'right dir)(if(equal? 0 ((get-pixel viewport) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (+ (posn-x p) 5) (posn-y p)) dir)
                                     l)
               (if(equal? 'up dir)(if(equal? 0 ((get-pixel viewport) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (- (posn-y p) 5)) dir)
                                     l)
                  (error "undefined direction"))))))
    (helper '() (make-posn (posn-x p) (posn-y p)) dir))
 
  (define (helper l)
    (if(null? l)(set! l '())
       (let([posn (get-field pos (car l))]
            [object (car l)]
            [dir (get-field dir (car l))])
         (begin
           (set-field! points object (list (get-left posn dir) (get-bottom posn dir) (get-right posn dir) (get-front posn dir)))
           (helper (cdr l))))))
  (helper (get-field list-of-enemies current-stats)))






