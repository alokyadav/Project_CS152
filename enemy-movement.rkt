;correct-dir-k
(define (enemy-movement)
  (define enemies (get-field list-of-enemies current-stats))
  (define (move enemy dir x y)
    (if(equal? dir 'up)(set-field! pos enemy (cons x (- y 10)))
       (if(equal? dir 'down)(set-field! pos enemy (cons x (+ y 10)))
          (if(equal? dir 'right)(set-field! pos enemy (cons (+ x 10) y))
             (if(equal? dir 'left)(set-field! pos enemy (cons (- x 10) y))
                (error "undefined dir"))))))
  (define (can-move? dir x y)(if(equal? dir 'up)(if(= ((get-pixel window) (make-posn x (- y 10))) 0)#t #f)
                                (if(equal? dir 'down)(if(= ((get-pixel window) (make-posn x (+ y 10))) 0)#t #f)
                                   (if(equal? dir 'left)(if(= ((get-pixel window) (make-posn (- x 10) y)) 0)#t #f)
                                      (if(equal? dir 'right)(if(= ((get-pixel window) (make-posn (+ x 10) y)) 0)#t #f)
                                         (error "undefined dir"))))))
  (define (update-graphics-single enemy)
    (let* ([posn (get-field pos enemy)]
           [x (car posn)]
           [y (cdr posn)]
           [dir (get-field dir enemy)])
      (if(equal? dir 'up)((draw-pimap window) "C:/project/tanku.jpg" (make-posn x y))
         (if(equal? dir 'down) ((draw-pixmap window) "C:/project/tankd.jpg" (make-posn x y))
            (if(equal? dir 'left)((draw-pixmap window) "C:/project/tankl.jpg" (make-posn x y))
               (if(equal? dir 'right)((draw-pixmap window) "C:/project/tankr.jpg" (make-posn x y))
                  (error "undefined error")))))))
  (define (helper l)
    (if(null? l)(set! l '())
       (let* ([enemy (car l)]
              [dir (get-field dir enemy)]
              [x (car (get-field pos enemy))]
              [y (cdr (get-field pos enemy))]
              [waytoking (get-field way2king enemy)]
              [waytotank (get-field way2rtank enemy)])
         (if(equal? (get-field priority enemy) 'k)
            (if(equal? waytoking 'undefined)(begin
                                              ((clear-solid-rectangle window) (make-posn x y) 30 30)
                                              (if(can-move? dir x y)(move enemy dir x y)
                                                 (begin
                                                   (set-field! dir enemy (turn-arbit))
                                                   (shoot dir x y)))
                                              (update-graphics-single enemy))
               (let([waytoking (cdr waytoking)])
                 (if(null? waytoking)
                    (if (equal? (get-field dir enemy) (correct-dir-k))
                        (if (< (random) .8)
                            (if (equal? dir 'up)(send current-stats addenemybullets (list (cons (+ x 10) (- y 5)) dir))
                                (if(equal? dir 'down)(send current-stats addenemybullets (list (cons (+ x 10) (+ y 35)) dir))
                                   (if(equal? dir 'right)(send current-stats addenemybullets (list (cons (+ x 35) (+ y 10)) dir))
                                      (if(equal? dir 'left)(send current-stats addenemybullets (list (cons (- x 5) (+ y 10)) dir))
                                         (error "undefined dir")))))
                            (begin
                              (set-field! dir enemy (turn-arbit))
                              (shoot dir x y)
                              (set-field! dir enemy (turn-arbit))))
                        (set-field! dir enemy (correct-dir-k)))
                    (if(send our-tank scope-k (cons x y))(if(equal? dir (correct-dir-k enemy))
                                                            (if(< (random) 0.8)(shoot dir x y)
                                                               (set-field dir enemy (turn-arbit)))
                                                            (set-field! dir enemy (correct-dir-k)))
                       (if(equal? dir (face-the-path waytoking))
                          (if(< (random) .8)(set-field! pos enemy (car (waytoking)))
                             (begin
                               (shoot dir x y)
                               (set-field! dir enemy (turn-arbit))))
                          (set-field! dir enemy (face-the-path waytoking)))))))
            (if(equal? waytotank 'undefined)(begin
                                              ((clear-solid-rectangle window) (make-posn x y) 30 30)
                                              (if(can-move? dir x y)(move enemy dir x y)
                                                 (begin
                                                   (set-field! dir enemy (turn-arbit))
                                                   (shoot dir x y)))
                                              (update-graphics-single enemy))
               (let([waytotank (cdr waytotank)])
                 (if(null? waytotank)
                    (if (equal? (get-field dir enemy) (correct-dir-e))
                        (if (< (random) .8)
                            (if (equal? dir 'up)(send current-stats addenemybullets (list (cons (+ x 10) (- y 5)) dir))
                                (if(equal? dir 'down)(send current-stats addenemybullets (list (cons (+ x 10) (+ y 35)) dir))
                                   (if(equal? dir 'right)(send current-stats addenemybullets (list (cons (+ x 35) (+ y 10)) dir))
                                      (if(equal? dir 'left)(send current-stats addenemybullets (list (cons (- x 5) (+ y 10)) dir))
                                         (error "undefined dir")))))
                            (begin
                              (set-field! dir enemy (turn-arbit))
                              (shoot dir x y)
                              (set-field! dir enemy (turn-arbit))))
                        (set-field! dir enemy (correct-dir-e)))
                    (if(send our-tank scope-e (cons x y))(if(equal? dir (correct-dir-e enemy))
                                                            (if(< (random) 0.8)(shoot dir x y)
                                                               (set-field dir enemy (turn-arbit)))
                                                            (set-field! dir enemy (correct-dir-e)))
                       (if(equal? dir (face-the-path waytotank))
                          (if(< (random) .8)(set-field! pos enemy (car (waytotank)))
                             (begin
                               (shoot dir x y)
                               (set-field! dir enemy (turn-arbit))))
                          (set-field! dir enemy (face-the-path waytotank)))))))))))
  
  (helper enemies))

(define (correct-dir-k enemy)
  (let([x (car (get-field pos enemy))]
       [y (cdr (get-field pos enemy))])
       
    (if(< x 270)'right
       (if(> x 360)'left
          'down))))
(define (correct-dir-t enemy)
  (let([x-o (car (get-field pos our-tank))]
       [y-o (cdr (get-field pos our-tank))]
       [x-e (car (get-field pos enemy))]
       [y-e (cdr (get-field pos enemy))])
    (if(and (> (abs (- x-e x-o)) (abs (- y-e y-o))) (> x-e x-o))'left
       (if(and (> (abs (- x-e x-o)) (abs (- y-e y-o))) (< x-e x-o))'right
          (if(and (< (abs (- x-e x-o)) (abs (- y-e y-o))) (< y-e y-o))'down
             'up)))))
            
    

(define (face-the-path path)
  (let([x-of-e (car (get-field pos enemy))]
       [y-of-e (cdr (get-field pos enemy))]
       [x-of-first (car (car path))]
       [y-of-first (cadr (car path))])
    (if(and (= x-of-e x-of-first) (< y-of-e y-of-first))'down
       (if(and (= x-of-e x-of-first) (< y-of-first y-of-e))'up
          (if (and (= y-of-e y-of-first) (< x-of-first x-of-e))'left
              (if(and (= y-of-e y-of-first) (< x-of-e x-of-first))'right))))))

(define (shoot dir x y)
  (if (equal? dir 'up)(send currentstats addenemybullets (list (cons (+ x 10) (- y 5)) dir))
      (if(equal? dir 'down)(send currentstats addenemybullets (list (cons (+ x 10) (+ y 35)) dir))
         (if(equal? dir 'right)(send currentstats addenemybullets (list (cons (+ x 35) (+ y 10)) dir))
            (if(equal? dir 'left)(send currentstats addenemybullets (list (cons (- x 5) (+ y 10)) dir))
               (error "undefined dir"))))))

