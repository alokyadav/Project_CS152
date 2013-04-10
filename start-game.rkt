;check-timer update-graphics produce-enemy
;change-prior-for-each update-points-for-each
(include "mat-gen.rkt")
(define window1 (open-pixmap "tankwars" 950 690))
(copy-viewport window window1)
(define game-mat 1)
(define time 0)
(define main 1)
(define current-stats 1)
(define our-tank 1)
(define (startgame)
  (start-game 1))
(define (start-game lev)
  (define init
    (begin
      (set! game-mat (mat-gen lev))
      (set! our-tank (make-object our-tank% 1 4))
      (set! current-stats (make-object level-stats% lev (+ 12 (* 4 lev))))))
  (begin
    init
    (set! main (open-viewport "Tank Wars" 950 690))
    (copy-viewport window main)
    (begin
      (produce-enemy 1)
      (update-current-stats-enemy 1)
      (update-points))
    (begin
      (produce-ourtank)
      (set! time (current-seconds))
      (graphics-update-pixmap)
      (begin-gameplay))))

(define (begin-gameplay)
  (ourtank-movement)
  (enemytank-movement)
  (graphics-update-pixmap-tanks)
  (check-timer)
  (bullet-move)
  (graphics-update-pixmap-bullets)
  (bullet-check)
  (update-graphics)
  (check-enemy-killed)
  (begin-gameplay))

(define (update-graphics) (copy-viewport window main))

(define (graphics-update-pixmap)
  (begin
    (copy-viewport window1 window)
    (place-our-tank)
    (place-enemy-tanks)))

(define (place-our-tank)
  (let([x (car (get-field pos our-tank))]
       [y (cdr (get-field pos our-tank))]
       [dir (get-field dir our-tank)])
    (if(equal? dir 'up) ((draw-pixmap window) "tanku.jpg" (make-posn x y))
       (if(equal? dir 'down) ((draw-pixmap window) "tankd.jpg" (make-posn x y))
          (if(equal? dir 'right) ((draw-pixmap window ) "tankr.jpg" (make-posn x y))
             (if(equal? dir 'left) ((draw-pixmap window) "tankl.jpg" (make-posn x y))
                (error "undefined dir")))))))

(define (place-enemy-tanks)
  (define enemies (get-field list-of-enemies current-stats))
  (define (helper l)
    (map (λ(t)((let([x (car (get-field pos t))]
                    [y (cdr (get-field pos t))]
                    [dir (get-field dir t)])
                 (if(equal? dir 'up) ((draw-pixmap window) "enemyu.jpg" (make-posn x y))
                    (if(equal? dir 'down) ((draw-pixmap window) "enemyd.jpg" (make-posn x y))
                       (if(equal? dir 'right) ((draw-pixmap window ) "enemyr.jpg" (make-posn x y))
                          (if(equal? dir 'left) ((draw-pixmap window) "enemyl.jpg" (make-posn x y))
                             (error "undefined dir")))))))) l))
  (helper enemies))

(define (graphics-update-pixmap-bullets)
  (define bullets (append (get-field enemy-bullets current-stats) (get-field our-bullets currecnt-stats)))
  (define (helper l)
    (map (λ(t)(let([x (car (car t))]
                   [y (cdr (car t))]
                   [dir (cadr t)])
                ((draw-solid-ellipse window) (make-posn x y) 10 10 "black"))) l))
  (helper bullets))

(define (killed)
  (begin
    (if(equal? #f (check-ourtank))(endgame)
       (produce-enemy-graphics)
       (update-ourtank))))

(define (check-ourtank)
  (if(= (get-field lives our-tank) 0)#f #t))

(define (update-ourtank)
  (begin
    (send our-tank killed)
    (set-field! posn our-tank (cons 80 210))
    (set-field! dir our-tank 'up)))

(define (endgame)
  (begin
    (close-viewport main)
    (close-graphics)))

(define (check-enemy-killed)
  (if (= (get-field enemy-killed current-stats) (get-field total-enemies current-stats)) (next-stage)))

(define (next-stage) (start-game (+ (get-field stage current-stats) 1)))

(define (check-timer)
  (define (prior i)
    (let([tp (modulo (- (current-seconds) time) 10)])
      (if(= i 1)(set! i 1)
         (if(and (>= tp 0) (< tp 1))(begin
                                      (change-prior-for-each)
                                      (prior 1))
            (set! i 1)))))
  (define (produce i)
    (let([te (modulo (- (current-seconds) time) 5)])
      (if(= i 1)(set! i 1)
         (if(and (>= te 0) (< te 1))(begin
                                      (produce-enemy 1)
                                      (produce 1))
            (set! i 1)))))
  (define (change-points i)
    (let([tc (modulo (- (current-seconds) time) 2)])
      (if(= i 1)(set! i 1)
         (if(and (>= tc 0) (< tc 1))(begin
                                      (update-points-for-each)
                                      (change-points 1))
            (set! i 1)))))
  (define (change-path i)
    (let([tp (modulo (- (current-seconds) time0) 5)])
      (if(= i 1)(set! i 1)
         (if( and (>= tc 0) (< tc 1))(begin
                                       (update-path-for-each)
                                       (change-path 1))
            (set! i 1)))))
  
  (begin
    (prior 0)
    (produce 0)
    (change-points 0)
    (change-path 0)))

(define (update-path-for-each)
  (define enemies (get-field list-of-enemies current-stats))
  (define (helper l)
    (if(null? l)void
       (let([enemy (car l)])
         (begin
           (send enemy shortest-path)
           (helper (cdr l))))))
  (helper enemies))

(define (update-points-for-each)
  (define enemies (get-field list-of-enemies current-stats))
  (define (helper l)
    (if(null? l)void
       (let([enemy (car l)])
         (begin
           (send enemy update-points)
           (helper (cdr l))))))
  (helper enemies))

(define (change-prior-for-each)
  (define enemies (get-field list-of-enemies current-stats))
  (define (helper l)
    (if(null? l)void
       (let([enemy (car l)])
         (begin
           (send enemy change-priority)
           (helper (cdr l))))))
  (helper enemies))

(define (produce-enemy lev)
  (if(equal? check-stats #f)void
     (begin
       (graphics-produce-enemy lev)
       (update-enemy lev))))
(define (check-stats)
  (if(= (get-field enemies-yet-to-come current-stats) 0)#f
     #t))

(define (update-enemy lev)
  (begin
    (send current-stats add-enemy (make-object enemy-tank% (cons 30 30) 'down lev))
    (send current-stats add-enemy (make-object enemy-tank% (cons 630 30) 'down lev))
    (set-field! enemies-yet2come (- enemies-yet2come 2))))

