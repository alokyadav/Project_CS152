;check-timer update-graphics produce-enemy
;change-prior-for-each update-points-for-each
;priority to be redefined
(define our-tank-timer 0)

(include "mat-gen.rkt")

(define our-tank%
  (class object%
    (init pow)
    (init l)
    (init-field [power pow])
    (init-field [lives l])
    (init-field [points (list '() '() '() '())])
    
    (init-field [pos (cons 240 630)])
    (init-field [dir 'up])
    (init-field [stone-breaking-power 'no])
    (super-new)
    
    (define/public (update-points)
      
      (define p (make-posn (car pos) (cdr pos)))
      
      (define (get-left dir)
        (define (helper l p dir)
          (if(equal? 'left dir)(if (equal? 0 ((get-pixel window ) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (+ (posn-y p) 5)) dir)
                                   l)
             (if(equal? 'down dir)(if(equal? 0 ((get-pixel window) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (+ (posn-x p) 5) (posn-y p)) dir)
                                     l)
                (if(equal? 'right dir)(if(equal? 0 ((get-pixel window) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (- (posn-y p) 5)) dir)
                                         l)
                   (if(equal? 'up dir)(if(equal? 0 ((get-pixel window) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (- (posn-x p) 5) (posn-y p)) dir)
                                         l)
                      (error "undefined direction"))))))
        (helper '() p dir))
      
      (define (get-down dir)
        (define (helper l p dir)
          (if(equal? 'left dir)(if (equal? 0 ((get-pixel window ) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (+ 5 (posn-x p)) (posn-y p)) dir)
                                   l)
             (if(equal? 'down dir)(if(equal? 0 ((get-pixel window) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (- (posn-y p) 5)) dir)
                                     l)
                (if(equal? 'right dir)(if(equal? 0 ((get-pixel window) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (- (posn-x p) 5) (posn-y p)) dir)
                                         l)
                   (if(equal? 'up dir)(if(equal? 0 ((get-pixel window) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (+ (posn-y p) 5)) dir)
                                         l)
                      (error "undefined direction"))))))
        (helper '() p dir))
      
      (define (get-right dir)
        (define (helper l p dir)
          (if(equal? 'left dir)(if (equal? 0 ((get-pixel window ) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (- (posn-y p) 5)) dir)
                                   l)
             (if(equal? 'down dir)(if(equal? 0 ((get-pixel window) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (- (posn-x p) 5) (posn-y p)) dir)
                                     l)
                (if(equal? 'right dir)(if(equal? 0 ((get-pixel window) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (+ (posn-y p) 5)) dir)
                                         l)
                   (if(equal? 'up dir)(if(equal? 0 ((get-pixel window) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (+ (posn-x p) 5) (posn-y p)) dir)
                                         l)
                      (error "undefined direction"))))))
        (helper '() p dir))
      
      (define (get-front dir)
        (define (helper l p dir)
          (if(equal? 'left dir)(if (equal? 0 ((get-pixel window ) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (- (posn-x p) 5) (posn-y p)) dir)
                                   l)
             (if(equal? 'down dir)(if(equal? 0 ((get-pixel window) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (+ (posn-y p) 5)) dir)
                                     l)
                (if(equal? 'right dir)(if(equal? 0 ((get-pixel window) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (+ (posn-x p) 5) (posn-y p)) dir)
                                         l)
                   (if(equal? 'up dir)(if(equal? 0 ((get-pixel window) p))(helper (cons (cons (posn-x p) (posn-y p)) l) (make-posn (posn-x p) (- (posn-y p) 5)) dir)
                                         l)
                      (error "undefined direction"))))))
        (helper '()  p dir))
      
      (set! points (list (get-left dir) (get-down dir) (get-right dir) (get-front dir))))
    
    (define/public (scope-k posn-e)
      (define x-e (car posn-e))
      (define y-e (cdr posn-e))
      (if(and (equal? (kill-x-k x-e) #f) (equal? (kill-y-k y-e) #f))#f
         (if(kill-x-k x-e)(if(all-white-x-k x-e y-e)#t #f)
            (if(kill-y-k y-e)(if(all-white-y-k x-e y-e)#t #f)))))
    (define (kill-x-k x-e)(if(and (>= x-e 289) (<= x-e 341))#t #f))
    (define (kill-y-k y-e)(if(>= y-e 619)#t #f))
    (define (all-white-x-k x-e y-e)
      
      (white-x? (+ x-e 15) (+ y-e 30) 629))
    (define (all-white-y-k x-e y-e)
      (if(> x-e 360)(white-y? (+ y-e 10) x-e 390)
         (if(< x-e 300)(white-y? (+ y-e 15) (+ x-e 30) 299))))
    (define/public (set-pos pair) (set! pos (cons (+ (car pair) (car pos)) (+ (cdr pair) (cdr pos)))))
    (define/public (set-dir dr) (set! dir dr))
    (define/public (scope-e posn-e)
      (define x-e (car posn-e))
      (define y-e (cdr posn-e))
      (if(and (equal? (kill-x-t x-e) #f) (equal?  (kill-y-t y-e) #f))#f
         (if(kill-x-t x-e)(if(all-white-x-o x-e y-e)#t #f)
            (if(kill-y-t y-e)(if(all-white-y-o x-e y-e)#t #f)))))
    (define (kill-x-t x-e)(if(<= (abs (- x-e (car pos))) 10)#t #f))
    (define (kill-y-t y-e)(if(<= (abs (- y-e (cdr pos))) 10)#t #f))
    (define (all-white-x-o x-e y-e)
      (if(> (cdr pos) y-e)(white-x? (+ x-e 15) (+ y-e 30) (- (cdr pos) 1))
         (if(> y-e (cdr pos))(white-x? (+ x-e 10)  (- y-e 1) (+ (cdr pos) 30)))))
    (define (all-white-y-o x-e y-e)
      (if(> (car pos) x-e)
         (white-y? (+ y-e 15) (+ x-e 30) (- (car pos) 1))
         (if(< (car pos) x-e)(white-y? (+ y-e 15) (- x-e 1) (+ 30 (car pos))))))
    (define (white-y? y-e x1 x2)
      
      (define xfinal (max x1 x2))
      (define (helper count x y)
        (if(> x xfinal)#t
           (if(> count 1)#f
              (if(or 
                  (equal? (rgb-green ((get-color-pixel window) (make-posn x y))) (/ 38 51))
                  (equal? (rgb-green ((get-color-pixel window) (make-posn x y))) (/ 32 255)) 
                  (equal? ((get-pixel window) (make-posn x y)) 0)
                  (equal? (rgb-green ((get-color-pixel window) (make-posn x y))) 1))(helper count (+ x 30) y)
                                                                                    (if(equal? (rgb-green ((get-color-pixel window) (make-posn x y))) (/ 4 17)) (helper (+ count 1) (+ x 30) y)
                                                                                       #f)))))
      (helper 0 (min x1 x2) y-e))
    
    (define (white-x? x-e y1 y2)
      (define yfinal (max y1 y2))
      (define (helper count x y)
        (if(> y yfinal)#t
           (if(>= count 2)#f
              (if(or (equal? 
                      (rgb-green ((get-color-pixel window) (make-posn x y))) 1) 
                     (equal? (rgb-green ((get-color-pixel window) (make-posn x y))) (/ 38 51)) 
                     (equal? (rgb-green ((get-color-pixel window) (make-posn x y))) (/ 32 255)) 
                     (equal? ((get-pixel window) (make-posn x y)) 0))
                 (helper count x (+ y 30))
                 (if(equal? ((get-color-pixel window) (make-posn x y)) (make-rgb (/ 44 85) (/ 4 17) (/ 12 85))) (helper (+ count 1) x (+ y 30))
                    #f)))))
      (helper 0 x-e (min y1 y2)))
    
    
    
    
    (define/public (killed) (if(< (- (current-seconds) our-tank-timer) 10)(void)
                               (if(= lives 0)(gameover)
                                  (begin
                                    (set! pos (cons 240 630))
                                    (set! lives (- lives 1))
                                    (set! our-tank-timer (current-seconds))))))))

;way2rtank and way2king will be a list of coordinates from enemy tank to our tank and the king
;priority will be a two valued variable with value either = 'k or 't representing the present aim of the tank (i.e. whether it wants to kill the king or our tank)
;the probability of the value of priority would be directly proportional to the length of the two paths
;The priority will be reset every few seconds and the new value would be an equally likely choice between king and our tank
;shortest will be a method to change the way2rtank after every move of our tank.

(define enemy-tank%
  (class object%
    
    (init posn)
    (init dirn)
    (init pow)
    (init-field [pos posn])
    (init-field [dir dirn])
    (init-field [power pow])
    (init-field [level 1])
    (init-field [way2rtank 'undefined])
    (init-field [way2king 'undefined])
    (init-field [priority 't])
    
    
    (super-new)
    
    (define/public (change-priority) (if(or (equal? 'undefined way2king) (equal? 'undefined way2rtank))
                                        (if(< (random) .5) 'k 't)
                                        (let ([c (/ (length way2rtank) (+ (length way2rtank) (length way2king)))]) (if (< (random) c)(set! priority 'k)
                                                                                                                       (set! priority 't)))))
    
    
    
   ; (define/public (shortest-path) 'undefined)))
    (define/public (shortest-path)
      (define (find-main fnl points)
        (if (null? fnl) #f
            (if (equal? (find-fnl (car fnl) points) #t) (cons #t (car fnl))
                (find-main (cdr fnl) points))))
      (define (find-fnl fnl points)
        (if (null? (filter (lambda (x) (and (= (car x) (car fnl)) (= (cadr x) (cdr fnl)))) points)) #f #t)) 
      (define (dedupe e)
        (if (null? e) '()
            (cons (car e) (dedupe (filter (lambda (x) (not (equal? x (car e)))) 
                                          (cdr e))))))
      (define (path ini fnl) 
        (define (main fnl points counter)
          (define (helper giv-point)
            (define (mylist) (filter (lambda (x) (not (equal? x #f))) 
                                     (list (if (equal? (check 'down 9) #t) (list (car giv-point) (+ (cdr giv-point) 10) (+ counter 1)) #f)
                                           (if (equal? (check 'up 9) #t) (list (car giv-point) (- (cdr giv-point) 10) (+ counter 1)) #f)
                                           (if (equal? (check 'right 9) #t) (list (+ (car giv-point) 10) (cdr giv-point) (+ counter 1)) #f)
                                           (if (equal? (check 'left 9) #t) (list (- (car giv-point) 10) (cdr giv-point) (+ counter 1)) #f))))
            
            (define (remove mylist)
              (if (null? mylist) '()
                  (if (null? (cdr mylist)) (if (null? (filter (lambda (x) (and (= (car x) (caar mylist)) 
                                                                               (= (cadr x) (cadar mylist))
                                                                               (>= (+ counter 1) (caddr x)))) points))
                                               (list (car mylist)) '())
                      (if (null? (filter (lambda (x) (and (= (car x) (caar mylist))
                                                          (= (cadr x) (cadar mylist))
                                                          (>= (+ counter 1) (caddr x)))) points))
                          (append (list (car mylist)) (remove (cdr mylist))) (remove (cdr mylist))))))
            
            (define (check dir i)
              ;(if ;(or (and (>= (car giv-point) 855) (equal? dir 'right))
              ;    (and (<= (car giv-point) 0) (equal? dir 'left))
              ;    (and (>= (cdr giv-point) 570) (equal? dir 'down))
              ;    (and (<= (cdr giv-point) 0) (equal? dir 'up))) 
              ;#f
              (cond [(equal? dir 'up) 
                     (if (= i 10) #t (if (and
                                          (or (= (rgb-green ((get-color-pixel window) (make-posn (+ (car giv-point) 1) (- (cdr giv-point) i)))) 1)
                                              (= (rgb-green ((get-color-pixel window) (make-posn (+ (car giv-point) 1) (- (cdr giv-point) i)))) (/ 38 51)))
                                          (or (= (rgb-green ((get-color-pixel window) (make-posn (+ (car giv-point) 15) (- (cdr giv-point) i)))) 1)
                                              (= (rgb-green ((get-color-pixel window) (make-posn (+ (car giv-point) 15) (- (cdr giv-point) i)))) (/ 38 51)))
                                          (or (= (rgb-green ((get-color-pixel window) (make-posn (+ (car giv-point) 28) (- (cdr giv-point) i)))) 1)
                                              (= (rgb-green ((get-color-pixel window) (make-posn (+ (car giv-point) 28) (- (cdr giv-point) i)))) (/ 38 51))))
                                         (check dir (+ i 1)) #f))]
                    [(equal? dir 'down) 
                     (if (= i 10) #t (if (and
                                          (or (= (rgb-green ((get-color-pixel window) (make-posn (+ (car giv-point) 1) (+ i 30 (cdr giv-point))))) 1)
                                              (= (rgb-green ((get-color-pixel window) (make-posn (+ (car giv-point) 1) (+ i 30 (cdr giv-point))))) (/ 38 51)))
                                          (or (= (rgb-green ((get-color-pixel window) (make-posn (+ (car giv-point) 15) (+ i 30 (cdr giv-point))))) 1)
                                              (= (rgb-green ((get-color-pixel window) (make-posn (+ (car giv-point) 15) (+ i 30 (cdr giv-point))))) (/ 38 51)))
                                          (or (= (rgb-green ((get-color-pixel window) (make-posn (+ (car giv-point) 28) (+ i 30 (cdr giv-point))))) 1)
                                              (= (rgb-green ((get-color-pixel window) (make-posn (+ (car giv-point) 28) (+ i 30 (cdr giv-point))))) (/ 38 51))))
                                         (check dir (+ i 1)) #f))]
                    [(equal? dir 'left) 
                     (if (= i 10) #t (if (and
                                          (or (= (rgb-green ((get-color-pixel window) (make-posn (- (car giv-point) i) (+ (cdr giv-point) 1)))) 1)
                                              (= (rgb-green ((get-color-pixel window) (make-posn (- (car giv-point) i) (+ (cdr giv-point) 1)))) (/ 38 51)))
                                          (or (= (rgb-green ((get-color-pixel window) (make-posn (- (car giv-point) i) (+ (cdr giv-point) 15)))) 1)
                                              (= (rgb-green ((get-color-pixel window) (make-posn (- (car giv-point) i) (+ (cdr giv-point) 15)))) (/ 38 51)))
                                          (or (= (rgb-green ((get-color-pixel window) (make-posn (- (car giv-point) i) (+ (cdr giv-point) 28)))) 1)
                                              (= (rgb-green ((get-color-pixel window) (make-posn (- (car giv-point) i) (+ (cdr giv-point) 28)))) (/ 38 51))))
                                         (check dir (+ i 1)) #f))]
                    [(equal? dir 'right) 
                     (if (= i 10) #t (if (and 
                                          (or (= (rgb-green ((get-color-pixel window) (make-posn (+ i 30 (car giv-point)) (+ (cdr giv-point) 1)))) 1)
                                              (= (rgb-green ((get-color-pixel window) (make-posn (+ i 30 (car giv-point)) (+ (cdr giv-point) 1)))) (/ 38 51)))
                                          (or (= (rgb-green ((get-color-pixel window) (make-posn (+ i 30 (car giv-point)) (+ (cdr giv-point) 15)))) 1)
                                              (= (rgb-green ((get-color-pixel window) (make-posn (+ i 30 (car giv-point)) (+ (cdr giv-point) 15)))) (/ 38 51)))
                                          (or (= (rgb-green ((get-color-pixel window) (make-posn (+ i 30 (car giv-point)) (+ (cdr giv-point) 28)))) 1)
                                              (= (rgb-green ((get-color-pixel window) (make-posn (+ i 30 (car giv-point)) (+ (cdr giv-point) 28)))) (/ 38 51))))
                                         (check dir (+ i 1)) #f))]))
            
            
            (define (new-points) (append points (remove (mylist))))
            
            (filter (lambda (x) (not (equal? x #f))) (remove (mylist))))
          
          (define (find-in-list pred take-list)
            (filter (lambda (x) (pred (caddr x) counter)) take-list))
          
          
          (define (apply)
            
            (apply-h (find-in-list = points))) 
          
          (define (apply-h giv-list)
            (if (null? giv-list) #f
                (if (null? (cdr giv-list)) (helper (cons (caar giv-list) (cadar giv-list)))
                    (append (helper (cons (caar giv-list) (cadar giv-list))) (apply-h (cdr giv-list))))))
          
          (cond [(cons? (find-main fnl points)) (cons (cdr (find-main fnl points)) points)]
                [(equal? #f (apply)) 'undefined]
                [(equal? counter 25) 'undefined]
                [else (main fnl (dedupe (append points (apply))) (+ counter 1))]))
        (main fnl (list ini) 0))
      (define (find-in-list pred take-list counter)
        (filter (lambda (x) (pred (caddr x) counter)) take-list))
      
      (define (change dest)  
        
        (let*
            ((mypath (path (list (car pos) (cdr pos) 0) dest)))
          
          ;(revpath (reverse mypath))
          ;(maxcounter (caddar revpath)))   
          (define (helper i point) 
            (if (= i 0) (list (list (car pos) (cdr pos) 0))
                (append (list (car 
                               (filter (lambda (x) 
                                         (cond 
                                           [(and (= (abs (- (car x) (car point))) 10) (< (abs (- (cadr x) (cadr point))) 10)) #t]
                                           [(and (= (abs (- (cadr x) (cadr point))) 10) (< (abs (- (car x) (car point))) 10)) #t]
                                           [else #f])) 
                                       (find-in-list = (cdr mypath) i))))
                        (helper (- i 1) (car (filter (lambda (x) 
                                                       (cond
                                                         [(and (= (abs (- (car x) (car point))) 10) (< (abs (- (cadr x) (cadr point))) 10)) #t]
                                                         [(and (= (abs (- (cadr x) (cadr point))) 10) (< (abs (- (car x) (car point))) 10)) #t]
                                                         [else #f]))
                                                     (find-in-list = (cdr mypath) i)))))))
          (if (equal? mypath 'undefined) 'undefined 
              (if (null? (cdr mypath)) (list (caar mypath) (cdar mypath) 0)
                  (reverse (append
                            (list (list (caar mypath) (cdar mypath) 0))
                            (helper (- (caddar (cdr (reverse mypath))) 1) (list (caar mypath) (cdar mypath) 0))))))))
      (begin
        ;(display (change (list (get-field pos our-tank))))
        (set! way2rtank (let ((position (get-field pos our-tank)))
                          (if (and
                               (= (rgb-green ((get-color-pixel window1) (make-posn (car position) (cdr position)))) 1)
                               (= (rgb-red ((get-color-pixel window1) (make-posn (car position) (cdr position)))) 0))
                              'undefined (change (concat (get-field points our-tank))))))
        (set! way2king (change (list (cons 240 630) (cons 300 570) (cons 330 570) (cons 390 630))))))))
(define (concat l)
  (foldr append '() l))
(define (increase-shield object)
  (set-field! power object (+ (get-field power object) 1)))

(define power%
  (class object%
    (init n)
    (init s-time)
    (init e-time)
    (init x-cor)
    (init y-cor)
    (init-field [name n])
    (init-field [start-time s-time])
    (init-field [end-time e-time])
    (init-field [x x-cor])
    (init-field [y y-cor])
    (super-new)))

(define (give-valid-x)
  (let([ i (* 10 (+ 3 (random 22)))])
    (if(= (random 2) 0)i
       (- 630 i))))

(define (produce-powers)
  (define (helper n)
    (let([ x (give-valid-x)])
      (if(< n 0.2)(begin
                  ((draw-pixmap window3) "king-shield.jpg" (make-posn x 630))
                  (set! shield-timer (+ (current-seconds) 15))
                  (send current-stats add-power (make-object power% 'king-shield (current-seconds) (+ (current-seconds) 15) x 630)))
         (if(< n .4)(begin
                     ((draw-pixmap window3) "grenade.jpg" (make-posn x 630))
                     (set! gt (+ (current-seconds) 15))
                     (send current-stats add-power (make-object power% 'grenade (current-seconds) (+ (current-seconds) 15) x 630)))
            (if(< n .6)(begin
                        ((draw-pixmap window3) "break-stones.jpg" (make-posn x 630))
                        (set! bt (+ (current-seconds) 15))
                        (send current-stats add-power (make-object power% 'break-stones (current-seconds) (+ (current-seconds) 3) x 630)))
               (if(< n .8)(begin
                           ((draw-pixmap window3) "shield.jpg" (make-posn x 630))
                           (set! st (+ (current-seconds) 15))
                           (send current-stats add-power (make-object power% 'shield (current-seconds) (+ (current-seconds) 3) x 630)))
                  (begin
                              ((draw-pixmap window3) "increase-power.jpg" (make-posn x 630))
                              (set! it (+ (current-seconds) 15))
                              (send current-stats add-power (make-object power% 'increase-power (current-seconds) (+ (current-seconds) 3)  x 630)))))))))
  (helper (random)))

(define (detect-power)
  (define powers (get-field powers current-stats))
  (define (helper l)
    (if(null? l) (void)
       (let*([pos (get-field pos our-tank)]
             [x (car pos)]
             [y (cdr pos)]
             [pow (car l)]
             [x-p (get-field x pow)]
             [y-p (get-field y pow)])
            (if(equal? 'taken x-p)(helper (cdr l))
               (if(and (< (abs (- x-p x))  25) (< (abs (- y-p y)) 25))
                  (begin
                    (set-field! x pow 'taken)
                    (display (get-field name pow))
                    (enact-power pow)
                    ((draw-solid-rectangle window3) (make-posn x-p y-p) 30 30 "black"))
                  (helper (cdr l)))))))
  (helper powers))

(define shield-timer 0)
(define it 0)
(define bt 0)
(define gt 0)
(define st 0)
  
(define (enact-power power)
  
  (let([name (get-field name power)]
       [list-p (get-field powers current-stats)])
    (if(equal? 'increase-power name)(begin
                                      (set-field! power our-tank (+ 1 (get-field power our-tank)))
                                      (set! it (+ (current-seconds) 15)))
       (if(equal? 'grenade name)(begin
                                  (set-field! list-of-enemies current-stats '())
                                  (set! gt (+ (current-seconds) 15)))
          (if(equal? 'shield name)(begin
                                    (set! our-tank-timer (current-seconds))
                                    (set! st (+ (current-seconds) 15)))
                                    
             (if(equal? 'break-stones name)(begin
                                             (set-field! stone-breaking-power our-tank 'yes)
                                             (set! bt (+ (current-seconds) 15)))
                (if(equal? 'king-shield name)(begin
                                               ((draw-solid-rectangle window1) (make-posn 270 630) 30 30 "black")
                                               ((draw-pixmap window3) "stone.jpg" (make-posn 270 630))
                                               ((draw-pixmap window3) "stone.jpg" (make-posn 270 600))
                                               ((draw-pixmap window3) "stone.jpg" (make-posn 300 600))
                                               ((draw-pixmap window3) "stone.jpg" (make-posn 330 600))
                                               ((draw-pixmap window3) "stone.jpg" (make-posn 360 600))
                                               ((draw-pixmap window3) "stone.jpg" (make-posn 360 630))
                                               ((draw-solid-rectangle window1) (make-posn 270 600) 30 30 "black")
                                               ((draw-solid-rectangle window1) (make-posn 300 600) 30 30 "black")
                                               ((draw-solid-rectangle window1) (make-posn 330 600) 30 30 "black")
                                               ((draw-solid-rectangle window1) (make-posn 360 600) 30 30 "black")
                                               ((draw-solid-rectangle window1) (make-posn 360 630) 30 30 "black")
                                               
                                               (set! shield-timer (+ (current-seconds) 15)))
                   (void))))))))






(define level-stats%
  (class object%
    
    (init lev)
    (init enem)
    
    (init-field [stage lev])
    (init-field [total-enemies enem])
    (init-field [enemies-yet2come enem])
    (init-field [enemy-dead 0])
    (init-field [lives 4])
    (init-field [list-of-enemies '()])
    (init-field [powers '()])
    (init-field [our-bullets '()])
    (init-field [enemy-bullets '()])
    (super-new)
    
    (define/public (add-power pow)(set! powers (cons pow powers)))
    
    (define/public (addourbullets l)(if(<= (length our-bullets) 3)
                                       (begin
                                         (set! our-bullets (append (list l) our-bullets ))
                                         )
                                       (void)))
    
    (define/public (addenemybullets l) (begin
                                         (set! enemy-bullets (append (list l) enemy-bullets))
                                         ))
    
    (define/public (add-enemy object) (begin
                                        (set! list-of-enemies (append (list object) list-of-enemies))
                                        ))
    
    (define/public (onedead)
      (set! enemy-dead (+ enemy-dead 1)))
    
    (define/public (killed)
      (set! lives [- lives 1]))))




(define window1 (open-pixmap "tankwars" 950 690))
(define window2 (open-pixmap "tankwars" 950 690))
(copy-viewport window window1)
(copy-viewport window3 window2)
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
    (copy-viewport window2 main)
    (begin
      (produce-enemy 1)
      )
    (begin
      (produce-ourtank)
      (send our-tank update-points)
      (set! time (current-seconds))
      (graphics-update-pixmap-tanks)
      (update-graphics)
      (set! our-tank-timer (current-seconds))
      (begin-gameplay))))

(define list-posn-bitp '())
(define mynum 18)
(define lives 4)
(define (update-sidebar)  
  (define (draw-no-of-enemy number)
    (define (helper x y i)
      (if 
       (= i number) void
       (begin
         ((draw-pixmap window3) "enemyu.jpg" (make-posn x y))
         (set! list-posn-bitp (append (list (cons x y)) list-posn-bitp))
         (if (= x 910) (helper 670 (+ y 60) (+ i 1)) (helper (+ x 60) y (+ i 1))))))
    (helper 670 240 1))
  (if (= mynum (get-field enemies-yet2come current-stats)) (void) 
      (begin (set! mynum (get-field enemies-yet2come current-stats))
             (draw-no-of-enemy mynum)))
  ((draw-pixmap window3) "ALIVES.bmp"(make-posn 660 480))
  (if (= lives (get-field lives current-stats)) (void) 
      (begin (set! mynum (get-field lives current-stats))
             (cond [(= lives 1) ((draw-pixmap window3) "1.bmp"(make-posn 850 460))]
                   [(= lives 2) ((draw-pixmap window3) "2.bmp"(make-posn 850 460))]
                   [(= lives 3) ((draw-pixmap window3) "3.bmp"(make-posn 850 460))]
                   [(= lives 4) ((draw-pixmap window3) "4.bmp"(make-posn 850 460))]))))




(define (produce-ourtank) (killed))
(define z 0)
(define (begin-gameplay)
  (if (= z 0) (update-shortest-path-for-each) (void))
  (if (= (remainder z 2) 0) (ourtank-movement) (void))
  (if (= z 0) (enemytank-movement) (void))
  (detect-power)
  (check-power (get-field powers current-stats))
  (bullet-check)
  (viewport-flush-input main)
  (graphics-update-pixmap-tanks)
  (check-timer)
  (bullet-move)
  (graphics-update-pixmap-bullets)
  (bullet-check)
  (update-graphics)
  (check-enemy-killed)
  (update-sidebar)
  (if (= z 5) (set! z 0) (set! z (+ z 1)))
  (begin-gameplay))

(define (check-power l)
    (if(null? l)(void)

  (let* ([power (car l)]
        [name (get-field name power)]
        [powers (get-field powers current-stats)]
        [x (get-field x power)]
        [y (get-field y power)])
    
  (if(equal? name 'king-shield)   
  (if(= shield-timer 0)(void)
     (if(> (current-seconds) shield-timer)(begin
                                           (set-field! powers current-stats (remove power powers))
                                           ((draw-solid-rectangle window1) (make-posn 270 630) 30 30 "brown")
                                           ((draw-solid-rectangle window1) (make-posn 270 600) 30 30 "brown")
                                           ((draw-pixmap window3) "block.jpg" (make-posn 270 630))
                                           ((draw-pixmap window3) "block.jpg" (make-posn 270 600))
                                           ((draw-pixmap window3) "block.jpg" (make-posn 300 600))
                                           ((draw-pixmap window3) "block.jpg" (make-posn 330 600))
                                           ((draw-pixmap window3) "block.jpg" (make-posn 360 600))
                                           ((draw-pixmap window3) "block.jpg" (make-posn 360 630))
                                           ((draw-solid-rectangle window1) (make-posn 300 600) 30 30 "brown")
                                           ((draw-solid-rectangle window1) (make-posn 330 600) 30 30 "brown")
                                           ((draw-solid-rectangle window1) (make-posn 360 600) 30 30 "brown")
                                           ((draw-solid-rectangle window1) (make-posn 360 630) 30 30 "brown")
                                           (set! shield-timer 0)
                                           ((draw-solid-rectangle window3) (make-posn x y) 30 30 "black")
                                           (set-field! powers current-stats (remove power powers))
                                           (check-power (cdr l)))
        (check-power (cdr l))))
     (if(equal? name 'break-stones)
        (if(= bt 0)(void)
           (if(> (current-seconds) bt)(begin
                                        ((draw-solid-rectangle window3) (make-posn x y) 30 30 "black")
                                        (set-field! powers current-stats (remove power powers))
                                        (set! bt 0)
                                        (check-power (cdr l)))
              (check-power (cdr l))))
        (if(equal? name 'shield)
           (if(= st 0)(void)
              (if(> (current-seconds) st)(begin
                                           ((draw-solid-rectangle window3) (make-posn x y) 30 30 "black")
                                           (set-field! powers current-stats (remove power powers))
                                           (set! st 0)
                                           (check-power (cdr l)))
                 (check-power (cdr l))))
           (if(equal? name 'grenade)
              (if(= gt 0)(void)
                 (if(> (current-seconds) gt)(begin
                                              ((draw-solid-rectangle window3) (make-posn x y) 30 30 "black")
                                              (set-field! powers current-stats (remove power powers))
                                              (set! gt 0)
                                              (check-power (cdr l)))
                    (check-power (cdr l))))
              (if(equal? name 'increase-power)
                 (if(= it 0)(void)
                    (if(> (current-seconds) it)(begin
                                                 ((draw-solid-rectangle window3) (make-posn x y) 30 30 "black")
                                                 (set-field! powers current-stats (remove power powers))
                                                 (set! it 0)
                                                 (check-power (cdr l)))
                       (check-power (cdr l))))
                 (error "unidentified power")))))))))
                                            
(define (update-shortest-path-for-each)
  (define enemies (get-field list-of-enemies current-stats))
  (define (helper l)
    (map (λ(t)(send t shortest-path)) l))
  (helper enemies))

(define (update-graphics) (begin (draw-grass 0 0 mat)
                                 (copy-viewport window2 main)))

(define (graphics-update-pixmap-tanks)
  (begin
    (copy-viewport window1 window)
    (copy-viewport window3 window2)
    (place-our-tank)
    (place-enemy-tanks)))

(define (place-our-tank)
  (let([x (car (get-field pos our-tank))]
       [y (cdr (get-field pos our-tank))]
       [dir (get-field dir our-tank)])
    (if(equal? dir 'up) (begin ((draw-solid-rectangle window) (make-posn x y) 30 30 "gray")
                               ;((draw-pixmap window) "tanku.jpg" (make-posn x y))
                               ((draw-pixmap window2) "tanku.jpg" (make-posn x y)))
       (if(equal? dir 'down) (begin ((draw-solid-rectangle window) (make-posn x y) 30 30 "gray")
                                    ;((draw-pixmap window) "tankd.jpg" (make-posn x y))
                                    ((draw-pixmap window2) "tankd.jpg" (make-posn x y)))
          (if(equal? dir 'right) (begin ((draw-solid-rectangle window) (make-posn x y) 30 30 "gray")
                                        ;((draw-pixmap window) "tankr.jpg" (make-posn x y))
                                        ((draw-pixmap window2) "tankr.jpg" (make-posn x y)))
             (if(equal? dir 'left) (begin ((draw-solid-rectangle window) (make-posn x y) 30 30 "gray")
                                          ;((draw-pixmap window) "tankl.jpg" (make-posn x y))
                                          ((draw-pixmap window2) "tankl.jpg" (make-posn x y)))
                (error "undefined dir")))))))
(define (place-enemy-tanks)
  (define enemies (get-field list-of-enemies current-stats))
  (define (helper l)
    (map (λ(t)(let([x (car (get-field pos t))]
                   [y (cdr (get-field pos t))]
                   [dir (get-field dir t)])
                (if(equal? dir 'up) (begin ((draw-solid-rectangle window) (make-posn x y) 30 30 "purple")
                                           ;((draw-pixmap window) "enemyu.jpg" (make-posn x y))
                                           ((draw-pixmap window2) "enemyu.jpg" (make-posn x y)))
                   (if(equal? dir 'down) (begin ((draw-solid-rectangle window) (make-posn x y) 30 30 "purple")
                                                ;((draw-pixmap window) "enemyd.jpg" (make-posn x y))
                                                ((draw-pixmap window2) "enemyd.jpg" (make-posn x y)))
                      (if(equal? dir 'right) (begin ((draw-solid-rectangle window) (make-posn x y) 30 30 "purple")
                                                    ;((draw-pixmap window) "enemyr.jpg" (make-posn x y))
                                                    ((draw-pixmap window2) "enemyr.jpg" (make-posn x y)))
                         (if(equal? dir 'left) (begin ((draw-solid-rectangle window) (make-posn x y) 30 30 "purple")
                                                      ;((draw-pixmap window) "enemyl.jpg" (make-posn x y))
                                                      ((draw-pixmap window2) "enemyl.jpg" (make-posn x y)))
                            (begin
                              (error "undefined dir")))))))) l))
  (helper enemies))

(define (delete-enemy x y)
  (define enemies (get-field list-of-enemies current-stats))
  (define (helper x y l)
    (if(null? l)(error "enemy-not-detected")
       (let*([enemy (car l)]
             [pos (get-field pos enemy)]
             [x-e (car pos)]
             [y-e (cdr pos)])
         (if(and (and (< (- x x-e) 29) (> (- x x-e) -9)) (and (< (- y y-e) 29) (> (- y y-e) -9)))(if(= 1 (get-field power enemy))
                                                                                                    (set-field! list-of-enemies current-stats (remove enemy enemies))
                                                                                                    (set-field! power enemy (- (get-field power enemy) 1)))
            (helper x y (cdr l))))))
  (helper x y enemies))

(define h 0)
(define m 0)
(define (graphics-update-pixmap-bullets)
  (define bullets (append (get-field enemy-bullets current-stats) (get-field our-bullets current-stats)))
  (define (helper l)
    
    (map (λ(t)(begin
                (let([x (car (car t))]
                     [y (cdr (car t))]
                     [dir (cdr t)])
                  (if (= (remainder (/ (+ x y) 10) 2) 1) ((draw-solid-ellipse window2) (make-posn (- x h)  (- y h)) (+ 10 (* 2 h)) (+ 10 (* 2 h)) "yellow")
                      ((draw-solid-ellipse window2) (make-posn (- x h)  (- y h)) (+ 10 (* 2 h)) (+ 10 (* 2 h)) "red")))
                (cond [(= h 3) (begin (set! h (- h 1))
                                      (set! m 1))]
                      [(= h 0) (begin (set! h (+ h 1))
                                      (set! m 0))]
                      [(= m 0) (set! h (+ h 1))]
                      [else (set! h (- h 1))]))) l))
  
  (helper bullets))

(define (killed)
  
  (if(equal? #f (check-ourtank))(endgame)
     (begin
       (produce-ourtank-graphics)
       (update-ourtank))))

(define (produce-ourtank-graphics)(void))


(define (check-ourtank)
  (if(= (get-field lives our-tank) 0)#f #t))

(define (update-ourtank)
  (begin
    (send our-tank killed)
    (set-field! pos our-tank (cons 240 630))
    (set-field! dir our-tank 'up)))

(define (endgame)
  (begin
    (close-viewport main)
    (close-graphics)))

(define (check-enemy-killed)
  (if (= (get-field enemy-dead current-stats) (get-field total-enemies current-stats)) (next-stage)))

(define (next-stage) (start-game (+ (get-field stage current-stats) 1)))
(define prior-i 1)
(define produce-i 1)
(define change-points-i 1)
(define change-path-i 1)
(define powers-i 1)

(define (check-timer)
  
  (define (prior)
    (let([tp (modulo (- (current-seconds) time) 10)])
      (if(= prior-i 1)(if(not (and (>= tp 0) (< tp 1)))(set! prior-i 0)
                         (void))
         (if(and (>= tp 0) (< tp 1))(begin
                                      (change-prior-for-each)
                                      (set! prior-i 1))
            (set! prior-i 0)))))
  (define (produce)
    (let([te (modulo (- (current-seconds) time) 10)])
      (if(= produce-i 1)(if(not (and (>= te 0) (< te 1)))(set! produce-i 0)
                           (void))
         (if(and (>= te 0) (< te 1))(if(<= (length (get-field list-of-enemies current-stats)) 2)
                                       (begin
                                         (produce-enemy 1)
                                         (set! produce-i 1))
                                       (void))
            (set! produce-i 0)))))
  (define (change-points)
    (let([tc (modulo (- (current-seconds) time) 8)])
      (if(= change-points-i 1)(if(not (and (>= tc 0) (< tc 1)))(set! change-points-i 0)
                                 (void))
         (if(and (>= tc 0) (< tc 1))(begin
                                      (send our-tank update-points)
                                      (set! change-points-i 1))
            (set! change-points-i 0)))))
  
  (define (change-path)
    (let([tp (modulo (- (current-seconds) time) 5)])
      (if(= change-path-i 1)(if (not ( and (>= tp 0) (< tp 1)))(set! change-path-i 0)
                                (void))
         (if( and (>= tp 0) (< tp 1))(begin
                                       (update-path-for-each)
                                       (set! change-path-i 1))
            (set! change-path-i 0)))))
  (define (produc-powers)
    (let([tp (modulo (- (current-seconds) time) 14)])
      (if(= powers-i 1)(if(not (and (>= tp 0) (< tp 1))) (set! powers-i 0)
                          (void))
         (if(and (>= tp 0) (< tp 1))(begin
                                      (produce-powers)
                                      (set! powers-i 1))
            (set! powers-i 0)))))
  
  (begin
    (prior)
    (produce)
    (change-points )
    (change-path )
    (produc-powers)))
    ;(display (get-field powers current-stats))
    ;(remove-powers (get-field powers current-stats))
    ;(newline)
    ;(display (get-field powers current-stats))
    ;(newline)))

(define (update-path-for-each)
  (define enemies (get-field list-of-enemies current-stats))
  (define (helper l)
    (if(null? l)(void)
       (let([enemy (car l)])
         (begin
           (send enemy shortest-path)
           (helper (cdr l))))))
  (helper enemies))


(define (change-prior-for-each)
  (define enemies (get-field list-of-enemies current-stats))
  (define (helper l)
    (if(null? l)(void)
       (let([enemy (car l)])
         (begin
           (send enemy change-priority)
           (helper (cdr l))))))
  (helper enemies))

(define (graphics-produce-enemy lev)
  (begin
    (send current-stats add-enemy (make-object enemy-tank% (cons 30 30) 'down lev))
    (send current-stats add-enemy (make-object enemy-tank% (cons 600 30) 'down lev)
          )))

(define (produce-enemy lev)
  (if(equal? check-stats #f)(void)
     (begin
       (graphics-produce-enemy lev)
       (update-enemy lev))))
(define (check-stats)
  (if(= (get-field enemies-yet-to-come current-stats) 0)#f
     #t))

(define (update-enemy lev)
  (set-field! enemies-yet2come current-stats (- (get-field enemies-yet2come current-stats) 2)))

(define (check pos dir i)
  (let((point pos))
    
    
    (cond [(equal? dir 'up) (if (= i 10) #t (if (and
                                                 (= (rgb-green ((get-color-pixel window) (make-posn (+ (car point) 1) (- (cdr point) i)))) 1)
                                                 (= (rgb-green ((get-color-pixel window) (make-posn (+ (car point) 15) (- (cdr point) i)))) 1)
                                                 (= (rgb-green ((get-color-pixel window) (make-posn (+ (car point) 28) (- (cdr point) i)))) 1))
                                                (check pos dir (+ i 1)) #f))]
          [(equal? dir 'down) (if (= i 10) #t (if (and (= (rgb-green ((get-color-pixel window) (make-posn (+ (car point) 15) (+ i 30 (cdr point))))) 1)
                                                       (= (rgb-green ((get-color-pixel window) (make-posn (+ (car point) 29) (+ i 30 (cdr point))))) 1)
                                                       (= (rgb-green ((get-color-pixel window) (make-posn (+ (car point) 1) (+ i 30 (cdr point))))) 1))
                                                  (check pos dir (+ i 1)) #f))]
          [(equal? dir 'left) (if (= i 10) #t (if (and (= (rgb-green ((get-color-pixel window) (make-posn (- (car point) i) (+ (cdr point) 1)))) 1)
                                                       (= (rgb-green ((get-color-pixel window) (make-posn (- (car point) i) (+ (cdr point) 15)))) 1)
                                                       (= (rgb-green ((get-color-pixel window) (make-posn (- (car point) i) (+ (cdr point) 29)))) 1))
                                                  (check pos dir (+ i 1)) #f))]
          [(equal? dir 'right) (if (= i 10) #t (if (and (= (rgb-green ((get-color-pixel window) (make-posn (+ i 30 (car point)) (+ (cdr point) 1)))) 1)
                                                        (= (rgb-green ((get-color-pixel window) (make-posn (+ i 30 (car point)) (+ (cdr point) 15)))) 1)
                                                        (= (rgb-green ((get-color-pixel window) (make-posn (+ i 30 (car point)) (+ (cdr point) 29)))) 1))
                                                   (check pos dir (+ i 1)) #f))])))

(define(ourtank-movement)
  (let ((dir  (ready-key-press main))
        (pos (get-field pos our-tank)))
    (cond[(equal? dir #f)(begin
                           (set-field! pos our-tank (get-field pos our-tank))
                           (set-field! dir our-tank (get-field dir our-tank)))]
         [(equal? (key-value dir) 'up) 
          (if(equal? (get-field dir our-tank) 'up)
             (if(equal? (check pos (key-value dir) 9) #t)
                (send our-tank set-pos (cons 0 -10))
                (send our-tank set-pos (cons 0 0)))
             (send our-tank set-dir 'up))]
         [(equal? (key-value dir) 'down) 
          (if(equal? (get-field dir our-tank) 'down)
             (if(equal? (check pos (key-value dir) 9) #t)
                (send our-tank set-pos (cons 0 10))
                (send our-tank set-pos (cons 0 0)))
             (send our-tank set-dir 'down))]
         [(equal? (key-value dir) 'right) 
          (if(equal? (get-field dir our-tank) 'right)
             (if(equal? (check pos (key-value dir) 9) #t)
                (send our-tank set-pos (cons 10 0))
                (send our-tank set-pos (cons 0 0)))
             (send our-tank set-dir 'right))]
         [(equal? (key-value dir) 'left) 
          (if(equal? (get-field dir our-tank) 'left)
             (if(equal? (check pos (key-value dir) 9) #t)
                (send our-tank set-pos (cons -10 0))
                (send our-tank set-pos (cons 0 0)))
             (send our-tank set-dir 'left))]
         [(equal? (key-value dir) #\space)
          (let* ((dir (get-field dir our-tank))
                 (pos (get-field pos our-tank))
                 (x (car pos))
                 (y (cdr pos)))
            (shoot-o x y dir))])))

(define (shoot-o x y dir)
  (if (equal? dir 'up)(send current-stats addourbullets (list (cons (+ x 10) y) dir))
      (if(equal? dir 'down)(send current-stats addourbullets (list (cons (+ x 10) (+ y 20)) dir))
         (if(equal? dir 'right)(send current-stats addourbullets (list (cons (+ x 20) (+ y 10)) dir))
            (if(equal? dir 'left)(send current-stats addenemybullets (list (cons x (+ y 10)) dir))
               (error "undefined dir"))))))

(define(update l)
  (begin
    (let ((pos (car l))
          (dr (cadr l)))
      
      (cond[(equal? dr 'up) (list (cons  (car pos) (- (cdr pos) 10)) dr)]
           [(equal? dr 'down) (list (cons (car pos) (+ (cdr pos) 10)) dr)]
           [(equal? dr 'right) (list (cons (+ (car pos) 10)(cdr pos)) dr)]
           [(equal? dr 'left) (list (cons (- (car pos) 10) (cdr pos)) dr)]))))

(define (bullet-move)
  (let([blt (get-field our-bullets current-stats)])
    (begin
      (set-field! our-bullets current-stats (map update blt))
      (set-field! enemy-bullets current-stats (begin
                                                
                                                (map update (get-field enemy-bullets current-stats))))
      )))


(define (checkb pos dir i)
  (let((point pos ));(send our-tank get-pos))) 
    (cond [(equal? dir 'up) (if (= i 10) #t (if (and (= ((get-pixel window) (make-posn (car point) (- (cdr point) i))) 0)
                                                     (= ((get-pixel window) (make-posn (+ (car point) 5) (- (cdr point) i))) 0)
                                                     (= ((get-pixel window) (make-posn (+ (car point) 10) (- (cdr point) i))) 0))
                                                (checkb pos dir (+ i 1)) #f))]
          [(equal? dir 'down) (if (= i 10) #t (if (and (= ((get-pixel window) (make-posn (+ (car point) 5) (+ i 10 (cdr point)))) 0)
                                                       (= ((get-pixel window) (make-posn (+ (car point) 10) (+ i 10 (cdr point)))) 0)
                                                       (= ((get-pixel window) (make-posn (car point) (+ i 10 (cdr point)))) 0))
                                                  (checkb pos dir (+ i 1)) #f))]
          [(equal? dir 'left) (if (= i 10) #t (if (and (= ((get-pixel window) (make-posn (- (car point) i) (cdr point))) 0)
                                                       (= ((get-pixel window) (make-posn (- (car point) i) (+ (cdr point) 5))) 0)
                                                       (= ((get-pixel window) (make-posn (- (car point) i) (+ (cdr point) 10))) 0))
                                                  (checkb pos dir (+ i 1)) #f))]
          [(equal? dir 'right) (if (= i 10) #t (if (and (= ((get-pixel window) (make-posn (+ i 10 (car point)) (cdr point))) 0)
                                                        (= ((get-pixel window) (make-posn (+ i 10 (car point)) (+ (cdr point) 5))) 0)
                                                        (= ((get-pixel window) (make-posn (+ i 10 (car point)) (+ (cdr point) 10))) 0))
                                                   (checkb pos dir (+ i 1)) #f))])))
(define (check1 dir x y i)
  (cond[(equal? dir 'up) (if (= i 9)#t  
                             (let((cal  ((get-color-pixel window) (make-posn (+ x 5) (- y i))))); (make-rgb 0 0 0)))
                               (cond [(equal? (rgb-green cal) (/ 4 17)) 1]
                                     [(equal? (rgb-green cal) 0)(if(equal? (get-field stone-breaking-power our-tank) 'yes)1  2)]
                                     [(equal? (rgb-green cal) 1) 3]
                                     [(equal? (rgb-green cal) (/ 32 255)) 4]
                                     [(equal? (rgb-green cal) (/ 38 51)) 5]
                                     [(equal? (rgb-green cal) (/ 11 17)) 6]
                                     [else (check1 dir x y (+ i 1))]
                                     )))]
       [(equal? dir 'down) (if (= i 9)#t  (let((cal  ((get-color-pixel window) (make-posn (+ x 5) (+ y 10 i))))); (make-rgb 0 0 0)))
                                            (cond [(equal? (rgb-green cal) (/ 4 17)) 1]
                                                  [(equal? (rgb-green cal) 0) (if(equal? (get-field stone-breaking-power our-tank) 'yes)1  2)]
                                                  [(equal? (rgb-green cal) 1) 3]
                                                  [(equal? (rgb-green cal) (/ 32 255)) 4]
                                                  [(equal? (rgb-green cal) (/ 38 51)) 5]
                                                  [(equal? (rgb-green cal) (/ 11 17)) 6]
                                                  [else (check1 dir x y (+ i 1))]
                                                  )))]
       [(equal? dir 'right) (if (= i 9)#t   (let((cal  ((get-color-pixel window) (make-posn (+ x 10 i) (+ y 5))))); (make-rgb 0 0 0)))
                                              (cond [(equal? (rgb-green cal) (/ 4 17)) 1]
                                                    [(equal? (rgb-green cal) 0) (if(equal? (get-field stone-breaking-power our-tank) 'yes)1  2)]
                                                    [(equal? (rgb-green cal) 1) 3]
                                                    [(equal? (rgb-green cal) (/ 32 255)) 4]
                                                    [(equal? (rgb-green cal) (/ 38 51)) 5]
                                                    [(equal? (rgb-green cal) (/ 11 17)) 6]
                                                    [else (check1 dir x y (+ i 1))]
                                                    )))]
       [(equal? dir 'left) (if (= i 9)#t (let((cal  ((get-color-pixel window) (make-posn (- x i) (+ y 5))))); (make-rgb 0 0 0)))
                                           (cond [(equal? (rgb-green cal) (/ 4 17)) 1]
                                                 [(equal? (rgb-green cal) 0) (if(equal? (get-field stone-breaking-power our-tank) 'yes)1  2)]
                                                 [(equal? (rgb-green cal) 1) 3]
                                                 [(equal? (rgb-green cal) (/ 32 255)) 4]
                                                 [(equal? (rgb-green cal) (/ 38 51)) 5]
                                                 [(equal? (rgb-green cal) (/ 11 17)) 6]
                                                 [else (check1 dir x y (+ i 1))]
                                                 )))]))

(define (check2 dir x y i)
  (cond[(equal? dir 'up) (if (= i 9)#t  
                             (let((cal  ((get-color-pixel window) (make-posn (+ x 5) (- y i))))); (make-rgb 0 0 0)))
                               (cond [(equal? (rgb-green cal) (/ 4 17)) 1]
                                     [(equal? (rgb-green cal) 0) 2]
                                     [(equal? (rgb-green cal) 1) 3]
                                     [(equal? (rgb-green cal) (/ 32 255)) 4]
                                     [(equal? (rgb-green cal) (/ 38 51)) 5]
                                     [(equal? (rgb-green cal) (/ 11 17)) 6]
                                     [else (check1 dir x y (+ i 1))]
                                     )))]
       [(equal? dir 'down) (if (= i 9)#t  (let((cal  ((get-color-pixel window) (make-posn (+ x 5) (+ y 10 i))))); (make-rgb 0 0 0)))
                                            (cond [(equal? (rgb-green cal) (/ 4 17)) 1]
                                                  [(equal? (rgb-green cal) 0) 2]
                                                  [(equal? (rgb-green cal) 1) 3]
                                                  [(equal? (rgb-green cal) (/ 32 255)) 4]
                                                  [(equal? (rgb-green cal) (/ 38 51)) 5]
                                                  [(equal? (rgb-green cal) (/ 11 17)) 6]
                                                  [else (check1 dir x y (+ i 1))]
                                                  )))]
       [(equal? dir 'right) (if (= i 9)#t   (let((cal  ((get-color-pixel window) (make-posn (+ x 10 i) (+ y 5))))); (make-rgb 0 0 0)))
                                              (cond [(equal? (rgb-green cal) (/ 4 17)) 1]
                                                    [(equal? (rgb-green cal) 0) 2]
                                                    [(equal? (rgb-green cal) 1) 3]
                                                    [(equal? (rgb-green cal) (/ 32 255)) 4]
                                                    [(equal? (rgb-green cal) (/ 38 51)) 5]
                                                    [(equal? (rgb-green cal) (/ 11 17)) 6]
                                                    [else (check1 dir x y (+ i 1))]
                                                    )))]
       [(equal? dir 'left) (if (= i 9)#t (let((cal  ((get-color-pixel window) (make-posn (- x i) (+ y 5))))); (make-rgb 0 0 0)))
                                           (cond [(equal? (rgb-green cal) (/ 4 17)) 1]
                                                 [(equal? (rgb-green cal) 0) 2]
                                                 [(equal? (rgb-green cal) 1) 3]
                                                 [(equal? (rgb-green cal) (/ 32 255)) 4]
                                                 [(equal? (rgb-green cal) (/ 38 51)) 5]
                                                 [(equal? (rgb-green cal) (/ 11 17)) 6]
                                                 [else (check1 dir x y (+ i 1))]
                                                 )))]))



(define (helpme a b c d l)
  (define (helper l1 l2)
    (if (null? l1) l2
        (let* ((en-pos (get-field pos (car l1)))
               (pow (get-field power (car l1)))
               (x1 (car en-pos))
               (y1 (cdr en-pos)))
          (if(and(and (> x1 a) (< x1 b)) (and (> y1 c) (< y1 d)))
             (begin 
               (if(equal? pow 1) (begin
                                   (set-field! list-of-enemies current-stats (append l2 (cdr l1)))
                                   (send current-stats onedead))
                  (set-field! power (car l1) (- pow 1)))
               '())
             (helper (cdr l1) (append l2 (list (car l1))))))))
  (helper l '()))



(define(bullt-h lis)
  ;(if(null? lis)'()
  (let* ((pos (car lis))
         (x (car pos))
         (y (cdr pos))
         (dr  (cadr lis))
         (enems (get-field list-of-enemies current-stats)))
    (define(helper l l1)
      (if (null? l) 'lev-complet
          (cond[(equal? dr 'up) (if(equal? (checkb pos dr 9) #t) (list (cons (car pos) (- (cdr pos) 0)) dr)
                                   (let* ((chk (check1 dr x y 8)))
                                     (cond[(equal? chk 1) 
                                           (begin
                                             (delete-brick x y dr)
                                             '())]
                                          [(equal? chk 2) '()]
                                          [(equal? chk 3) (list (cons (car pos) (- (cdr pos) 0)) dr)]
                                          [(equal? chk 4) (begin
                                                            (delete-enemy x (- y 10))
                                                            '())]
                                          [(equal? chk 6)(gameover)]
                                          ;[(equal? chk #t) (helpme (- x 29) (+ x 9) (- y 39) (- y 30) l)]
                                          ;'()]
                                          [else 'undefined])))]
               
               [(equal? dr 'down) (if(checkb pos dr 9) (list (cons (car pos) (+ (cdr pos) 0)) dr)
                                     (let*((chk (check1 dr x y 8)))
                                       (cond[(equal? chk 1) 
                                             (begin
                                               (delete-brick x y dr)
                                               '())]
                                            [(equal? chk 2) '()]
                                            [(equal? chk 6)(gameover)]
                                            [(equal? chk 3) (list (cons (car pos) (+ (cdr pos) 0)) dr)]
                                            [(equal? chk 4) (begin
                                                              (delete-enemy x (+ y 10 10))
                                                              '())]
                                            
                                            ;  [(equal? chk #t) (helpme x (+ x 40) (+ y 10) (+ y 20) l) l]
                                            [else 'undefined])))]
               
               [(equal? dr 'right) (if(checkb pos dr 9) (list (cons (+ (car pos) 0) (cdr pos)) dr)
                                      (let* ((chk (check1 dr x y 8)))
                                        (cond [(equal? chk 1) 
                                               (begin
                                                 
                                                 (delete-brick x y dr)
                                                 '())]
                                              [(equal? chk 2) '()]
                                              [(equal? chk 6)(gameover)]
                                              [(equal? chk 3) (list (cons (car pos) (+ (cdr pos) 0)) dr)]
                                              [(equal? chk 4) (begin
                                                                (delete-enemy (+ x 20) y)
                                                                '())]
                                              ;   [(equal? chk #t) (helpme (+ x 10) (+ x 20) (+ y 40) y l)]
                                              [else 'undefined])))]
               
               [(equal? dr 'left) (if(checkb pos dr 9) (list (cons (- (car pos) 0) (cdr pos)) dr)
                                     (let* ((chk (check1 dr x y 8)))
                                       (begin
                                         (display chk)
                                         (cond [(equal? chk 1) 
                                                (begin
                                                  (delete-brick x y dr)
                                                  '())]
                                               [(equal? chk 2) '()]
                                               [(equal? chk 6)(gameover)]
                                               [(equal? chk 3) (list (cons (car pos) (+ (cdr pos) 0)) dr)]
                                               [(equal? chk 4) (begin
                                                                 (delete-enemy (- x 10) y)
                                                                 '())]
                                               ; [(equal? chk #t) (helpme (- x 9) x (+ y 40) y l)]
                                               [else 'undefined]))))])))                                   
    
    (helper enems '())));(get-field bullets our-tank) '())))

(define (ourbt-check)
  (define(helper l lis)
    (if (null? l) (set-field! our-bullets current-stats lis)
        (let ((val (bullt-h (car l))))
          (cond[(null? val) (helper (cdr l) lis)]
               [(equal? val 'lev-complet) (next-stage)]
               [(list? val) (let ((lis1  (cons val lis)))
                              (helper (cdr l) lis1))]))))
  (helper (get-field our-bullets current-stats)'()))
(define(bullet-check)
  (begin
    (ourbt-check)
    (enem-blt current-stats)))

(define(enem-blt ob)
  (let((enm-lst (get-field enemy-bullets ob)))
    
    (define(helper l l1)
      (if(null? l)  (set-field! enemy-bullets ob l1)
         ;(if(null? (car l)) (begin
         ;                    (set-field! enemy-bullets ob l1)
         ;                   '())
         (let* ((pos (car l))
                (pos1 (car pos))
                (x (car pos1))
                (y (cdr pos1))
                (dr (cadr (car l))))
           (cond[(equal? dr 'up) (if(checkb pos1 dr 9) (helper (cdr l) (append l1 (list  (list (cons (car pos1) (cdr pos1)) dr))))
                                    (let((chk (check2 dr x y 8)))
                                      (begin
                                        (cond[(equal? chk 5)
                                              (begin
                                                (if(equal? (get-field power our-tank) 1) (send our-tank killed)
                                                   (set-field! power our-tank (- (get-field power our-tank) 1)))
                                                (helper (cdr l) l1))]
                                             [(equal? chk 1) 
                                              (begin
                                                (delete-brick x y dr)
                                                (helper (cdr l) l1))]
                                             [(equal? chk 2) (helper (cdr l) l1)]
                                             [(equal? chk 6)(gameover)]
                                             [(equal? chk 3) (helper (cdr l) (append l1 (list(list (cons (car pos1) (- (cdr pos1) 0)) dr))))]
                                             [else  (helper (cdr l) (append  l1 (list (list (cons (car pos1) (- (cdr pos1) 0)) dr))))]))))]
                [(equal? dr 'down) (if(checkb pos1 dr 9) (helper (cdr l) (append  l1 (list(list (cons (car pos1) (+ (cdr pos1) 0)) dr))))
                                      (let((chk (check2 dr x y 8)))
                                        (cond[(equal? chk 5)
                                              (begin
                                                (if(equal? (get-field power our-tank) 1) (send our-tank killed)
                                                   (set-field! power our-tank (- (get-field power our-tank) 1)))
                                                (helper (cdr l) l1))]
                                             [(equal? chk 1) 
                                              (begin
                                                (delete-brick x y dr)
                                                (helper (cdr l) l1))]
                                             [(equal? chk 6)(gameover)]
                                             [(equal? chk 2) (helper (cdr l) l1)]
                                             [(equal? chk 3) (helper (cdr l) (append l1 (list (list (cons (car pos1) (+ (cdr pos1) 0)) dr))))]
                                             [else (helper (cdr l) (append l1 (list (list (cons (car pos1) (+ (cdr pos1) 0)) dr))))])))]
                [(equal? dr 'right) (if(checkb pos1 dr 9) (helper (cdr l) (append  l1 (list (list (cons (+ (car pos1) 0)  (cdr pos1)) dr))))
                                       (let((chk (check2 dr x y 8)))
                                         (cond[(equal? chk 5)
                                               (begin
                                                 (if(equal? (get-field power our-tank) 1) (send our-tank killed)
                                                    (set-field! power our-tank (- (get-field power our-tank) 1)))
                                                 (helper (cdr l) l1))]
                                              [(equal? chk 1) 
                                               (begin
                                                 (delete-brick x y dr)
                                                 (helper (cdr l) l1))]
                                              [(equal? chk 6)(gameover)]
                                              [(equal? chk 2) (helper (cdr l) l1)]
                                              [(equal? chk 3) (helper (cdr l) (append l1 (list (list (cons (- (car pos1) 0) (cdr pos1)) dr))))]
                                              [else (helper (cdr l) (append l1 (list (list (cons (- (car pos1) 0) (cdr pos1)) dr))))])))]
                [(equal? dr 'left) (if(checkb pos1 dr 9) (helper (cdr l) (append l1 (list (list (cons (+ (car pos1) 0)  (cdr pos1)) dr))))
                                      (let((chk (check2 dr x y 8)))
                                        (cond[(equal? chk 5)
                                              (begin
                                                (if(equal? (get-field power our-tank) 1) (send our-tank killed)
                                                   (set-field! power our-tank (- (get-field power our-tank) 1)))
                                                (helper (cdr l) l1))]
                                             [(equal? chk 1) 
                                              (begin
                                                (delete-brick x y dr)
                                                (helper (cdr l) l1))]
                                             [(equal? chk 6)(gameover)]
                                             [(equal? chk 2) (helper (cdr l) l1)]
                                             [(equal? chk 3) (helper (cdr l) (append l1 (list (list (cons (- (car pos1) 0) (cdr pos1)) dr))))]
                                             [else  (helper (cdr l) (append l1 (list (list (cons (- (car pos1) 0) (cdr pos1)) dr))))])))]))))
    (helper enm-lst '())))

(define (delete-brick x y dir)
  (begin
    (cond [(equal? dir 'up) (begin ((clear-solid-rectangle window1) (make-posn (* (floor (/ x 30)) 30) (- y 10)) 30 10)
                                   ((clear-solid-rectangle window) (make-posn (* (floor (/ x 30)) 30) (- y 10)) 30 10)
                                   ((draw-solid-rectangle window2) (make-posn (* (floor (/ x 30)) 30) (- y 10)) 30 10 "black")
                                   ((draw-solid-rectangle window3) (make-posn (* (floor (/ x 30)) 30) (- y 10)) 30 10 "black"))]
          [(equal? dir 'down) (begin ((clear-solid-rectangle window1) (make-posn (* (floor (/ x 30)) 30) (+ y 10)) 30 10)
                                     ((clear-solid-rectangle window) (make-posn (* (floor (/ x 30)) 30) (+ y 10)) 30 10)
                                     ((draw-solid-rectangle window2) (make-posn (* (floor (/ x 30)) 30) (+ y 10)) 30 10 "black")
                                     ((draw-solid-rectangle window3) (make-posn (* (floor (/ x 30)) 30) (+ y 10)) 30 10 "black"))]
          [(equal? dir 'left) (begin ((clear-solid-rectangle window1) (make-posn (- x 10) (* (floor (/ y 30)) 30)) 10 30)
                                     ((clear-solid-rectangle window) (make-posn (- x 10) (* (floor (/ y 30)) 30)) 10 30)
                                     ((draw-solid-rectangle window2) (make-posn (- x 10) (* (floor (/ y 30)) 30)) 10 30 "black")
                                     ((draw-solid-rectangle window3) (make-posn (- x 10) (* (floor (/ y 30)) 30)) 10 30 "black"))]
          [(equal? dir 'right) (begin ((clear-solid-rectangle window1) (make-posn (+ x 10) (* (floor (/ y 30)) 30)) 10 30)
                                      ((clear-solid-rectangle window2) (make-posn (+ x 10) (* (floor (/ y 30)) 30)) 10 30)
                                      ((draw-solid-rectangle window3) (make-posn (+ x 10) (* (floor (/ y 30)) 30)) 10 30 "black")
                                      ((draw-solid-rectangle window) (make-posn (+ x 10) (* (floor (/ y 30)) 30)) 10 30 "black"))])
    (begin (draw-grass 0 0 mat)
           (copy-viewport window2 main))))

(define (gameover)
  (begin
    ((draw-pixmap main) "gameover.bmp" (make-posn 0 0))
    (set! z -1)))

(define (enemytank-movement)
  (define enemies (get-field list-of-enemies current-stats))
  (define (turn-arbit) (let([c (random)])
                         (if(< c .25)'left
                            (if(< c .50)'right
                               (if(< c .75)'up
                                  'down)))))
  (define (move enemy dir x y)
    (if(equal? dir 'up)(set-field! pos enemy (cons x (- y 10)))
       (if(equal? dir 'down)(set-field! pos enemy (cons x (+ y 10)))
          (if(equal? dir 'right)(set-field! pos enemy (cons (+ x 10) y))
             (if(equal? dir 'left)(set-field! pos enemy (cons (- x 10) y))
                (error "undefined dir"))))))
  (define (can-move? dir x y)(check (cons x y) dir 9)) 
  (define (update-graphics-single enemy)
    (let* ([posn (get-field pos enemy)]
           [x (car posn)]
           [y (cdr posn)]
           [dir (get-field dir enemy)])
      (if(equal? dir 'up) (begin ((draw-solid-rectangle window) (make-posn x y) 30 30 "purple")
                                 ;((draw-pixmap window) "enemyu.jpg" (make-posn x y))
                                 ((draw-pixmap window2) "enemyu.jpg" (make-posn x y)))
         (if(equal? dir 'down) (begin ((draw-solid-rectangle window) (make-posn x y) 30 30 "purple")
                                      ;((draw-pixmap window) "enemyd.jpg" (make-posn x y))
                                      ((draw-pixmap window2) "enemyd.jpg" (make-posn x y)))
            (if(equal? dir 'right) (begin ((draw-solid-rectangle window) (make-posn x y) 30 30 "purple")
                                          ;((draw-pixmap window) "enemyr.jpg" (make-posn x y))
                                          ((draw-pixmap window2) "enemyr.jpg" (make-posn x y)))
               (if(equal? dir 'left) (begin ((draw-solid-rectangle window) (make-posn x y) 30 30 "purple")
                                            ;((draw-pixmap window) "enemyl.jpg" (make-posn x y))
                                            ((draw-pixmap window2) "enemyl.jpg" (make-posn x y)))
                  (begin
                    (error "undefined dir"))))))))
  (define (turn-arbit-k)
    (let([ c (random)])
      (if(< c .5)'down
         (if(< c .7)'right
            (if(< c .9)'left
               'up)))))
  
  (define (helper l)
    (if(null? l)(set! l '())
       (let* ([enemy (car l)]
              [dir (get-field dir enemy)]
              [x (car (get-field pos enemy))]
              [y (cdr (get-field pos enemy))]
              [waytoking (get-field way2king enemy)]
              [waytotank (get-field way2rtank enemy)])
         ;         (begin
         ;           (display (send our-tank scope-e (cons x y)))
         ;           (newline)
         ;           (display (correct-dir-e x y))
         ;           (newline)
         ;           (if(send our-tank scope-e (cons x y))
         ;            (if(equal? dir (correct-dir-e x y))(begin
         ;                                                 (shoot dir x y)
         ;                                                 (helper (cdr l)))
         ;               (begin
         ;                 (set! dir (correct-dir-e x y))
         ;                 (helper (cdr l))))
         ;            (helper (cdr l)))))))
         ;  (helper enemies))
         (if(send our-tank scope-k (cons x y))
            (if(equal? dir (correct-dir-k x y))
               (if (< (random) .9)
                   (begin
                     (shoot dir x y)
                     (helper (cdr l)))
                   (begin
                     (set-field! dir enemy (turn-arbit))
                     (shoot dir x y)
                     (helper (cdr l))))
               (begin
                 (set-field! dir enemy (correct-dir-k x y))
                 (helper (cdr l))))
            (if(equal? (get-field priority enemy) 'k)
               (if(equal? waytoking 'undefined)(begin
                                                 (if(can-move? dir x y)(if(< (random) .8)(begin
                                                                                           (move enemy dir x y)
                                                                                           (update-graphics-single enemy)
                                                                                           (helper (cdr l)))
                                                                          (begin
                                                                            (set-field! dir enemy (turn-arbit-k))
                                                                            (shoot dir x y)
                                                                            ;(update-graphics-single enemy)
                                                                            (helper (cdr l))))
                                                    (begin
                                                      (set-field! dir enemy (turn-arbit-k))
                                                      (shoot dir x y)
                                                      ;(update-graphics-single enemy)
                                                      (helper (cdr l)))))
                  
                  (let([waytoking (cdr waytoking)])
                    (if(>= 2  (length waytoking))
                       (if (equal? (get-field dir enemy) (correct-dir-k))
                           (if (< (random) 1)
                               (begin
                                 (shoot dir x y)
                                 (helper (cdr l)))
                               (begin
                                 (set-field! dir enemy (turn-arbit))
                                 (shoot dir x y)
                                 ;(update-graphics-single enemy)
                                 (helper (cdr l))))
                           (begin
                             (set-field! dir enemy (correct-dir-k))
                             (update-graphics-single enemy)
                             (helper (cdr l))))
                       
                       (if(equal? dir (face-the-path waytoking enemy))
                          (if(< (random) 1)(begin
                                             (if(can-move? dir x y)
                                                (begin
                                                  (set-field! pos enemy (cons (caar waytoking) (cadar waytoking)))
                                                  (set-field! way2king enemy waytoking))
                                                (begin
                                                  (shoot dir x y)
                                                  (shoot dir x y)))
                                             (update-graphics-single enemy)
                                             (helper (cdr l)))
                             (begin
                               (shoot dir x y)
                               (set-field! dir enemy (turn-arbit))
                               ;(update-graphics-single enemy)
                               (helper (cdr l))))
                          (begin
                            (set-field! dir enemy (face-the-path waytoking enemy))
                            (update-graphics-single enemy)
                            (helper (cdr l)))))))
               
               (if(equal? waytotank 'undefined)(begin
                                                 ;((clear-solid-rectangle window) (make-posn x y) 30 30)
                                                 (if(can-move? dir x y)(if(< (random) .8)
                                                                          (begin
                                                                            (move enemy dir x y)
                                                                            (update-graphics-single enemy)
                                                                            (helper (cdr l)))
                                                                          (begin
                                                                            (shoot dir x y)
                                                                            (set-field! dir enemy (turn-arbit-k))
                                                                            (shoot dir x y)))
                                                    (begin
                                                      (set-field! dir enemy (turn-arbit-k))
                                                      (shoot dir x y)
                                                      ;(update-graphics-single enemy)
                                                      (helper (cdr l))))
                                                 ;(update-graphics-single enemy))
                                                 )
                  (if(send our-tank scope-e (cons x y))
                     (if(equal? dir (correct-dir-e x y))(if (< (random) .9)
                                                            (begin
                                                              (shoot dir x y)
                                                              (helper (cdr l)))
                                                            (begin
                                                              (set-field! dir enemy (turn-arbit))
                                                              (shoot dir x y)
                                                              (helper (cdr l))))
                        (begin
                          (set-field! dir enemy (correct-dir-e x y))
                          (helper (cdr l))))
                     (let([waytotank (cdr waytotank)])
                       (if(<= (length waytotank) 2)
                          (if (equal? (get-field dir enemy) (correct-dir-e x y))
                              (if (< (random) 1)
                                  (begin
                                    (shoot dir x y)
                                    (helper (cdr l)))
                                  (begin
                                    (set-field! dir enemy (turn-arbit))
                                    (shoot dir x y)
                                    ;(update-graphics-single enemy)
                                    (helper (cdr l))))
                              (begin
                                (set-field! dir enemy (correct-dir-e x y))
                                (helper (cdr l))))
                          
                          (if(equal? dir (face-the-path waytotank enemy))
                             (if(< (random) 1)(begin
                                                (if(can-move dir x y)
                                                   (begin
                                                     (set-field! pos enemy (cons (caar waytotank) (cadar waytotank)))
                                                     (set-field! way2rtank enemy waytotank))
                                                   (begin
                                                     (shoot dir x y) 
                                                     (shoot dir x y)))
                                                (update-graphics-single enemy)
                                                (helper (cdr l)))
                                (begin
                                  (shoot dir x y)
                                  (set-field! dir enemy (turn-arbit))
                                  ;(update-graphics-single enemy)
                                  (helper (cdr l))))
                             (set-field! dir enemy (face-the-path waytotank enemy))))))))))))
  
  (helper enemies))

(define (correct-dir-k x y)
  
  
  (if(< x 270)'right
     (if(>= x 390)'left
        'down)))
(define (correct-dir-t enemy)
  (let([x-o (car (get-field pos our-tank))]
       [y-o (cdr (get-field pos our-tank))]
       [x-e (car (get-field pos enemy))]
       [y-e (cdr (get-field pos enemy))])
    (if(and (> (abs (- x-e x-o)) (abs (- y-e y-o))) (> x-e x-o))'left
       (if(and (> (abs (- x-e x-o)) (abs (- y-e y-o))) (< x-e x-o))'right
          (if(and (< (abs (- x-e x-o)) (abs (- y-e y-o))) (< y-e y-o))'down
             'up)))))



(define (face-the-path path enemy)
  (let([x-of-e (car (get-field pos enemy))]
       [y-of-e (cdr (get-field pos enemy))]
       [x-of-first (car (car path))]
       [y-of-first (cadr (car path))])
    (if(and (= x-of-e x-of-first) (< y-of-e y-of-first))(begin
                                                          ;(display 'down)
                                                          'down)
       
       (if(and (= x-of-e x-of-first) (< y-of-first y-of-e))(begin
                                                             ;(display'up)
                                                             'up)
          (if (and (= y-of-e y-of-first) (< x-of-first x-of-e))(begin
                                                                 ; (display 'left)
                                                                 'left)
              (if(and (= y-of-e y-of-first) (< x-of-e x-of-first))(begin
                                                                    ; (display 'right)
                                                                    'right)))))))

(define (shoot dir x y)
  (if (equal? dir 'up)(send current-stats addenemybullets (list (cons (+ x 10) y) dir))
      (if(equal? dir 'down)(send current-stats addenemybullets (list (cons (+ x 10) (+ y 20)) dir))
         (if(equal? dir 'right)(send current-stats addenemybullets (list (cons (+ x 20) (+ y 10)) dir))
            (if(equal? dir 'left)(send current-stats addenemybullets (list (cons x (+ y 10)) dir))
               (error "undefined dir"))))))

(define (correct-dir-e x y)
  (let*([pos-t (get-field pos our-tank)]
        [x-t (car pos-t)]
        [y-t (cdr pos-t)])
    (if(> (abs (- x-t x)) (abs (- y-t y)))
       (if(> x x-t)'left
          'right)
       (if(> y y-t)'up
          'down))))

(require (lib "trace.ss"))
(trace produce-powers)