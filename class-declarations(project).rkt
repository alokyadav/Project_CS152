(define our-tank%
  (class object%
    (init pow)
    (init l)
    (init-field [power pow])
    (init-field [lives l])
    
    (init-field [pos (cons 80 210)])
    (init-field [dir 'up])
    (super-new)
    
    (define/public (set-pos pair) (set! pos (cons (+ (car pair) (car pos)) (+ (cdr pair) (cdr pos)))))
    (define/public (set-dir dr) (set! dir dr))))

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
    (init-field [way2rtank '()])
    (init-field [way2king '()])
    (init-field [priority 'k])
    (init-field [points (list '() '() '() '())])
    
    (super-new)
    
    (define/public (change-priority) (let ([c (/ (length way2rtank) (+ (length way2rtank) (length way2king)))]) (if (< (random) c)(set! priority 'k)
                                                                                                                    (set! priority 't))))
    
    (define/public (update-points)
      
      (define-struct posn (x y))
      
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
    
    (define/public (shortest-path)
      (define (path ini fnl)
        (define (face-2-face pos dir)
          (cond [(equal? dir 'up) (if (null? (filter (lambda (x) (equal? pos x)) (cadddr points))) #f #t)]
                [(equal? dir 'down) (if (null? (filter (lambda (x) (equal? pos x)) (cadr points))) #f #t)]
                [(equal? dir 'left) (if (null? (filter (lambda (x) (equal? pos x)) (cadr points))) #f #t)]
                [(equal? dir 'right) (if (null? (filter (lambda (x) (equal? pos x)) (caddr points))) #f #t)]))
        
        (define (find-fnl fnl points)
          (if (null? (filter (lambda (x) (and (= (car x) (car fnl)) (= (cadr x) (cdr fnl)))) points)) #f #t)) 
        (define (dedupe e)
          (if (null? e) '()
              (cons (car e) (dedupe (filter (lambda (x) (not (equal? x (car e)))) 
                                            (cdr e))))))
        (define (main fnl points counter)
          (define (helper giv-point)
            (define (mylist) (filter (lambda (x) (not (equal? x #f))) 
                                     (list (if (and 
                                                (equal? (check 'down 0) #t) 
                                                (equal? (face-2-face (cons (car giv-point) (+ (cdr giv-point) 5)) dir) #t))
                                               (list (car giv-point) (+ (cdr giv-point) 5) (+ counter 1)) #f)
                                           (if (and 
                                                (equal? (check 'up 0) #t) 
                                                (equal? (face-2-face (cons (car giv-point) (- (cdr giv-point) 5)) dir) #t))
                                               (list (car giv-point) (- (cdr giv-point) 5) (+ counter 1)) #f)
                                           (if (and 
                                                (equal? (check 'right 0) #t)
                                                (equal? (face-2-face (cons (+ (car giv-point) 5) (cdr giv-point)) dir) #t))
                                               (list (+ (car giv-point) 5) (cdr giv-point) (+ counter 1)) #f)
                                           (if (and 
                                                (equal? (check 'left 0) #t)
                                                (equal? (face-2-face (cons (- (car giv-point) 5) (cdr giv-point)) dir) #t))
                                               (list (- (car giv-point) 5) (cdr giv-point) (+ counter 1)) #f))))
            
            (define (remove mylist) ;(begin (display mylist)
              (if (null? (cdr mylist)) (if (null? (filter (lambda (x) (and (= (car x) (caar mylist)) 
                                                                           (= (cadr x) (cadar mylist))
                                                                           (>= (+ counter 1) (caddr x)))) points))
                                           (list (car mylist)) '())
                  (if (null? (filter (lambda (x) (and (= (car x) (caar mylist))
                                                      (= (cadr x) (cadar mylist))
                                                      (>= (+ counter 1) (caddr x)))) points))
                      (append (list (car mylist)) (remove (cdr mylist))) (remove (cdr mylist)))))
            
            (define (check dir i)
              (if (or (and (>= (car giv-point) 855) (equal? dir 'right))
                      (and (<= (car giv-point) 0) (equal? dir 'left))
                      (and (>= (cdr giv-point) 570) (equal? dir 'down))
                      (and (<= (cdr giv-point) 0) (equal? dir 'up))) #f
                                                                     (cond [(equal? dir 'up) 
                                                                            (if (= i 5) #t (if (and
                                                                                                (= (rgb-green ((get-color-pixel fake) (make-posn (+ (car giv-point) 1) (- (cdr giv-point) i)))) 1)
                                                                                                (= (rgb-green ((get-color-pixel fake) (make-posn (+ (car giv-point) 15) (- (cdr giv-point) i)))) 1)
                                                                                                (= (rgb-green ((get-color-pixel fake) (make-posn (+ (car giv-point) 28) (- (cdr giv-point) i)))) 1))
                                                                                               (check dir (+ i 1)) #f))]
                                                                           [(equal? dir 'down) 
                                                                            (if (= i 5) #t (if (and
                                                                                                (= (rgb-green ((get-color-pixel fake) (make-posn (+ (car giv-point) 1) (+ i 30 (cdr giv-point))))) 1)
                                                                                                (= (rgb-green ((get-color-pixel fake) (make-posn (+ (car giv-point) 15) (+ i 30 (cdr giv-point))))) 1)
                                                                                                (= (rgb-green ((get-color-pixel fake) (make-posn (+ (car giv-point) 28) (+ i 30 (cdr giv-point))))) 1))
                                                                                               (check dir (+ i 1)) #f))]
                                                                           [(equal? dir 'left) 
                                                                            (if (= i 5) #t (if (and
                                                                                                (= (rgb-green ((get-color-pixel fake) (make-posn (- (car giv-point) i) (+ (cdr giv-point) 1)))) 1)
                                                                                                (= (rgb-green ((get-color-pixel fake) (make-posn (- (car giv-point) i) (+ (cdr giv-point) 15)))) 1)
                                                                                                (= (rgb-green ((get-color-pixel fake) (make-posn (- (car giv-point) i) (+ (cdr giv-point) 28)))) 1))
                                                                                               (check dir (+ i 1)) #f))]
                                                                           [(equal? dir 'right) 
                                                                            (if (= i 5) #t (if (and 
                                                                                                (= (rgb-green ((get-color-pixel fake) (make-posn (+ i 30 (car giv-point)) (+ (cdr giv-point) 1)))) 1)
                                                                                                (= (rgb-green ((get-color-pixel fake) (make-posn (+ i 30 (car giv-point)) (+ (cdr giv-point) 15)))) 1)
                                                                                                (= (rgb-green ((get-color-pixel fake) (make-posn (+ i 30 (car giv-point)) (+ (cdr giv-point) 28)))) 1))
                                                                                               (check dir (+ i 1)) #f))])))
            
            (define (new-points) (append points (remove (mylist))))
            (filter (lambda (x) (not (equal? x #f))) (remove (mylist))))
          
          
          (define (apply)
            ;(display (apply-h (find-in-list = points)))
            ;(newline)
            (apply-h (find-in-list = points counter))) 
          
          (define (apply-h giv-list)      
            (if (null? (cdr giv-list)) (helper (cons (caar giv-list) (cadar giv-list)))
                (append (helper (cons (caar giv-list) (cadar giv-list))) (apply-h (cdr giv-list)))))
          
          (cond [(equal? (find-fnl fnl points) #t) points]
                [else (main fnl (dedupe (append points (apply))) (+ counter 1))]))
        (main fnl (list ini) 0))
      (define (find-in-list pred take-list counter)
        (filter (lambda (x) (pred (caddr x) counter)) take-list))
      
      
      (let* ((mypath (path (list (car pos) (cdr pos) 0) (get-field pos our-tank)))
             (revpath (reverse mypath))
             (maxcounter (caddar revpath)))
        (define (helper i point) 
          (if (= i 0) (list initial)
              (append (list (car (filter (lambda (x) (or (= (abs (- (car x) (car point))) 5)
                                                         (= (abs (- (cdr x) (cdr point))) 5))) (find-in-list = mypath i))))
                      (helper (- i 1) (car (filter (lambda (x) (or (= (abs (- (car x) (car point))) 5)
                                                                   (= (abs (- (cdr x) (cdr point))) 5))) (find-in-list = mypath i)))))))
        (helper maxcounter (get-field pos our-tank))))))

(define level-stats%
  (class object%
    
    (init lev)
    (init enem)
    
    (init-field [stage lev])
    (init-field [total-enemies enem])
    (init-field [enemies-yet2come enem])
    (init-field [enemy-dead 0])
    (init-field [lives 4])
    (init-field [list-of-ememies '()])
    (init-field [our-bullets '()])
    (init-field [enemy-bullets '()])
    
    (define/public (addourbullet l)(set! our-bullets (cons l bullets)))
    
    (define/public (addenemybullet l) (set1 enemy-bullets (cons l enemy-bullets)))
    
    (define/public (add-enemy object) (set! list-of-enemies (cons object list-of-enemies)))
    
    (define/public (onedead)
      (set! enemy-dead (+ enemy-dead 1)))
    
    (define/public (killed)
      (set! lives [- lives 1]))))

