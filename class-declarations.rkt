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

(define enemy-tank1%
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
      
      (define/public (shortest) 1))))

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

