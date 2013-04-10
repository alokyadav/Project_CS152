(define (correct-path-e x y)
  (let([pos-t (get-field pos our-tank)]
       [x-t (car pos)]
       [y-t (cdr pos)])
    (if(> (abs (- x-t x)) (abs (- y-t y)))
       (if(> x x-t)'left
          'right)
       (if(> y y-t)'up
          'down))))