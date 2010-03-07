(module pattern-utilities scheme
  (require "juggling-core.ss" srfi/1)
  
  (provide (all-defined-out))
  
  (define (mangle-hands hands order)
    (map list-ref (circular-list hands) order))
  
  ; Useful for future patterns...
     ; A hand (list throw catch) at radius r from 0 0 at angle a, pointing at 0,0
  (define (angle-hand a r lr)
    (let* 
        ((split (/ pi (* r 16)))
         (a1 (- a split)) (a2 (+ a split))
         (p1 (make-position (* r (sin a2)) (* r (cos a2)) 1.0))
         (p2 (make-position (* r (sin a1)) (* r (cos a1)) 1.0)))
       (make-hand (if (eq? lr 'right) p2 p1)
                  (if (eq? lr 'right) p1 p2)
                  (+ a pi))))
 
  (define pair-of-hands
    (list (make-hand (make-position 0.3 0 1.0) (make-position 0.60 -0.05 1.1) (* pi -5/12))
          (make-hand (make-position -0.3 0 1.0) (make-position -0.60 -0.05 1.1) (* pi -7/12))))
  (define j pair-of-hands)
  
  (define pair-of-jugglers 
    (append (translate-hands (rotate-hands pair-of-hands pi) 0 2.0 0)
            (translate-hands  pair-of-hands 0 -2.0 0)))
  
  
  (define triangle 
      (list (angle-hand (- 0 (/ pi 16)) 2.5 'left) (angle-hand (+ 0 (/ pi 16)) 2.5 'right)
            (angle-hand (- (* pi 2/3) (/ pi 12)) 2.5 'left) (angle-hand (+ (* pi 2/3) (/ pi 12)) 2.5 'right)
            (angle-hand (- (* pi 4/3) (/ pi 12)) 2.5 'left) (angle-hand (+ (* pi 4/3) (/ pi 12)) 2.5 'right)))
  
  ; n jugglers r meters from center
  (define 2pi (* 2 pi))
  (define (juggler-circle n r)
      (apply append
             (for/list ((i (in-range 0 (- 2pi (/ 2pi (* n 2))) (/ 2pi n))))
               (rotate-hands (translate-hands pair-of-hands 0 (- r) 0) i))))
  
  (define (juggler-line n space angle) ; Jugglers from 0,0 in a line along angle 0 facing toward "angle"
    (apply append
           (map
            (λ (n) (translate-hands
                    (rotate-hands pair-of-hands angle)
                    (* space n) 0 0))
            (iota n))))
    
  ; Hand list starting with "n" pairs facing the front,
  ; an optional side-shifted guy in the back,
  ; and an optional side-shifted guy in the front facing the rest.
  (define (dropback-line n space include-rear? include-front?)
    (append
     (translate-hands (juggler-line n space (- (/ pi 2))) space 0 0)
     (if include-rear? (translate-hands
                        (rotate-hands pair-of-hands (- (/ pi 2)))
                        (* space (+ n 1)) (/ space 5) 0)
         '())
     (if include-front? (translate-hands
                        (rotate-hands pair-of-hands (/ pi 2))
                        0 (/ space 5) 0)
         '())))
    
  
  ; Put n jugglers (two hands) on the ground 
  ;(define (juggler-line n x y space angle)
  
  ; N jugglers do a self v (same hand if even, opposite if odd) from hand h ('left/'right)
  (define (self n v h)
    (sync (* 2 n) v (if (eq? h 'left) -1 +1) (if (eq? h 'left) odd? even?)))
  
  (define (sync number-of-hands throw-value destination-offset pred)
    (map 
     (lambda (i) (if (pred i) 
                     (let* 
                         ((unfixed-dest (+ i destination-offset))
                          (dest-index (cond
                                         ((>= unfixed-dest number-of-hands) (remainder unfixed-dest number-of-hands))
                                         ((< unfixed-dest 0) (+ number-of-hands unfixed-dest))
                                         (#t unfixed-dest))))                     
                       (list throw-value dest-index)) '-))
     (iota number-of-hands)))
  
  (define (3-count number-of-jugglers number-of-props . rest)
    (let-values (((even-offset odd-offset) (if (null? rest) (values -1 1)
                                               (values (car rest) (cadr rest)))))
      (list (sync (* number-of-jugglers 2) number-of-props even-offset even?) 
            (sync (* number-of-jugglers 2) number-of-props -1 odd?) 
            (sync (* number-of-jugglers 2) number-of-props 1 even?)
            (sync (* number-of-jugglers 2) number-of-props odd-offset odd?) 
            (sync (* number-of-jugglers 2) number-of-props 1 even?) 
            (sync (* number-of-jugglers 2) number-of-props -1 odd?))))
  
  ; AUUUUUGH.
  ; I should have written functions for the individual roles in a feed
  ; And then finally started writing functions to sew jugglers together...
  
  ; Only works with jugglers 0-(n-2) being fed and n-1 feeding for now
  ; (sync needs to be improved to make this easier, maybe)
  (define (typewriter-feed n count objects)
    (if (or (odd? count) (even? objects)) 'flagrant-error
        ; The pattern should be (n - 1) * count throws long... (the feeder doesn't feed themself)
        (apply append
               (map
                (λ (i) ; It's  i's turn, 0 based
                  ; list of beats, goes to append
                  (cons
                   ; pass beat
                   (let ((feeder-dest-hand (sub1 (* n 2)))
                         (feedee-dest-hand (sub1 (* (add1 i) 2))))
                     (apply append
                            (map
                             (λ (j) ; juggler j
                               (cond ((= j (sub1 n)) (list (list objects feedee-dest-hand) '-))
                                     ((= j i) (list (list objects feeder-dest-hand) '-))
                                     ; all others, right self
                                     (#t (list (list objects (add1 (* j 2))) '-))))
                             (iota n))))
                       
                   ; self beat(s)
                   (for/list ((j (in-range 1 count)))
                     (sync (* n 2) objects (if (even? j) 1 -1) (if (even? j) even? odd?)))))
                (iota (sub1 n))))))
  
  ; Change the order of hands. The *order* of the reordering is the new order of throws.
  ; The *numbers* in the reordering are hands in the order of R1, L1, R2, L2...
  
  ; Assumes you knew how many throws were there in the first place...
  (define (reorder-throws sexp old-ordering ordering)
    (let loop-beats ((sexp-rest sexp) (t 0))
      (if (null? sexp-rest) '()
          (cons
           (match-let* ((beat (begin
                          (display (format "Beat: ~a~nt:~a~n" (car sexp-rest) t))
                          (car sexp-rest)))
                  (old-hand (list-ref old-ordering t))
                  (new-hand (list-ref ordering t))
                  ((list-rest len dest options) (list-ref beat old-hand))
                  (new-dest
                   ; really slow.
                   (let loop-orderings ((old-rest old-ordering) (new-rest ordering) (i 0))
                     (if (null? old-rest) 'flagrant-error
                         (if (= dest (list-ref old-ordering i)) (list-ref ordering i) (loop-orderings (cdr old-rest) (cdr new-rest) (add1 i))))))
                  (new-throw (cons len (cons new-dest options))))
                  
             
             (let loop-hands ((h 0))
               (if (< h (length ordering))
                   (cons
                    (if (= h new-hand)
                        new-throw
                        '-)
                    (loop-hands (add1 h))
                    )
                   '()
                   )))
           (loop-beats (cdr sexp-rest)
                       (if (< (add1 t) (length ordering)) (add1 t)
                           0))))))
  )