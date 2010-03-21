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
  (define (dropback-line n space shift include-rear? include-front?)
    (append
     (translate-hands (juggler-line n space (- (/ pi 2))) space 0 0)
     (if include-rear? (translate-hands
                        (rotate-hands pair-of-hands (- (/ pi 2)))
                        (* space (+ n 1)) shift 0)
         '())
     (if include-front? (translate-hands
                        (rotate-hands pair-of-hands (/ pi 2))
                        0 shift 0)
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
  
  ; The numbers in old-ordering refer to destinations in the sexp
  
  ; Assumes you knew how many throws were there in the first place...
  
  ; big fat slow algorithm. Might be too slow  if you're autogenerating old-ordering and ordering for large hand lists.
  
  ; e.g. (reorder-throws (4hss->sexp "7") '(0 3 1 2))
  (define (reorder-throws sexp new-ordering)
    (let-values (((stars beats) (if (list? (car sexp))
                                (values '() sexp)
                                (values (list (car sexp)) (cadr sexp)))))
      (append stars
            (map
             (λ (beat)
               (map (λ (old-hand)
                      ; grab throw (whether 'throw' or 'rest') that is in BEAT at the same position as OLD-HAND is in OLD-ORDERING.
                      (match 
                          (list-ref beat old-hand) ; beat is indexed by  old-hand
                        ('- '-)
                        ((list-rest height old-dest-hand options)
                         (cons height
                               (cons 
                                (list-index (λ (x) (equal? x old-dest-hand)) new-ordering)
                                options)))))
                    new-ordering))
             beats))))
  
  (define (4h-rlrl sexp)
    (reorder-throws sexp '(0 2 1 3)))
  
  (define (6h-reverse-j2 sexp)
    (reorder-throws sexp '(0 1 3 2 4 5)))
  )