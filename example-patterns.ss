(module example-patterns scheme
  (require "juggling-core.ss" srfi/1)
  #;(require "fourhss-converter.ss")
  
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
    (list (make-hand (make-position 0.35 0 1.0) (make-position 0.60 -0.05 1.1) (* pi -5/12))
          (make-hand (make-position -0.35 0 1.0) (make-position -0.60 -0.05 1.1) (* pi -7/12))))
  
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
               (rotate-hands (translate-hands pair-of-hands 0 (- r) 0) i)
               #;(list (angle-hand (- i (/ pi (* 4 r))) r 'left) (angle-hand (+ i (/ pi (* 4 r))) r 'right)))))
  
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
  
  (define 4-hand-examples
    '("966" "996" "9629669669969929" "86277" "86727" "5" "7" "9" "b" "db97" "db97531" "7966"
            "747" "747b47747707" "7b7740747747" "945747747" "794646" "7946466"))
  
  (define 2-ss-examples
    '("3" "4" "5" "6" "7" "8" "9" "7531" "db97531" "64514" "55550" "552" "5551" "555505551" "744" "51" "71" "91"))
  
  (define 6-ss-examples
    '("999d" ; 4-count triangle
      "9ddd" ; 3/4-count triangle?
      "a" ; triangorilla?
      "e" ; 14-object "gorilla"
      "ama" ; with "aam", "maa" a set of 14-object triple/single gorillas
      "9" ; cascades
      "9a8 999 a89 999" ; 4-count feed
      "9a8 a89 999" ; PPS feed
      "aa7999" ; 3-man line (looks astonishingly accurate...)
      ))
  
  (define passing-ss-examples
    '("<3p 3 3|3p 3 3>"
      "<3p2 3 3p3 3|3p1 3 3 3|3 3 3p1 3>"
      "<3p 3p 3 3|3p 3p 3 3>"
      "<5p 3 3 3|3 3 5p 3>"
      "<5p2 3 5p3 3|3 3 5p1 3|5p1 3 3 3>"
      "<3p3 3p2 3|3 3p1 3|3p1 3 3>"
      "<3p2 3p3 3p4|3p1 3 3 |3 3p1 3|3 3 3p1>"
      "<4p 3|3 4p>" ; Don't ask... This mess might be fixed someday
      "<4p 3|4p 3>"
      "<4p2 3 4p3 3|3 3 4p1 3|4p1 3 3 3>"))
  
  (define syncss-examples
    '("(6x,4x)"
      "(6x,4)*"
      "(2x,4x)(2x,4)*"
      "(4,4)"
      "(6,6)"
      "(6x,4)(2,4x)*"
      "(6,4)(4x,2x)*"
      "(6x,4p,6x,4p)(6p,4x,6p,4x)"
      "(4p,4x,4p,4x)"
      "(4px,4x,4x,6px)"))
  
  (define sexp-examples
    '("'(* ((6 1) (4 1)) (- -)) ; (6x,4)*"
      "'(** ((4 3) - - (3 2))) ; 7 'club' 2-count"
      "'(*** ((4 2) - (3 3) -)) ; 7 'club' crossing 2-count"
      "'(* ((3 3) - (3 1) -) (- (3 0) - (3 2)) ((3 1) - (3 3) -)) ; 3-count"
      "#;(Almost Jim's 3-count) '(* ((3 3) - (3 0) -) ((3 1) - - (4 2)) (- (3 0) (2 3) -) ((3 3) - - (3 1)) (- (4 0) - (3 2)) ((2 1) - (3 3) -))"
      "#;(Jim's 3-count) '(* ((3 3) - (3 0) -) ((3 1) - - (4 2 antihurry)) (- (3 0) (2 3 hurry) -) ((3 3) - - (3 1)) (- (4 0 antihurry) - (3 2)) ((2 1 hurry) - (3 3) -))"
      "'(* ((4 0) (2 0)) (- -)) ; box"
      "'(* ((8 0) (2 0)) (- -) ((4 0) (2 0)) (- -)) ; 4-ball box"
      "'(* ((4 0) (2 0)) (- -) ((2 1) (4 0)) (- -)) ; shower box"
      "'(((3 3) - (3 5) - (3 1) -) (- (3 0) - (3 2) - (3 4))) ; Dropback line/Triangle"
      "'(((3 3) - (3 5) - (3 1) -) (- (3 0) - (3 2) - (3 4)) ((3 1) - (3 3) - (3 5) -) (- (3 0) - (3 2) - (3 4))) ; 4-count Dropbacks/Triangle"
      
      "#;(\"Mill's Mess\") '(* ((3 1) -) ((3 1) -) ((3 1) -))"
      "#;(4-ball \"Mill's Mess\") '(* ((4 1) -) ((4 1) -) ((4 0) -))"
      "#;(5-ball \"Mill's Mess\") '(* ((5 1) -) ((5 0) -) ((5 0) -))"
      "#;(441 \"Mill's Mess\") '(* ((4 1) -) ((1 0) -) ((4 0) -))"
      "#;(10-club triple dropback) '(((5 3) - (3 5) - (3 1) -) (- (3 0) - (3 2) - (3 4)))"
      "#;(10-club doubles dropbacks) '(((4 3) - - (3 2) (3 1) -) (- (3 0) (4 5) - - (3 4)))"
      "'(* ((3 5) - (3 7) - (3 9) - (3 1) - (3 3) -) (- (3 0) - (3 2) - (3 4) - (3 6) - (3 8)) ((3 1) - (3 3) - (3 5) - (3 7) - (3 9) -)) ; 3-count Star"
      "#;(Almost a Berkley Y...) `(* ((3 7) - (3 9) - (3 11) - (3 5) - (3 1) - (3 3) -) ,(self 5 3 'left) ,(self 5 3 'right))"
      "'(((5 7) - (5 9) - (5 11) - (3 5) - (3 1) - (3 3) -) (- (3 0) - (3 2) - (3 4) - (3 6) - (3 8) - (3 10))) ; Big double/triple dropback ring"
      "#;(5-man 5p 3) (list (sync 10 5 3 even?) (sync 10 3 -1 odd?))"
      "#;(400-man 5p 3) (list (sync 80 5 3 even?) (sync 80 3 -1 odd?))"
      "#;(Gorilla sync) '(((5 5) (5 2) - - - -) (- - - (5 1) (5 0) -) (- - (5 3) - - (5 4)))"
      "#;(20 cascades) (list (self 20 3 'right) (self 20 3 'left))"
      "#;(15 juggler 3-count - traveling props) (list (sync 30 3 1 even?) (sync 30 3 -1 odd?) (sync 30 3 9 even?) (sync 30 3 -1 odd?) (sync 30 3 1 even?)  (sync 30 3 19 odd?))"
      "#;(15 juggler 3-count... Wedding Cake?) (list (sync 30 3 1 even?) (sync 30 3 -1 odd?) (sync 30 9 9 even?) (sync 30 3 -1 odd?) (sync 30 3 1 even?)  (sync 30 9 19 odd?))"
      "#;(15 juggler nonsense) (list (sync 30 5 1 even?) (sync 30 5 -1 odd?) (sync 30 11 9 even?) (sync 30 5 -1 odd?) (sync 30 5 1 even?)  (sync 30 11 19 odd?))"
      "#;(More 15-man garbage) (list (sync 30 11 11 even?) (sync 30 9 9 odd?) (sync 30 5 1 even?) (sync 30 5 9 odd?) (sync 30 5 1 even?)  (sync 30 7 9 odd?))"
      "#;(65 juggler 3-count) (list (sync 130 3 1 even?) (sync 130 3 -1 odd?) (sync 130 3 -1 even?) (sync 130 3 -1 odd?) (sync 130 3 1 even?)  (sync 130 3 1 odd?))"
      "#;(600 juggler 3-count - Slow) (3-count 600 3)"
      "#;(5-club feed, 10 feedees) (typewriter-feed 11 2 5)"
      "#;(7-club singles) '(((11 3) - - -) () (- (10 0)) (- - (11 1)) () (- - - (10 2)))"
      "#;(Some 3/4 man 747-ish feeds) 747-feed"
      "777-feed"
      "777-747-744-feed "
      "777-feed-3s"
      "777-feed-3s-vs-744"
      "777-feed-3s-vs-takeouts"
      ))
   
    (define jims-3-star
    '(*
      ((3 3) - (3 0) -) 
      ((3 1) - - (4 2)) 
      (- (3 0) (2 3) -)
      ((3 3) - - (3 1)) 
      (- (4 0) - (3 2)) 
      ((2 1) - (3 3) -)))
  
  (define mills-hands ; Without animating hands, this is sorta simple... 2 hands, 
    ; each catches in the middle and throws from one side.
    (list 
     ; "Right" hand
     (list (make-position 0.4 0 1.0) (make-position 0 0 1.0))
     ; "Left" hand
     (list (make-position -0.4 0 1.0) (make-position 0 0 1.0))))
  
  (define hands-examples
    (list "pair-of-hands"
          "mills-hands"
          "pair-of-jugglers ; r1 l1 r2 l2)"
          "funky-pair-of-jugglers ; r1 r2 l1 l2 (4-hand SS)"
          "3-man-line ; r1 l1 ..."
          "#;(Back to back) (append pair-of-hands (translate-hands (rotate-hands pair-of-hands pi) 0 -1.5 0))"
          "#;(Triangle) (juggler-circle 3 2.5)"
          "#;(Star) (juggler-circle 5 3.0)"
          "#;(Star) (rotate-hands (juggler-circle 5 3.0) (/ (* pi 0) 16)) "
          "#;(Dropback ring) (append (juggler-circle 3 2.5) (juggler-circle 3 4.5))"
          "#;(15-juggler ring) (juggler-circle 15 8.0)"
          "#;(15-jugglers - 3 rings) (append (juggler-circle 5 8.0) (juggler-circle 5 5.0) (juggler-circle 5 10.0))"
          "#;(20 jugglers) (append (juggler-circle 5 3.0) (juggler-circle 15 6.5))"
          "#;(65 jugglers) (append (juggler-circle 5 3.0) (juggler-circle 15 6.5) (juggler-circle 20 10.0) (juggler-circle 25 12.0))"
          "#;(600 jugglers, 6 rings) (append (juggler-circle 100 60.0) (juggler-circle 100 63.0) (juggler-circle 100 66.0) (juggler-circle 100 51.0)  (juggler-circle 100 54.0) (juggler-circle 100 57.0))"
          "#;(Async gorilla hands) (mangle-hands (juggler-circle 3 3.0) '(3 0 1 2 4 5))"
          "#;(5-man dropback line) (dropback-line 3 3.0 #t #t)"
          "#;(40-man \"canoe\" (longboat?)) (append (dropback-line 19 3.0 #t #f) (translate-hands (rotate-hands (dropback-line 19 3.0 #t #f) pi) 60 3 0))"
          "#;(11-man feed, elevated feeder (feeder last)) (append (take (juggler-circle 30 10.0) 20) (translate-hands (rotate-hands pair-of-hands (* pi 9/7)) 0 0 3.0)))"
          "#;(Wide 4-man feed (feeder first)) (append (rotate-hands pair-of-hands (* pi 1.14)) (take (juggler-circle 15 9.0) 6)))"
          "(append (rotate-hands pair-of-hands (* pi 1.14)) (map list-ref (circular-list (juggler-circle 15 9.0)) '(0 1 4 5)) (rotate-hands (translate-hands pair-of-hands 0 -1.5 0.5) (* pi 1.14)))"
          ))
  
  (define 3-man-line (list
                      (list (make-position -0.15 0 1.0) (make-position -0.4 0 1.0))
                      (list (make-position 0.15 0 1.0) (make-position 0.4 0 1.0))
                      (list (make-position -0.15 3 1.0) (make-position -0.4 3 1.0))
                      (list (make-position 0.15 3 1.0) (make-position 0.4 3 1.0))
                      (list (make-position -.15 -3 1.0) (make-position 0.1 -3 1.0))
                      (list (make-position -0.55 -3 1.0) (make-position -0.8 -3 1.0))))
  
  
  (define 747-sexp 
    '(((7 3) - - -)
      (- - (4 2) -)
      (- (7 2) - -)
      (- - - (7 1))
      ((4 0) - - -)
      (- - (7 0) -)
      (- (7 2) - -)
      (- - - (4 3))
      ((7 3) - - -)
      (- - (7 0) -)
      (- (4 1) - -)
      (- - - (7 1))))
  
  (define 747-feed 
    '(((7 3) - - - - -)
      (- - (4 2) - (4 4) -)
      (- (7 4) - -)
      (- - - (7 1) - (4 5))
      ((4 0) - - -)
      (- - (4 2) - (7 0) -)
      (- (7 2) - -)
      (- - - (4 3) - (4 5))
      ((7 5) - - -)
      (- - (7 0) - (4 4) -)
      (- (4 1) - -)
      (- - - (4 3) - (7 1))))
  
  
  (define 777-747-744-feed 
    '(((7 3) - - - - -)
      (- - (7 0) - (4 4) -)
      (- (7 4) - -)
      (- - - (7 1) - (4 5))
      ((7 3) - - -)
      (- - (4 2) - (7 0) -)
      (- (7 2) - -)
      (- - - (7 1) - (4 5))
      ((7 5) - - -)
      (- - (7 0) - (4 4) -)
      (- (7 2) - -)
      (- - - (4 3) - (7 1))))
  
  (define 777-feed 
    '(((7 3) - - - - -)
      (- - (4 2) - (4 4) - (7 0) -)
      (- (7 4) - -)
      (- - - (7 1) - (4 5) - (4 7))
      ((7 7) - - -)
      (- - (4 2) - (7 0) - (4 6) -)
      (- (7 2) - -)
      (- - - (4 3) - (4 5) - (7 1))
      ((7 5) - - -)
      (- - (7 0) - (4 4) - (4 6) -)
      (- (7 6) - -)
      (- - - (4 3) - (7 1) - (4 7))))
  
  (define 777-feed-3s 
    '(((7 3) - - - - -)
      (- - (9 1) - (6 5) - (6 7) -)
      (- (7 4) - -)
      (- - - (6 2) - (9 0) - (6 6))
      ((7 7) - - -)
      (- - (6 3) - (6 5) - (9 1) -)
      (- (7 2) - -)
      (- - - (9 0) - (6 4) - (6 6))
      ((7 5) - - -)
      (- - (6 3) - (9 1) - (6 7) -)
      (- (7 6) - -)
      (- - - (6 2) - (6 4) - (9 0))))
  
  (define 777-feed-3s-vs-744
    '(((7 3) - - - - -)
      (- - (9 1) - (6 5) - (7 0) -)
      (- (7 4) - -) ; a
      (- - - (6 2) - (9 0) - (4 7)) 
      ((7 7) - - -) ; to d
      (- - (6 3) - (6 5) - (4 6) -) ; c - To beat a
      (- (7 2) - -)
      (- - - (9 0) - (6 4) - (7 1))
      ((7 5) - - -)
      (- - (6 3) - (9 1) - (4 6) -)
      (- (7 6) - -) ; to c
      (- - - (6 2) - (6 4) - (4 7)))) ; d - To beat b
  
  (define 777-feed-3s-vs-takeouts
    '(((7 3) - - - - - - (2 1))
      (- - (9 1) - (6 5) - - (2 6))
      (- (7 4) - -) ; a
      (- - - (6 2) - (9 0) (2 6) -) 
      ((3 6) - - -) ; to d
      (- - (6 3) - (6 5) - (1 6) -) ; c - To beat a
      (- (7 2) - - - - (2 0) -)
      (- - - (9 0) - (6 4) (2 7) -)
      ((7 5) - - -)
      (- - (6 3) - (9 1) - - (2 7))
      (- (3 7) - -) ; to c
      (- - - (6 2) - (6 4) - (1 7)))) ; d - To beat b
  
  )