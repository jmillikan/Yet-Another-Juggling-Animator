(module example-patterns scheme
  (require "juggling-core.ss" srfi/1)
  
  (provide (all-defined-out))
  
  (define 4-hand-examples
    '("966" "996" "9629669669969929" "86277" "86727" "5" "7" "9" "b" "db97" "db97531"))
  
  (define 2-ss-examples
    '("3" "4" "5" "6" "7" "8" "9" "7531" "db97531" "64514" "55550" "552" "5551" "555505551" "744" "51" "71" "91"))
  
  (define sexp-examples
    '("'(* ((6 1) (4 1)) (- -)) ; (6x,4)*"
      "'(** ((4 3) - - (3 2))) ; 7 'club' 2-count"
      "'(*** ((4 2) - (3 3) -)) ; 7 'club' crossing 2-count"
      "'(* ((3 3) - (3 1) -) (- (3 0) - (3 2)) ((3 1) - (3 3) -)) ; 3-count"
      "'(* ((4 0) (2 0)) (- -)) ; box"
      "'(* ((8 0) (2 0)) (- -) ((4 0) (2 0)) (- -)) ; 4-ball box"
      "'(* ((4 0) (2 0)) (- -) ((2 1) (4 0)) (- -)) ; shower box"
      "'(((3 3) - (3 5) - (3 1) -) (- (3 0) - (3 2) - (3 4))) ; Dropback line/Triangle"
      "'(((3 3) - (3 5) - (3 1) -) (- (3 0) - (3 2) - (3 4)) ((3 1) - (3 3) - (3 5) -) (- (3 0) - (3 2) - (3 4))) ; 4-count Dropbacks/Triangle"
      "'(((5 3) - (3 5) - (3 1) -) (- (3 0) - (3 2) - (3 4))) ; 10-club triple dropback"
      "'(((5 3) - - (3 2) (3 1) -) (- (3 0) (4 5) - - (3 4))) ; 10-club doubles dropback"
      "'(* ((3 5) - (3 7) - (3 9) - (3 1) - (3 3) -) (- (3 0) - (3 2) - (3 4) - (3 6) - (3 8)) ((3 1) - (3 3) - (3 5) - (3 7) - (3 9) -)) ; 3-count Star"
      "#;(Almost a Berkley Y...) `(* ((3 7) - (3 9) - (3 11) - (3 5) - (3 1) - (3 3) -) ,(self 5 3 'left) ,(self 5 3 'right))"
      "'(((5 7) - (5 9) - (5 11) - (3 5) - (3 1) - (3 3) -) (- (3 0) - (3 2) - (3 4) - (3 6) - (3 8) - (3 10))) ; Big double/triple dropback ring"
      "#;(Gorilla sync) '(((5 5) (5 2) - - - -) (- - - (5 1) (5 0) -) (- - (5 3) - - (5 4)))"
      "#;(20 cascades) (list (self 20 3 'right) (self 20 3 'left))"
      "#;(15 juggler 3-count - traveling props) (list (sync 30 3 1 even?) (sync 30 3 -1 odd?) (sync 30 3 9 even?) (sync 30 3 -1 odd?) (sync 30 3 1 even?)  (sync 30 3 19 odd?))"
      "#;(15 juggler 3-count... Wedding Cake?) (list (sync 30 3 1 even?) (sync 30 3 -1 odd?) (sync 30 9 9 even?) (sync 30 3 -1 odd?) (sync 30 3 1 even?)  (sync 30 9 19 odd?))"
      "#;(15 juggler nonsense) (list (sync 30 5 1 even?) (sync 30 5 -1 odd?) (sync 30 11 9 even?) (sync 30 5 -1 odd?) (sync 30 5 1 even?)  (sync 30 11 19 odd?))"
      "#;(More 15-man garbage) (list (sync 30 11 11 even?) (sync 30 9 9 odd?) (sync 30 5 1 even?) (sync 30 5 9 odd?) (sync 30 5 1 even?)  (sync 30 7 9 odd?))"
      "#;(65 juggler 3-count) (list (sync 130 3 1 even?) (sync 130 3 -1 odd?) (sync 130 3 -1 even?) (sync 130 3 -1 odd?) (sync 130 3 1 even?)  (sync 130 3 1 odd?))"
      "#;(600 juggler 3-count - Slow) (3-count 600 3)"
      
      ))
  
  (define internal-examples
    '("(sexp->pattern '(((11 3) - - -) (- - - -) (- (10 0) - -) (- - (11 1) -) (- - - -) (- - - (10 2))) 0.10 0.20 pair-of-jugglers) ; 7-singles"
      "(sexp->pattern '(** ((11 3) - - -) (- - - -) (- (10 0) - -)) 0.10 0.20 pair-of-jugglers) ; 7-singles"
      "(sexp->pattern (4hss->sexp \"b0a\") 0.10 0.20 funky-pair-of-jugglers) ; 7 clubs... Jim's 2-count?"))
  
  (define ex-left-throw (make-position -0.15 0 1.0))
  (define ex-left-catch (make-position -0.4 0 1.0))
  (define ex-right-throw (make-position 0.15 0 1.0))
  (define ex-right-catch (make-position 0.4 0 1.0))
  
  (define pair-of-hands (list (list ex-right-throw ex-right-catch) (list ex-left-throw ex-left-catch)))    
  
  (define pair-of-jugglers (list (list  ex-right-throw ex-right-catch) 
                             (list ex-left-throw ex-left-catch)
                             (list (make-position -0.15 3 1.0) (make-position -0.4 3 1.0))
                             (list (make-position 0.15 3 1.0) (make-position 0.4 3 1.0))))
  
  (define hands-examples
    (list "pair-of-hands"
          "pair-of-jugglers ; r1 l1 r2 l2)"
          "funky-pair-of-jugglers ; r1 r2 l1 l2 (4-hand SS)"
          "3-man-line ; r1 l1 ..."
          "#;(Triangle) (juggler-circle 3 2.5)"
          "#;(Star) (juggler-circle 5 3.0)"
          "#;(Dropback ring) (append (juggler-circle 3 2.5) (juggler-circle 3 4.5))"
          "#;(15-juggler ring) (juggler-circle 15 8.0)"
          "#;(15-jugglers - 3 rings) (append (juggler-circle 5 8.0) (juggler-circle 5 5.0) (juggler-circle 5 10.0))"
          "#;(20 jugglers) (append (juggler-circle 5 3.0) (juggler-circle 15 6.5))"
          "#;(65 jugglers) (append (juggler-circle 5 3.0) (juggler-circle 15 6.5) (juggler-circle 20 10.0) (juggler-circle 25 12.0))"
          "#;(600 jugglers, 6 rings) (append (juggler-circle 100 50.0) (juggler-circle 100 53.0) (juggler-circle 100 56.0) (juggler-circle 100 41.0)  (juggler-circle 100 44.0) (juggler-circle 100 47.0))"
          ))
  
  (define 3-man-line (list
                      (list ex-left-throw ex-left-catch)
                      (list  ex-right-throw ex-right-catch) 
                      (list (make-position -0.15 3 1.0) (make-position -0.4 3 1.0))
                      (list (make-position 0.15 3 1.0) (make-position 0.4 3 1.0))
                      (list (make-position -.15 -3 1.0) (make-position 0.1 -3 1.0))
                      (list (make-position -0.55 -3 1.0) (make-position -0.8 -3 1.0))))
  
  ; Useful for future patterns...
  
  ; A hand (list throw catch) at radius r from 0 0 at angle a
  (define (angle-hand a r lr)
    
    (let* 
        (
         (split (/ pi (* r 16)))
         (a1 (- a split)) (a2 (+ a split)))
      ((if (eq? lr 'right) reverse (lambda (x) x))
       (list (make-position (* r (sin a2)) (* r (cos a2)) 1.0)
            (make-position (* r (sin a1)) (* r (cos a1)) 1.0)))))
  
  (define triangle 
      (list (angle-hand (- 0 (/ pi 16)) 2.5 'left) (angle-hand (+ 0 (/ pi 16)) 2.5 'right)
            (angle-hand (- (* pi 2/3) (/ pi 12)) 2.5 'left) (angle-hand (+ (* pi 2/3) (/ pi 12)) 2.5 'right)
            (angle-hand (- (* pi 4/3) (/ pi 12)) 2.5 'left) (angle-hand (+ (* pi 4/3) (/ pi 12)) 2.5 'right)))
  
  ; n jugglers r meters from center
  (define 2pi (* 2 pi))
  (define (juggler-circle n r)
      (apply append
             (for/list ((i (in-range 0 2pi (/ 2pi n))))
               (list (angle-hand (- i (/ pi (* 4 r))) r 'left) (angle-hand (+ i (/ pi (* 4 r))) r 'right)))))
    
  (define funky-pair-of-jugglers
    (list (list-ref pair-of-jugglers 0)
          (list-ref pair-of-jugglers 2)
          (list-ref pair-of-jugglers 1)
          (list-ref pair-of-jugglers 3)))
  
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
  
  (define (3-count number-of-jugglers number-of-props)
    (list (sync (* number-of-jugglers 2) number-of-props -1 even?) 
          (sync (* number-of-jugglers 2) number-of-props -1 odd?) 
          (sync (* number-of-jugglers 2) number-of-props 1 even?)
          (sync (* number-of-jugglers 2) number-of-props 1 odd?) 
          (sync (* number-of-jugglers 2) number-of-props 1 even?) 
          (sync (* number-of-jugglers 2) number-of-props -1 odd?)))
  
    #;(65 juggler 3-count) #;(list (sync 370 3 1 even?) (sync 370 3 -1 odd?) (sync 370 3 -1 even?) (sync 370 3 -1 odd?) (sync 370 3 1 even?)  (sync 370 3 1 odd?))
       
  
  
  )