(module example-patterns scheme
  (require "juggling-core.ss" "pattern-utilities.ss" srfi/1)
  
  (provide (all-defined-out))
  
  (define pair-of-jugglers-with-levitator
    #;(pair-of-jugglers)
`(,(make-hand (make-position -0.3 2.0 1.0) (make-position -0.6 2.05 1.1) 1.832595714594046)
  ,(make-hand (make-position 0.3 2.0 1.0) (make-position 0.6 2.0500000000000003 1.1) 1.308996938995747)
  ,(make-path-state 0 #f (circular-list (make-juggler-path-segment 5 (lambda _ (lambda (t) (make-hand (make-position 0.3 -2.0 (+ (* 2 t) 1.0)) (make-position 0.6 -2.05 (+ (* 2 t) 1.1)) -1.3089969389957472))))))
  ,(make-path-state 0 #f (circular-list (make-juggler-path-segment 5 (lambda _ (lambda (t) (make-hand (make-position -0.3 -2.0 (+ (* 2 t) 1.0)) (make-position -0.6 -2.05 (+ (* 2 t) 1.1)) -1.8325957145940461))))))))
  
  
  (define pair-of-jugglers-with-circler
    (append
     (list
      (make-path-state 0 #f (circular-list
                            (make-juggler-path-segment 
                             (* 2 pi)
                             (lambda _ (lambda (t)
                                         (list-ref 
                                          (translate-hands (rotate-hands pair-of-hands t) (* 2 (sin t)) (+ 2.0 (* 2 (cos t))) 0)
                                          0))))))
     
      (make-path-state 0 #f (circular-list
                            (make-juggler-path-segment 
                             (* 2 pi)
                             (lambda _ (lambda (t)
                                         (list-ref 
                                          (translate-hands (rotate-hands pair-of-hands t) (* 2 (sin t)) (+ 2.0 (* 2 (cos t))) 0)
                                          1)))))))
                                                       
     #;(translate-hands (rotate-hands pair-of-hands pi) 0 2.0 0)
            (translate-hands  pair-of-hands 0 -2.0 0)))
  
  (define 4-hand-examples
    '("5" "7" "9" "b" 
      "966" ; 7-club 3-count
      "996" ; 8-club pps
      "9629669669969929" ; Copenhagen countdown
      "86277" "86727" ; Whynots
      "7288a"
      "855"
      "885"
      #;("b97" "db97") ; Some scary only-theoretical ultimates patterns
      "7966" ; A staggered 7-club 2-count
      "75666" ; Cool pattern from Madison, spotted at Madfest 2010
      "756" ; Much harder variant
      "747" ; pass-pass-hold (2/3 of 7 ultimates)
      #;("747b47747707" "7b7740747747")  ; Early and late triples
      "945747747" ; double-joe in 747
      "794646" "7946466" ; 6-club oddities showing hte matching "signature" of 744 and 966
      #;("b784847" "b7848477777") ; an ill-concieved 7 club popcorn
            ))
  
  ; What's the fun of having a million of these?
  (define 2-ss-examples
    '("3" "4" "5" "6" "7" "8" "9" "7531" "db97531" "64514" "55550" "552" "5551" "555505551" "744" "51" "71" "91"))
  
  (define 6-ss-examples
    '(
      "8"
      "a" ; 10 ultimates/gorilla
      "b" ; 11 ultimates
      "e" ; 14-object "gorilla"
      
      "9a8 999 a89 999" ; 4-count feed
      "9a8 a89 999" ; PPS feed
      "9a8 a89" ; 1-countt feed
      
      "d999" ; 4-count triangle
      "aa7999" ; 3-man line (looks astonishingly accurate...)
      
      ; Some patterns from Luke from Madison
      "a8999" ; 
      "c99aa" ;
      ))
  
  ; Incorrect support for a subset of Passing SS
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
      "#;(3-count) '(* ((3 3) - (3 1) -) (- (3 0) - (3 2)) ((3 1) - (3 3) -))"
      "#;(3-count with tomahawks) '(* ((3 3 tomahawk) - (3 1) -) (- (3 0) - (3 2)) ((3 1) - (3 3) -))"
      "#;(Jim's 3-count) '(* ((3 3) - (3 0) -) ((3 1) - - (4 2 antihurry)) (- (3 0) (2 3 hurry) -) ((3 3) - - (3 1)) (- (4 0 antihurry) - (3 2)) ((2 1 hurry) - (3 3) -))"
      "#;(Mild Madness) `(*  
((3 3) - (3 0) -)
((1 1) (3 2) - (3 1))
(- (3 0) (2 3 hurry) -)
((3 3) - - (3 1))
(- (3 2) (3 0) (1 2))
((2 1 hurry) - (3 3) -)
)"
      "#;(PPS) 
'(((3 3) - (3 1) -)
(- (3 2) - (3 0))
((3 1) - (3 3) -))"
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
      "#;(40-man 5p 3) (list (sync 80 5 3 even?) (sync 80 3 -1 odd?))"
      "#;(Gorilla sync) '(((5 5) (5 2) - - - -) (- - - (5 1) (5 0) -) (- - (5 3) - - (5 4)))"
      "#;(Split Gorilla) '(
((5 5) (5 2) - - - - - -) 
(- - - (5 1) (7 5) - (5 0)) 
(- - (5 3) - - (5 4) - (5 6))
((5 7) (5 2)) 
(- - - (5 1) (5 0) - (7 7)) 
(- - (5 3) - - (5 4) - (5 6)))"
      "#;(15 juggler 3-count - traveling props) (list (sync 30 3 1 even?) (sync 30 3 -1 odd?) (sync 30 3 9 even?) (sync 30 3 -1 odd?) (sync 30 3 1 even?)  (sync 30 3 19 odd?))"
      "#;(15 juggler 3-count... Wedding Cake?) (list (sync 30 3 1 even?) (sync 30 3 -1 odd?) (sync 30 9 9 even?) (sync 30 3 -1 odd?) (sync 30 3 1 even?)  (sync 30 9 19 odd?))"
      "#;(15 juggler nonsense) (list (sync 30 5 1 even?) (sync 30 5 -1 odd?) (sync 30 11 9 even?) (sync 30 5 -1 odd?) (sync 30 5 1 even?)  (sync 30 11 19 odd?))"
      "#;(600 juggler 3-count - Slow) (3-count 600 3)"
      "#;(5-club feed, 10 feedees) (typewriter-feed 11 2 5)"
      "#;(7-club singles) '(((11 3) - - -) () (- (10 0)) (- - (11 1)) () (- - - (10 2)))"
      "#;(9-club doubles?) '(((17 3) - - -) () (- (10 0)) (- - (17 1)) () (- - - (10 2)))"
      "#;(Some 3/4 man 747-ish feeds) 747-feed"
      "777-feed"
      "777-747-744-feed "
      "777-feed-3s"
      "777-feed-3s-vs-744"
      "777-feed-3s-vs-takeouts"
      "#;(10 club  ultimate triangle, all crossing) (reorder-throws (6hss->sexp \"a\") '(0 2 4 1 3 5) '(0 3 4 1 2 5))"
      ))
   
  
  (define mills-hands ; Without animating hands, this is sorta simple... 2 hands, 
    ; each catches in the middle and throws from one side.
    (list 
     ; "Right" hand
     (list (make-position 0.4 0 1.0) (make-position 0 0 1.0))
     ; "Left" hand
     (list (make-position -0.4 0 1.0) (make-position 0 0 1.0))))
  
  (define 4-man-feed (append (rotate-hands pair-of-hands (* pi 1.14)) (take (juggler-circle 15 6.0) 6)))
  (define back-to-back (append j (translate-hands (rotate-hands j pi) 0 -2.5 0)))
  
  (define hands-examples
    (list "j #;(1 juggler - a pair of hands)"
          "mills-hands #;(An outright cheat since we can't do moving hands yet)"
          "pair-of-jugglers ; r1 l1 r2 l2)"
          "#;(Dropback line) (dropback-line 1 3.0 1.0 #t #t)"
          "#;(Back to back) (append j (translate-hands (rotate-hands j pi) 0 -2.5 0))"
          "#;(Triangle) (juggler-circle 3 2.5)"
          "#;(Star) (juggler-circle 5 3.0)"
          "#;(Star) (rotate-hands (juggler-circle 5 3.0) (/ (* pi 0) 16)) "
          "#;(Dropback ring) (append (juggler-circle 3 2.5) (juggler-circle 3 4.5))"
          "#;(15-jugglers - 3 rings) (append (juggler-circle 5 8.0) (juggler-circle 5 5.0) (juggler-circle 5 10.0))"
          "#;(600 jugglers, 6 rings) (append (juggler-circle 100 60.0) (juggler-circle 100 63.0) (juggler-circle 100 66.0) (juggler-circle 100 51.0)  (juggler-circle 100 54.0) (juggler-circle 100 57.0))"
          "#;(Async gorilla hands) (mangle-hands (juggler-circle 3 3.0) '(3 0 1 2 4 5))"
          "#;(5-man dropback line) (dropback-line 3 3.0 1.0 #t #t)"
          "#;(40-man \"canoe\" (longboat?)) (append (dropback-line 19 3.0 1.0 #t #f) (translate-hands (rotate-hands (dropback-line 19 3.0 1.0 #t #f) pi) 60 3 0))"
          "#;(11-man feed, elevated feeder (feeder last)) (append (take (juggler-circle 30 10.0) 20) (translate-hands (rotate-hands pair-of-hands (* pi 9/7)) 0 0 3.0)))"
          "#;(Wide 4-man feed (feeder first)) (append (rotate-hands pair-of-hands (* pi 1.14)) (take (juggler-circle 15 9.0) 6)))"
          "(append (rotate-hands j (* pi 1.14)) (map list-ref (circular-list (juggler-circle 15 9.0)) '(0 1 4 5)) (rotate-hands (translate-hands j 0 -1.5 0.5) (* pi 1.14)))"
          ))
  
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
  
  (define complete-patterns-internal
    (append
     '(("-- Rhythms --"))
     `(("5 club ultimates" "(4hss->sexp \"744\")" "pair-of-jugglers" 0.16 0.3 4)
       ("5 club flats" "(4hss->sexp \"5\")" "pair-of-jugglers" 0.2 0.6 4)
       ("5 club flurry" "(4hss->sexp \"726\")" "pair-of-jugglers" 0.16 0.25 4)
       ("5 club Whynot?" "(4hss->sexp \"78424\")" "pair-of-jugglers" 0.16 0.25 4)
       ("5 club NotWhy?" "(4hss->sexp \"28474\")" "pair-of-jugglers" 0.16 0.25 4)
       
       ("5 club Ultimate-Zip" "'(((2 1) (3 2) - -) (- - (2 3) (3 0)))" "pair-of-jugglers" 0.32 0.25 4)
       ("2-count" "`(,(sync 4 3 3 even?) ,(sync 4 3 -1 odd?))" "pair-of-jugglers" 0.35 0.3 2)
       ("4-count" "`(,(sync 4 3 3 even?) ,(sync 4 3 -1 odd?) ,(sync 4 3 1 even?) ,(sync 4 3 -1 odd?))" "pair-of-jugglers" 0.35 0.3 2)
       ("3-count" "`(* ,(sync 4 3 3 even?) ,(sync 4 3 -1 odd?) ,(sync 4 3 1 even?))" "pair-of-jugglers" 0.35 0.3 2)
       ("PPS" "`(* ,(sync 4 3 3 even?) ,(sync 4 3 1 odd?) ,(sync 4 3 1 even?))" "pair-of-jugglers" 0.35 0.3 2)
       ("PPSPS" "(passing-ss->sexp \"<3p 3p 3 3p 3|3p 3p 3 3p 3>\")" "pair-of-jugglers" 0.35 0.3 2)
       ("Jim's 3-count (Fast)" "'(* ((3 3) - (3 0) -) ((3 1) - - (4 2 antihurry)) (- (3 0) (2 3 hurry) -) ((3 3) - - (3 1)) (- (4 0 antihurry) - (3 2)) ((2 1 hurry) - (3 3) -))" "pair-of-jugglers" 0.35 0.3 2)
       ("Jim's 3-count (Slow)" "(4hss->sexp \"7746666\")" "pair-of-jugglers" 0.16 0.25 4)
       ("Mild Madness (Fast)" "`(*((3 3) - (3 0) -) ((1 1) (3 2) - (3 1))
           (- (3 0) (2 3 hurry) -) ((3 3) - - (3 1))
           (- (3 2) (3 0) (1 2)) ((2 1 hurry) - (3 3) -))" "pair-of-jugglers" 0.35 0.25 4)
       ("Mild Madness (Slow)" "(4hss->sexp \"7777266\")" "pair-of-jugglers" 0.16 0.25 4)
       ("Jim's 1-count" "'(((4 3 antihurry) - - (2 1 hurry)) 
(- (2 2 hurry) - (4 1 antihurry)) 
(- (4 2 antihurry) (2 0 hurry) -) 
((2 3 hurry) - (4 0 antihurry) -))" "pair-of-jugglers" 0.37 0.25 2)
       ("Martin's Ultimate" "'(* ((3 3) - (3 0) -) ((1 1) (2 2 hurry) - (3 1))
(- (3 2) (3 0) -) ((3 3) - (1 3) (2 1 hurry)))" "pair-of-jugglers" 0.35 0.25 2)
       ("Martin's Ultimate (Slow)" "(4hss->sexp \"77772\")" "pair-of-jugglers" 0.16 0.25 4)
       ("Whynot?" "(4hss->sexp \"86277\")" "pair-of-jugglers" 0.16 0.25 4)
       ("NotWhy?" "(4hss->sexp \"86772\")" "pair-of-jugglers" 0.16 0.25 4)
       ("6 club ultimate-zip, straights (j1)" "'(((1 1) (3 2) - (3 0)) ((3 3) (1 0) (3 1) -))" "pair-of-jugglers" 0.35 0.25 2)
       ("6 club ultimate-zip, crossing (j1 & j2)" "'(((1 1) (3 3) (3 0) (1 2)) ((3 2) (1 0) (1 3) (3 1)))" "pair-of-jugglers" 0.35 0.25 2)
       ("7 club 2-count" "(passing-ss->sexp \"<4p 3|4p 3>\")" "pair-of-jugglers" 0.35 0.3 2)
       ("7 club 2-count crossing" "(passing-ss->sexp \"<4p 3|3 4p>\")" "pair-of-jugglers" 0.35 0.3 2)
       ("7 singles (six-beat)" 
      "'(((11 3) - - -) () (- (10 0)) (- - (11 1)) () (- - - (10 2)))" "pair-of-jugglers" 0.11 0.25 4)
       ("7 ultimates" "(4hss->sexp \"7\")" "pair-of-jugglers" 0.16 0.25 4)
       ("7 club 3-count" "(4hss->sexp \"966\")" "pair-of-jugglers" 0.16 0.25 4)
       ("7 club 4-count" "(passing-ss->sexp \"<5p 3 3 3|3 3 5p 3>\")" "pair-of-jugglers" 0.35 0.3 4)
       ("Oddz Goddz" "'(* ((5 3 antihurry) - (2 3 hurry) -) ((2 1 hurry) - - (5 1 antihurry)))" "pair-of-jugglers" 0.35 0.25 2)
       ("7 club techno (3-count singles)" "'(* ((3 3) (2 0 hurry) - (2 2 hurry)) ((2 1 hurry) - (3 0) (2 2 hurry)))" "pair-of-jugglers" 0.36 0.25 2)
       ("7 club 3-count popcorn (French Techno)" "(4hss->sexp \"786\")" "pair-of-jugglers" 0.16 0.25 4)
       ("7 club PPS 1" "(passing-ss->sexp \"<4p 4p 3|4p 3 3p>\")" "pair-of-jugglers" 0.35 0.28 2)
       ("7 club PPS 2" "(reorder-throws (passing-ss->sexp \"<4p 4p 3|4p 3 3p>\") '(0 1 3 2))"  "pair-of-jugglers" 0.35 0.28 2)
       ("7 club PPS Doubles vs. Singles" "(passing-ss->sexp \"<3p 3p 3|4p 4 4p>\")" "pair-of-jugglers" 0.35 0.28 2)
       
       ("8 club singles" "'(((2 3) (2 0) (2 1) (2 2)))" "pair-of-jugglers" 0.6 0.3 4)
       ("8 club doubles" "`(((7 3) - (7 1) -) (- (5 0) - (5 2)) ())" "pair-of-jugglers" 0.2 0.2 4)
       ("8 club triples" "(passing-ss->sexp \"<5p 3|5p 3>\")" "pair-of-jugglers" 0.35 0.3 4)
       ("8 club ultimates (async)" "(passing-ss->sexp \"<5p 5p|3p 3p>\")" "pair-of-jugglers" 0.35 0.3 4)
       ("8 club ultimates (sync)" "'(((7 3) (7 2) - -) (- - (5 1) (5 0)) ())" "pair-of-jugglers" 0.24 0.3 4)
       ("9 club doubles" "(reorder-throws (4hss->sexp \"b7\") '(0 2 1 3))" "pair-of-jugglers" 0.16 0.25 4)
       ("9 club ultimates" "(4hss->sexp \"9\")" "pair-of-jugglers" 0.16 0.25 4)
       ("9 club ultimates (4/5)" "(passing-ss->sexp \"<4px 4px|5p 5p>\")" "pair-of-jugglers" 0.3 0.25 4)
       ("10 club doubles" "(passing-ss->sexp \"<5p 5|5p 5>\")" "pair-of-jugglers" 0.3 0.25 4))
     
     '(("-- Feeds et cetera --"))
     '(("Martin's Madness" "
`(*
(- (3 3) (3 1) - (3 5) -) 
((3 4) (1 0) - (4 2 antihurry) - (3 0))
((3 1) -  (2 3 hurry) - (4 5 antihurry) -) 
(- (3 3) - (3 0) - (2 4 hurry))
((3 4) - - (3 2) (3 1) -) 
(- (2 0 hurry)  (3 3) - (3 5) -)
)" "4-man-feed" 0.35 0.25 2)
       
       ("Martin's Mildness" "
`(*
((3 3) - (3 0) - (3 5) -) 
((1 1) (3 4) - (4 2 antihurry) - (3 1))
(- (3 0) (2 3 hurry) - (4 5 antihurry) -) 
((3 3) - - (3 1) - (2 4 hurry))
(- (3 4) - (3 2) (3 0) -) 
((2 1 hurry) - (3 3) - (3 5) -)
)" "4-man-feed" 0.35 0.25 2)
       ("PPS Feed" "'(* ((3 3) - (3 1) - (3 5) -) (- (3 4) - (3 2) - (3 0)) ((3 1) - (3 3) - (3 5) -))" "4-man-feed" 0.35 0.25 2)
       ("747 Feed" "747-feed" "4-man-feed"  0.16 0.25 4)
       ("744 vs 747 Feed" "777-747-744-feed" "4-man-feed" 0.16 0.25 4)
       ("7 vs 966 Feed" "777-feed-3s" "4-man-feed" 0.16 0.25 4)
       ("10 Club Gorilla" "(reorder-throws (6hss->sexp \"a\") '(0 1 5 2 3 4))" "4-man-feed" 0.11 0.25 6)
       ("10 Club Defensive Gorilla" "(reorder-throws (6hss->sexp \"a\") '(0 1 4 3 2 5))" "4-man-feed" 0.11 0.25 6)
       ("Split Gorilla" "'(
((5 5) (5 2) - - - - - -) 
(- - - (5 1) (7 5) - (5 0)) 
(- - (5 3) - - (5 4) - (5 6))
((5 7) (5 2) - - - - - -) 
(- - - (5 1) (5 0) - (7 7)) 
(- - (5 3) - - (5 4) - (5 6)))" "4-man-feed" 0.22 0.25 3)
       
       ("10-club feed as 6-hand siteswap" "(6hss->sexp \"9d9 99b 9b9 d99\")" "(juggler-circle 3 2.5)" 0.11 0.25 6)
       ("Complete PPS Feed" "`(* 
((3 3) - (3 1) - (3 7) - (3 5) -) 
(- (3 6) - (3 2) - (3 4) - (3 0)) 
((3 1) - (3 5) - (3 3) - (3 7) -)
)" "(append pair-of-jugglers
  (translate-hands pair-of-jugglers 4.0 0 0))" 0.35 0.25 2)
       ("Complete Madness"
        "`(*
((3 3) -       (3 0) -       (3 7) (1 4) (3 4) -) 
((1 1) (3 6) - (2 2 hurry)      (3 5) -        - (3 1)) 
(- (3 0)       (3 4) -       - (3 2)        (2 7 hurry) -)
((3 3) -       (1 3) (3 1) (3 7) -       - (3 5)) 
(- (3 6)       - (3 2)       - (2 4 hurry)        (3 0) (1 6)) 
((2 1 hurry) -       (3 4) -       (3 3) -        (3 7) -) 
)"  "(append pair-of-jugglers
  (translate-hands pair-of-jugglers 4.0 0 0))" 0.35 0.2 2)
     
     ("6-man Clock"
      "`( 
((3 1) - (3 11) - (3 9) - (3 7) - (3 5) - (3 3) -)
,(sync 12 3 -1 odd?)
((3 11) - (3 9) - (3 7) - (3 5) - (3 3) - (3 1) - )
,(sync 12 3 -1 odd?)
((3 9) - (3 7) - (3 5) - (3 3) - (3 1) - (3 11) -)
,(sync 12 3 -1 odd?)
((3 7) - (3 5) - (3 3) - (3 1) - (3 11) - (3 9) -)
,(sync 12 3 -1 odd?)
((3 5) - (3 3) - (3 1) - (3 11) - (3 9) - (3 7) -)
,(sync 12 3 -1 odd?)
((3 3) - (3 1) - (3 11) - (3 9) - (3 7) - (3 5) -)
,(sync 12 3 -1 odd?)
)"
      "(juggler-circle 6 3.0)" 0.35 0.25 2)
     ("6-count feed" "(passing-ss->sexp \"<3p2 3 3p3 3 3p4 3|3p1 3 3 3 3 3|3 3 3p1 3 3 3|3 3 3 3 3p1 3>\")"  "(append
  pair-of-hands
  (translate-hands
    (rotate-hands 
       (juggler-line 3 2.5 0)
pi)
2.5 5 0)
)" 0.35 0.25 2)
     
     ("Feed weave" "(passing-ss->sexp \"<3p2 3 3p3 3 3p4 3|3p1 3 3 3 3 3|3 3 3p1 3 3 3|3 3 3 3 3p1 3>\")"
     "(append
  pair-of-hands
(pair-of-hands-1-segment 0 9.6 (lambda (t) 
(translate-hands (rotate-hands  pair-of-hands  pi)
(* 2.5 (sin (* (/ (* 2 pi) 9.6) t))) 
                                           (+ 5 (* 1.3 (sin (* (/ (* 4 pi) 9.6) t))))
                                           0)))
(pair-of-hands-1-segment 6.4 9.6 (lambda (t) 
(translate-hands (rotate-hands pair-of-hands pi)
(* 2.5 (sin (* (/ (* 2 pi) 9.6) t))) 
                                           (+ 5 (* 1.3 (sin (* (/ (* 4 pi) 9.6) t))))
                                           0)))
(pair-of-hands-1-segment 3.2 9.6 (lambda (t) 
(translate-hands (rotate-hands pair-of-hands pi)
(* 2.5 (sin (* (/ (* 2 pi) 9.6) t))) 
                                           (+ 5 (* 1.3 (sin (* (/ (* 4 pi) 9.6) t))))
                                           0)))
)" 0.4 0.45 2)
     
     ("2-count Mass Hysteria" "(passing-ss->sexp \"<
3p3 3 3p5 3 3p5 3 3p4 3 3p4 3 3p3 3|
3p5 3 3p4 3 3p4 3 3p3 3 3p3 3 3p5 3|
3p1 3 3p1 3 3p2 3 3p2 3 3p5 3 3p4 3|
3p2 3 3p2 3 3p3 3 3p5 3 3p1 3 3p1 3|
3p4 3 3p3 3 3p1 3 3p1 3 3p2 3 3p2 3>\"))"
     "(append
  pair-of-hands
(translate-hands (rotate-hands pair-of-hands pi) 0 10.0 0)
(pair-of-hands-1-segment 0 9.6 (lambda (t) 
(translate-hands (rotate-hands  pair-of-hands  pi)
(* 2.5 (sin (* (/ (* 2 pi) 9.6) t))) 
                                           (+ 5 (* 1.3 (sin (* (/ (* 4 pi) 9.6) t))))
                                           0)))
(pair-of-hands-1-segment 6.4 9.6 (lambda (t) 
(translate-hands (rotate-hands pair-of-hands pi)
(* 2.5 (sin (* (/ (* 2 pi) 9.6) t))) 
                                           (+ 5 (* 1.3 (sin (* (/ (* 4 pi) 9.6) t))))
                                           0)))
(pair-of-hands-1-segment 3.2 9.6 (lambda (t) 
(translate-hands (rotate-hands pair-of-hands pi)
(* 2.5 (sin (* (/ (* 2 pi) 9.6) t))) 
                                           (+ 5 (* 1.3 (sin (* (/ (* 4 pi) 9.6) t))))
                                           0)))
)" 0.4 0.45 2)
     )
     
     '(("-- Dropbacks --"))
     
     '(
       ("9-club line" "`(,(sync 6 3 3 even?) ,(sync 6 3 -1 odd?))" 
                     "(dropback-line 1 3.0 1.0 #t #t)"
                     0.35 0.3 2)
       
       ("5 man line, 5p 3" "(list (sync 10 5 3 even?) (sync 10 3 -1 odd?))" 
                     "(dropback-line 3 3.0 1.0 #t #t)"
                     0.35 0.3 2)
       
       ("9-club line (aa7999)" "(6hss->sexp \"aa7999\")" 
                     "(dropback-line 1 3.0 1.0 #t #t)"
                     0.13 0.25 2)
       
       ("10-club line (d999aa)" "(reorder-throws (6hss->sexp \"d999aa\") '(0 1 3 2 5 4))"
                     "(dropback-line 1 3.0 1.0 #t #t)"
                     0.13 0.25 2)
       
       ("3 man 8-club ultimate line" "(reorder-throws (6hss->sexp \"8\") '(4 5 2 3 0 1))" 
                     "(dropback-line 1 3.0 0.0 #t #t)"
                     0.15 0.3 2)
       
       ("3 man 10-club ultimate line" "(6hss->sexp \"a\")" 
                     "(dropback-line 1 3.0 0.0 #t #t)"
                     0.13 0.25 2)
       
       ("Canoe"  "#;((list (sync 8 5 3 even?) '() (sync 8 4 -1 odd?))) `(((5 3) - (5 5 antihurry) - (5 7) - (5 1 antihurry) -) () (- (4 0) - (4 2) - (4 4) - (4 6)))"
                        "(append (dropback-line 1 3.0 1.0 #t #f) (translate-hands (rotate-hands (dropback-line 1 3.0 1.0 #t #f) pi) 7 3 0))"
                        0.27 0.3 2)
       )
       
     '(("-- Large Patterns --"))
     
     '(("40 man canoe"  "#;(40-man 5p 3) (list (sync 80 5 3 even?) (sync 80 3 -1 odd?))"
                        "#;(40-man \"canoe\" (longboat?)) (append (dropback-line 19 3.0 1.0 #t #f) (translate-hands (rotate-hands (dropback-line 19 3.0 1.0 #t #f) pi) 60 3 0))"
                        0.35 0.3 2)
       ("15 man 3-count" "#;(15 juggler 3-count - traveling props) (list (sync 30 3 1 even?) (sync 30 3 -1 odd?) (sync 30 3 9 even?) (sync 30 3 -1 odd?) (sync 30 3 1 even?)  (sync 30 3 19 odd?))"
                         "#;(15-juggler ring) (juggler-circle 15 7.0)" 0.35 0.3 2)
       ("15 man fantasy?" "#;(15 juggler nonsense) (list (sync 30 5 1 even?) (sync 30 5 -1 odd?) (sync 30 11 9 even?) (sync 30 5 -1 odd?) (sync 30 5 1 even?)  (sync 30 11 19 odd?))"
                         "#;(15-juggler ring) (juggler-circle 15 7.0)" 0.25 0.2 2)
       ("11 man 55-club typewriter feed + pedestal" 
      "#;(5-club feed, 10 feedees) (typewriter-feed 11 2 5)"
      #;(11-man feed, elevated feeder (feeder last)) 
                                                         "(append (take (juggler-circle 30 10.0) 20) (translate-hands (rotate-hands pair-of-hands (* pi 9/7)) 0 0 3.0)))" 0.25 0.2 2)
       ("18-man typewriter feed" 
      "(typewriter-feed 19 2 5)"
      #;(18-man typewriter feed, elevated feeder (feeder last)) 
      "(append (take (juggler-circle 16 5.0) 8) 
              (take (juggler-circle 23 8.0) 12) 
              (take (juggler-circle 30 11.0) 16) 
              (translate-hands (rotate-hands pair-of-hands (* pi 9/7)) 0 0 3.0)))" 0.25 0.2 2))
     
     '(("-- 4-hand siteswaps --"))
     (map (λ (ss)
           (list ss (format "(4hss->sexp \"~a\")" ss) "pair-of-jugglers" 0.16 0.25 4)) 4-hand-examples)
     (map (λ (ss)
           (list (format "~a (RLRL)" ss) (format "(4h-rlrl (4hss->sexp \"~a\"))" ss) "pair-of-jugglers" 0.16 0.25 4)) 4-hand-examples)
     '(("-- 6-hand siteswaps --"))
     (map (λ (ss)
           (list ss (format "(6hss->sexp \"~a\")" ss) "(juggler-circle 3 2.5)" 0.11 0.25 6)) 6-ss-examples)
     (map (λ (ss)
           (list (format "~a (J2 reversed)" ss) (format "(6h-reverse-j2 (6hss->sexp \"~a\"))" ss) "(juggler-circle 3 2.5)" 0.11 0.25 6)) 6-ss-examples)
     
     '(("== Boring Stuff ==")) ; "Boring Stuff..."
     ; 2-hand...
     '(("-- 2-hand siteswaps --"))
     (map (λ (ss)
           (list ss (format "(2hss->sexp \"~a\")" ss) "pair-of-hands" 0.25 0.2 2)) 2-ss-examples)
     )
    )
  
  )