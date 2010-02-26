(module example-patterns scheme
  (require "juggling-core.ss" srfi/1)
  
  (provide (all-defined-out))
  
  (define 4-hand-examples
    '("966" ; 7-club 3-count
      "996" ; 8-club pps
      "9629669669969929" ; Copenhagen countdown
      "86277" "86727" ; Whynots
      "5" "7" "9" "b" 
      "b97" "db97" ; Some scary theoretical ultimates patterns
      "7966" ; A staggered 7-club 2-count
      "75666" ; Cool pattern from Madison, spotted at Madfest 2010
      "747" ; pass-pass-hold (2/3 of 7 ultimates)
      "747b47747707" "7b7740747747"  ; Early and late triples
      "945747747" ; double-joe in 747
      "794646" "7946466" ; 6-club oddities showing hte matching "signature" of 744 and 966
      "b784847" "b7848477777" ; an ill-concieved 7 club popcorn
            ))
  
  ; What's the fun of having a million of these?
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
      "#;(Some 3/4 man 747-ish feeds) 747-feed"
      "777-feed"
      "777-747-744-feed "
      "777-feed-3s"
      "777-feed-3s-vs-744"
      "777-feed-3s-vs-takeouts"
      ))
   
  
  (define mills-hands ; Without animating hands, this is sorta simple... 2 hands, 
    ; each catches in the middle and throws from one side.
    (list 
     ; "Right" hand
     (list (make-position 0.4 0 1.0) (make-position 0 0 1.0))
     ; "Left" hand
     (list (make-position -0.4 0 1.0) (make-position 0 0 1.0))))
  
  (define hands-examples
    (list "j #;(1 juggler - a pair of hands)"
          "mills-hands #;(An outright cheat since we can't do moving hands yet)"
          "pair-of-jugglers ; r1 l1 r2 l2)"
          "#;(Dropback line) (dropback-line 1 3.0 #t #t)"
          "#;(Back to back) (append j (translate-hands (rotate-hands j pi) 0 -2.5 0))"
          "#;(Triangle) (juggler-circle 3 2.5)"
          "#;(Star) (juggler-circle 5 3.0)"
          "#;(Star) (rotate-hands (juggler-circle 5 3.0) (/ (* pi 0) 16)) "
          "#;(Dropback ring) (append (juggler-circle 3 2.5) (juggler-circle 3 4.5))"
          "#;(15-juggler ring) (juggler-circle 15 8.0)"
          "#;(15-jugglers - 3 rings) (append (juggler-circle 5 8.0) (juggler-circle 5 5.0) (juggler-circle 5 10.0))"
          "#;(600 jugglers, 6 rings) (append (juggler-circle 100 60.0) (juggler-circle 100 63.0) (juggler-circle 100 66.0) (juggler-circle 100 51.0)  (juggler-circle 100 54.0) (juggler-circle 100 57.0))"
          "#;(Async gorilla hands) (mangle-hands (juggler-circle 3 3.0) '(3 0 1 2 4 5))"
          "#;(5-man dropback line) (dropback-line 3 3.0 #t #t)"
          "#;(40-man \"canoe\" (longboat?)) (append (dropback-line 19 3.0 #t #f) (translate-hands (rotate-hands (dropback-line 19 3.0 #t #f) pi) 60 3 0))"
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
  
  )