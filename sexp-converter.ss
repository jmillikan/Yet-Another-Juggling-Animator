(module sexp-converter scheme
  (require srfi/1)
  (require "juggling-core.ss")
  
  (provide 
   sexp->pattern
   )
  
  ; (list '- '* '* '() '-)
  ; (delay next-throw last-throw throw-list-backwards first-hand)
  
  (define (replace-item list elem new-elem)
    (cond
      ((null? list) '())
      ((equal? (car list) elem) (cons new-elem list))
      (#t (cons (car list) (replace-item (cdr list) elem new-elem)))))                  
  
  ; UUUUUUGH, TODO: LAST THROW MUST MATCH HAND THROWN FROM. ID WON'T WORK.
  ; This function *did* get to long... Let's see, what can I exorcise to somewhere else?
  
  ; dwell-value =< beat-value
  ; (length hands-lst) >= all (length (car sexp-pattern)), etc.
  
  (define (has? lst-or-atom mem)
    (or (equal? lst-or-atom mem)
        (and (list? lst-or-atom)
             (member mem lst-or-atom))))
  
  (define (sexp->pattern sexp-pattern beat-value dwell-value hands-lst)   
    (sexp->pattern-internal 
     (cond ((has? (car sexp-pattern) '*)
            (append (cdr sexp-pattern) (star-pattern (cdr sexp-pattern))))              
           ((has? (car sexp-pattern) '**)
            (append (cdr sexp-pattern) (twostar-pattern (cdr sexp-pattern))))
           ((has? (car sexp-pattern) '***) ; "This is incredibly serious business."
            (append (cdr sexp-pattern) (star-pattern (twostar-pattern (cdr sexp-pattern)))))
           (#t sexp-pattern))
     beat-value dwell-value hands-lst))          
  
  ; Double up the pattern with all throws in and going to opposite hands - hand n + 1 % 2 in multi-hand setups
  ; This may result in flagrant errors if there are an odd number of manipulators.
  (define (star-pattern p)
    (map star-beat p))
  
  (define (star-beat b)
    (match b ((list-rest right-throw left-throw rest)
              (cons (star-throw left-throw) (cons (star-throw right-throw) (star-beat rest))))
      (odd-or-none odd-or-none)))
  
  (define (star-throw t)
    (match t ((list-rest height hand rest)              ; There isn't a "rest" here yet, but that's probably where throw decorations will go if they ever happen.
              (cons height (cons 
                            (if (even? hand)
                                (add1 hand)
                                (sub1 hand)) rest)))
      (rest rest)))
  
  ; Like star-pattern, except it swaps the places of jugglers one and two (and any subsequent pairs) instead of the hands of juggler one.
  (define (twostar-pattern p)
    (map twostar-beat p))
  
  (define (twostar-beat b)
    (match b ((list-rest r1-throw l1-throw r2-throw l2-throw rest)
              (append (map twostar-throw (list r2-throw l2-throw r1-throw l1-throw)) (twostar-beat rest)))
      (rest rest))) ; No or too few hands left.
  
  (define (twostar-throw t)
    (match t ((list-rest height hand rest)     
              (cons height (cons
                            (+ (remainder (+ hand 2) 4) (floor (/ hand 4))) rest)))
      (three-or-less-hands three-or-less-hands)))
  
  (define (sexp->pattern-internal sexp-pattern beat-value dwell-value hands-lst)    
    (define (build-segments current-hand current-throw)
             (let
                       ((throw-hand (list-ref hands-lst current-hand))
                        (catch-hand (list-ref hands-lst (cadr current-throw))))
               (cond ((and (= (car current-throw) 2 #;(length (car sexp-pattern))) ; Long dwell on at least SOME 2s. Will still look a bit funky.
                               
                                 (= current-hand (cadr current-throw))) 
                            (begin 
                              (list (dwell-hold-path-segment (* beat-value (car current-throw)) throw-hand))))
                           (#t
                            (let ((orientation 
                                   (if (= (floor (/ current-hand 2)) (floor (/ (cadr current-throw) 2))) 
                                       'perpendicular ; pass! Paralell to throw line
                                       'parallel ; Hand to hand - perpendicular to throw line.
                                       )))
                              (list
                               ; 1st dwell segment comes right after...
                               (dwell-hold-path-segment dwell-value catch-hand
                                                        (list 'orientation
                                     
                                      orientation))
                               ; 1st toss segment
                               (ball-toss-path-segment 
                                (- (* beat-value (car current-throw)) dwell-value) 
                                throw-hand catch-hand
                                (list 'orientation
                                     
                                      orientation))))))))
    
  ; There is funky, heavy duplication between this and update-object
    (define (start-object objects starting-object current-throw time current-hand)
      (let ((new-object
             (list time ; delay
                   current-throw ; next-throw, counted down
                   current-throw ; last throw, tested with eq? for identity
                   (build-segments current-hand current-throw)
                   current-hand ; first hand, for use later
                   )))
        (values #f (replace eq? objects starting-object new-object))))    
    
    (define (update-object objects object-matching-throw current-throw time current-hand)
      (match object-matching-throw 
        ((list delay next-throw last-throw throw-list first-hand)
         (if (eq? last-throw current-throw) ; circular list, so ID works... We just have to store the exact throw.                
             ; Around to the start. Don't add this throw, it matches the 1st one.
             (values #t (replace eq? objects object-matching-throw 
                                 (list delay '- last-throw throw-list first-hand)))
             ; Not the last throw. Add dwell and toss backwards, update next throw           
             (values #f 
                     (replace 
                      eq? objects object-matching-throw                              
                      (list delay current-throw last-throw 
                            (append (build-segments current-hand current-throw) throw-list) first-hand)))))))
    
    (make-pattern
     ; Do something uncomplicated with the results of this horrible recursion...
     (map
      (lambda (o)
        (match o ((list delay _ first-throw throw-lst first-hand )
                  (let ((start-hand (list-ref hands-lst first-hand)))
                     (make-path-state 0 (cons ; Initial dwell hold runs for the whole delay, which could be very slow and hilariously trippy in some cases.
                                         ; These dwell holds are in the wrong hand, they're in the destination hand...
                                         ; I didn't save the starting hand >_<
                                         (dwell-hold-path-segment (* delay beat-value) start-hand)
                                         (apply circular-list (reverse throw-lst))))))))
      ; horrible recursion...
      (let* ((c-pattern (apply circular-list sexp-pattern))
             (number-of-hands (length (car c-pattern)))
             (number-of-objects (ss-value sexp-pattern)))
        
        (let loop-throws ((time 0)
                          (unfinished-objects number-of-objects)
                          (c-pattern c-pattern)
                          (objects (make-list number-of-objects (list '- '* '* '() '-)))
                          (throws-this-beat (car c-pattern))
                          (hand 0))              
          (cond ((= unfinished-objects 0) ; Done: Return objects, which includes initial delay and throw lists
                 objects)
                ((null? throws-this-beat) ; Out of throw - move to next beat
                 (loop-throws (add1 time) unfinished-objects (cdr c-pattern) (advance-beat objects) (cadr c-pattern) 0))
                (#t
                 ; Process one throw against all objects.                     
                 (let* ((objects-matching-throw (match-throw objects hand)) ; Search for all objects that are up (delay of 0)
                        (unstarted-objects (match-waiting objects))                             
                        (current-throw (car throws-this-beat))
                        (value (throw-value current-throw)))
                   (cond ((> (length objects-matching-throw) 1) ; No multiplexes for now. :(
                          'flagrant-error)
                         ((null? objects-matching-throw) ; No object expecting a throw
                          (if (not (eq? value '-)) 
                              ; There's a throw that no object expected.
                              (if (null? unstarted-objects)
                                  ; and no object is waiting to be started...
                                  
                                  ; This COULD be a throw past the end of an object's cycle. Ugh.
                                  ; I'd like to be able to throw errors here, but I'm pretty sure that the error would show up elsewhere eventually.
                                  (loop-throws time unfinished-objects c-pattern objects (cdr throws-this-beat) (add1 hand))
                                  ; ...or start first available object.
                                  ; objects starting-object current-throw time beat-value current-hand hand-lst
                                  (let-values ((( finished updated-objects) 
                                                (start-object objects (car unstarted-objects) current-throw time hand)))
                                    (loop-throws time 
                                                 (if finished (sub1 unfinished-objects) unfinished-objects)                                                                     c-pattern 
                                                 updated-objects
                                                 (cdr throws-this-beat) 
                                                 (add1 hand))))
                              
                              ; Throw not expected by any object. Continue with next hand.
                              (loop-throws time unfinished-objects c-pattern objects (cdr throws-this-beat) (add1 hand))))
                         (#t                              
                          ; Exactly one object expecting this throw.
                          (if (eq? value '-) 
                              ; There isn't a throw for the object expecting it...
                              'flagrant-error                                  
                              ; Non-zero throw value for matched object. Plenium.
                              (let-values (((finished updated-objects ) (update-object objects (car objects-matching-throw) current-throw time hand)))
                                (loop-throws time 
                                             (if finished (sub1 unfinished-objects) unfinished-objects)                                               
                                             c-pattern 
                                             updated-objects
                                             (cdr throws-this-beat) 
                                             (add1 hand))))))))))))))
  
  (define (throw-value throw)
    (match throw 
      ((list value hand) value)
      ('- '-)
      ('* '-)))
  
  (define (advance-beat objects)
    (map 
     (lambda (o) (match o ((list delay next-throw last-throw throw-list first-hand)
                           (list 
                            delay
                            (if (list? next-throw) (list (sub1 (car next-throw)) (cadr next-throw)) next-throw)
                            last-throw
                            throw-list
                            first-hand))))                     
     objects))                     
  
  (define (match-throw objects pattern-hand)
    (filter
     (lambda (o) (match o 
                   ((list _ (list countdown object-hand)  _ _ _)
                    (and (= countdown 0) (= pattern-hand object-hand)))
                   (_ #f)))
     objects))
  
  (define (match-waiting objects)
    (filter
     (lambda (o) (match o ((list _ '* _ _ _) #t) (_ #f)))
     objects))
  
  (define (ss-value sexp-pattern)
    (/ (foldr + 0 (map car (filter pair? (apply append sexp-pattern)))) (length sexp-pattern)))
  ; buh.
  (define (replace pred lst old-elem new-elem)
    (cond
      ((null? list) '())
      ((pred old-elem (car lst)) (cons new-elem (cdr lst)))
      (#t (cons (car lst) (replace pred (cdr lst) old-elem new-elem))))))









