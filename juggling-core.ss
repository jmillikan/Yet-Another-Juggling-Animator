(module juggling-core scheme
  
  (require srfi/1)
  ; The first goal here is to get basic ball patterns animating ASAP. This means skipping interpreters for high-level languages (MHN, beatmap, Causals... JuggleML... Seems like there's one I'm not thinking of. It probably isn't too important.)
  
  ; Details for how to animate hands and figures might come later. Allowances are made in the granularity of the data.
  
  ; Innermost stored representation of a single part of a ball path, e.g. the air segment of a toss, the hand motion to a throw, etc. - [0, t] is the domain for a parametric function giving position x,y,z (and potentially rotation in the future).
  ; This "should" allow floor bounces, wall bounces, items with different drags... 
  
  (define-struct position
    (x y z)
    #:transparent)
  
  (define-struct rotation
    (x y z)
    #:transparent)
  
  ; Also the lowest-level language it will possible to accept for patterns, probably via read or eval. (Free config language!)
  (define-struct path-segment
    (duration pos-fn)
    #:transparent)
  
  ; The entire path of a ball will thus be stored as a circular list of path-segments with a possible time offset into the first path offset. After that, the only liability will be eventual loss of accuracy due to floating point arithmetic... 
  ; The current position in each path should be storable in an identical way. So in fact, this is just an initial position.
  (define-struct path-state
    (t-offset segment-list)
    #:transparent
    #:mutable)
  
  ; A pattern will just be a list of starting positions, then, I guess...
  (define-struct pattern
    (positions)
    #:transparent)
  
  (define-struct hand
    (throw-position
     catch-position
     facing) ; radians - way the hand points when held vaguely straight out from the body, so that there's a 100 to 150-odd degree arc of possible throws
    #:transparent)
  
  (define (rotate p angle)
    (match-let (((struct position (x y z)) p))
      (make-position
       (+ (* x (cos (- angle))) (* y (sin angle)))
       (+ (* y (cos angle)) (* x (sin (- angle))))
       z)))
  
  (define (rotate-hands hands angle)
    (map (λ (h) 
           (match-let (((struct hand (p1 p2 a)) h))
             (make-hand (rotate p1 angle) (rotate p2 angle) (+ angle a)))) hands))
  
  (define (translate p x y z)
    (match-let (((struct position (x1 y1 z1)) p))
      (make-position
       (+ x x1)
       (+ y y1)
       (+ z z1))))
  
  (define (translate-hands hands x y z)
    (map (λ (h) 
           (match-let (((struct hand (p1 p2 a)) h))           
             (make-hand (translate p1 x y z) (translate p2 x y z) a))) hands))

  ;At a higher level, these might be represented abstract to allow speeding up and slowing down the pattern. (Slowing and speeding time itself should still be possible during arcs.)
  
  ; Unit is assumed to be the meter, time is seconds, gravity pulls along z.
  
  ; Functions for creating toss and dwell segments for any normal ball-throwing pattern
  ; Closures should be way more than fast enough under a few hundred fps and a few dozen objects. Massive speedup would be... Storing data separately from fn.
  
  (define (option options name)
    (if (assoc name options)
        (cadr (assoc name options))
        'default))
  
  ; Todo: Fix rotation for clubs^Wrings
  (define (ball-toss-path-segment tf h1 h2 . options)
    (match-let* (((struct hand (p1 _ a1)) h1)
                 ((struct hand (_ p2 a2)) h2)
                 (a1-deg (radians->degrees a1))
                 (a2-deg (radians->degrees a2))
                 (spins
                  (cond ((< tf 1.1) 1)
                        ((< tf 1.6) 2)
                        ((< tf 2.1) 3)
                        (#t 4)))
                 
                 ; raise the catch point a bit for passes...
                 (catch-offset (match (option options 'orientation)
                                 ('parallel 0.8)
                                 ('perpendicular 0.1)
                                 ('default 0.0)))
                 (throw-offset (match (option options 'orientation)
                                 ('parallel -0.3)
                                 ('perpendicular -0.1)
                                 ('default 0.0))))
      (make-path-segment 
       tf
       ; Solve for parametric function p(t) with gravity pulling negative on z
       ; See page of handwritten math that really ought to be scanned in.
       ; With points p1 and p2, x(t) and y(t) are linear -
       ; x(t) = (x1/(x2 - tf))t + x1
       ; y(t) = (y1/(y2 - tf))t + y1
       ; z(t) = -9t^2 + mz t + z1 where
       ;  mz = (z2/tf) + (-z1/tf) + 9tf  (Not too sure about this one.)
       (if (= tf 0) (lambda (t) p1)  
           (match (cons p1 p2)
             ((cons (struct position (x1 y1 z1)) (struct position (x2 y2 z2)))
              (let* ((mx (/ (- x2 x1) tf))
                     (bx x1)
                     (my (/ (- y2 y1) tf))
                     (by y1)
                    
                     (mz (+ (/ (+ z2 catch-offset) tf) (/ (- (+ z1 throw-offset)) tf) (* 9.8 tf)))
                     (bz (+ z1 throw-offset))
                     
                     (y-rot
                      (match (option options 'orientation)
                        ('parallel (get-angle (- x1 x2) (- y1 y2)))
                        ('perpendicular a2-deg)
                        ('default (+ 90 (get-angle (- x1 x2) (- y1 y2)))))))
                (lambda (t)         
                  (values 
                   (make-position 
                    (+ (* mx t) bx)
                    (+ (* my t) by)
                    (+ (* -9.8 t t) (* mz t) bz))
                   
                   ; Ugh. Since I'm bad at math, we need two different rotations here. (One to orient it "straight" and another for the spin.)
                   (make-rotation -90 y-rot -90)
                   (make-rotation 0 0 (+
                                       (match (option options 'orientation)
                                         ('parallel 60)
                                         ('perpendicular 0)
                                         ('default 0))
                                       ; The actual spin as the club moves along.
                                       
                                       (- (* t (/ 
                                                (+ 
                                                 ; This branch adds the "extra" spin that brings passes perpendicular
                                                 ; to the ground
                                                 (match (option options 'orientation)
                                                   ('parallel 150) ; 90 degrees from ground + 60 offset from above
                                                   ('perpendicular 10)
                                                   ('default 10))  
                                                 (* spins 360)) 
                                             tf))))))))))))))
    
  (define (radians->degrees a)
    (* 57.2957795 a))
  ; Ugh, don't ask. My geometry is bad.
  (define (get-angle x y)
    (- 180 ; It's not just about living forever, Jackie. The trick is living with yourself forever.
     (if (zero? x) 0 ; doesn't matter, I think
         (let*         
             ((a (atan (/ y x)))
              (a-deg (* 57.2957795 a)))
           (cond ((and (positive? x) (positive? y)) a-deg)
                 ((and (positive? y)) (+ 180 a-deg))
                 ((and (positive? x)) (+ 360 a-deg))
                 (#t (+ 180 a-deg)))))))
  
  ; Ignoring dwell holds that are largely travelling vertically for the moment, 
  ; dwell holds are basically moving from (x1, y1, c) to (x2, y2, c) and we want
  ; to orient the prop perpendicular to the line ((x1, y1), (x2, y2))
  
  ; After flipping it up around the x axis, it should be perpendicular to the y axis
  ; ((0,0),(0,1)) so we add 90 degrees to the angle we get from x1 - x2 and y1 - y2 (or vice versa)
  ; Gosh, this sucks... 
  
  ; In order to have the correct rotation for rings, 
  ; what we  really need to know is the orientation of the hands. >_<
  
  ; Options are an alist, I guess  
  
  (define (dwell-hold-path-segment tf h1 . options)
    (match-let* (((struct hand (p2 p1 a1)) h1)
                 (a1-deg (radians->degrees a1)))
      (make-path-segment
       tf
       ; On this one... I'm just going to go linearly for now.
       ; A better future plan might be an approximation of the direction of the last segment
       ; x(t) = (x1/(x2 - tf))t + x1
       ; y(t) = (y1/(y2 - tf))t + y1
       ; z(t) = (z1/(z2 - tf))t + z1
       
       (if (= tf 0) (lambda args p1) ; Special case for tf = 0, which causes murder later.
           (begin 
             (match (cons p1 p2) 
               
               ((cons (struct position (x1 y1 z1)) (struct position (x2 y2 z2)))
                (let* (
                       (catch-offset (match (option options 'orientation)
                                       ('parallel 0.8)
                                       ('perpendicular 0.1)
                                       ('default 0.0)))
                       (mx (/ (- x2 x1) tf))
                       (bx x1)
                       (my (/ (- y2 y1) tf))
                       (by y1)
                       (mz (/ (- z2 (+ z1 catch-offset)) tf))
                       (bz (+ z1 catch-offset))
                       
                       (y-rot a1-deg #;(match (option options 'orientation)
                                ('parallel (get-angle (- x1 x2) (- y1 y2)))
                                ('perpendicular a1-deg)
                                ('default a1-deg))))                           
                  (lambda (t)        
                    (values
                     (make-position 
                      (+ (* mx t) bx)
                      (+ (* my t) by)
                      (+ (* mz t) bz))
                     
                     (make-rotation -90 y-rot -90)
                     (make-rotation 0 0 
                                    (match (option options 'orientation)
                                      ; Get from upright to 10 degrees below
                                      ('parallel (+ -90 (* t (/ 100 tf))))
                                      ('perpendicular (+ -10 (* t (/ 10 tf))))
                                      ('default (+ -10 (* t (/ 10 tf)))))
                                    )))))))))))
  
  ; Advance a path-state by t-tick
  (define (advance-path-state! ps t-tick)
    ; Bluh. Starting to wish I'd done this a simpler way.
    (match ps ((struct path-state (t-offset seg-lst)) ; path-state we are advancing
               (match (car seg-lst) ((struct path-segment (duration _)) ; Current segment
                                     (if (< (+ t-offset t-tick) duration)
                                         ; Still in the same segment - add t-tick to t-offset
                                         (set-path-state-t-offset! ps (+ t-offset t-tick))
                                         ; Not in the same segment. shave remaining time off the tick and start the next segment.
                                         (let ((t-leftover (- t-tick (- duration t-offset))))
                                           (set-path-state-t-offset! ps 0)
                                           (set-path-state-segment-list! ps (cdr seg-lst))
                                           (advance-path-state! ps t-leftover))))))))
  
  (define (advance-pattern! patt t-tick)
    (map (lambda (path-seg) (advance-path-state! path-seg t-tick)) (pattern-positions patt)))
  
  ; Export the examination of the pattern (instead of exporting all of the data structures)
  ; Apply f to each object, giving object & position
  ; f is (f object position)
  (define (map-pattern f pattern objects)
    (map (lambda (ps o)
           (match ps ((struct path-state (t (cons (struct path-segment (_ pos-f)) _)))
                      (let-values (((pos rot spin) (pos-f t)))
                        (f o pos rot spin)))))
         (pattern-positions pattern) objects))
  
  (provide 
   (struct-out position)
   (struct-out rotation)
   (struct-out pattern)
   (struct-out path-segment)
   (struct-out path-state)
   (struct-out hand)
   ; todo: Figure out how to export less?
   
   ball-toss-path-segment dwell-hold-path-segment 
   advance-path-state! advance-pattern! map-pattern
   
   rotate translate
   rotate-hands translate-hands
   ))