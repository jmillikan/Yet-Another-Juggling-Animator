(module juggling-core scheme
  
  (require srfi/1)
  
  ; Details for how to animate hands and figures might come later. Allowances are made in the granularity of the data.
  
  ; Innermost stored representation of a single part of a ball path, e.g. the air segment of a toss, the hand motion to a throw, etc. - [0, t] is the domain for a parametric function giving position x,y,z and two rotations that are applied in order r1x r1y r1z r2x r2y r2z because I'm bad at math.
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
  
  ; In degrees >_<
  (define (angle-diff a1 a2)
    (min (abs (- a1 a2))
         (abs (- (+ 360 (min a1 a2)) (max a1 a2)))))
  
  ; Todo: Fix rotation for clubs^Wrings^Wclub backdrops
  (define (ball-toss-path-segment tf h1 h2 . options)
    (match-let* (((struct hand ((struct position (x1 y1 z1)) _ a1)) h1)
                 ((struct hand (_ (struct position (x2 y2 z2)) a2)) h2)
                 (a1-deg (radians->degrees a1))
                 (a2-deg (radians->degrees a2))
                 
                 (angle-to-dest (get-angle (- x1 x2) (- y1 y2)))
                 (backwards? 
                  (begin #;(display (format "Checking angles for backwards. a1: ~a, dest: ~a, diff: ~a~n" a1-deg angle-to-dest (angle-diff a1-deg angle-to-dest)))
                  (< 100 (angle-diff a1-deg angle-to-dest))))
                 
                 ; Ugh, at least now I can change this quickly
                 (throw-type (match (option options 'orientation)
                               ('parallel (if backwards? 'backdrop 'pass))
                               ('perpendicular 'self)
                               ('default 'unknown)))
                 
                 ; These five variables go a little ways toward styling the throw
                 ; catch and throw offset are z-offsets from the catch and throw point
                 ; and at least the 'catch' point one should be matched by dwell holds.
                 
                 ; initial-rotation allows passes to start low and rotated and
                 ; extra-rotation gives the club "overspin" to bring it around for passes, etc.
                 
                 (spins
                  (cond 
                    ((< tf 0.5) 0) ; meh, not really helping
                    ((< tf 1.0) 1)
                    ((< tf 1.4) 2)
                    ((< tf 2.0) 3)
                    (#t 4)))
                 
                 (orientation
                  (make-rotation -90 
                                 (match throw-type
                                   ('pass angle-to-dest)
                                   ; You can't throw a backdrop certain ways
                                   ; Well, I can't, anyhow.
                                   ('backdrop 
                                    (begin #;(display (format "Checking angles for backdrop. a1: ~a, a2: ~a, diff: ~a~n" a1-deg a2-deg (angle-diff a1-deg a2-deg)))
                                    (if (> 45 (angle-diff a1-deg a2-deg)) a2-deg a1-deg)))
                                   (_ a2-deg))
                                 -90))
                 ; raise the catch point a bit for all throws
                 (catch-offset (match throw-type
                                 ('pass 0.8)
                                 (_ 0.1)))
                 (throw-offset (match throw-type
                                 ('pass -0.3)
                                 (_ -0.1)))       
                 (initial-rotation (match throw-type
                                     ('pass 60)
                                     (_ 0)))
                 (extra-rotation (match throw-type
                                   ('pass 90)
                                   ('backdrop (if (> 45 (angle-diff a1-deg a2-deg)) 10 180)) 
                                   (_ 10))))
      (make-path-segment 
       tf
       ; Solve for parametric function p(t) with gravity pulling negative on z
       ; See page of handwritten math that really ought to be scanned in.
       ; With points p1 and p2, x(t) and y(t) are linear -
       ; x(t) = (x1/(x2 - tf))t + x1
       ; y(t) = (y1/(y2 - tf))t + y1
       ; z(t) = -9t^2 + mz t + z1 where
       ;  mz = (z2/tf) + (-z1/tf) + 9tf  (Not too sure about this one.)
       (if (= tf 0) (λ _ 'flagrant-error)  ; This shouldn't get executed.
              (let* ((mx (/ (- x2 x1) tf))
                     (bx x1)
                     (my (/ (- y2 y1) tf))
                     (by y1)
                     (mz (+ (/ (+ z2 catch-offset) tf) (/ (- (+ z1 throw-offset)) tf) (* 9.8 tf)))
                     (bz (+ z1 throw-offset)))
                (λ (t)         
                  (values 
                   (make-position 
                    (+ (* mx t) bx)
                    (+ (* my t) by)
                    (+ (* -9.8 t t) (* mz t) bz))
                   
                   ; Ugh. Since I'm bad at math, we need two different rotations here. (One to orient it "straight" and another for the spin.)
                   orientation
                   (make-rotation 0 0 (+ initial-rotation
                                       ; The actual spin as the club moves along.
                                       (* -1 t 
                                          (/ (+ initial-rotation extra-rotation (* spins 360)) tf)))))))))))
    
  (define (radians->degrees a) (* 57.2957795 a))
  ; Ugh, don't ask. My geometry is bad.
  (define (get-angle x y)
    (- 180 ; It's not just about living forever, Jackie. The trick is living with yourself forever.
     (if (zero? x) 0 ; doesn't matter, I think
         (let* ((a (atan (/ y x)))
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
  
  (define (hold-path-segment tf h1 . options)
    (make-path-segment tf
                       (match-let* (((struct hand (p1 _ a1)) h1)
                                    (a1-deg (radians->degrees a1)))
                         (λ _ (values p1 (make-rotation -90 a1-deg -90) (make-rotation 0 10 0))))))
    
  (define (dwell-hold-path-segment tf h1 h-throw . options)
    (match-let* (((struct hand ((struct position (x2 y2 z2)) 
                                (struct position (x1 y1 z1)) 
                                a1)) h1)
                 ((struct hand ((struct position (x-throw y-throw _)) _ a2)) h-throw)
                 (a1-deg (radians->degrees a1))
                 (a2-deg (radians->degrees a2))
                 (angle-to-dest (get-angle (- x-throw x1) (- y-throw y1)))                 
                 (backwards? (< 100 (angle-diff a2-deg angle-to-dest)))
                 
                 ; Ugh, at least now I can change this quickly
                 (throw-type (match (option options 'orientation)
                               ('parallel (if backwards? 'backdrop 'pass))
                               ('perpendicular 'self)
                               ('default 'unknown)))
                 (catch-rotation (match throw-type
                                   ('pass 90)
                                   (_ 10)))
                 ; raise the catch point a bit for all throws
                 (catch-offset (match throw-type
                                 ('pass 0.8)
                                 (_ 0.1))))
      (make-path-segment
       tf
       ; On this one... I'm just going to go linearly for now.
       ; A better future plan might be an approximation of the direction of the last segment
       ; x(t) = (x1/(x2 - tf))t + x1
       ; y(t) = (y1/(y2 - tf))t + y1
       ; z(t) = (z1/(z2 - tf))t + z1
       
       (if (= tf 0) (λ _ 'flagrant-error) ; Special case for tf = 0, which causes murder later.
           (begin 
             (let* ((mx (/ (- x2 x1) tf))
                    (bx x1)
                    (my (/ (- y2 y1) tf))
                    (by y1)
                    (mz (/ (- z2 (+ z1 catch-offset)) tf))
                    (bz (+ z1 catch-offset))
                    
                    (y-rot a1-deg))                           
               (λ (t)        
                 (values
                  (make-position 
                   (+ (* mx t) bx)
                   (+ (* my t) by)
                   (+ (* mz t) bz))
                  
                  (make-rotation -90 y-rot -90)
                  (make-rotation 0 0 
                                 (+ (- catch-rotation) (* t (/ (+ catch-rotation 10) tf))))))
                                 ))))))

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
   
   ball-toss-path-segment dwell-hold-path-segment hold-path-segment
   advance-path-state! advance-pattern! map-pattern
   
   radians->degrees
   angle-diff
   
   rotate translate
   rotate-hands translate-hands))