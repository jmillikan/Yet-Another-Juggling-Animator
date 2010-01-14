(module juggling-canvas scheme
  (require mred
           mzlib/class
           mzlib/math
           sgl
           sgl/gl-vectors
           srfi/1
           "juggling-core.ss")
  
  (provide juggling-canvas%)
  
  (define juggling-canvas%
    (class* canvas% ()
      (inherit refresh with-gl-context swap-gl-buffers get-parent)
      
      (define view-rotx -70.0)
      (define view-roty 0.0)
      (define view-rotz 20.0)
      
      (define view-posx 0.0)
      (define view-posy 0.0)
      
      (define view-zoom -15.0)
      (define quadric #f)
      
      (define internal-pattern #f)
      
      (define jugglers #f) ; lambda rendering jugglers
      
      (define step? #t)
      
      (define view-time-scale 1.0)
      
      (define last-ms #f)
      
      (define current-model #f)
      
      (define/public (set-model m)
        (cond ((equal? m "ball") (set! current-model ball-model))
              ((equal? m "ring") (set! current-model ring-model))
              ((equal? m "club") (set! current-model club-model))
              (#t 'flagrant-error)))
      
      (define grid-size 100.0)
      (define grid-unit 1.0)
      
      (define/public (run)
        (set! step? #t)
        (refresh))
      
      (define/public (scale-time x)
        (if (number? x)
            (set! view-time-scale x)
            'flagrant-error))                  
      
      (define/public (move-left)
        (set! view-rotz (+ view-rotz 5.0))
        (refresh))
      
      (define/public (move-right)
        (set! view-rotz (- view-rotz 5.0))
        (refresh))
      
      (define/public (move-up)
        (let ((new-view-rotx (+ view-rotx 5.0)))
          (if (and 
               (>= new-view-rotx -90.0)
               (<= new-view-rotx 90.0))
              (begin 
                (set! view-rotx new-view-rotx)
                (refresh))
              'nope)))
      
      (define/public (move-down)
        (let ((new-view-rotx (- view-rotx 5.0)))
          (if (and
               (>= new-view-rotx -90.0)
               (<= new-view-rotx 90.0))
              (begin 
                (set! view-rotx new-view-rotx)
                (refresh))
              'nope)))
      
      ; Constrain zoom to reasonable levels for this prop size/gravity
      (define/public (zoom-in)
        (set! view-zoom (min 0.0 (+ view-zoom 3.0)))
        (refresh))
            
      (define/public (zoom-out)
        (set! view-zoom (max -100.0 (- view-zoom 3.0)))
        (refresh))
      
      (define screen-x-last 0)
      (define screen-y-last 0)
      (define screen-rot-tracking? #f)
      (define screen-pos-tracking? #f)
      
      (define/override (on-event e)
        (cond
          ((send e button-down? 'left) 
           (begin
             (set! screen-x-last (send e get-x))
             (set! screen-y-last (send e get-y))
             (set! screen-rot-tracking? #t))
             
             #;(display (format "Left click!~n")))
          ((send e button-up? 'left) 
           (begin
             (set! screen-rot-tracking? #f)
             #;(display (format "Left unclick!~n"))))
          ((send e button-down? 'right)
           (begin
             (set! screen-x-last (send e get-x))
             (set! screen-y-last (send e get-y))
             (set! screen-pos-tracking? #t)))
          ((send e button-up? 'left) 
           (begin
             (set! screen-pos-tracking? #f)
             #;(display (format "Left unclick!~n"))))
          ((send e dragging?)
           (cond (screen-rot-tracking?
             
                  (let ((new-x (send e get-x)) (new-y (send e get-y)))
                    (set! view-rotz (+ view-rotz 
                                       (* (- new-x screen-x-last) 0.2))) ; 1/5th a degree per pixel?
                    (set! view-rotx (+ view-rotx
                                       (* (- new-y screen-y-last) 0.2))) ; 1/5th a degree per pixel?
                    
                    (set! screen-x-last new-x)
                    (set! screen-y-last new-y)
                    #;(display (format "Dragging to ~a, ~a~n" new-x new-y))))
                 (screen-pos-tracking?
                  (let ((new-x (send e get-x)) (new-y (send e get-y)))
                    (set! view-posx (+ view-posx 
                                       (* (- new-x screen-x-last) 0.1))) ; 1/5th a degree per pixel?
                    (set! view-posy (+ view-posy
                                       (* (- new-y screen-y-last) 0.1))) ; 1/5th a degree per pixel?
                    
                    (set! screen-x-last new-x)
                    (set! screen-y-last new-y)))))
        
        
          (#t (begin
                (set! screen-rot-tracking? #f)
                (set! screen-pos-tracking? #f)))))
      
      ; TODO: Get resizing working again...
      (define/override (on-size width height)
        (with-gl-context
         (lambda ()
           (gl-viewport 0 0 width height)
           (gl-matrix-mode 'projection)
           (gl-load-identity)
           (let ((h (/ height width)))
             (gl-frustum -1.0 1.0 (- h) h 5.0 1000.0))
           (gl-matrix-mode 'modelview)
               
           (gl-enable 'cull-face)
           (gl-enable 'lighting)
           (gl-enable 'light0)
           (gl-enable 'depth-test)
                      
           (unless internal-pattern
             (set! quadric (gl-new-quadric))
             (set! last-ms (current-milliseconds))
             (set! internal-pattern (make-pattern '()))
             (create-objects)
             (set! current-model ball-model)
             
             (gl-enable 'normalize))))
        (refresh))
      
      (define (create-objects)
        (set! ball-model (gl-gen-lists 1))
        (gl-new-list ball-model 'compile)          
        (gl-sphere quadric 0.1 10 10)
        (gl-end-list)
        
        (set! ring-model (gl-gen-lists 1))
        (gl-new-list ring-model 'compile)
        (gl-disk quadric 0.23 0.32 10 1)            
        (gl-cylinder quadric 0.32 0.32 0.02 10 1)
        (gl-quadric-orientation quadric 'inside)
        (gl-cylinder quadric 0.23 0.23 0.02 10 1)
        (gl-quadric-orientation quadric 'outside)
        (gl-translate 0.0 0.0 0.02)
        (gl-rotate 180 1.0 0 0)                    
        (gl-disk quadric 0.23 0.32 10 1)
        (gl-end-list)    
        
        (set! club-model (gl-gen-lists 1))
        (gl-new-list club-model 'compile)
        (gl-rotate -90 1.0 0 0)
        (gl-translate 0 0 -0.3)
        (gl-sphere quadric 0.045 5 5)
        (gl-cylinder quadric 0.025 0.04 0.2 10 1)
        (gl-translate 0 0 0.2)
        (gl-cylinder quadric 0.04 0.085 0.2 10 1)
        (gl-translate 0 0 0.2)
        (gl-cylinder quadric 0.085 0.05 0.2 10 1)
        (gl-translate 0 0 0.2)
        (gl-sphere quadric 0.05 5 5)
        
        ;(gl-translate
        (gl-end-list)
        )
        
      
      ; This is misnamed... At the moment, it's colors.
      (define colors
        (circular-list
           '(0.1 0.1 0.1 1.0) 
           '(0.1 1.0 0.1 0.1)
           '(0.1 0.1 1.0 0.1)
           '(0.1 0.6 0.6 0.1)
           '(0.1 0.1 0.6 0.6)
           '(0.1 0.6 0.1 0.6)
           '(0.1 1.0 0.2 0.6)
           ))
      
      (define ball-model #f)
      
      (define ring-model #f)
          
      (define club-model #f)
      
      (define/public (set-pattern p)
        (if (pattern? p)
            (begin
              ; BUG: This pattern can't be shown for some reason until it has been advance-pattern!ed, at least once, even with 0 ms...
              (set! internal-pattern p)
              (advance-pattern! internal-pattern 0))
            
            'flagrant-error))
      
      (define/public (set-jugglers j)
            (set! jugglers (jugglers-lambda j)))
      
      (define/override (on-paint)
        (when internal-pattern
          (when step? 
            (let ((last-interval (- (current-milliseconds) last-ms)))              
              (advance-pattern! internal-pattern (* view-time-scale (/ last-interval 1000))))
            
            (set! last-ms (current-milliseconds)))
          (with-gl-context
           (lambda ()
             (gl-clear-color 0.0 0.0 0.0 0.0)
             (gl-clear 'color-buffer-bit 'depth-buffer-bit)
             
             (gl-push-matrix)
             
             (gl-translate view-posx (- view-posy) 0)
             (gl-translate 0.0 -1.0 view-zoom)
             (gl-translate 0.0 (+ (/ view-zoom 8) 1.0) view-zoom)
             (gl-rotate view-rotx 1.0 0.0 0.0)
             (gl-rotate view-roty 0.0 1.0 0.0)
             (gl-rotate view-rotz 0.0 0.0 1.0)
             
             (map-pattern 
              (lambda (prop pos rot spin)
                (match-let
                    (((list size r g b) prop)
                     ((struct position (x y z)) pos)
                     ((struct rotation (rot-x rot-y rot-z)) rot)
                     ((struct rotation (spin-x spin-y spin-z)) spin))                     
                  (begin
                    (gl-push-matrix)
                    (gl-material-v 'front
                                   'ambient-and-diffuse
                                   (vector->gl-float-vector (vector r g b 1.0)))                    
                    (gl-translate x y z)
                    (gl-rotate rot-x 1.0 0 0)
                    (gl-rotate rot-y 0 1.0 0)
                    (gl-rotate rot-z 0 0 1.0)
                    
                    (gl-rotate spin-x 1.0 0 0)
                    (gl-rotate spin-y 0 1.0 0)
                    (gl-rotate spin-z 0 0 1.0)
                    
                    (gl-call-list current-model)
                    (gl-normal 0 0 1)
                    (gl-pop-matrix))))
              internal-pattern colors)
             
             (if (procedure? jugglers) 
                   (jugglers) '())
             
             ; Show a nice grid (size determined by 2 x grid-size, each square is grid-unit across.)
             (gl-material-v 'front-and-back
                            'ambient-and-diffuse
                            (vector->gl-float-vector (vector 0.7 0.7 0.7 1.0)))
             
             ; Trivia: You can also do this with evaluators, assuming PLT Scheme supports them...
             (gl-begin 'lines)
             ; north-south bars (x is constant)
             (for ((y (in-range (- grid-size) (+ grid-size grid-unit) grid-unit)))
               (begin
                 (gl-vertex (- grid-size) y 0.0)
                 (gl-vertex grid-size y 0.0)))
             
             (for ((x (in-range (- grid-size) (+ grid-size grid-unit) grid-unit)))
               (begin
                 (gl-vertex x (- grid-size) 0.0)
                 (gl-vertex x grid-size 0.0)))
             
             (gl-end)
             
             (gl-pop-matrix)
             
             (swap-gl-buffers)
             (gl-flush)))
          (when step?
            (set! step? #f)
            (queue-callback (lambda x (send this run))))))
      
      ; Create a lambda rendering hand positions for a list of hands.
      ; Right now, it just does squares beneath the catch/receive positions.
      (define (jugglers-lambda hands-lst)
        (lambda ()       
          (map 
           (lambda (h) ; render squares vaguely under both the throw and catch position.
             ; render arrow in the direction of angle from x1 y1
             (match-let 
                 (((struct hand ((struct position (x1 y1 z1)) (struct position (x2 y2 z2)) angle)) h))
               (begin
                 (gl-material-v 'front
                                'ambient-and-diffuse
                                (vector->gl-float-vector (vector 0.2 1.0 0.3 1.0)))
                 
                 (gl-begin 'quads)
                 (gl-vertex (+ x1 0.2) (+ y1 0.2) (- z1 1.0))
                 (gl-vertex (- x1 0.2) (+ y1 0.2) (- z1 1.0))
                 (gl-vertex (- x1 0.2) (- y1 0.2) (- z1 1.0))
                 (gl-vertex (+ x1 0.2) (- y1 0.2) (- z1 1.0))
                 (gl-end)
                 
                 (gl-material-v 'front
                                'ambient-and-diffuse
                                (vector->gl-float-vector (vector 1.0 0.2 0.1 1.0)))
                 
                 (gl-begin 'quads)
                 (gl-vertex (+ x2 0.2) (+ y2 0.2) (- z2 1.0))
                 (gl-vertex (- x2 0.2) (+ y2 0.2) (- z2 1.0))
                 (gl-vertex (- x2 0.2) (- y2 0.2) (- z2 1.0))
                 (gl-vertex (+ x2 0.2) (- y2 0.2) (- z2 1.0))
                 (gl-end)
                 
                 (gl-material-v 'front
                                'ambient-and-diffuse
                                (vector->gl-float-vector (vector 0.2 1.0 0.3 1.0)))
                 
                 ; There's way more obvious ways to do this just using OpenGL... Oh well.
                 (match-let*
                     ((unit (make-position 1 0 0))
                      ((struct position (ax ay _)) (rotate unit angle)))
                   (begin
                     (gl-begin 'lines)
                     (gl-vertex x1 y1 (- z1 1.0))
                     (gl-vertex (+ x1 ax) (+ ay y1) (- z1 1.0))
                     (gl-end)))
                                
                            )))
           
           hands-lst)))
      
      (super-instantiate () (style '(gl no-autoclear))))))