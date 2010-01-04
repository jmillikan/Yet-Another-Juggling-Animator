(module juggling-main scheme
  (require mred
           mzlib/class
           mzlib/math
           sgl
           sgl/gl-vectors
           srfi/1
           "juggling-core.ss"
           "sexp-converter.ss"
           "fourhss-converter.ss"
           "example-patterns.ss"
           "juggling-canvas.ss")
  
  (define (instantiate-pattern-forms c h)
    (let* ((v-pattern-forms (instantiate vertical-panel% (h)
                             (alignment '(center center)) (stretchable-height #f)))
           (hands-select (instantiate combo-field% ("Juggler Pattern" hands-examples v-pattern-forms) (min-width 250) (init-value "") (stretchable-width #f)))
           (get-hands 
            (lambda ()
              (eval (call-with-input-string (send hands-select get-value) read)))))
      (let* ((h-2hss (instantiate horizontal-panel% (v-pattern-forms) (alignment '(center center)) (stretchable-width #f)))
             (2hss-input (instantiate combo-field% ("Async Siteswap" 2-ss-examples h-2hss) (min-width 250) (init-value "7531") (stretchable-width #f))))              
        (instantiate button% ("Run" h-2hss (lambda x 
                                             (with-handlers ((exn:fail? (lambda (e) 'flagrant-error)))
                                               (let ((sexp-pattern (2hss->sexp (send 2hss-input get-value))))
                                                 (send c set-pattern (sexp->pattern sexp-pattern 0.25 0.16 pair-of-hands)))
                                               (send c set-jugglers pair-of-hands))))
          (stretchable-width #t)))
      
      (let* ((h-4hss (instantiate horizontal-panel% (v-pattern-forms) (alignment '(center center)) (stretchable-width #f)))
             (4hss-input (instantiate combo-field% ("4-hand Siteswap" 4-hand-examples h-4hss) (min-width 250) (init-value "966") (stretchable-width #f))))              
        (instantiate button% ("Run" h-4hss (lambda x 
                                             (with-handlers ((exn:fail? (lambda (e) 'flagrant-error)))
          (let ((sexp-pattern (4hss->sexp (send 4hss-input get-value))))
            (send c set-pattern (sexp->pattern sexp-pattern 0.15 0.13 pair-of-jugglers))
            (send c set-jugglers funky-pair-of-jugglers)))))
          (stretchable-width #t)))
      
      (let* ((h-sexp (instantiate horizontal-panel% (v-pattern-forms) (alignment '(center center)) (stretchable-width #f)))
             (sexp-input (instantiate combo-field% ("S-Expression (MHN)" sexp-examples h-sexp) (min-width 250) (init-value "") (stretchable-width #f))))              
        (instantiate button% ("Run" h-sexp (lambda x 
                                             (with-handlers ((exn:fail? (lambda (e) 'flagrant-error)))
          (send c set-pattern (sexp->pattern (eval (call-with-input-string (send sexp-input get-value) read)) 0.25 0.2 (get-hands)))
                                               (send c set-jugglers (get-hands)))))
          (stretchable-width #t)))
      
      (let* ((h-internal (instantiate horizontal-panel% (v-pattern-forms) (alignment '(center center)) (stretchable-width #f)))
             (internal-input (instantiate combo-field% ("Scheme (Internals)" internal-examples h-internal) (init-value "") (min-width 250) (stretchable-width #f))))              
        (instantiate button% 
          ("Run" h-internal 
                 (lambda x 
                   (send c set-pattern 
                                 (eval (call-with-input-string (send internal-input get-value) read)))
                   (send c set-jugglers (get-hands)))) (stretchable-width #t)))))
  
  (define (instantiate-view-controls c h)
    (let ((v (instantiate vertical-panel% (h) (alignment '(center center)))))
      (let ((h (instantiate horizontal-panel% (v)
                 (alignment '(center center)))))
        (instantiate button% ("Left" h (lambda x (send c move-left)))
          (stretchable-width #t))
        (let ((v (instantiate vertical-panel% (h)
                   (alignment '(center center)) (stretchable-width #f))))
          (instantiate button% ("Up" v (lambda x (send c move-up)))
            (stretchable-width #t))
          (instantiate button% ("Down" v (lambda x (send c move-down)))
            (stretchable-width #t)))
        (instantiate button% ("Right" h (lambda x (send c move-right)))
          (stretchable-width #t))
        (instantiate button% ("+" h (lambda x (send c zoom-in)))
          (stretchable-width #t))
        (instantiate button% ("-" h (lambda x (send c zoom-out)))
          (stretchable-width #t)))
      (let* ((h-time (instantiate horizontal-panel% (v) (alignment '(center center)) (stretchable-width #f)))
             (time-input (instantiate text-field% ("Time Scale" h-time) (min-width 250) (init-value "1.0") (stretchable-width #f))))              
        (instantiate button% ("Set" h-time (lambda x 
                                             (with-handlers ((exn:fail? (lambda (e) 'flagrant-error)))
                                               (send c scale-time (call-with-input-string (send time-input get-value) read)))))
          (stretchable-width #t)))))
  
  
  (define (initialize-and-show)
    (let* ((window (make-object frame% "Juggling Animator" #f))
           (c (instantiate juggling-canvas% (window) (min-width 600) (min-height 400))))
        (let ((h (instantiate horizontal-panel% (window)
                   (alignment '(center center)) (stretchable-height #f))))
          (instantiate-view-controls c h)
          (instantiate-pattern-forms c h))      
      (send window show #t)))
  
  ; This just doesn't belong here at all...
  (define (jugglers-lambda hands-lst)
    (lambda ()       
      (map 
       (lambda (hand) ; render squares vaguely under both the throw and catch position.
         (match hand ((list (struct position (x1 y1 z1)) (struct position (x2 y2 z2)))
                      (begin
                        (gl-material-v 'front
                                       'ambient-and-diffuse
                                       (vector->gl-float-vector (vector 0.9 0.9 0.9 1.0)))
                        
                        (gl-begin 'quads)
                        (gl-vertex (+ x1 0.2) (+ y1 0.2) (- z1 1.0))
                        (gl-vertex (- x1 0.2) (+ y1 0.2) (- z1 1.0))
                        (gl-vertex (- x1 0.2) (- y1 0.2) (- z1 1.0))
                        (gl-vertex (+ x1 0.2) (- y1 0.2) (- z1 1.0))
                        
                        (gl-vertex (+ x2 0.2) (+ y2 0.2) (- z2 1.0))
                        (gl-vertex (- x2 0.2) (+ y2 0.2) (- z2 1.0))
                        (gl-vertex (- x2 0.2) (- y2 0.2) (- z2 1.0))
                        (gl-vertex (+ x2 0.2) (- y2 0.2) (- z2 1.0))
                        (gl-end)))))
       hands-lst)))
  
  (initialize-and-show))