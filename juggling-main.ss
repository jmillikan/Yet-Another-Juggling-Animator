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
  
  (define main-window
    (class* frame% ()
      (inherit show)
      (super-instantiate ("Juggling Animator" #f))
      (define canvas (instantiate juggling-canvas% (this) (min-width 600) (min-height 400)))
      (define control-panel (instantiate horizontal-panel% (this)
                  (alignment '(center center)) (stretchable-height #f) (min-height 150)))
      
      (define/public (show-pattern p j)
        (send canvas set-pattern p)
        (send canvas set-jugglers j))
      
      #;(instantiate-pattern-forms canvas control-panel)
      
      (define pattern-forms (instantiate pattern-forms% (control-panel)))
      (instantiate-view-controls canvas control-panel)
      
      ))
  
  ; Tab panels don't automatically change panels when clicked... You have to rig it up yourself. Awesome.
  (define pattern-forms% 
    (class* tab-panel% ()
      (inherit get-selection add-child delete-child)
      (init-field w)
      (super-instantiate ((list "Siteswap" "Scheme") w) 
        (callback (lambda (c x) (send this change-tab)))
        (min-width 400))
      ; Why does this have to be public?  ;_;
      (define/public (change-tab)
         (let* ((i (get-selection))
                (new-form 
                 (cond ((= i 0) siteswap-form)
                       
                 ((= i 1) scheme-form)
                 (#t 'flagrant-error))))

           (delete-child current-form)
           (add-child new-form)
           (set! current-form new-form)
        ))
      
      (define siteswap-form
        (instantiate siteswap-form% (this)))
      
      (define current-form siteswap-form)
      
      (define scheme-form
        (instantiate scheme-form% (this)))
      
      (delete-child scheme-form)
      
      #;(send scheme-form show #f)
      
      
      
      ))
  
  (define siteswap-form% 
    (class* vertical-panel% ()
      (init-field parent)
      (super-instantiate (parent) (alignment '(center center)) (stretchable-height #f))
      
      (define h-2hss (instantiate horizontal-panel% (this) (alignment '(center center)) (stretchable-width #t)))
      (define 2hss-input (instantiate combo-field% ("Async Siteswap" 2-ss-examples h-2hss) (min-width 250) (init-value "7531") (stretchable-width #t)))
      (define 2hss-beat (instantiate text-field% ("" h-2hss) (init-value  "0.25")))
      (define 2hss-dwell (instantiate text-field% ("" h-2hss) (init-value  "0.16")))
      
      (instantiate button% 
        ("Run" h-2hss (lambda x 
                                           (with-handlers ((exn:fail? (lambda (e) 'flagrant-error)))
                                             (let* 
                                                 
                                                 ((beat-value (string->number (send 2hss-beat get-value)))
                                                  (dwell-value (string->number (send 2hss-dwell get-value)))
                                                  (sexp-pattern (2hss->sexp (send 2hss-input get-value)))
                                                  (pattern (sexp->pattern sexp-pattern beat-value dwell-value pair-of-hands)))
                                               (send w show-pattern
                                                     pattern
                                                     pair-of-hands)))))
        (stretchable-width #t))
      
      (define h-4hss (instantiate horizontal-panel% (this) (alignment '(center center)) (stretchable-width #t)))
      (define 4hss-input (instantiate combo-field% ("4-hand Siteswap" 4-hand-examples h-4hss) (min-width 250) (init-value "966") (stretchable-width #t)))              
      
      (define 4hss-beat (instantiate text-field% ("" h-4hss) (init-value  "0.15")))
      (define 4hss-dwell (instantiate text-field% ("" h-4hss) (init-value  "0.13")))
      
      (instantiate button% 
        ("Run" h-4hss (lambda x 
                                           (with-handlers ((exn:fail? (lambda (e) 'flagrant-error)))
                                             (let* ((beat-value (string->number (send 4hss-beat get-value)))
                                                    (dwell-value (string->number (send 4hss-dwell get-value)))
                                                    (sexp-pattern (4hss->sexp (send 4hss-input get-value)))
                                                    (pattern (sexp->pattern sexp-pattern beat-value dwell-value (juggler-circle 2 3.0))))
                                               (send w show-pattern pattern (juggler-circle 2 3.0))))))
        (stretchable-width #t))
      
      
      (define h-syncss (instantiate horizontal-panel% (this) (alignment '(center center)) (stretchable-width #t)))
      (define syncss-input (instantiate combo-field% ("Synchronous Siteswap" syncss-examples h-syncss) (min-width 250) (init-value "(6x,4x)") (stretchable-width #t)))              
      
      (define syncss-beat (instantiate text-field% ("" h-syncss) (init-value  "0.25")))
      (define syncss-dwell (instantiate text-field% ("" h-syncss) (init-value  "0.20")))
      
      (instantiate button% 
        ("Run" h-syncss (lambda x 
                                           (with-handlers ((exn:fail? (lambda (e) 'flagrant-error)))
                                             (let* ((beat-value (string->number (send syncss-beat get-value)))
                                                    (dwell-value (string->number (send syncss-dwell get-value)))
                                                    (sexp-pattern (sync-ss->sexp (send syncss-input get-value)))
                                                    (pattern (sexp->pattern sexp-pattern beat-value dwell-value (juggler-circle 2 3.0))))
                                               (send w show-pattern pattern (juggler-circle 2 3.0))))))
        (stretchable-width #t))
      ))
      
  
  (define scheme-form% 
    (class* vertical-panel% ()
      (init-field parent)
      (super-instantiate (parent) (alignment '(center center)) (stretchable-height #f))
      
      (define hands-select (instantiate combo-field% ("Juggler/Hand List" hands-examples this) (min-width 250) (init-value "") (stretchable-width #t)))
      (define (get-hands)
              (eval (call-with-input-string (send hands-select get-value) read)))
      
      (define h-sexp (instantiate horizontal-panel% (this) (alignment '(center center)) (stretchable-width #t)))
      (define sexp-input (instantiate combo-field% ("S-Expression (MHN)" sexp-examples h-sexp) (min-width 250) (init-value "") (stretchable-width #t)))              
      (define sexp-beat (instantiate text-field% ("" h-sexp) (init-value  "0.25")))
      (define sexp-dwell (instantiate text-field% ("" h-sexp) (init-value  "0.2")))
      (instantiate button% 
        ("Run" h-sexp (lambda x 
                        (with-handlers ((exn:fail? (lambda (e) 'flagrant-error)))
                          (send w show-pattern 
                                (sexp->pattern (eval (call-with-input-string (send sexp-input get-value) read)) 
                                               (string->number (send sexp-beat get-value))
                                               (string->number (send sexp-dwell get-value)) 
                                               (get-hands)) 
                                (get-hands)))))
        (stretchable-width #t))
      
      ))
  
  (define (instantiate-view-controls c h)
    (let ((v (instantiate vertical-panel% (h) (alignment '(center center)) (stretchable-width #f) (min-width 200))))
      (let ((h (instantiate horizontal-panel% (v)
                 (alignment '(center center)))))
        (instantiate button% ("+" h (lambda x (send c zoom-in)))
          (stretchable-width #t))
        (instantiate button% ("-" h (lambda x (send c zoom-out)))
          (stretchable-width #t)))
      (let* ((h-time (instantiate horizontal-panel% (v) (alignment '(center center)) (stretchable-width #f)))
             (time-input (instantiate text-field% ("Time Scale" h-time) (min-width 100) (init-value "1.0") (stretchable-width #t))))              
        (instantiate button% ("Set" h-time (lambda x 
                                             (with-handlers ((exn:fail? (lambda (e) 'flagrant-error)))
                                               (send c scale-time (call-with-input-string (send time-input get-value) read)))))
          (stretchable-width #f)))))
  
  
  (define w (make-object main-window))
    (send w show #t)
        
  
  )