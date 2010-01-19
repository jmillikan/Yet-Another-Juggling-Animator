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
  
  (define w #f) ; screw it.
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
      
      (define pattern-forms (instantiate pattern-forms% (control-panel)))
      (instantiate-view-controls canvas control-panel)
      
      (define/public (set-error e)
        (send error-box set-value e))
      
      (define error-box (instantiate text-field% ("" this)))
      ))
  
  ; Tab panels don't automatically change panels when clicked... You have to rig it up yourself. Awesome.
  (define pattern-forms% 
    (class* tab-panel% ()
      (inherit get-selection add-child delete-child)
      (init-field w)
      (super-instantiate ((list "Easy Mode" "Hard Mode") w) 
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
          (set! current-form new-form)))
      
      (define siteswap-form
        (instantiate siteswap-form% (this)))
      
      (define current-form siteswap-form)
      
      (define scheme-form
        (instantiate scheme-form% (this)))
      
      ; Is there a sane way to do this?
      (delete-child scheme-form)))
  
  ; A single line in the interface for pattern definition.
  (define pattern-line%
    (class* horizontal-panel% ()
      (init-field name)
      (init-field initial-beat)
      (init-field initial-dwell)
      (init-field initial-pattern)
      (init-field pattern-lambda) ; string -> sexp
      (init-field hands-lambda) ; thunk -> hands
      (init-field juggling-window) ; show juggling-window show-pattern...
      (init-field examples-list) ; for input combo
      (init-field parent)
      
      (super-instantiate (parent) (alignment '(center center)) (stretchable-width #t))
    
      (define input-pattern (instantiate combo-field% (name examples-list this) (min-width 250) (init-value initial-pattern) (stretchable-width #t)))
      (define input-beat (instantiate text-field% ("" this) (init-value  initial-beat) (min-width 60) (stretchable-width #f)))
      (define input-dwell (instantiate text-field% ("" this) (init-value  initial-dwell) (min-width 60) (stretchable-width #f)))
      
      (instantiate button% 
        ("Run" this (λ _ 
                        (with-handlers ((exn:fail? (λ (e) (send w set-error (exn-message e)))))
                          (let*   
                              ((beat-value (string->number (send input-beat get-value)))
                               (dwell-value (string->number (send input-dwell get-value)))
                               (sexp-pattern (pattern-lambda (send input-pattern get-value)))
                               (pattern (sexp->pattern sexp-pattern beat-value dwell-value (hands-lambda))))
                            (send w show-pattern
                                  pattern
                                  (hands-lambda))))))
        (stretchable-width #f))))

  (define siteswap-form% 
    (class* vertical-panel% ()
      (init-field parent)
      (super-instantiate (parent) (alignment '(center center)) (stretchable-height #f))
      (instantiate pattern-line% ("Siteswap" "0.25" "0.16" "744" 2hss->sexp 
                                             (λ _ pair-of-hands) w 2-ss-examples this))
      (instantiate pattern-line% ("4-hand SS" "0.15" "0.13" "966" 4hss->sexp 
                                              (λ _ (juggler-circle 2 3.0)) w 4-hand-examples this))
      (instantiate pattern-line% ("Synchronous" "0.25" "0.20" "(6x,4)*" sync-ss->sexp 
                                                (λ _ (juggler-circle 2 3.0)) w syncss-examples this))))
  
  ; For now, the stuff in the evals can see/do everything
  (define-namespace-anchor nsa)  
  (define eval-namespace (namespace-anchor->namespace nsa))
  
  (define scheme-form% 
    (class* vertical-panel% ()
      (init-field parent)
      (super-instantiate (parent) (alignment '(center center)) (stretchable-height #f))
      
      (define hands-select (instantiate combo-field% ("Juggler/Hand List" hands-examples this) (min-width 250) (init-value "") (stretchable-width #t)))
      (define (get-hands)
        (eval (call-with-input-string (send hands-select get-value) read) eval-namespace))
      
      (instantiate pattern-line% ("Scheme List" "0.25" "0.2" "" 
                                                (λ (s) (eval (call-with-input-string s read) eval-namespace))
                                                get-hands w sexp-examples this))
      (instantiate pattern-line% ("6-hand SS" "0.10" "0.08" "a" 6hss->sexp get-hands w 6-ss-examples this))))
  
  (define (instantiate-view-controls c h)
    (let ((v (instantiate vertical-panel% (h) (alignment '(center center)) (stretchable-width #f) (min-width 200))))
      (let ((h (instantiate horizontal-panel% (v)
                 (alignment '(center center)))))
        (instantiate button% ("+" h (λ _ (send c zoom-in)))
          (stretchable-width #t))
        (instantiate button% ("-" h (λ _ (send c zoom-out)))
          (stretchable-width #t)))
      (instantiate combo-field% ("Object" (list "ball" "ring" "club") v) (callback 
                                                                              (λ (l v) 
                                                                                (with-handlers ((exn:fail? (λ (e) (send w set-error (exn-message e)))))
                                                                                  (send c set-model 
                                                                                      (send l get-value))))))
      (let* ((h-time (instantiate horizontal-panel% (v) (alignment '(center center)) (stretchable-width #f)))
             (time-input (instantiate text-field% ("Time Scale" h-time) (min-width 100) (init-value "1.0") (stretchable-width #t))))              
        (instantiate button% ("Set" h-time (λ _ 
                                             (with-handlers ((exn:fail? (λ _ 'flagrant-error)))
                                               (send c scale-time (call-with-input-string (send time-input get-value) read)))))
          (stretchable-width #f)))))
  
  (set! w (make-object main-window))
  (send w show #t))