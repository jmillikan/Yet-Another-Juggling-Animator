(module juggling-main scheme
  (require mred
           mzlib/class
           mzlib/math
           sgl
           sgl/gl-vectors
           srfi/1
           mzlib/pconvert
           
           "juggling-core.ss"
           "sexp-converter.ss"
           "pattern-utilities.ss"
           "fourhss-converter.ss"
           "example-patterns.ss"
           "juggling-canvas.ss")
  
  (define main-window
    (class* frame% ()
      (inherit show set-status-text)
      (super-instantiate ("Yet Another Overcomplicated Pattern Animator" #f))
      
      (define/augment (on-close)
        (exit))
      
      (send this create-status-line)
      
      (define h-split (instantiate horizontal-panel% (this)))
      (define v-split (instantiate vertical-panel% (h-split)))
      
      (define prefab-column (instantiate vertical-panel% (h-split)))
      
      (define prefab-list (instantiate list-box% 
                            (#f (map car complete-patterns-internal) prefab-column) (style '(single)) (min-width 170)))
      
      (define prefab-buttons (instantiate horizontal-panel% (prefab-column) (stretchable-height #f)))
      
      (define examine-button (instantiate button% ("Edit" prefab-buttons 
                                                          (λ (_ e)
                                  (with-handlers ((exn:fail? (λ (e) (set-error (exn-message e)))))
                                    (let* 
                                        ((sels (send prefab-list get-selections))) ; Should have 0 or 1 selections.
                                      (if (= (length sels) 0)
                                          (error "No pattern selected")
                                          (match-let*
                                              (((list pattern-name pattern-t jugglers-t beat dwell hold) 
                                                (list-ref complete-patterns-internal (car sels))))
                                            (send ed-win edit-pattern
                                                  pattern-t jugglers-t beat dwell hold)
                                            
                                            (set! show-editor #t)
                                            (send ed-win show #t)
                                            (send mi-editor check #t)))))))))
      
      (define run-button (instantiate button% 
                               ("Run" prefab-buttons 
                                (λ (_ e)
                                  (with-handlers ((exn:fail? (λ (e) (set-error (exn-message e)))))
                                    (let* 
                                        ((sels (send prefab-list get-selections))) ; Should have 0 or 1 selections.
                                      (if (= (length sels) 0)
                                          (error "No pattern selected")
                                          (match-let*
                                              (((list pattern-name pattern-t jugglers-t beat dwell hold) 
                                                (list-ref complete-patterns-internal (car sels)))
                                               (hands (eval-string jugglers-t)))
                                            (show-pattern 
                                             (sexp->pattern (eval-string pattern-t) beat dwell hands hold)
                                             hands)))))))))
      
      (define canvas (instantiate juggling-canvas% (v-split) (min-width 600) (min-height 450)))
      
      (define control-panel (instantiate horizontal-panel% (v-split)
                              (alignment '(center center)) (stretchable-height #f) (min-height 150)))
      
      (define/public (show-pattern p j)
        (send canvas set-pattern p)
        (send canvas set-jugglers j
              ; allow jugglers-lambda or related functions to throw warnings about juggler placement...
              (λ (s) (send this set-error s))))
      
      (define ed-win (instantiate editor-window% (this)))
      
      (define pattern-forms (instantiate pattern-forms% (control-panel this)))
      (instantiate-view-controls canvas control-panel this)
      
      (define/public (set-error e)
        (set-status-text e))
      
      (define/public (clear-error)
        (set-status-text ""))
      
      (define mb (new menu-bar% [parent this]))
      
      (define show-editor #f)
      
      (define/public (editor-closed)
        (set! show-editor #f)
        (send mi-editor check #f))
      
      (define m-view (new menu% [label "&View"] [parent mb]))
      (define mi-editor (instantiate checkable-menu-item% 
                          ("&Editor Window"       
                           m-view
                           (λ _ 
                             (set! show-editor (not show-editor))
                             (send ed-win show show-editor)
                             (send mi-editor check show-editor)))
                          (checked show-editor)))
      
      (define show-butt-collisions #f)
      (send canvas set-butt-collisions #f)
      
      (define mi-butt-collisions (instantiate checkable-menu-item%
                                   ("Butt proximity warnings"
                                    m-view
                                    (λ _
                                      (set! show-butt-collisions (not show-butt-collisions))
                                      (send canvas set-butt-collisions show-butt-collisions)
                                      (send mi-butt-collisions check show-butt-collisions)))
                                   (checked show-butt-collisions)))))
  
  (define editor-window%
    (class* frame% ()
      (inherit show)
      (init-field main-window)
      (super-instantiate ("Pattern Editor" #f) (enabled #t) (width 600))
      
      (define/augment (on-close)
        (send main-window editor-closed))
      
      (define values-row (instantiate horizontal-panel% (this)))
      (define input-beat (instantiate text-field% ("Beat length" values-row) 
                           (init-value  "0.32") (min-width 60) (stretchable-width #f)))
      (define input-dwell (instantiate text-field% ("Dwell length" values-row) 
                            (init-value  "0.28") (min-width 60) (stretchable-width #f)))
      (define hold-beats (instantiate text-field% ("Hold Beats (max)" values-row) 
                           (init-value  "2") (min-width 60) (stretchable-width #f)))
      
      (define/public (edit-pattern p j b d h)
        (send input-beat set-value (number->string b))
        (send input-dwell set-value (number->string d))
        (send hold-beats set-value (number->string h))
        (send juggler-t erase)
        (send juggler-t insert j 0)
        (send pattern-t erase)
        (send pattern-t insert p 0))
      
      (instantiate button% 
        ("Run" values-row (λ _ 
                            (with-handlers ((exn:fail? (λ (e) (send main-window set-error (exn-message e)))))
                              (let*   
                                  ((hands (eval-string (send juggler-t get-text)))
                                   (beat-value (string->number (send input-beat get-value)))
                                   (dwell-value (string->number (send input-dwell get-value)))
                                   (sexp-pattern (eval-string (send pattern-t get-text)))
                                   (pattern (sexp->pattern sexp-pattern beat-value dwell-value hands (string->number (send hold-beats get-value)))))
                                (send main-window show-pattern pattern hands)))))
        (stretchable-width #f))
      
      (define juggler-ec (new editor-canvas% [parent this] [line-count 4]))
      (define juggler-t (new text%))
      (send juggler-ec set-editor juggler-t)
      
      (define pattern-ec (new editor-canvas% [parent this] [line-count 20]))
      (define pattern-t (new text%))
      (send pattern-ec set-editor pattern-t)
      
      (define mb (new menu-bar% [parent this]))
      
      (define m-edit (new menu% [label "&Edit"] [parent mb]))
      (append-editor-operation-menu-items m-edit #t)
      
      (define m-scheme (new menu% [label "&Scheme"] [parent mb]))
      (instantiate menu-item% 
        ("&Eval and replace"       
         m-scheme
         (λ _ 
           (let*
               ((active-editor (send (cond ((send juggler-ec has-focus?) juggler-ec)
                                           (#t pattern-ec)) get-editor))
                (sel-start (send active-editor get-start-position))
                (sel-end (send active-editor get-end-position))
                
                (result-string
                 (format "~n~a" 
                         (pretty-format (print-convert 
                                         (eval-string (send active-editor get-text sel-start sel-end)))))))
             (send active-editor insert result-string sel-end)
             ; ... will this *always* work?
             (send active-editor insert ")" sel-end)
             (send active-editor insert "#;(" sel-start)))))))
  
  ; Tab panels don't automatically change panels when clicked... You have to rig it up yourself. Awesome.
  (define pattern-forms% 
    (class* tab-panel% ()
      (inherit get-selection add-child delete-child)
      (init-field w)
      (init-field window)
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
        (instantiate siteswap-form% (this window)))
      
      (define current-form siteswap-form)
      
      (define scheme-form
        (instantiate scheme-form% (this window)))
      
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
      (init-field hold-beats-thunk)
      (init-field hands-lambda) ; thunk -> hands
      (init-field juggling-window) ; show juggling-window show-pattern...
      (init-field examples-list) ; for input combo
      (init-field parent)
      
      (super-instantiate (parent) (alignment '(center center)) (stretchable-width #t))
      
      (define input-pattern (instantiate combo-field% (name examples-list this) (min-width 250) 
                              (init-value initial-pattern) (stretchable-width #t)))
      (define input-beat (instantiate text-field% ("" this) 
                           (init-value  initial-beat) (min-width 60) (stretchable-width #f)))
      (define input-dwell (instantiate text-field% ("" this) 
                            (init-value  initial-dwell) (min-width 60) (stretchable-width #f)))
      
      (instantiate button% 
        ("Run" this (λ _ 
                      (with-handlers ((exn:fail? (λ (e) (send juggling-window set-error (exn-message e)))))
                        (let*   
                            ((beat-value (string->number (send input-beat get-value)))
                             (dwell-value (string->number (send input-dwell get-value)))
                             (sexp-pattern (pattern-lambda (send input-pattern get-value)))
                             (pattern (sexp->pattern sexp-pattern beat-value dwell-value (hands-lambda) (hold-beats-thunk))))
                          (send juggling-window show-pattern
                                pattern
                                (hands-lambda))))))
        (stretchable-width #f))))
  
  (define siteswap-form% 
    (class* vertical-panel% ()
      (init-field parent)
      (init-field window)
      (super-instantiate (parent) (alignment '(center center)) (stretchable-height #f))
      (instantiate pattern-line% ("Siteswap" "0.25" "0.16" "744" 2hss->sexp (λ _ 2) 
                                             (λ _ pair-of-hands) window 2-ss-examples this))
      (instantiate pattern-line% ("4-hand SS" "0.15" "0.2" "966" 4hss->sexp (λ _ 4) 
                                              (λ _ pair-of-jugglers) window 4-hand-examples this))
      (instantiate pattern-line% ("Synchronous" "0.25" "0.20" "(6x,4)*" sync-ss->sexp (λ _ 2) 
                                                (λ _ (juggler-circle 2 3.0)) window syncss-examples this))))
  
  ; For now, the stuff in the evals can see/do everything
  (define-namespace-anchor nsa)  
  (define eval-namespace (namespace-anchor->namespace nsa))
  (define (eval-string s)
    (eval (call-with-input-string s read) eval-namespace))
  
  (define scheme-form% 
    (class* vertical-panel% ()
      (init-field parent)
      (init-field window)
      (super-instantiate (parent) (alignment '(center center)) (stretchable-height #f))
      
      (define hands-select (instantiate combo-field% ("Juggler/Hand List" hands-examples this) (min-width 250) (init-value "") (stretchable-width #t)))
      (define (get-hands)
        (eval-string (send hands-select get-value)))
      
      (define hold-length #f)
      (define sexp-line (instantiate pattern-line% ("Scheme List" "0.35" "0.3" "" 
                                                                  (λ (s) (eval-string s)) 
                                                                  (λ _ (string->number (send hold-length get-value)))
                                                                  get-hands window sexp-examples this)))
      (set! hold-length  (instantiate text-field% ("H" sexp-line) (init-value "2")))
      
      (instantiate pattern-line% ("6-hand SS" "0.10" "0.2" "a" 6hss->sexp (λ _ 6) get-hands window 6-ss-examples this))
      
      
      (instantiate pattern-line% ("Passing SS" "0.28" "0.20" "<3p 3 3|3p 3 3>" passing-ss->sexp (λ _ 2) 
                                               get-hands window passing-ss-examples this))))
  
  (define (instantiate-view-controls c h window)
    (let ((v (instantiate vertical-panel% (h) (alignment '(center center)) (stretchable-width #f) (min-width 200))))
      (let ((h (instantiate horizontal-panel% (v)
                 (alignment '(center center)))))
        (instantiate button% ("+" h (λ _ (send c zoom-in)))
          (stretchable-width #t))
        (instantiate button% ("-" h (λ _ (send c zoom-out)))
          (stretchable-width #t)))
      (instantiate combo-field% ("Object" (list "ball" "ring" "club") v) 
        (callback 
         (λ (l v) 
           (with-handlers ((exn:fail? (λ (e) (send window set-error (exn-message e)))))
             (send c set-model 
                   (send l get-value))))))
      (let* ((h-time (instantiate horizontal-panel% (v) (alignment '(center center)) (stretchable-width #f)))
             (time-input (instantiate text-field% ("Time Scale" h-time) (min-width 100) (init-value "1.0") (stretchable-width #t))))              
        (instantiate button% ("Set" h-time (λ _ 
                                             (with-handlers ((exn:fail? (λ _ 'flagrant-error)))
                                               (send c scale-time (call-with-input-string (send time-input get-value) read)))))
          (stretchable-width #f)))))
  
  (define w (make-object main-window))
  (send w show #t))