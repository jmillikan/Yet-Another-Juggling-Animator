(module fourhss-converter racket
  (require srfi/1 "sexp-converter.ss" "pattern-utilities.ss")
  
  (provide 6hss->sexp 4hss->sexp 2hss->sexp sync-ss->sexp passing-ss->sexp)
  
  ; Changes 0-9a-z siteswap strings into a list of throw lengths
  (define (string->throw-list s)
    (map throw-length (string->list (regexp-replace* #px"\\s" s ""))))
  
  (define (2hss->sexp s) (ss->sexp s 2))
  
  (define (4hss->sexp s) (ss->sexp s 4))
  
  (define (6hss->sexp s) (ss->sexp s 6))

  ;; Viewing string s as a hand-count hand siteswap with hands in rr...ll... order, convert
  ;; to sexp with hands in rlrl... order.
  (define (ss->sexp s hand-count)
    (let* ((first-left (ceiling (/ hand-count 2)))
           ;; for n hands, repeat pattern as a list n times
           (full-throw-list (apply append (make-list hand-count (string->throw-list s)))))
       (for/list 
         ([beat (in-naturals 0)]
          [throw full-throw-list])
          (let* 
              ((throw-hand (remainder beat hand-count))
               (dest-hand (remainder (+ throw-hand throw) hand-count))
               ; e.g. 3 for 6-hand, 2 for 4-hand, 4 for 7-hand?
               (sexp-throw (if (zero? throw) 
                               '- 
                               ; "fix" the fact that n-hand ss goes rrr...lll...
                               ; where sexps go rlrlrl
                               ; Odd hands will be an extra right. Thus the mess with first-left.
                               (list throw
                                     (if (< dest-hand first-left)
                                         #;(right) (* dest-hand 2)
                                         #;(left) (+ 1 (* 2 (- dest-hand first-left))))))))
            ;;  Make a big empty row with one throw in the correct spot
            (for/list 
                ([hand [in-range 0 hand-count]])
              (if (= throw-hand 
                     (if (even? hand)
                         #;(right) (/ hand 2)
                         #;(left) (+ first-left (floor (/ hand 2)))))
                  sexp-throw '-))))))
  
    (define (sync-ss->sexp s)
      (let* ((star? (regexp-match? #px"\\*\\s*$" s))
             ; I can't figure out how to get a list of all capture matches
             (beat-strings (map
                            (λ (b) (cadr (regexp-match #rx"\\(([^)]*)\\)" b)))
                            (regexp-match* #rx"\\([^)]*\\)"  s))))
        (append (if star? '(*) '())
                ; The concatenate/zip/circular list '() business inserts empty beats.
                (concatenate
                 (zip (map 
                     (λ (bs) 
                       (let [(throw-strings (regexp-split #px"," bs))]
                         (map convert-sync-throw throw-strings
                              (iota (length throw-strings)))))
                     beat-strings)
                      (circular-list '()))))))
  
  
  ; Translate a sync throw
  ; <throw height><crossing><pass/pass destination>
  ; to a sexp throw (list <height> <hand>)
  (define (convert-sync-throw throw hand)
    ; crossing is whether "x" was indicated, not whether 
    (let*
        ([height (string->number (car (regexp-match #px"[0-9]+" throw)))]
         [crossing? (regexp-match? #px"x" throw)]
         [pass? (regexp-match? #px"p" throw)])
      (if (zero? height) '-
          (list height 
                ; Determine receiver (0 or 2)
                ; This is correct but muddled.
                (let ((reciever-same-hand 
                       (if pass? (remainder (+ hand 2) 4) hand)))
                  (if (or 
                       (and (not pass?) crossing?)
                       (and (not crossing?) pass?))
                      (if (even? reciever-same-hand)
                          (add1 reciever-same-hand)
                          (sub1 reciever-same-hand))
                      reciever-same-hand))))))
  
  ; This whole thing is garbage. For my troubles, though, it interprets a big enough
  ; subset of passing ss to handle some easy sync patterns.
  (define (passing-ss-throw-parts s)    
    (let* ((matches (regexp-match #px"^([0-9\\.]+)((?:p[0-9]?)?)(x?)" s))
           (height (string->number (list-ref matches 1)))
           (pass-matches (regexp-match #px"(p)((?:[0-9]+)?)" (list-ref matches 2)))
           (pass? (if pass-matches #t #f))
           (destination (match pass-matches 
                          ((list _ _ (regexp #px"[0-9]+")) (string->number (list-ref pass-matches 2)))
                          (_ 'no-destination)))           
           (crossing? (regexp-match #px"x" (list-ref matches 3))))
      (list height pass? destination crossing?)))
  
  ; for now, these can only be "async hands, right hand first, sync jugglers". sad face.
  ; "No destination" passes go to the opposite juggler in a two juggler setup,
  ; and to the 0th juggler in multiple juggler setups.
  (define (passing-ss->sexp passing-ss)
    (define (parse-passing-ss passing-ss)
      (map (λ (juggler-part)
             (let ((undoubled-juggler
                    (map passing-ss-throw-parts (regexp-split #px"\\s+" juggler-part))))
               (if (odd? (length undoubled-juggler))
                   (append undoubled-juggler undoubled-juggler)
                   undoubled-juggler)))
           (regexp-split #px"\\s*\\|\\s*" 
                         (cadr (regexp-match #px"^\\s*<\\s*(.*)\\s*>\\s*$" passing-ss)))))
    
    ; HOW HIGH do you have to BE to even WRITE a function like that
    (define (unscrew-throws juggler-throws stagger-non1st-jugglers?)
      (map concatenate
       (apply map 
              (cons list 
                    (if stagger-non1st-jugglers?
                        (cons
                         (car juggler-throws)
                         (map reverse ; two-element list...
                              (cdr juggler-throws)))
                        
                        juggler-throws)
                    ; This still just gives us two hand parts arranged by juggler
                    ))))
    
    (define (convert-throws juggler-parts staggered-hands?)
      (for/list ((juggler juggler-parts) (juggler-index (in-range 0 (length juggler-parts))))
        (for/list ((pss-throw juggler) (time (in-range 0 (length juggler))))
          (match-let (((list height pass? destination crossing?) pss-throw))
            (let* ((destination-juggler
                    (if pass?
                        (if (not (eq? destination 'no-destination))
                            (sub1 destination) ; destinations are 1-based
                            (if (= 2 (length juggler-parts))
                                ; Assume opposite juggler
                                (remainder (add1 juggler-index) 2)
                                (error "No destination given for pass"))) ; Assume feeder... Bleh
                        juggler-index))
                   (throw
                    (list height 
                          (+ (* 2 destination-juggler)
                             (remainder
                              (+ (if (odd? time) 0 1)
                                 (if (odd? height) 0 1)
                                 (if (and pass? staggered-hands?) 1 0))
                              2)))))
              (if (odd? time) 
                  (list '- throw)
                  (list throw '-)))))))
    
    (let* ((juggler-parts (parse-passing-ss passing-ss))           
           ; First try: Both jugglers start right handed, sync. Everything is straightforward
           (juggler-throws (convert-throws juggler-parts #f))
           (try-this-sexp (unscrew-throws juggler-throws #f)))
      (with-handlers ((exn:fail? (λ _ 
                                   (let* ((juggler-throws (convert-throws juggler-parts #t))
                                          (try-this-sexp (unscrew-throws juggler-throws #t)))
                                     (begin(sexp->pattern try-this-sexp 0.25 0.20 
                                                          (juggler-circle (length (car try-this-sexp)) 5.0) 2)
                                           try-this-sexp)))))                                            
        (sexp->pattern try-this-sexp 0.25 0.20 (juggler-circle 
                                                (length (car try-this-sexp)) 5.0) 2)
        try-this-sexp)))
  
  (define (throw-length c)
    (let ((i (char->integer c)))
      (cond [(and (>= i 48) (<= i 57)) (- i 48)] ;; 0-9
            [(and (>= i 97) (<= i 122)) (- i 87)] ;; a-z
            [(and (>= i 65) (<= i 90)) (- i 55)] ;; A-Z
            [else (error (string-append "Invalid throw in siteswap: " (string c)))]))))