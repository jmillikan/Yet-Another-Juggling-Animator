(module fourhss-converter scheme
    (require srfi/1 "sexp-converter.ss" "pattern-utilities.ss")
  
  (provide 6hss->sexp 4hss->sexp 2hss->sexp sync-ss->sexp passing-ss->sexp)
  
  (define (2hss->sexp s)
    (let* ((ss-cleaned (regexp-replace* #px"\\s" s ""))
      (ss-list (regexp-split #px"" ss-cleaned))      
      (throw-list (map (lambda (c) (hash-ref throw-lengths (string-ref c 0))) ss-list))            
      (full-throw-list (if (even? (length throw-list)) throw-list (append throw-list throw-list))))
      (foldr (λ (throw hand lst)
                        (cons
                         (let ((hand (remainder hand 2)))
                           (if (zero? hand) 
                               (list 
                                (if (zero? throw) '-
                                    (list throw (if (even? throw) 0 1))) '-)
                               (list '- 
                                     (if (zero? throw) '- (list throw (if (even? throw) 1 0))))))
                         lst))
                      '() full-throw-list (iota (length full-throw-list)))))
  
  (define (4hss->sexp s)
    (let* ((ss-cleaned (regexp-replace* #px"\\s" s ""))
      (ss-list (regexp-split #px"" ss-cleaned))      
      (throw-list (map (λ (c) (hash-ref throw-lengths (string-ref c 0))) 
                       ss-list))            
      (full-throw-list (if (zero? (remainder (length throw-list) 4)) 
                           throw-list 
                           (append throw-list throw-list throw-list throw-list))))
      
      (foldr (λ (throw hand lst)
                        (cons
                         (let* 
                             ((throw-hand (remainder hand 4))
                              (dest-hand (remainder (+ throw-hand throw) 4))
                              (sexp-throw (if (zero? throw) '- (list throw 
                                                                     (cond 
                                                                       ((= dest-hand 1) 2)
                                                                       ((= dest-hand 2) 1)
                                                                       (#t
                                                                        dest-hand))))))
                           (list
                            (if (= throw-hand 0) sexp-throw '-)
                            (if (= throw-hand 2) sexp-throw '-)
                            (if (= throw-hand 1) sexp-throw '-)
                            (if (= throw-hand 3) sexp-throw '-)))
                         lst))
                      '() full-throw-list (iota (length full-throw-list)))))
  
  ; Ugh, it really is about time to generalize this.
  (define (6hss->sexp s)
    (let* ((ss-cleaned (regexp-replace* #px"\\s" s ""))
      (ss-list (regexp-split #px"" ss-cleaned))      
      (throw-list (map (λ (c) (hash-ref throw-lengths (string-ref c 0)))
                       ss-list))            
      (full-throw-list (if (zero? (remainder (length throw-list) 6)) 
                           throw-list 
                           ; This could be less in special cases, really, the result just needs to be divisible by 6. Primes 5+ are the degenerate cases
                           (append throw-list throw-list throw-list throw-list throw-list throw-list))))
      
      (foldr (λ (throw hand lst)
               (cons
                (let* 
                    ((throw-hand (remainder hand 6))
                     (dest-hand (remainder (+ throw-hand throw) 6))
                     (sexp-throw (if (zero? throw) 
                                     '- 
                                     ; "fix" the fact that 6hand ss goes rrrlll
                                     ; where sexps go rlrlrl
                                     (list throw 
                                           (cond 
                                             ((= dest-hand 0) 0) ; Clarity
                                             ((= dest-hand 1) 2)
                                             ((= dest-hand 2) 4)
                                             ((= dest-hand 3) 1)
                                             ((= dest-hand 4) 3)
                                             ((= dest-hand 5) 5) ; Clarity
                                             (#t dest-hand))))))
                  (list
                   (if (= throw-hand 0) sexp-throw '-)
                   (if (= throw-hand 3) sexp-throw '-)
                   (if (= throw-hand 1) sexp-throw '-)
                   (if (= throw-hand 4) sexp-throw '-)
                   (if (= throw-hand 2) sexp-throw '-)
                   (if (= throw-hand 5) sexp-throw '-)))
                lst))
             '() full-throw-list (iota (length full-throw-list)))))
  
  ; No pass fixes for now...
  ; This gets REALLY messy in convert-sync-throw, and will be worse for passing... 
  ; Maybe a lexer/parser would be better?
  (define (sync-ss->sexp s)
    (let* ((s-no-commas (regexp-replace* #px"," s " "))
           (star? (regexp-match? #px"\\*\\s*$" s-no-commas))
                  ; strips star and encloses so we can read it in as a sexp
           (s-enclosed (regexp-replace #px"^(.*\\))\\*?\\s*$" s-no-commas "(\\1)"))
           (sexp-no-commas (call-with-input-string s-enclosed read))
           )
      (append (if star? '(*) '())
              ; The apply append and '() business inserts the empty beats.
              (apply append (map
                             (λ (beat)
                               (list 
                                (map convert-sync-throw beat (iota (length beat)))
                                '()))
                             sexp-no-commas)))))
  
    
    ; Translate a sync throw
  ; <throw height><crossing><pass/pass destination>
  ; to a sexp throw (list <height> <hand>)
  (define (convert-sync-throw throw hand)
    ; crossing is whether "x" was indicated, not whether 
    (let-values (((height crossing? pass?)
                  (if (number? throw) (values throw #f #f)
                      (values
                       (string->number (regexp-replace #px"^([0-9]+)[px]*$" (symbol->string throw) "\\1"))
                       (regexp-match? #px"x" (symbol->string throw))
                       (regexp-match? #px"p" (symbol->string throw))))))
      (if (zero? height)
          '-
          (list height 
                ; Determine receiver (0 or 2)
                (let ((reciever-same-hand 
                       (if pass? (remainder (+ hand 2) 4) hand)))
                  (if (or 
                       (and (not pass?) crossing?)
                       (and (not crossing?) pass?))
                      (if (even? reciever-same-hand)
                                    (add1 reciever-same-hand)
                                    (sub1 reciever-same-hand))
                      reciever-same-hand))))))
 
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
    
    
    (define (unscrew-throws juggler-throws stagger-non1st-jugglers?)
     (map 
              (λ (beat)
                (apply append beat))
              (apply map 
                     (cons list 
                           (if stagger-non1st-jugglers?
                               (append
                                (list (car juggler-throws))
                                (map (λ (j)
                                       (append (drop j 1)
                                               (take j 1)))
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
                                              (if (and pass? staggered-hands?) 1 0)
                                              )
                                           
                                           2)))))
                           (if (odd? time) 
                               (list '- throw)
                               (list throw '-))
                           )))))
    
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
          
   (define throw-lengths
     #hash((#\0 . 0)
           (#\1 . 1)
          (#\2 . 2)
          (#\3 . 3)
          (#\4 . 4)
          (#\5 . 5)
          (#\6 . 6)
          (#\7 . 7)
          (#\8 . 8)
          (#\9 . 9)
          (#\a . 10)
          (#\b . 11)
          (#\c . 12)
          (#\d . 13)
          (#\e . 14)
          (#\f . 15)
          (#\g . 16)
          (#\h . 17)
          (#\i . 18)
          (#\j . 19)
          (#\k . 20)
          (#\l . 21)
          (#\m . 22)
          (#\n . 23)
          (#\o . 24)
          (#\p . 25)
          (#\q . 26)
          (#\r . 27)
          (#\s . 28)
          (#\t . 29)
          (#\u . 30)
          (#\v . 31)
          (#\w . 32)
          (#\x . 33)
          (#\y . 34)
          (#\z . 35)
          (#\A . 10)
          (#\B . 11)
          (#\C . 12)
          (#\D . 13)
          (#\E . 14)
          (#\F . 15)
          (#\G . 16)
          (#\H . 17)
          (#\I . 18)
          (#\J . 19)
          (#\K . 20)
          (#\L . 21)
          (#\M . 22)
          (#\N . 23)
          (#\O . 24)
          (#\P . 25)
          (#\Q . 26)
          (#\R . 27)
          (#\S . 28)
          (#\T . 29)
          (#\U . 30)
          (#\V . 31)
          (#\W . 32)
          (#\X . 33)
          (#\Y . 34)
          (#\Z . 35)))
  )