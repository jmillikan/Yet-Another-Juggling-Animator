(module fourhss-converter scheme
    (require srfi/1)
  
  (provide 4hss->sexp 2hss->sexp sync-ss->sexp)
  
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
                      '() full-throw-list (iota (length full-throw-list)))
      ))
  

  
  (define (4hss->sexp s)
    (let* ((ss-cleaned (regexp-replace* #px"\\s" s ""))
      (ss-list (regexp-split #px"" ss-cleaned))      
      (throw-list (map (λ (c) (hash-ref throw-lengths (string-ref c 0))) ss-list))            
      (full-throw-list (if (zero? (remainder (length throw-list) 4)) throw-list (append throw-list throw-list throw-list throw-list))))
      
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
    (let-values (((height crossing?)
                 (cond ((number? throw) (values throw #f))
                       ((regexp-match? #px"[0-9]+x" (symbol->string throw))
                        (values
                         (string->number (regexp-replace #px"([0-9]+)x" (symbol->string throw) "\\1"))
                         #t))
                       ; Add support for pass/crossing pass here later.
                       (#t 'flagrant-error))))
      (if (zero? height)
          '-
          (list height 
                (if crossing? 
                    (if (even? hand)
                        ; Even... Go to hand + 1
                        (add1 hand)
                        ; Odd... Go to hand - 1
                        (sub1 hand))
                    hand)))))
    
                
        
                                                   
        
  
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