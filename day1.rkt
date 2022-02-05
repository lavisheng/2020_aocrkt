#lang racket
(define in (open-input-file "input/day1.txt" #:mode 'text))
(define test (file->string "input/day1.txt"))
(define in-lst (string-split test "\n"))
(define test-lst '("1721"
"979"
"366"
"299"
"675"
"1456"))
(define (sol1 lst)
  (let ([x (foldr (λ (a final)
                    (let ([anum (string->number a)])
                      (match final
                        [(list (? list? c) (? list? d))
                         (match (member anum d)
                           ; update with new number
                           [#f (list (append c (list anum)) (append d (list (- 2020 anum))))]
                           ; found solution
                           [_ (cons anum (- 2020 anum))]
                           )]
                        ; not list/list we ignore and just continue
                        [_ final])
                      ))
                  ; two lists, one for number one for 2020 - number
                  '((list ) (list ))
                  lst)])
    ; get the two numbers to multiply with
    (* (car x) (cdr x))))
(define (sol2 lst)
  (for ([a lst])
    (for ([b lst])
      (for ([c lst]
            #:when (= 2020 (+ (string->number a) (string->number b) (string->number c))))
   
    (displayln (* (string->number a) (string->number b) (string->number c)))))))
