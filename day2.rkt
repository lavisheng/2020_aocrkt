#lang racket
(define in (file->string "input/day2.txt"))
(define in-lst (string-split in "\n"))
(define test (list 
"1-3 a: abcde"
"1-3 b: cdefg"
"2-9 c: ccccccccc"))
; helper to parse
(define (parse str)
  ; let separates the string into each part
  (let* ([lst (string-split str  ":")]
         ; the character we care about
         [char (string-ref (second (string-split (first lst) " ")) 0)]
         ; lower and upper numbers
         [numbers (map string->number (string-split (first (string-split (first lst) " ")) "-"))]
         ; resultant, password
         [res (list-tail (string->list (second lst)) 1)])
    `(,numbers ,char ,res)))
(define (sol1 lst)
  (foldl (λ (in sum)
           (let* ([parse-res (parse in)]
                  [numbers (first parse-res)]
                  [char (second parse-res)]
                  [pass (third parse-res)]
                  ; count occurences of the char
                  [occ (count (λ (comp) (eq? char comp)) pass)])
             ; check range
             (if (and (<= (first numbers) occ) (>= (second numbers) occ))
                 (add1 sum)
                 sum)
             )
           )
         0
         lst))

(define (sol2 lst)
  (foldl (λ (in sum)
           (let* ([parse-res (parse in)]
                  [numbers (first parse-res)]
                  [char (second parse-res)]
                  [pass (third parse-res)])
             ; use a xor to check, and look in password at lower and upper 
             (if (xor (eq? (list-ref pass (- (first numbers) 1)) char)
                      (eq? (list-ref pass (- (second numbers) 1)) char))
                 (add1 sum)
                 sum)
             )
           )
         0
         lst))
         