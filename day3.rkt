#lang racket
(define in (file->string "input/day3.txt"))
(define in-lst (map string->list (string-split in "\n")))
(define test
"..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")
(define test-lst (map string->list (string-split test "\n")))

(define (sol1 lst)
  ; define width for ease later
  (let* ([width (length (list-ref lst 0))]
         [result (first
                  (foldl (λ (row out)
                           (let* ([x (second out)]
                                 [xnew (modulo (+ 3 x) width)])
                             (if (eq? #\# (list-ref row (modulo x width)))
                                 (list (add1 (first out)) xnew)
                                 (list (first out) xnew))
                           ))
                         ; (sum, x position)
                         (list 0 0)
                         lst)
                  )
                 ])
    result
    )
  )
; traversal helper for each of the deltas
(define (traverse delta lst width)
  ; fold
  (first (foldl (λ (row out)
                  (let ([y (third out)])
                    ; make sure on right y
                    (if (= 0 (modulo y (second delta)))
                        ; same as sol 1 code except with y updates now too
                        (let* ([x (second out)]
                               [xnew (modulo (+ (first delta) x) width)]
                               [ynew (+ 1 y)])
                          (if (eq? #\# (list-ref row (modulo x width)))
                              (list (add1 (first out)) xnew ynew)
                              (list (first out) xnew ynew)))
                        ; if on invalid y just increement
                        (list (first out) (second out) (add1 y))
                        )
                    ))
                ; (sum, x, y)
                (list 0 0 0)
                lst)))

(define (sol2 lst)
  (let* ([width (length (list-ref lst 0))]
         ; the deltas to check
         [deltas (list (list 1 1) (list 3 1) (list 5 1) (list 7 1) (list 1 2))])
    ; fold over the deltas and multiply for solution
    (foldl (λ (in mul)
             (* mul (traverse in lst width)))
           1 deltas)))
    
  