#lang racket
(define in (open-input-file "input.txt"))

(define (not-whitespace? char)
  (not (char-whitespace? char)))

(define (char->points c)
  (let ([i (char->integer c)])
  (if (> i 67) (- i 87) (- i 64))))

(define (calc-p1 p)
  (let ([result (modulo (- (second p) (first p)) 3)])
  (cond
    [(= result 0) (+ 3 (second p))]
    [(= result 1) (+ 6 (second p))]
    [(= result 2) (second p)])))

(define (calc-p2 p)
  (cond
    [(= (second p) 1) (+ 1 (modulo (+ (first p) 1) 3))]
    [(= (second p) 2) (+ 3 (first p))]
    [(= (second p) 3) (+ 7 (modulo (first p) 3))]))

(define (parse-input in)
  (let ([line (read-line in)])
    (if (or (eof-object? line) (= (string-length line) 0))
        '()
        (cons (map char->points (filter not-whitespace? (string->list line))) (parse-input in)))))

(define values (parse-input in))

(foldl + 0 (map calc-p1 values)) ; part 1
(foldl + 0 (map calc-p2 values)) ; part 2
