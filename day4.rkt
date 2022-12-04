#lang racket
(define in (open-input-file "input.txt"))

(define (parse-line l)
  (map (lambda (x) (map string->number (string-split x "-"))) (string-split l ",")))

(define (check-full-overlap p) ; this sucks
  (or
   (and
    (>= (caar p) (caadr p))
    (<= (cadar p) (cadadr p)))
   (and
    (>= (caadr p) (caar p))
    (<= (cadadr p) (cadar p)))))

(define (check-any-overlap p) ; this also sucks
  (or
   (or
    (and (>= (caar p) (caadr p)) (<= (caar p) (cadadr p)))
    (and (>= (cadar p) (caadr p)) (<= (cadar p) (cadadr p))))
   (or
    (and (>= (caadr p) (caar p)) (<= (caadr p) (cadar p)))
    (and (>= (cadadr p) (caar p)) (<= (cadadr p) (cadar p))))))

(define (parse-input in)
  (let ([line (read-line in)])
    (if (or (eof-object? line) (= (string-length line) 0))
        '()
        (cons (parse-line line) (parse-input in)))))



(define pairs (parse-input in))

(foldl + 0 (map (lambda (x) (if (check-full-overlap x) 1 0)) pairs)) ; part 1
(foldl + 0 (map (lambda (x) (if (check-any-overlap x) 1 0)) pairs)) ; part 2