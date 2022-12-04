#lang racket
(define in (open-input-file "input.txt"))

(define (parse-line l)
  (map (lambda (x) (map string->number (string-split x "-"))) (string-split l ",")))

(define (check-full-overlap p) ; this sucks
  (let-values ([(p11 p12 p21 p22) (apply values (flatten p))])
  (or
   (and
    (>= p11 p21)
    (<= p21 p22))
   (and
    (>= p21 p11)
    (<= p22 p21)))))

(define (check-any-overlap p) ; this also sucks
  (let-values ([(p11 p12 p21 p22) (apply values (flatten p))])
  (or
   (or
    (and (>= p11 p21) (<= p11 p22))
    (and (>= p12 p22) (<= p12 p22)))
   (or
    (and (>= p21 p11) (<= p21 p12))
    (and (>= p22 p11) (<= p22 p12))))))

(define (parse-input in)
  (let ([line (read-line in)])
    (if (or (eof-object? line) (= (string-length line) 0))
        '()
        (cons (parse-line line) (parse-input in)))))


(define pairs (parse-input in))

(foldl + 0 (map (lambda (x) (if (check-full-overlap x) 1 0)) pairs)) ; part 1
(foldl + 0 (map (lambda (x) (if (check-any-overlap x) 1 0)) pairs)) ; part 2
