#lang racket
(define in (open-input-file "input.txt"))

(define (parse-input in)
  (let ([line (read-line in)])
    (if (or (eof-object? line) (= (string-length line) 0))
        '()
        (cons (string->list line) (parse-input in)))))

(define (find-repeat r)
  (let-values ([(fh sh) (split-at r (/ (length r) 2))])
    (car (filter (lambda (x) (and (member x fh) (member x sh))) r))))

(define (char->priority c)
  (let ([i (char->integer c)])
    (if (> i 96) (- i 96) (- i 38))))

(define (find-badge rs)
  (car (filter (lambda (x) (and (member x (cadr rs)) (member x (caddr rs)))) (car rs))))

(define (badge-priorities v)
  (if (empty? v)
      0
      (+ (char->priority (find-badge (drop-right v (- (length v) 3))))
         (badge-priorities (take-right v (- (length v) 3))))))

(define values (parse-input in))
(foldl + 0 (map char->priority (map find-repeat values))) ; part 1
(badge-priorities values) ; part 2