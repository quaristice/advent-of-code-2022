#lang racket
(define in (open-input-file "input.txt"))

(define (parse-elf in)
  (let ([line (read-line in)])
    (if (or (eof-object? line) (= (string-length line) 0))
        '()
        (cons (string->number line) (parse-elf in)))))
(define (parse-elves in)
  (let ([elfinv (parse-elf in)])
    (if (empty? elfinv)
        '()
        (cons elfinv (parse-elves in)))))

(define calories (parse-elves in))

(define (sum l) (foldl + 0 l))

(foldl (lambda (l cur_max) (max cur_max (sum l))) -1 calories) ; part 1
(sum (foldl (lambda (l cur_maxes) (cdr (sort (cons (sum l) cur_maxes) <))) '(-1 -1 -1) calories)) ; part 2
