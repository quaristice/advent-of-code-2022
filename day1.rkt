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

(foldl (lambda (l cur_max) (max cur_max (foldl + 0 l))) -1 calories) ; part 1
(foldl + 0 (foldl (lambda (l cur_maxes) (cdr (sort (append cur_maxes (list (foldl + 0 l))) <))) '(-1 -1 -1) calories)) ; part 2
