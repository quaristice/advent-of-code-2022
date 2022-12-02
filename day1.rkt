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

(foldl (lambda (l cur_max) (max cur_max (foldl + 0 l))) -1 (parse-elves in))
