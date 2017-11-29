#lang racket

;question 1
(define first  (lambda (x) (car x)))
(define second (lambda (x) (cadr x)))
(define third  (lambda (x) (caddr x)))
(define fourth (lambda (x) (cadddr x)))
(define fifth  (lambda (x) (car(cddddr x))))
(define rest   (lambda (x) (cdr x)))
(define family '(sam andy melissa dathan tejas))
(first family)
(second family)
(third family)
(fourth family)
(fifth family)

;question 2
(define boolList (list #t #f #t #t #t #f #f #t))
(define (truecount aList)
  (length (filter identity aList)))
(truecount boolList)

;question 3
(define sqrlist (list 1 2 3 4 5 6))
(define (squarelist alist)
  (map (lambda (x) (* x x)) alist))
(squarelist sqrlist)

;question 4
(define filtlist (list 222 12 133 198 111))
(define (hundreds? alist)
  (filter (lambda (x) (> x 100)) alist))
(hundreds? filtlist)

;question 5
(define (collatz n)
  (display n)(newline)
  (cond ((eq? n 1) (display "end")(newline))
        ((even? n) (collatz (/ n 2)))
        ((odd? n) (collatz (+(* 3 n) 1)))))
(collatz 31)