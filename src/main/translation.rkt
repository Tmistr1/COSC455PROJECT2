#lang racket
;define lists
(define chinese '(ling yi er san si wu liu qi ba jiu shi))
(define english '(zero one two three four five six seven eight nine ten))

 ; check if character is in given list
(define (member? x alist)
  (if (null? alist) #f
      (if (eq? x (car alist)) #t
          (member? x (cdr alist)))))

;perform additions recursively
(define (add alist)
  (if
   (null? alist) 0
   (+ (car alist) (add (cdr alist)))))

;perform multiplications recursively
(define (multiply alist)
  (if
   (null? alist) 1
   (* (car alist) (multiply (cdr alist)))))

;display the addition symbvol
(define (showplus alist)
 (display (car alist))
  (map (lambda (x) (display " + ")(display x)) (cdr alist)))

;display the multiplication symbol
(define (showmulti alist)
 (display (car alist))
  (map (lambda (x) (display " * ")(display x)) (cdr alist)))

;show the list!
(define (displist alist)
 (display (car alist))
  (map (lambda (x) (display " ")(display x)) (cdr alist)))

;translate the list
(define (translate alist)
  (cond
    ((null? alist) '())
    ((eq? (member? (car alist) chinese) #t) (cons (index-of chinese (car alist)) (translate (cdr alist))))
    ((eq? (member? (car alist) english) #t) (cons (index-of english (car alist)) (translate (cdr alist))))
    (else (translate (cdr alist)))))

;our main driving function
(define (go alist)
  (display "Translation: ")(displist (translate alist))(newline)
  (display "Addition: ") (showplus (translate alist))(display " = ")(display (add (translate alist)))(newline)
  (display "Multiplication: ")(showmulti (translate alist))(display " = ")(display (multiply (translate alist)))(newline))


(go '(liu seven six shi))
(go '(yi josh three si))