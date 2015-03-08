(require (lib "trace.ss"))

;; Exercitiul 1

;; stack

(define (putere-stack x y) 
  (if (= y 0)
      1
      (* x (putere-stack x (- y 1)))))

(putere-stack 2 3)
(putere-stack 4 2)

;; tail recursion

(define (putere-tail x y)
  (putere-tail-acc x y 1))

(define (putere-tail-acc x y acc)
  (if (= y 0)
      acc
      (putere-tail-acc x (- y 1) (* acc x))))

(putere-tail 2 3)
(putere-tail 4 2)

;; Exercitiul 2

;; stack

(define (replace-stack lista x y)
  (cond
    ((null? lista) '())
    ((= (car lista) x) (cons y (replace-stack (cdr lista) x y)))
    (else (cons (car lista) (replace-stack (cdr lista) x y)))))

(replace-stack '(3 2 4 5 3) 3 6) 

;; tail

(define (replace-tail lista x y)
  (replace-tail-acc lista x y '()))

(define (replace-tail-acc lista x y acc)
  (cond
    ((null? lista) (reverse acc))
    ((= (car lista) x) (replace-tail-acc (cdr lista) x y (cons y acc)))
    (else (replace-tail-acc (cdr lista) x y (cons (car lista) acc)))))

(replace-tail '(3 2 4 5 3) 3 6)

;; Exercitiul 3

;; stack
(define (intersect-stack lista1 lista2)
  (cond
    ((null? lista1) '())
    ((member (car lista1) lista2) (cons (car lista1) (intersect-stack (cdr lista1) lista2)))
    (else (intersect-stack (cdr lista1) lista2))))

(intersect-stack '(1 2 3 4) '(5 6 1 2 7))

;; tail
(define (intersect-tail lista1 lista2)
  (intersect-tail-acc lista1 lista2 '()))

(define (intersect-tail-acc lista1 lista2 acc)
  (cond
    ((null? lista1) (reverse acc))
    ((member (car lista1) lista2) (intersect-tail-acc (cdr lista1) lista2 (cons (car lista1) acc)))
    (else (intersect-tail-acc (cdr lista1) lista2 acc))))

(intersect-tail '(1 2 3 4) '(5 6 1 2 7))

;; Exercitiul 4
;; Lista numerelor prime <= n

(define (is-prime? n)
  (if (< n 2) #f (is-prime-helper? n (sqrt n) 2)))

(define (is-prime-helper? n div_max div)
  (cond
    ((> div div_max) #t)
    ((= (modulo n div) 0) #f)
    (else (is-prime-helper? n div_max (+ div 1)))))

(define (primes n)
  (if (= n 1) '() 
      (if (is-prime? n) (cons n (primes (- n 1))) (primes (- n 1)))))

;(trace is-prime-helper?)
;(is-prime-helper? 28 4 2)
(primes 17)

;; Exercitiul 5
;; Descompunerea in factori primi ai unui nr natural.

(define (desc-factor n)
  (if (= n 1) (cons 1 1) (desc-factor-helper n 2)))

(define (desc-factor-helper n div)
  (let ((pereche (divizor n div)))
  (cond
    ((> div n) '())
    ((= (modulo n div) 0) (append (list (divizor n div)) (desc-factor-helper (quotient n (putere-stack (car pereche) (cdr pereche))) (+ div 1))))
    (else (desc-factor-helper n (+ div 1))))))

(define (divizor n div)
  (cons div (divizor-helper n div)))

(define (divizor-helper n div)
  (cond
    ((not (= (modulo n div) 0)) 0)
    (else (+ 1 (divizor-helper (quotient n div) div)))))

(desc-factor 12)

;; Exercitiul 6
(define (perfecte n)
  (cond
    ((< n 6) '())
    ((is-perfect? n) (cons n (perfecte (- n 1))))
    (else (perfecte (- n 1)))))

(define (is-perfect? n)
  (eq? n (apply + (divisor-list n))))

(define (divisor-list n)
  (divisor-list-helper n 1))

(define (divisor-list-helper n div)
  (cond
    ((= div n) '())
    ((= (modulo n div) 0) (cons div (divisor-list-helper n (+ div 1))))
    (else (divisor-list-helper n (+ div 1)))))

(perfecte 8200)

;; Exercitiul 7
;; Liniarizarea unei liste

(define (liniarize L)
  (cond
    ((null? L) '())
    ((list? (car L)) (append (liniarize (car L)) (liniarize (cdr L))))
    (else (append (list (car L)) (liniarize (cdr L))))))

(liniarize '(1 (2 3) ((2 3) 4) 2))