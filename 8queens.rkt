;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 8queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; ==========================
; constants

(define WIDTH 3)
(define ROWS
  (build-list WIDTH (lambda (y) (build-list WIDTH (lambda (x) (+ (* WIDTH y) x))))))
(define COLUMNS
  (build-list WIDTH (lambda (y) (build-list WIDTH (lambda (x) (+ y (* WIDTH x)))))))
(define (construct-diagonals n op c0)
  ; N [Number Number] -> Number N -> [ListOf [ListOf N]]
  ; constructs the pos- and neg-sloped diagonals
  (build-list
   (- (* 2 n) 1)
   (lambda (z)
     (foldr append '()
            (build-list
             n
             (lambda (y)
               (filter (lambda (r) (>= r 0))
                       (build-list
                        n
                        (lambda (x)
                          (if (= (op x y) (- z c0)) (+ (* y n) x) -1))))))))))
(define DIAGONALS+ (construct-diagonals WIDTH + 0))
(define DIAGONALS- (construct-diagonals WIDTH - (- WIDTH 1)))
(define ZONES (append ROWS COLUMNS DIAGONALS+ DIAGONALS-))
(define ITERATOR (build-list (sqr WIDTH) identity))



; =======================
; checks

(check-expect (construct-diagonals 3 + 0)  '((0) (1 3) (2 4 6) (5 7) (8)))
(check-expect (construct-diagonals 3 - 2)  '((6) (3 7) (0 4 8) (1 5) (2)))