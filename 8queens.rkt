;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 8queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define WIDTH 8)
(define ROWS
  (build-list WIDTH (lambda (y) (build-list WIDTH (lambda (x) (+ (* WIDTH y) x))))))
(define COLUMNS
  (build-list WIDTH (lambda (y) (build-list WIDTH (lambda (x) (+ y (* WIDTH x)))))))
(define DIAGONALS+
  (build-list
   (- (* 2 WIDTH) 1)
   (lambda (z)
     (foldr append '()
            (build-list
             WIDTH
             (lambda (y)
               (filter (lambda (r) (>= r 0))
                       (build-list
                        WIDTH
                        (lambda (x)
                          (if (= (+ x y) z) (+ (* y WIDTH) x) -1))))))))))
(define DIAGONALS-
  (build-list
   (- (* 2 WIDTH) 1)
   (lambda (z)
     (foldr append '()
            (build-list
             WIDTH
             (lambda (y)
               (filter (lambda (r) (>= r 0))
                       (build-list
                        WIDTH
                        (lambda (x)
                          (if (= (- x y 1) (- z WIDTH))
                              (+ (* y WIDTH) x) -1))))))))))
(define ZONES (append ROWS COLUMNS DIAGONALS+ DIAGONALS-))
(define ITERATOR (build-list (sqr WIDTH) identity))


