;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 8queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; ==========================
; constants

(define WIDTH 8)
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
(define EMPTYBOARD (make-list (sqr WIDTH) 0))


; =========================
; functions
#;
(define (solve board)
  ; [ListOf N] -> [ListOf N]
  ; attempts to solve the NQueens problem through
  ; backtracking, depth-first search
  (local (
          (define openings (availability board)))))


(define (availability board)
  ; [ListOf N] -> [ListOf Boolean]
  ; derive the list of possible values where the next queen can go
  (local (
          (define (rastor n)
            ; N [ListOf N] -> Boolean
            ; determines if a queen can slot into a particular square
            (cond
              [(= (read-square n board) 1) #f]
              [else
               (local (
                       (define exclusion-zone
                         (list-to-set (foldr append '()
                                             (filter (λ (z) (member? n z)) ZONES)))))
                 ; - IN -
                 (andmap (λ (sq) (= (read-square sq board) 0))
                         exclusion-zone))])))
    ; - IN -
    (map rastor ITERATOR)))


(define (read-square n board)
  ; N [ListOf N] -> N
  ; retrieves the value of the square at position n
  (cond
    [(= 0 n) (first board)]
    [else (read-square (sub1 n) (rest board))]))


(define (list-to-set lst)
  ; [ListOf X] -> [ListOf X]
  ; converts a list into a set
  (cond
    [(empty? lst) '()]
    [(member? (first lst) (rest lst)) (list-to-set (rest lst))]
    [else (cons (first lst) (list-to-set (rest lst)))]))



; =======================
; checks

(define q1 (cons 1 (rest EMPTYBOARD)))
(check-expect (construct-diagonals 3 + 0)  '((0) (1 3) (2 4 6) (5 7) (8)))
(check-expect (construct-diagonals 3 - 2)  '((6) (3 7) (0 4 8) (1 5) (2)))
(check-expect (availability q1) (list #f #f #f #f #f #f #f #f
                                      #f #f #t #t #t #t #t #t
                                      #f #t #f #t #t #t #t #t
                                      #f #t #t #f #t #t #t #t
                                      #f #t #t #t #f #t #t #t
                                      #f #t #t #t #t #f #t #t
                                      #f #t #t #t #t #t #f #t
                                      #f #t #t #t #t #t #t #f))