;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 8queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)



; ==========================
; constants

; !!! this stuff should not be sitting at global scope!
(define WIDTH 7)
(define ROWS
  (build-list WIDTH (lambda (y) (build-list WIDTH (lambda (x) (+ (* WIDTH y) x))))))
(define COLUMNS
  (build-list WIDTH (lambda (y) (build-list WIDTH (lambda (x) (+ y (* WIDTH x)))))))
(define (construct-diagonals n op c0)
  ; N [Number Number -> Number] N -> [ListOf [ListOf N]]
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
(define TEXTSIZE 24)
(define SQUARE (rectangle TEXTSIZE TEXTSIZE "solid" "white"))
(define BLANKTANGLE (rectangle 0 0 "solid" "white"))


; =========================
; functions

(define (place-queen opps board)
  ; [ListOf N] [ListOf N] -> [ListOf N]
  ; coordinates the backtracking aspects by trying squares
  ; one-by-one such that search stops as soon as a solution
  ; has been identified
  (local (
          (define (stick n board)
            ; N [ListOf N] -> N
            ; sticks a queen at square n
            (cond
              ;[(empty? board) '()]
              [(= n 0) (cons 1 (rest board))]
              [else (cons (first board) (stick (sub1 n) (rest board)))])))
    ; - IN -
    (cond
      [(= (foldr + 0 board) WIDTH) board]
      [(empty? opps) #f]
      [else (local (
                    (define attempted-solution
                      (solve (stick (first opps) board))))
              (if (false? attempted-solution)
                  (place-queen (rest opps) board)
                  attempted-solution))])))


(define (solve board)
  ; [ListOf N] -> [ListOf N]
  ; attempts to solve the NQueens problem through
  ; backtracking, depth-first search
  (local (
          (define opportunities (availability board)))
          
    (place-queen opportunities board)))


(define (valid? n board)
  ; N [ListOf N] -> Boolean
  ; determines if a queen can slot into a particular square
  (andmap (λ (sq) (= (read-square sq board) 0)) (qrange n)))


(define (availability board)
  ; [ListOf N] -> [ListOf Boolean]
  ; derive the list of possible values where the next queen can go
  (local (
          (define truth (map (λ (sq) (valid? sq board)) ITERATOR)))
    ; - IN -
    (filter (λ (n) (read-square n truth)) ITERATOR)))


(define (read-square n board)
  ; N [ListOf N] -> N
  ; retrieves the value of the square at position n
  (cond
    [(empty? board) #f]
    [(= 0 n) (first board)]
    [else (read-square (sub1 n) (rest board))]))


(define (qrange n)
  ; N -> [ListOf N]
  ; returns all squares threatened by a queen at n
  (list-to-set (foldr append '()
                      (filter (λ (z) (member? n z)) ZONES))))


(define (list-to-set lst)
  ; [ListOf X] -> [ListOf X]
  ; converts a list into a set
  (cond
    [(empty? lst) '()]
    [(member? (first lst) (rest lst)) (list-to-set (rest lst))]
    [else (cons (first lst) (list-to-set (rest lst)))]))


;; !!! refactor this
(define (display board)
  ; [ListOf X] -> Img
  ; displays the state of the sudoku board
  (local (
          (define-struct formatter [pref suff])
          (define (get-row rze n)
            (cond
              [(= n 0) rze]
              [else (get-row
                     (make-formatter
                      (cons (first (formatter-suff rze)) (formatter-pref rze))
                      (rest (formatter-suff rze)))
                     (sub1 n))]))
          (define (board->rows rze)
            ; [[ListOf N] [ListOf N] -> [ListOf [ListOf N]]
            ; convers a solved board into n rows of n
            (cond 
              [(empty? (formatter-suff rze)) (list (formatter-pref rze))]
              [else (local (
                            (define stuff (get-row rze WIDTH)))
                      (cons (formatter-pref stuff)
                            (board->rows
                             (make-formatter '() (formatter-suff stuff)))))]))
          (define rows (board->rows (make-formatter '() board))))
    ; - IN -
    (foldr above BLANKTANGLE
           (map (λ (r) (foldl beside BLANKTANGLE
                              (map display-square r))) rows))))


(define (display-square n)
  ; N -> Img
  ; generates a visually appealing image of a sudoku square
  (overlay (text (if (= n 0) "" (number->string n)) TEXTSIZE "green") SQUARE))



(display (solve EMPTYBOARD))



; =======================
; checks

(define q1 (cons 1 (rest (make-list (sqr 4) 0))))
(check-expect (construct-diagonals 3 + 0)  '((0) (1 3) (2 4 6) (5 7) (8)))
(check-expect (construct-diagonals 3 - 2)  '((6) (3 7) (0 4 8) (1 5) (2)))
(check-expect (availability EMPTYBOARD) ITERATOR)