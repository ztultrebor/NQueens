;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 8queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)



; =========================
; functions

(define (solve board)
  ; [ListOf N] -> [ListOf N]
  ; attempts to solve the NQueens problem through
  ; backtracking, depth-first search
  (local (
          (define opportunities
            (sort (shuffle (availability board)) options<?)))
    (place-queen opportunities board)))


(define (place-queen opps board)
  ; [ListOf N] [ListOf N] -> [ListOf N]
  ; coordinates the backtracking aspects by placing queens at squares
  ; one-by-one such that search stops as soon as a solution
  ; has been identified
  (local (
          (define (stick n board)
            ; N [ListOf N] -> N
            ; sticks a queen at square n
            (cond
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


(define (valid? n board)
  ; N [ListOf N] -> Boolean
  ; determines if a queen can slot into a particular square
  (andmap (λ (sq) (= (@index sq board) 0)) (@index n THREATS)))


(define (availability board)
  ; [ListOf N] -> [ListOf Boolean]
  ; derive the list of possible values where the next queen can go
  (local (
          (define truth (map (λ (sq) (valid? sq board)) ITERATOR)))
    ; - IN -
    (filter (λ (n) (@index n truth)) ITERATOR)))


(define (@index n lst)
  ; N [ListOf N] -> N
  ; retrieves the value of the square at position n
  (cond
    [(empty? lst) #f]
    [(= 0 n) (first lst)]
    [else (@index (sub1 n) (rest lst))]))


(define (list->set lst)
  ; [ListOf X] -> [ListOf X]
  ; converts a list into a set
  (cond
    [(empty? lst) '()]
    [(member? (first lst) (rest lst)) (list->set (rest lst))]
    [else (cons (first lst) (list->set (rest lst)))]))


(define (shuffle lst)
  ; [ListOf X] -> [ListOf X]
  ; shuffle a list of X in the most efficien manner
  (local (
          (define-struct shufflor [rand ord orig])
          (define ini-shf (make-shufflor '() '() lst))
          (define (randomize shf)
            ;Shufflor -> [ListOf X]
            (local (
                    (define rnd (shufflor-rand shf))
                    (define ordr (shufflor-ord shf))
                    (define og (shufflor-orig shf))
                    (define (take n s)
                      ; N Shufflor -> Shufflor
                      (local (
                              (define r (shufflor-rand s))
                              (define d (shufflor-ord s))
                              (define o (shufflor-orig s)))
                        (cond
                          [(= n 0) (randomize (make-shufflor
                                               (cons (first o) r)
                                               '() (append d (rest o))))]
                          [else (take (sub1 n)
                                      (make-shufflor
                                       r (cons (first o) d) (rest o)))]))))
              ; - IN -              
              (cond
                [(and (empty? ordr) (empty? og)) rnd]
                [(or (empty? og) (empty? (rest og)))
                 (randomize (make-shufflor (cons (first og) rnd) '() ordr))]
                [else (take (random (length og)) shf)]))))
    ; - IN -
    (randomize ini-shf)))


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


(define (display-square n)
  ; N -> Img
  ; generates a visually appealing image of a sudoku square
  (overlay (circle (/ TEXTSIZE 3) "solid" (if (= n 0) "white" "black")) SQUARE))


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



; =======================
; constants

(define WIDTH 9)
(define ROWS
  (build-list WIDTH
              (lambda (y) (build-list WIDTH (lambda (x) (+ (* WIDTH y) x))))))
(define COLUMNS
  (build-list WIDTH
              (lambda (y) (build-list WIDTH (lambda (x) (+ y (* WIDTH x)))))))
(define DIAGONALS+ (construct-diagonals WIDTH + 0))
(define DIAGONALS- (construct-diagonals WIDTH - (- WIDTH 1)))
(define ZONES (append ROWS COLUMNS DIAGONALS+ DIAGONALS-))
(define ITERATOR (build-list (sqr WIDTH) identity))
(define THREATS
  (map (λ (n) (list->set
               (foldr append '()
                      (filter (λ (z) (member? n z)) ZONES))))
       ITERATOR))
(define EMPTYBOARD (make-list (sqr WIDTH) 0))
(define TEXTSIZE 24)
(define SQUARE (overlay
                (rectangle TEXTSIZE TEXTSIZE "outline" "black")
                (rectangle TEXTSIZE TEXTSIZE "solid" "white")))
(define BLANKTANGLE (rectangle 0 0 "solid" "white"))


(define (options<? x y)
  ; N N -> Boolean
  ; #t if a queen at x threatens fewer squares than one at y, else #f
  (< (length (@index x THREATS)) (length (@index y THREATS))))



; =======================
; checks

(define q1 (cons 1 (rest (make-list (sqr 4) 0))))
(check-expect (construct-diagonals 3 + 0)  '((0) (1 3) (2 4 6) (5 7) (8)))
(check-expect (construct-diagonals 3 - 2)  '((6) (3 7) (0 4 8) (1 5) (2)))
(check-expect (availability EMPTYBOARD) ITERATOR)
(check-expect (@index 3 '(5 4 3 2 1)) 2)


; ========================
; action!

(display (solve EMPTYBOARD))