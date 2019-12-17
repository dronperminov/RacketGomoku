#lang racket
(require racket/class)
(require racket/gui/base)
(require pict racket/draw)

(define bitmap-blank
  (lambda [[w 0] [h #false] #:backing-scale [backing-scale 2.0]]
    (define width  (max 1 (exact-ceiling w)))
    (define height (max 1 (exact-ceiling (or h w))))
    (make-bitmap width height #:backing-scale backing-scale)))

; честно найденная функция изменения размера изображений
(define bitmap-scale
  (case-lambda
    [(bmp scale)
     (if (= scale 1.0) bmp (bitmap-scale bmp scale scale))]
    [(bmp scale-x scale-y)
     (cond [(and (= scale-x 1.0) (= scale-y 1.0)) bmp]
           [else (let ([w (max 1 (exact-ceiling (* (send bmp get-width) scale-x)))]
                       [h (max 1 (exact-ceiling (* (send bmp get-height) scale-y)))])
                   (define dc (make-object bitmap-dc% (bitmap-blank w h)))
                   (send dc set-smoothing 'aligned)
                   (send dc set-scale scale-x scale-y)
                   (send dc draw-bitmap bmp 0 0)
                   (or (send dc get-bitmap) (bitmap-blank)))])]))

(define my-canvas%
  (class canvas%
    (init-field on-mouse-left-down)
    
    (define/override (on-event event)
      (cond ((and (send event get-left-down) (not (null? on-mouse-left-down))) (on-mouse-left-down this event)))
    )

    (super-new))
)

(define USE-GUI #t) ; использовать ли графический интерфейс
(define ENEMY "minimax") ; противник - random/minimax/human
;(define ENEMY "human") ; противник - random/minimax/human
;(define ENEMY "random") ; противник - random/minimax/human

(define FIELD-SIZE 15) ; размер поля
(define WIN-COUNT 5) ; количество клеток для выигрыша
(define WHITE -1) ; белая фишка
(define BLACK 1) ; чёрная фишка
(define EMPTY 0) ; пустая клетка

(define BLACK-CHAR "B") ; обозначение чёрной фишки
(define WHITE-CHAR "W") ; обозначение белой фишки

(define MAX-DEPTH 0) ; глубина поиска minimax

(define WIDTH 600) ; ширина окна
(define HEIGHT 600) ; высота окна
(define FIELD-WIDTH 600) ; ширина картинки с полем
(define IMG-SIZE (/ FIELD-WIDTH FIELD-SIZE)) ; размер картинок
(define IMG-SCALE (/ IMG-SIZE 100)) ; коэффициент масштабирования

; картинки для отрисовки поля
(define renju-background (read-bitmap "imgs/board.jpg")) ; фоновая картинка

(define cell (bitmap-scale (read-bitmap "imgs/renjuCell.png") IMG-SCALE)) ; основная клетка
(define cell-point (bitmap-scale (read-bitmap "imgs/renjuCellPoint.png") IMG-SCALE)) ; клетка с точкой

(define cell-bottom (bitmap-scale (read-bitmap "imgs/renjuCellBottom.png") IMG-SCALE))
(define cell-left (bitmap-scale (read-bitmap "imgs/renjuCellLeft.png") IMG-SCALE))
(define cell-right (bitmap-scale (read-bitmap "imgs/renjuCellRight.png") IMG-SCALE))
(define cell-top (bitmap-scale (read-bitmap "imgs/renjuCellTop.png") IMG-SCALE))

(define cell-left-top (bitmap-scale (read-bitmap "imgs/renjuCellLeftTop.png") IMG-SCALE))
(define cell-left-bottom (bitmap-scale (read-bitmap "imgs/renjuCellLeftBottom.png") IMG-SCALE))
(define cell-right-top (bitmap-scale (read-bitmap "imgs/renjuCellRightTop.png") IMG-SCALE))
(define cell-right-bottom (bitmap-scale (read-bitmap "imgs/renjuCellRightBottom.png") IMG-SCALE))

(define cell-black (bitmap-scale (read-bitmap "imgs/renjuCellBlack.png") (* IMG-SCALE 2))) ; чёрная фишка
(define cell-white (bitmap-scale (read-bitmap "imgs/renjuCellWhite.png") (* IMG-SCALE 2))) ; белая фишка

(define green-brush (make-object brush% "GREEN" 'solid))

(define-struct coord (x y)) ; структура для координаты

(define game-field%
  (class object%
    (super-new)

    (init-field (field (make-vector (* FIELD-SIZE FIELD-SIZE) EMPTY))) ; вектор 15х15
    (init-field (all-cells (build-vector (* FIELD-SIZE FIELD-SIZE) (lambda (x) (make-coord (modulo x FIELD-SIZE) (quotient x FIELD-SIZE)))))) ; вектор свободных клеток (по индексам)
    (init-field (last-move -1))

    (init-field (frame (new frame% [label "Рендзю | Перминов А. И. 427"] [width WIDTH]  [height HEIGHT] [alignment '(left top)])))

    (init-field (mouse-click-event null))
    (init-field (canvas (new my-canvas%
                             [parent frame] ; создаём холст на окне
                             [min-width FIELD-WIDTH] ; задаём ширину
                             [min-height FIELD-WIDTH] ; заадём высоту
                             [stretchable-width #f] ; запрещаем растягивание
                             [vert-margin 0] ; вертикальные отступы
                             [horiz-margin 0] ; горизонтальные отступы
                             [on-mouse-left-down mouse-click-event] ; обработчик клика мыши
                             [paint-callback (lambda (canvas dc)
        (send dc draw-bitmap renju-background 0 0)
        (for ([i FIELD-SIZE])
            (for ([j FIELD-SIZE])          
                (send dc draw-bitmap (coord-to-img i j FIELD-SIZE) (* j IMG-SIZE) (* i IMG-SIZE))

                (cond
                  ((= (+ (* i FIELD-SIZE) j) last-move)
                      (send dc set-brush green-brush)
                      (send dc draw-rectangle (+ 4 (* j IMG-SIZE)) (+ 4 (* i IMG-SIZE)) (- IMG-SIZE 8) (- IMG-SIZE 8))
                  )
                )
              
                (cond
                  ((= (vector-ref field (coord-to-index j i)) BLACK) (send dc draw-bitmap cell-black (* j IMG-SIZE) (* i IMG-SIZE)))
                  ((= (vector-ref field (coord-to-index j i)) WHITE) (send dc draw-bitmap cell-white (* j IMG-SIZE) (* i IMG-SIZE)))
                )
            )
        )
    )])))
    
    ; перевод координат в индекс вектора
    (define (coord-to-index x y)
        (+ (* y FIELD-SIZE) x)
    )

    ; проверка клетки на пустоту
    (define (is-empty point)
        (= (vector-ref field (coord-to-index (coord-x point) (coord-y point))) EMPTY)
    )

    ; проверка клетки на валидность
    (define (is-valid x y)
        (and (>= x 0) (< x FIELD-SIZE) (>= y 0) (< y FIELD-SIZE))
    )

    ; перевод координат на поле в изображение
    (define (coord-to-img i j N)
      (cond
        ((or (and (= i 3) (= j 3)) (and (= i (- N 4)) (= j (- N 4))) (and (= i 3) (= j (- N 4))) (and (= i (- N 4)) (= j 3))) cell-point)
        ((and (= i (quotient N 2)) (= j (quotient N 2))) cell-point)
        ((and (= i 0) (= j 0)) cell-left-top)
        ((and (= i 0) (= j (- N 1))) cell-right-top)
        ((and (= i (- N 1)) (= j (- N 1))) cell-right-bottom)
        ((and (= i (- N 1)) (= j 0)) cell-left-bottom)
        ((= i 0) cell-top)
        ((= j 0) cell-left)
        ((= i (- N 1)) cell-bottom)
        ((= j (- N 1)) cell-right)
        (else cell)
      )
    )
    
    ; отрисовка поля в текстовом режиме
    (define (txt-draw)
        (printf "   ")
        (for ([i FIELD-SIZE]) (printf " ~a" (integer->char (+ (char->integer #\A) i)))) ; линия из букв
        (printf "\n")

        (printf "   +")
        (for ([i FIELD-SIZE]) (printf "-+")) ; линия - граница поля
        (printf "\n")

        ; вывод самого поля
        (for ([i FIELD-SIZE])
            (printf "~a~a |" (if (< (- FIELD-SIZE i) 10) " " "") (- FIELD-SIZE i))
        
            (for ([j FIELD-SIZE])
                (let ((cell (vector-ref field (coord-to-index j i))))
                   (cond
                        ((= cell BLACK) (printf "~a|" BLACK-CHAR))
                        ((= cell WHITE) (printf "~a|" WHITE-CHAR))
                        (else (printf " |"))
                    )
                )
            )

            (printf "\n")
        )
      
        (printf "   +")
        (for ([i FIELD-SIZE]) (printf "-+")) ; линия - нижняя граница поля
        (printf "\n")
    )

    ; отрисовка поля
    (define/public (draw)
        (cond
          (USE-GUI (begin (send frame show #t) (send canvas refresh-now) (yield)))
          (else (txt-draw))
        )
    )

    ; закрытие окна
    (define/public (close)
        (send frame show #f)
    )

    ; добавление фишки на поле
    (define/public (add-move x y player)
        (if (= (vector-ref field (coord-to-index x y)) EMPTY) ; если клетка свободна
            (begin
              (vector-set! field (coord-to-index x y) player) ; обновляем поле
              (set! last-move (coord-to-index x y))
            )
            (error "Cell is not empty") ; иначе кидаем ошибку
        )
    )

    ; удаление фишки с поля
    (define/public (remove-move x y player)
        (if (= (vector-ref field (coord-to-index x y)) player) ; если клетка занята этим же игроком
            (vector-set! field (coord-to-index x y) EMPTY) ; убираем фишку с поля
            (error "Cell is not of this player") ; иначе кидаем ошибку
        )
    )

    ; смена цвета последнего хода
    (define/public (change-last)
        (vector-set! field last-move (if (= (vector-ref field last-move) BLACK) WHITE BLACK))
    )

    ; проверка клетки на свободу
    (define/public (is-available point)
        (= (vector-ref field (coord-to-index (coord-x point) (coord-y point))) EMPTY)
    )

    (define (check-radius point)
        (if (is-empty point)
            (let* ((res #f) (x (coord-x point)) (y (coord-y point)) (r 2) (len (+ (* 2 r) 1)) (x0 (- x r)) (y0 (- y r)))
              (begin
                (for ([i len])
                  (for ([j len])
                    (cond
                      ((and (is-valid (+ x0 j) (+ y0 i)) (not (is-empty (make-coord (+ x0 j) (+ y0 i)))))
                       (set! res #t)
                       )
                      )
                    )
                  )
                res
                )
              )
            #f
        )
    )
    
    ; получение свободных клеток поля (на будущее)
    (define/public (get-empty-cells)
        ;(vector-filter is-empty all-cells) ; осталвяем только те клетки, что не содержат фишек
        (vector-filter check-radius all-cells) ; осталвяем только те клетки, что не содержат фишек в радиусе 4
    )

    ; получение состояния клетки x y
    (define/public (state x y)
        (vector-ref field (coord-to-index x y))
    )
    
    ; проверка линии на игрока
    (define/public (check-line x0 y0 dx dy len player)
        (let ((res #t))
            (for ([i len]) ; идём по всей длине линии
                (cond
                    ((or
                         (not (is-valid (+ x0 (* dx i)) (+ y0 (* dy i)))) ; если клетка не валидна
                         (not (= (state (+ x0 (* dx i)) (+ y0 (* dy i))) player)) ; если в клетке не нужный игрок
                    )
                    (set! res #f)) ; обновляем значение set на ложь
                )
            )

            res ; возвращаем результат
        )
    )

    ; получение линии поля, начинающейся в точке (x0, y0) длиной len в направлении (dx, dy)
    (define (get-line x0 y0 dx dy len)
        (let loop ((res (make-vector len)) (x x0) (y y0) (index 0)) ; создаём вектор по длине линии
            (cond
              ((= index len) res) ; если дошли до конца поля, возвращаем заполненный вектор
              (else
                 (vector-set! res index (vector-ref field (coord-to-index x y))) ; иначе записываем в вектор состояние клетки
                 (loop res (+ x dx) (+ y dy) (add1 index)) ; переходим к следующей точке линии
              )
            )
        )
    )

    ; получение вертикальных и горизонтальных линий
    (define/public (get-vert-horiz-lines)
        (let loop (
             (lines (make-vector (* 2 FIELD-SIZE))) ; вектор для линий
             (index 0) ; текущий номер линии
           )
           (cond
                ((= index (* 2 FIELD-SIZE)) lines)
                ((< index FIELD-SIZE)
                    (vector-set! lines index (get-line index 0 0 1 FIELD-SIZE))
                    (loop lines (add1 index))
                )
                (else
                    (vector-set! lines index (get-line 0 (- index FIELD-SIZE) 1 0 FIELD-SIZE))
                    (loop lines (add1 index))
                )
            )
        )
    )

    ; получение главных диагоналей
    (define/public (get-main-diag-lines)
        (let loop (
             (lines (make-vector (- (* 2 FIELD-SIZE) 1))) ; вектор для линий
             (index (- 1 FIELD-SIZE)) ; текущий номер линии
           )
           (cond
                ((= index FIELD-SIZE) lines)
                ((>= index 0)
                    (vector-set! lines index (get-line 0 index 1 1 (- FIELD-SIZE index)))
                    (loop lines (add1 index))
                )
                (else
                    (vector-set! lines (- FIELD-SIZE (add1 index)) (get-line (- 0 index) 0 1 1 (+ FIELD-SIZE index)))
                    (loop lines (add1 index))
                )
            )
        )
    )

    ; получение побочных диагоналей
    (define/public (get-side-diag-lines)
        (let loop (
             (lines (make-vector (- (* 2 FIELD-SIZE) 1))) ; вектор для линий
             (index (- 1 FIELD-SIZE)) ; текущий номер линии
           )
           (cond
                ((= index FIELD-SIZE) lines)
                ((>= index 0)
                    (vector-set! lines index (get-line (- FIELD-SIZE 1 index) 0 -1 1 (- FIELD-SIZE index)))
                    (loop lines (add1 index))
                )
                (else
                    (vector-set! lines (- FIELD-SIZE (add1 index)) (get-line (- FIELD-SIZE 1) (- 0 index) -1 1 (+ FIELD-SIZE index)))
                    (loop lines (add1 index))
                )
            )
        )
    )
  )
)

; класс игрок
(define player%
  (class object%
    (super-new)

    ; преобразование столбца
    (define (column-processing column)
        (cond
            ((char? column) (char-upcase column)) ; если считан символ, то возвращаем его в верхнем регистре
            (else (char-upcase (string-ref (symbol->string column) 0))) ; иначе переводим в строку и возвращаем первый символ
        )
    )
    
    ; считывание хода игрока
    (define/public (make-move field)
        (let* (
               (column (column-processing (read))) ; считываем столбец
               (row (read)) ; считываем строку
               (point (make-coord (- (char->integer column) (char->integer #\A)) (- FIELD-SIZE row))) ; формируем точку для хода
            )
          
            (if (send field is-available point) ; если точка свободна
                point ; возвращаем эту точку
                (begin (printf "Invalid move, try again: ") (make-move field)) ; иначе сообщаем об ошибке и запускаем ход повторно
            )
        )      
    )
  )
)

; случайный игрок
(define random-player%
  (class object%
    (super-new)

    ; выполнение хода игрока
    (define/public (make-move field)
        (let ((moves (send field get-empty-cells)))
            (vector-ref moves (random (vector-length moves))) ; выбираем случайный из доступных ходов
        )
    )
  )
)

; игрок, использующий оценочную функцию и minimax алгоритм
(define minimax-player%
  (class object%
    (super-new)

    ; шаблоны фигур (шаблон, вес)
    (define templates
      #(
             #(#(1 1 1 1 1) 100000) ; пятёрка
             
             #(#(0 1 1 1 1 0) 500) ; открытая четвёрка
             
             #(#(1 1 1 1 0) 40) ; полузакрытая четвёрка
             #(#(0 1 1 1 1) 40) ; полузакрытая четвёрка

             #(#(0 1 0 1 1 1) 20) ; полузакрытая четвёрка с брешью
             #(#(0 1 1 0 1 1) 20) ; полузакрытая четвёрка с брешью
             #(#(0 1 1 1 0 1) 20) ; полузакрытая четвёрка с брешью
             #(#(1 1 1 0 1 0) 20) ; полузакрытая четвёрка с брешью
             #(#(1 1 0 1 1 0) 20) ; полузакрытая четвёрка с брешью
             #(#(1 0 1 1 1 0) 20) ; полузакрытая четвёрка с брешью

             #(#(0 1 1 1 0) 30) ; открытая тройка

             #(#(1 1 1 0) 15) ; полузакрытая тройка
             #(#(0 1 1 1) 15) ; полузакрытая тройка

             #(#(0 1 1 0 1) 8) ; полузакрытая тройка с брешью
             #(#(0 1 0 1 1) 8) ; полузакрытая тройка с брешью
             #(#(1 1 0 1 0) 8) ; полузакрытая тройка с брешью
             #(#(1 0 1 1 0) 8) ; полузакрытая тройка с брешью

             #(#(0 1 1 0) 4) ; открытая двойка
       )
    )

    ; проверка, что в строке line длины n, начиная с индекса start, есть шаблон длины m
    (define (check-values line start template score n m change)
        (let loop ((index 0))
            (cond
                ((= index m) score) ; если дошли до конца, значит шаблон подходит
                ((not (= (* (vector-ref line (+ start index)) (if change -1 1)) (vector-ref template index))) 0) ; если значения линии и шаблона не совпали, значит шаблон не подходит
                (else (loop (add1 index))) ; иначе идём дальше
            )
        )
    )
    
    ; проверка линии на шаблон
    (define (check-template line template-row player)
        (let* (
                (template (vector-ref template-row 0)) ; сам шаблон
                (score (vector-ref template-row 1)) ; вес шаблона
                (n (vector-length line)) ; длина прямой
                (m (vector-length template)) ; длина шаблона
             )
             (cond
                 ((< n m) 0) ; если строка короче шаблона, то шаблона точно нет
                 (else
                     (let loop (
                            (index 0) ; начинаем с начала линии
                            (sum 0) ; сумма очков шаблонов
                           )
                         (cond
                             ((> index (- n m)) sum) ; если просмотрели всю прямую, то выходим с суммой
                             (else (loop (add1 index) (+ sum (check-values line index template score n m (= player WHITE)))))
                         )
                     )
                 )
             )
        )
    )

    ; проверка линии на все шаблоны
    (define (check-line-templates line player)
        (let loop ((index 0) (total (vector-length templates)) (sum 0))
            (cond
              ((= index total) sum) ; если прошли все шаблоны, то возвращаем очки
              (else (loop (add1 index) total (+ sum (check-template line (vector-ref templates index) player)))) ; иначе считаем текущий шаблон и переходим к следующему
            )
        )
    )

    ; проверка линий на все шаблоны
    (define (check-templates lines player)
        (let loop ((index 0) (total (vector-length lines)) (sum 0))
            (cond
              ((= index total) sum) ; если прошли все линии, то возвращаем очки
              (else (loop (add1 index) total (+ sum (check-line-templates (vector-ref lines index) player)))) ; иначе считаем шаблоны и переходим к следующей линии
            )
        )
    )

    ; оценочная функция поля field для игрока player
    (define (score-function field player)
        (let* (
                 (vh-lines (send field get-vert-horiz-lines)) ; получаем вертикальные и горизонтальные линии
                 (md-lines (send field get-main-diag-lines)) ; получаем линии главных диагоналей
                 (sd-lines (send field get-side-diag-lines)) ; получаем линии побочных диагоналей
                 (opponent (if (= player BLACK) WHITE BLACK))
               )
               (+ (check-templates vh-lines player) (check-templates md-lines player) (check-templates sd-lines player))
        )
    )

    ; оценка текущего состояния поля для игрока player
    (define (evaluate-field field player opponent)
        (let ((score 0))
          (begin
            (set! score (score-function field player))
            (send field change-last)
            (set! score (+ score (score-function field opponent)))
            (send field change-last)

            score
          )
        )
    )

    ; индекс максимального элемента в векторе
    (define (index-of-max vec)
        (let loop ((curr-max -inf.0) (max-index 0) (index 0) (total (vector-length vec)))
            (cond
                ((= index total) max-index)
                ((> (vector-ref vec index) curr-max) (loop (vector-ref vec index) index (add1 index) total))
                (else (loop curr-max max-index (add1 index) total))
            )
        )
    )

    ; алгоритм минимакса
    (define (minimax field depth alpha beta is-max player opponent)
      (cond
        ((= depth 0) (- 0 (evaluate-field field opponent player)))
        (else
          (let* (
                (moves (send field get-empty-cells)) ; получаем доступные ходы
                (moves-count (vector-length moves))
                (bestMove (if is-max -inf.0 +inf.0))
                (func (if is-max max min))
                )
           (let loop ((i 0))
               (cond
                   ((= i moves-count) bestMove)
                   (else
                      (begin
                      (send field add-move (coord-x (vector-ref moves i)) (coord-y (vector-ref moves i)) opponent)
                      (set! bestMove (func bestMove (minimax field (- depth 1) alpha beta (not is-max) opponent player)))
                      (send field remove-move (coord-x (vector-ref moves i)) (coord-y (vector-ref moves i)) opponent)
                      
                      (if is-max (set! alpha (max alpha bestMove)) (set! beta (min beta bestMove)))
                      ;(printf "i: ~a, bestMove: ~a, alpha: ~a beta: ~a\n" i bestMove alpha beta)
                      ;(send field draw)
                      ;(read)
                      (cond
                          ((<= beta alpha) bestMove)
                          (else (loop (add1 i)))
                      )
                      )
                   )
               )
           )
         )
        )
      )
    )
    
    ; выполнение хода игрока
    (define/public (make-move field)
        (let* (
               (moves (send field get-empty-cells)) ; получаем доступные ходы
               (player WHITE) ; фишка игрока
               (opponent BLACK) ; фишка противника
               (n (vector-length moves)) ; количество доступных ходов
               (scores (make-vector n 0)) ; вектор для очков
             )
             ;(define t0 (current-inexact-milliseconds))
             (for ([i n])
                 (send field add-move (coord-x (vector-ref moves i)) (coord-y (vector-ref moves i)) player)
                 ;(vector-set! scores i (minimax field MAX-DEPTH -inf.0 +inf.0 #f player opponent))
                 ;(vector-set! scores i (evaluate-field field player opponent))
                 (vector-set! scores i (if (= MAX-DEPTH 0) (evaluate-field field player opponent) (minimax field MAX-DEPTH -inf.0 +inf.0 #f player opponent)))
                 (send field remove-move (coord-x (vector-ref moves i)) (coord-y (vector-ref moves i)) player)
             )
             ;(define t1 (current-inexact-milliseconds))
             ;(printf "~a sec\n" (- t1 t0))
             (vector-ref moves (index-of-max scores))
        )
    )
  )
)

; класс игра
(define game%
  (class object%
    (init black-player) ; игрок чёрных фишек
    (init white-player) ; игром белых фишек
    (super-new)

    ; проверка состояния
    (define (check-state)
        (cond
          ((is-win? BLACK) (begin (set! is-game-over #t) (message-box "Game over" "BLACK WIN") (send field close))) ; если выиграли чёрные
          ((is-win? WHITE) (begin (set! is-game-over #t) (message-box "Game over" "WHITE WIN") (send field close))) ; если выиграли белые
          ((is-draw?) (begin (set! is-game-over #t) (message-box "Game over" "Draw!\n") (send field close))) ; если ничья
          (else (set! curr-player (if (= curr-player BLACK) WHITE BLACK))) ; меняем текущего игрока
        )
    )

    ; ход противника
    (define (opponent-move)
        (cond
          ((and (not (eq? ENEMY "human")) (not is-game-over))
              (let ((point (send white-player make-move field)))
                (move (coord-x point) (coord-y point) curr-player))
              (draw)
              (check-state)
          )
        )
    )
    
    ; обработчик нажатия мыши
    (define (mouse-click-event canvas event)
        (let* (
              (x (quotient (send event get-x) IMG-SIZE))
              (y (quotient (send event get-y) IMG-SIZE))
              (is-empty (send field is-available (make-coord x y)))
            )
            (cond
              ((and is-empty (not is-game-over))
                (move x y curr-player) ; делаем ход в эту клетку
                (draw) ; отображаем поле
                (check-state)
                (opponent-move)
              )
            )
        )
    )
    
    (init-field (field (new game-field% [mouse-click-event mouse-click-event]))) ; создаём новое поле
    (init-field (curr-player BLACK)) ; текущий игрок
    (init-field (is-game-over #f)) ; конец ли игры
    
    ; отрисовка игры
    (define/public (draw)
        (send field draw)
    )

    ; выполнение хода игроком player
    (define/public (move column row player)
        (send field add-move column row player)
    )

    ; провека на ничью
    (define/public (is-draw?)
        (= 0 (vector-length (send field get-empty-cells))) ; ничья, если больше некуда ходить
    )
    
    ; проверка на выигрыш игрока player
    (define/public (is-win? player)
        (let (
              (dx (vector 1 0 1 -1)) ; смещения по x
              (dy (vector 0 1 1 1)) ; смещения по y
              (res #f)
             )
          (for ([index 4]) ; проходим по всем направлениям
              (for ([i FIELD-SIZE]) ; проходим по всем строкам
                  (for ([j FIELD-SIZE]) ; проходим по всем столбцам
                      (if (send field check-line j i (vector-ref dx index) (vector-ref dy index) WIN-COUNT player) ; если какая-то из линий в 5 элементов соответствует игроку
                          (set! res #t) ; значит выигрыш
                          res
                      )
                  )

               )
           )

          res ; возвращаем результат
        )
    )
    
    ; считывание хода игрока
    (define (make-move player)
        (printf "~a>" (if (= player BLACK) BLACK-CHAR WHITE-CHAR))
      
        (let* (
                   (curr-player (if (= player BLACK) black-player white-player)) ; получаем текущего игрока
                   (m (send curr-player make-move field)) ; делаем игроком ход
              )
            (move (coord-x m) (coord-y m) player) ; делаем ход
        )      
    )
    
    ; запуск игры
    (define/public (start)
        (draw) ; отрисовываем начальное поле
      
        (let game-loop ((player BLACK))
            (cond
                ((is-win? BLACK) (printf "BLACK WIN")) ; если выиграли чёрные
                ((is-win? WHITE) (printf "WHITE WIN")) ; если выиграли белые
                ((is-draw?) (printf "Draw!\n")) ; если ничья
                (else
                    (make-move player) ; ходит игрок
                    (draw) ; отрисовываем игру
                    (game-loop (if (= player BLACK) WHITE BLACK)) ; запускаемся заново для противоположного игрока
                )
            )
        )
    )
  )
)

(define black-player (new player%))
(define white-player (new player%))
(define random-player (new random-player%))
(define minimax-player (new minimax-player%))

(define game (new game% [black-player black-player]
                  [white-player (cond
                                  ((eq? ENEMY "human") white-player)
                                  ((eq? ENEMY "random") random-player)
                                  ((eq? ENEMY "minimax") minimax-player)
                                )]))

(if USE-GUI
    (send game draw)
    (send game start)
)