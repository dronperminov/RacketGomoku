#lang racket
(require racket/class racket/set)

; Рендзю - версия 5 в ряд на поле 15 на 15
; Реализация выполнена так, чтобы можно было без труда создать игру n в ряд на поле mxm с помощью констант

(define FIELD-SIZE 15) ; размер поля
(define WIN-COUNT 5) ; количество клеток для выигрыша
(define WHITE -1) ; белая фишка
(define BLACK 1) ; чёрная фишка
(define EMPTY 0) ; пустая клетка

(define BLACK-CHAR "B") ; обозначение чёрной фишки
(define WHITE-CHAR "W") ; обозначение белой фишки

(define-struct coord (x y)) ; структура для координаты

(define game-field%
  (class object%
    (super-new)

    (init-field (field (make-vector (* FIELD-SIZE FIELD-SIZE) EMPTY))) ; вектор 15х15
    (init-field (all-cells (build-vector (* FIELD-SIZE FIELD-SIZE) (lambda (x) (make-coord (modulo x FIELD-SIZE) (quotient x FIELD-SIZE)))))) ; вектор свободных клеток (по индексам)

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
    
    ; отрисовка поля
    (define/public (draw)
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
                (let ((cell (vector-ref field (coord-to-index i j))))
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

    ; добавление фишки на поле
    (define/public (add-move x y player)
        (if (= (vector-ref field (coord-to-index y x)) EMPTY) ; если клетка свободна
            (vector-set! field (coord-to-index y x) player) ; обновляем поле
            (error "Cell is not empty") ; иначе кидаем ошибку
        )
    )

    ; проверка клетки на свободу
    (define/public (is-available point)
        (= (vector-ref field (coord-to-index (coord-y point) (coord-x point))) EMPTY)
    )
    
    ; получение свободных клеток поля (на будущее)
    (define/public (get-empty-cells)
        (vector-filter is-empty all-cells) ; осталвяем только те клетки, что не содержат фишек
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

; класс игра
(define game%
  (class object%
    (init black-player) ; игрок чёрных фишек
    (init white-player) ; игром белых фишек
    (super-new)
    (init-field (field (new game-field%))) ; создаём новое поле

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
                      (if (send field check-line j i (vector-ref dx index) (vector-ref dy index) WIN-COUNT player) ; если какая-то из линий в 5 элеметнов соответствует игроку
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
(define game (new game% [black-player black-player] [white-player white-player]))

(send game start)