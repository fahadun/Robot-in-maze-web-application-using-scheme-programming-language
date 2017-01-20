#lang racket

; The requirements to start a web server with some html-form features
(require web-server/servlet
         web-server/servlet-env
         web-server/formlets)
 


 
; Default Maze Map Setup: As a list of lists. Each sublist represents each row. It is a crude representation of the graphical maze, but it simplifies
; coding. The original point (1,1) of the coordination system we use here starts from the top left corner of the map.
; naming rules for the image file: from most priority to the left of the names: robot/target/robot-in-target -> left->right->top->bottom
(define ORIGINAL-MAZE
  (list '("lefttop.png" "topbottom.png" "topbottom.png" "righttop.png" "lefttop.png" "topbottom.png" "righttopbottom.png")
        '("leftbottom.png" "top.png" "righttop.png" "leftbottom.png" "right.png" "lefttop.png" "righttopbottom.png")
        '("lefttop.png" "rightbottom.png" "leftbottom.png" "righttopbottom.png" "left.png" "rightbottom.png" "leftrighttop.png")
        '("left.png" "topbottom.png" "righttop.png" "lefttop.png" "rightbottom.png" "lefttop.png" "right.png")
        '("robotleftrightbottom.png" "lefttopbottom.png" "rightbottom.png" "leftbottom.png" "topbottom.png" "rightbottom.png" "targetleftrightbottom.png")))


; We need MAZE copied from the Original map so that we can manipulate this maze as much as we want without damaging the original map.
(define MAZE ORIGINAL-MAZE)


; Consumes a request and produces a page that displays all of the
; web content.
(define (start request)
  (render-maze-page MAZE request))


; replaces the cell at coordinate coor in pair format (row . col) with icon (blank/robot/target/robottarget)
(define (replace-maze coor icon MAZE)
  ; find the intended column to replace and return the row
  (define (replace-row MAZE-ROW icon pos)
    (cond ((= 1 pos)
           (cons (cond ((eq? icon "blank") (string-trim (string-trim (car MAZE-ROW) "robot") "target"))
                       (else (string-append icon (car MAZE-ROW)))) (cdr MAZE-ROW)))
          (else (cons (car MAZE-ROW) (replace-row (cdr MAZE-ROW) icon (- pos 1))))))
  
  (if (null? MAZE)  ;; This null check controls when we have enough rows to rebuilt another copy of MAZE
      '() 
      (if (= 1 (car coor))  ;; find the intended row
          (cons (replace-row (car MAZE) icon (cdr coor)) (replace-maze (cons (- (car coor) 1) (cdr coor)) icon (cdr MAZE)))
          (cons (car MAZE) (replace-maze (cons (- (car coor) 1) (cdr coor)) icon (cdr MAZE))))))


; get coordinates of icon in pair format (row . col)
(define (getloc icon MAZE)
  (define (getcol icon MAZE-COL)  ;; This finds which which column the icon is.
    (if (null? MAZE-COL)
        1
        (if (regexp-match? (regexp icon) (car MAZE-COL))
            1
            (+ 1 (getcol icon (cdr MAZE-COL))))))

  ; it is not necessary to check for null? MAZE,
  ; for it is not possible in this case
  (if (<= (getcol icon (car MAZE)) (length (car MAZE)))  ;; This finds out which row the intended icon resides in.
      (cons 1 (getcol icon (car MAZE)))
      (cons (+ 1 (car (getloc icon (cdr MAZE)))) (cdr (getloc icon (cdr MAZE))))))



; move the robot, return a new MAZE map with the user's choice of move.
(define (moveup MAZE)
  (let ((coor (getloc "robot" MAZE)))
    (set! MAZE (replace-maze coor "blank" MAZE))
    (set! MAZE (replace-maze (cons (- (car coor) 1) (cdr coor)) "robot" MAZE))
    MAZE))

(define (movedown MAZE)
  (let ((coor (getloc "robot" MAZE)))
    (set! MAZE (replace-maze coor "blank" MAZE))
    (set! MAZE (replace-maze (cons (+ (car coor) 1) (cdr coor)) "robot" MAZE))
    MAZE))

(define (moveleft MAZE)
  (let ((coor (getloc "robot" MAZE)))
    (set! MAZE (replace-maze coor "blank" MAZE))
    (set! MAZE (replace-maze (cons (car coor) (- (cdr coor) 1)) "robot" MAZE))
    MAZE))

(define (moveright MAZE)
  (let ((coor (getloc "robot" MAZE)))
    (set! MAZE (replace-maze coor "blank" MAZE))
    (set! MAZE (replace-maze (cons (car coor) (+ (cdr coor) 1)) "robot" MAZE))
    MAZE))




(define (move direction MAZE)
  (cond ((eq? direction "top") (moveup MAZE))
        ((eq? direction "bottom") (movedown MAZE))
        ((eq? direction "left") (moveleft MAZE))
        ((eq? direction "right") (moveright MAZE))
        (else MAZE)))



; Check if the robot can walk to the given direction (walls or no walls)
(define (movable? direction MAZE)
  (define (tracel direction MAZE)
    (cond ((regexp-match? #rx"robot" (car MAZE))
           (cond ((not (regexp-match? (regexp direction) (car MAZE))))
                   (else #f)))
          (else (cond ((not (null? (cdr MAZE))) (tracel direction (cdr MAZE)))
                      (else #f)))))

  (cond ((tracel direction (car MAZE)) #t)
        (else (cond ((not (null? (cdr MAZE))) (movable? direction (cdr MAZE)))
                    (else #f)))))
  

; extract binding from Form parsing
(define (get-direction bindings)
  (cond ((exists-binding? 'up bindings) "top")
        ((exists-binding? 'down bindings) "bottom")
        ((exists-binding? 'left bindings) "left")
        ((exists-binding? 'right bindings) "right")
        (else "reset")))

(define (user-command bindings)
  (cond ((eq? (get-direction bindings) "reset") (begin (set! MAZE ORIGINAL-MAZE) MAZE))
         (else (cond ((movable? (get-direction bindings) MAZE) (begin (set! MAZE (move (get-direction bindings) MAZE)) MAZE))
                     (else MAZE)))))
 
; Consumes a maze page and a request, and produces an HTML page
; of the content of the maze page.
(define (render-maze-page a-maze request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "My Maze"))
            (body
             (center
              (h1 "My Maze")
              ,(render-table a-maze)  ;; generate the maze table
              (form ((action  ;; html-form
                      ,(embed/url insert-map-handler)))
                    (p
                     (input ((type "submit") (name "up") (value "up"))))
                    (p
                     (input ((type "submit") (name "left") (value "left")))
                     (input ((type "submit") (name "down") (value "down")))
                     (input ((type "submit") (name "right") (value "right"))))
                    (p
                     (input ((type "submit") (name "reset") (value "reset"))))))))))

  ;; handles the parsing of FORM data
  (define (insert-map-handler request)
    (render-maze-page
     (user-command (request-bindings request))
     request))
  (send/suspend/dispatch response-generator))


; render the cells and row of the table -> xexpr
; Consumes cells, produces an xexpr fragment the whole table
(define (render-cell a-maze)
  `(td (img ([src ,a-maze]))))

(define (render-row a-maze)
  `(tr ,@(map render-cell a-maze)))
 
; render-table: maze -> xexpr
; Consumes a maze, produces an xexpr fragment
; of all its table cells.
(define (render-table a-maze)
  `(table ,@'(((cellspacing "0") (cellpadding "0") (border "0"))) ,@(map render-row a-maze)))


;; Starts the server with parameters
(serve/servlet start
               #:listen-ip #f    ;; This allows for public hosting (less secure but who cares, its very cool)
               #:servlet-path "/standalone.rkt"  ;; this allows the server to be open to serving image files
               #:extra-files-paths (list (build-path (current-directory)))) ;; This is the path to our image files