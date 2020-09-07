;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-makrand) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders By Makrand Sumant

;; =================
;; Constants:
;; =================

(define WIDTH  300)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define TANK-Y (- HEIGHT TANK-HEIGHT/2))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


;; ------------------------------------------ Invader data definitions --------------------------------------
(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right
(define I4 (make-invader 120 230 -1))

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvaders is one of:
;;  - empty
;;  - (cons invader ListOfInvaders)
;; interp. a list of invaders
(define LOI1 empty)
(define LOI2 (cons I1 (cons I2 empty)))

(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))
         ]))
;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons invader ListOfInvaders)
;; - reference: (first loi) 
;; - self-reference: (rest loi) is ListOfInvaders

;; ListOfInvaders -> ListOfInvaders
;; produces list containing next state of each of the invaders in passed in list.
(define (next-loi loi) LOI2)


;; ------------------------------------------ Missile data definitions --------------------------------------
(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissiles is one of:
;;  - empty
;;  - (cons missile ListOfMissiles)
;; interp. a list of missiles
(define LOM1 empty)
(define LOM2 (cons M1 (cons M2 empty)))
(define LOM3 (cons M1 (cons M2 (cons M3 empty))))

(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))
         ]))
;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons missile ListOfMissies)
;; - reference: (first lom) 
;; - self-reference: (rest lom) is ListOfMissies


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1 I4) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =================
;; Functions:
;;
;; Game -> Game
;; called to make the Game; start with (main (make-game 0 3))
;; no tests for main function

(define (main g)
  (big-bang g
    (on-tick next-game)     ; Game -> Game
    (to-draw render-game)   ; Game -> Image
    (on-key handle-key)     ; Game KeyEvent -> Game
    (stop-when game-over?)  ; Game -> Boolean
    ))


;=====================================================    Tank Functions   =================================================================
;; Tank -> Tank
;; Changes Tank's x coordinate by TANK-SPEED depending on the tank's direction
(check-expect (next-tank (make-tank (/ WIDTH 2) 1)) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (next-tank (make-tank (/ WIDTH 2) -1)) (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))
(check-expect (next-tank (make-tank (- WIDTH TANK-SPEED) 1)) (make-tank WIDTH 1))
(check-expect (next-tank (make-tank TANK-SPEED -1)) (make-tank (- TANK-SPEED TANK-SPEED) -1))
(check-expect (next-tank (make-tank (- WIDTH (- TANK-SPEED 1)) 1)) (make-tank WIDTH -1))
(check-expect (next-tank (make-tank (- TANK-SPEED 1) -1)) (make-tank 0 1))


;(define (next-tank t) (make-tank 50 1)) ; stub
(define (next-tank t)
  ( cond [ (> (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) WIDTH) (make-tank WIDTH (-(tank-dir t)))]
         [ (< (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) 0) (make-tank 0 (-(tank-dir t)))]
         [ else
           (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]
         )
  )

;; tank -> Image
;; Places tank image at x coordinate of the passed in tank and TANK-Y on MTS.
(check-expect (render-tank T0) (place-image TANK (tank-x T0) TANK-Y MTS))

(define (render-tank t)
  (place-image TANK (tank-x t) TANK-Y MTS)
  )

;; Tank, dx -> Tank
;; Turns the tank into passed in direction.
(check-expect (turn-tank T1 -1) (make-tank (tank-x T1) -1)) ; turns the tank left
(check-expect (turn-tank T2 -1) (make-tank (tank-x T2) -1)) 
(check-expect (turn-tank T1 1) (make-tank (tank-x T1) 1)) ; turns the tank right
(check-expect (turn-tank T2 1) (make-tank (tank-x T2) 1))
(check-expect (turn-tank T2 3) (make-tank (tank-x T2) -1)) ; invalid direction, keeps the direction unchanged

;(define (turn-tank T1 dx) T2) ;stub

(define (turn-tank t dx)
  (cond [ (= dx -1) (make-tank (tank-x t) -1)]
        [ (= dx 1) (make-tank (tank-x t) 1)]
        [ else (make-tank (tank-x t) (tank-dir t))])
  )


;=====================================================    Invader Functions   =================================================================
;; Invader -> Invader
;; Changes Invader's x coordinate by INVADER-X-SPEED & y coordinates by INVADER-Y-SPEED. Direction is reversed if x and y coordinates reach the screen x & y coordinates resp.
(check-expect (next-invader (make-invader (/ WIDTH 2) 50 1)) (make-invader (+ (/ WIDTH 2) (* INVADER-X-SPEED 1)) (+ 50 INVADER-Y-SPEED) 1)) ;; at centre moving right -> keep moving
(check-expect (next-invader (make-invader (/ WIDTH 2) 50 -1)) (make-invader (+ (/ WIDTH 2) (* INVADER-X-SPEED -1)) (+ 50 INVADER-Y-SPEED) -1)) ;; at centre moving left -> keep moving
(check-expect (next-invader (make-invader WIDTH 100 1)) (make-invader WIDTH (+ 100 INVADER-Y-SPEED) -1))  ;; reaches exact right edge, moving right -> reverse direction
(check-expect (next-invader (make-invader 0 100 -1)) (make-invader 0 (+ 100 INVADER-Y-SPEED) 1)) ;; reaches exact left edge, moving left -> reverse direction
(check-expect (next-invader (make-invader (+ WIDTH INVADER-X-SPEED) 100 1)) (make-invader WIDTH (+ 100 INVADER-Y-SPEED) -1))  ;; tries to pass right edge -> reverse direction and set x to width
(check-expect (next-invader (make-invader (- 0 INVADER-X-SPEED) 100 -1)) (make-invader 0 (+ 100 INVADER-Y-SPEED) 1))  ;; tries to pass right edge -> reverse direction and set x to width


;(define (next-invader t) I1) ; stub

(define (next-invader invader)
  (cond [ (>= (+ (invader-x invader) (* INVADER-X-SPEED (invader-dx invader))) WIDTH)
          (make-invader WIDTH (+ (invader-y invader) INVADER-Y-SPEED) (- (invader-dx invader)))]
        [ (<= (+ (invader-x invader) (* INVADER-X-SPEED (invader-dx invader))) 0)
          (make-invader 0 (+ (invader-y invader) INVADER-Y-SPEED) (- (invader-dx invader)))]
        [ else
          (make-invader(+ (invader-x invader) (* INVADER-X-SPEED (invader-dx invader))) (+ (invader-y invader) INVADER-Y-SPEED) (invader-dx invader))]
        )
  )


;; ListOfInvaders -> ListOfInvaders
;; Advances each of the invader using it's x, y co-ordinates and direction.
(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders LOI2) (cons (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 1) (cons (make-invader (- 150 INVADER-X-SPEED) (+ HEIGHT INVADER-Y-SPEED) -1) empty)))

;(define (advance-invaders loi) loi)

(define (advance-invaders loi)
  (cond[(empty? loi) empty]
       [else
        (cons (next-invader (first loi))
              (advance-invaders (rest loi)))]))

;; ListOfInvaders Image -> Image
;; Plases image of INVADER for each invader in passed in list. Uses Backgroud image as base image.
(check-expect (render-invaders empty MTS) MTS)
(check-expect (render-invaders (list I1 I2) MTS) (place-image INVADER
                                                              (invader-x I1)
                                                              (invader-y I1) (place-image INVADER
                                                                                          (invader-x I2)
                                                                                          (invader-y I2) MTS)))
;(define (render-invaders loi img) img) ;stub

(define (render-invaders loi img)
  (cond[(empty? loi) img]
       [else
        (place-image INVADER
                     (invader-x (first loi))
                     (invader-y (first loi))
                     (render-invaders (rest loi) img))]))

;; ListOfInvaders -> ListOfInvaders
;; Randomly creates a new invader and adds it to the passed list of invaders.
(check-random (add-invader empty) (cond [(> INVADE-RATE (random 5000)) (cons (make-invader (random WIDTH) 10 (if (> 5 (random 10)) 1 -1)) empty)]
                                        [else empty]))
(check-random (add-invader LOI2) (cond [(> INVADE-RATE (random 5000)) (cons (make-invader (random WIDTH) 10 (if (> 5 (random 10)) 1 -1)) LOI2)]
                                       [else LOI2]))

(define (add-invader loi)
  (cond [(> INVADE-RATE (random 5000)) (cons (make-invader (random WIDTH) 10 (if (> 5 (random 10)) 1 -1)) loi)]
        [else loi])
  )

;; ListOfInavader ListOfMissiles -> ListOfInvaders
;; Destroyes invaders which come under hit range of passed in missiles.
(check-expect (destroy-invaders empty (cons M1 (cons (make-missile 300 200)  empty))) empty)
(check-expect (destroy-invaders (list I1 I4) empty) (list I1 I4))
(check-expect (destroy-invaders (list I1 I4) LOM1) (list I1 I4))
(check-expect (destroy-invaders (list I1 I4) LOM3) (list I4))
(check-expect (destroy-invaders (list I1 I4) (list (make-missile (- (invader-x I4) 9) (+ (invader-y I4) 9)))) (list I1))

;;(define (destroy-invaders loi lom) loi) ;stub

(define (destroy-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else (if (is-invader-hit? (first loi) lom)
                  (destroy-invaders (rest loi) lom)
                  (cons (first loi) (destroy-invaders (rest loi) lom)))]))

;; Invader ListOfMissiles -> Boolean
;; Produces true, if any of the missiles from ListOfMissiles hit the Invader.
(check-expect (is-invader-hit? I1 empty) false)
(check-expect (is-invader-hit? I1 (cons M1 (cons (make-missile 300 200)  empty))) false)
(check-expect (is-invader-hit? I1 (cons M2 (cons (make-missile 300 200)  empty))) true)
(check-expect (is-invader-hit? I4 (cons M2 (cons (make-missile (- (invader-x I4) 9) (+ (invader-y I4) 9))  empty))) true)

;;(define (is-invader-hit? i lom) false) ;stub

(define (is-invader-hit? i lom)
  (cond [(empty? lom) false]
        [else (or (is-hit? i (first lom)) (is-invader-hit? i (rest lom)))
              ]))

;; Invader Missile -> Boolean
;; Produce true if Missile is hitting Invader.
(check-expect (is-hit? I1 M1) false)
(check-expect (is-hit? I1 M2) true)
(check-expect (is-hit? I4 (make-missile (- (invader-x I4) 9) (+ (invader-y I4) 9))) true)

;(define (is-hit? i m) false) ;stub

(define (is-hit? i m)
  (and (<= (abs (- (invader-x i)  (missile-x m))) HIT-RANGE)
       (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))

;; ListOfInvader -> Boolean
;; Produces true if any of the invaders has landed. 
(check-expect (landed? empty) false)
(check-expect (landed? (list I1 I2)) true)
(check-expect (landed? (list I1)) false)
(check-expect (landed? (list I3)) true)

;;(define (landed? i) false) ;stub

(define (landed? loi)
  (cond[(empty? loi) false]
       [else
        (if (>= (invader-y (first loi)) HEIGHT)
            true
            (landed? (rest loi)))]))

;=====================================================    Missile Functions   =================================================================
;; Missile -> Missile
;; Produces a Missile which represnts next state of the passed in missile. Decreases missiles y coordinate by MISSILE-SPEED.
(check-expect (next-missile (make-missile (/ WIDTH 2) 50)) (make-missile (/ WIDTH 2) (- 50 MISSILE-SPEED))) ;; at centre

;;(define (next-missile t) M1) ; stub

(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED))
  )

;; ListOfMissiles -> ListOfMissiles
;; produces list containing next state of each of the missiles from the passed in list. Advances each of the missile in upward direction.

(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles LOM2) (cons (make-missile 150 (- 300 MISSILE-SPEED)) (cons (make-missile (invader-x I1) (- (+ (invader-y I1) 10) MISSILE-SPEED)) empty)))

;(define (advance-missiles lom) LOM2) ;stub

(define (advance-missiles lom)
  (cond[(empty? lom) empty]
       [else
        (cons (next-missile (first lom))
              (advance-missiles (rest lom)))]))

;; ListOfMissiles Image -> Image
;; Places image of MISSILE for each missiles from passed in list. Uses passed in image as base image.
(check-expect (render-missiles empty MTS) MTS)
(check-expect (render-missiles (list M1 M2) MTS) (place-image MISSILE
                                                              (missile-x M1)
                                                              (missile-y M1) (place-image MISSILE
                                                                                          (missile-x M2)
                                                                                          (missile-y M2) MTS)))
;(define (render-missiles lom img) img) ;stub

(define (render-missiles lom img)
  (cond[(empty? lom) img]
       [else
        (place-image MISSILE
                     (missile-x (first lom))
                     (missile-y (first lom))
                     (render-missiles (rest lom) img))]))


;; ListOfMissiles -> ListOfMissiles
;; Cleans up missiles which leaves the screen, i.e. which have y coordinate < 0.
(check-expect (cleanup-missiles empty) empty)
(check-expect (cleanup-missiles LOM2) LOM2)
(check-expect (cleanup-missiles (cons (make-missile 150 -3) LOM2)) LOM2)

;(define (cleanup-missiles LOM1) LOM1) ;stub

(define (cleanup-missiles lom)
  ( cond [(empty? lom) empty]
         [else (if (< (missile-y (first lom)) 0)
                   (cleanup-missiles (rest lom))
                   (cons (first lom) (cleanup-missiles (rest lom)))
                   )]
         )
  )


;=====================================================    Game Functions   =================================================================

;; Game -> Game
;; Produces next game by:
;; Moving tank in correct direction, increases or decreases tank's x co-ordinates by TANK-SPEED
;; Crates new invaders randomly & removes invaders which get hit by missiles.
;; Moves invaders as per their directions.
;; Moves missiles in upward direction.

;; Tests are redundant since individual functions have their tests.
    
;;(define (next-game g) G0) ;stub

(define (next-game g)
  (make-game (destroy-invaders (add-invader (advance-invaders (game-invaders g))) (game-missiles g)) (cleanup-missiles (advance-missiles (game-missiles g)))
             (next-tank (game-tank g))))

;; Game -> Image
;; Places appropriate Game Images in the following order using properties of passed in Game.
;; Empty Scene -> Tank image -> All the Missiles -> Invaders. (Seq. bottom -> top)
(define (render-game g) (render-invaders (game-invaders g) (render-missiles (game-missiles g) (render-tank (game-tank g))))) ;stub  


;; Game KeyEvent-> Game
;; set the direction of the tank based on the pressed button (i.e. left or right).
;; When Space bar is pressed a new missile gets fired.

;(define (handle-key g ke) G3) ;stub

(define (handle-key g ke)
  (cond [(key=? ke "left") (make-game (game-invaders g) (game-missiles g) (turn-tank (game-tank g) -1))]
        [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (turn-tank (game-tank g) 1))]
        [(key=? ke " ") (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) ( - HEIGHT (image-height TANK)) ) (game-missiles g)) (game-tank g))]
        [else 
         g]))

;; Game -> Boolean
;; returns true if any of the invader's Y co-ordinates is >= HEIGHT.
(check-expect (game-over? G1) false)
(check-expect (game-over? G2) false)
(check-expect (game-over? G3) true)

;(define (game-over? g) false) ;stub

(define (game-over? g)
  (cond[(empty? (game-invaders g)) false]
       [else
        (landed? (game-invaders g))]))