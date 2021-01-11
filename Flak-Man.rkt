;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Flak-Man) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@htdw GameState)

;; =================
;; Constants:

(define WIDTH 1000)
(define HEIGHT 800)
(define CENTER (/ WIDTH 2))
(define MTS (empty-scene WIDTH HEIGHT "black"))

(define GUN1-X 15)
(define GUN1-Y (- HEIGHT (/ HEIGHT 5)))
(define GUN2-X (- WIDTH  GUN1-X))
(define GUN2-Y (- HEIGHT (/ HEIGHT 5)))

(define BASE1-X 150)
(define BASE2-X (+ BASE1-X (/ (- (- WIDTH BASE1-X) BASE1-X) 2)))
(define BASE3-X (- WIDTH 150))
(define BASE-Y (- HEIGHT (/ HEIGHT 10)))
(define BASE-HP 3)
(define BASE-TEXT-SIZE 15)
(define BASE-TEXT-COLOR "white")
(define BASE-TEXT-OFFSET 40)
(define BASE-RANGE 30)

(define AMMO 50)
(define PLANE-SPEED 2)
(define BOMB-SPEED 10)
(define ROCKET-SPEED 10)
(define EROCKET-SPEED 3)
(define EROCKET-OFFSET 150)
(define MAX-EROCKET-SPEED 10)
(define MAX-SPEED-SCORE 50000)
(define FACTOR (/ MAX-SPEED-SCORE (- MAX-EROCKET-SPEED EROCKET-SPEED)))
(define PLANE-SCORE 100)
(define ROCKET-SCORE 200)

(define EROCKET-Y-START -5)
(define DMG 1)
(define SPLIT-Y 100)
(define EXPL-RATE 4)
(define O-EXPL-RATE 3)
(define MAX-EXPL-SIZE 75)
(define O-MAX-EXPL-SIZE 45)
(define RANGE 11)
(define E-RANGE 20)

(define GUN-IMG (rectangle 30 4 "solid" "DarkGrey"))
(define DR-IMG (square 4 "solid" "white"))
(define ER-IMG (above (triangle 4 "solid" "white")
                      (rectangle 4 10 "solid" "DarkRed")))
(define PLANE-IMG (above/align "right" (rectangle 8 12 "solid" "silver")
                               (above/align "center"
                                            (rectangle 40 8 "solid" "silver")
                                            (rectangle 12 5 "solid" "silver"))))
(define WINDOW (rectangle 7 7 "solid" "black"))
(define SPACE (rectangle 4 10 "solid" "DarkGrey"))
(define BASE-IMG (overlay/align "middle" "top"
                                (above (beside WINDOW SPACE WINDOW SPACE
                                               WINDOW SPACE WINDOW SPACE
                                               WINDOW SPACE WINDOW)
                                       (beside WINDOW SPACE WINDOW SPACE
                                               WINDOW SPACE WINDOW SPACE
                                               WINDOW SPACE WINDOW)
                                       (beside WINDOW SPACE WINDOW SPACE
                                               WINDOW SPACE WINDOW SPACE
                                               WINDOW SPACE WINDOW))
                                (rectangle 70 40 "solid" "DarkGrey")))

(define PLANE-SPAWN-MIN 200)
(define PLANE-SPAWN-MAX 300)
(define ER-SPAWN-MIN 50)
(define ER-SPAWN-MAX 100)
(define FIRST-EROCKET 5)
(define FIRST-PLANE 45)
(define TIME-STEP-MIN 1)
(define TIME-STEP-MAX 5)
(define MAX-SPAWN-SCORE 55000)
(define SPAWN-FACTOR (/ MAX-SPAWN-SCORE (- TIME-STEP-MAX TIME-STEP-MIN)))
(define COMBO-TIME 15)

(define GAME-TITLE "FLAK  MAN")
(define TITLE-TEXT-SIZE 75)
(define TITLE-TEXT-COLOR "white")
(define TITLE-Y (* HEIGHT 0.4))
(define MENU-TEXT-SIZE 40)
(define MENU-TEXT-COLOR "DarkGreen")
(define HIGHLIGHTED-TEXT-COLOR "green")
(define MENU1-Y (+ TITLE-Y (* HEIGHT 0.2)))
(define SCORE-TEXT-SIZE 30)
(define SCORE-TEXT-COLOR "white")
(define INSTRUCTIONS "*CLICK* TO FIRE FLAK   *SPACE* TO CHANGE GUNS")
(define INSTRUCTIONS-SIZE 25)
(define INSTRUCTIONS-COLOR "white")
(define INSTRUCTIONS-Y (+ MENU1-Y (* HEIGHT 0.2)))

(define FONT "Impact")
(define SCORE-FONT "Synchro LET")



;; =================
;; Data definitions:

(@htdd Gun)
(define-struct gun (x y r ammo hp active?))
;; Gun is (make-gun Number Number Number Natural Natural Boolean)
;; interp.  a moveable gun with x/y pos, rotation, remaining ammo and hit points
;;     and whether the player can fire it or not
(define GUN1 (make-gun GUN1-X GUN1-Y 0 59 2 true))
(define GUN2 (make-gun GUN2-X GUN2-Y 0 59 2 false))

(define (fn-for-gun g)
  (...(gun-x g)
      (gun-y g)
      (gun-r g)
      (gun-ammo g)
      (gun-hp g)
      (gun-active? g)))


(@htdd DRocket)
(define-struct drocket (x y dx dy ex ey))
;; DRocket is (make-drocket Number Number Number Number Number)
;; interp. a defensive rocket with x/y posn, x/y velocity, and target altitude

(define (fn-for-drocket dr)
  (... (drocket-x dr)
       (drocket-y dr)
       (drocket-dx dr)
       (drocket-dy dr)
       (drocket-ex dr)
       (drocket-ey dr)))


(@htdd DExplosion)
(define-struct dexpl (x y r rate))
;; DExplosion is (make-dexpl Number Number Number Number)
;; interp. defensive explosion with x/y on MTS, radius r and explosion speed

(define (fn-for-dexpl de)
  (... (dexpl-x de)
       (dexpl-y de)
       (dexpl-r de)
       (dexpl-rate de)))


(@htdd OExplosion)
(define-struct oexpl (x y r rate))
;; OExplosion is (make-dexpl Number Number Number Number)
;; interp. offensive explosion with x/y on MTS, radius r and explosion speed

(define (fn-for-oexpl de)
  (... (oexpl-x de)
       (oexpl-y de)
       (oexpl-r de)
       (oexpl-rate de)))


(@htdd Plane)
(define-struct plane (x y dx target targets))
;; plane is (make-plane Number Number Posn (listof Posn))
;; interp. an enemy plane with x/y pos, x speed and target positions

(define (fn-for-plane p)
  (... (plane-x p)
       (plane-y p)
       (plane-target p)
       (plane-targets p)))

(@htdd Instantiator)
(define-struct inst (timer pt ert))
;; Instantiator is (make-inst Natural Natural Natural)
;; interp. a time device with current time (in frames), and the times to
;;         instantiate the next plane and enemy rockets
(define INST1 (make-inst 0 FIRST-PLANE FIRST-EROCKET))

(define (fn-for-inst i)
  (... (inst-timer i)
       (inst-pt i)
       (inst-ert i)))

(@htdd ERocket)
(define-struct erock (x y dx dy speed switch? target))
;; ERocket is (make-erock Number Number Number Number Number Boolean Posn)
;; interp. an enemy rocket with x/y pos, x/y direction, rocket speed,
;; whether or not all subrockets have been launched and target positions

(define (fn-for-erock er)
  (... (erock-x er)
       (erock-y er)
       (erock-dx er)
       (erock-dy er)
       (erock-speed er)
       (erock-switch? er)
       (erock-target er)))


(@htdd Base)
(define-struct base (x y hp))
;; Base is (make-base Number Number Natural)
;; interp. a friendly base with x/y pos and remaining hp
(define BASE1 (make-base BASE1-X BASE-Y BASE-HP))
(define BASE2 (make-base BASE2-X BASE-Y BASE-HP))
(define BASE3 (make-base BASE3-X BASE-Y BASE-HP))
(define LOB1 (list BASE1 BASE2 BASE3))

(define (fn-for-base b)
  (...  (base-x b)
        (base-y b)
        (base-hp b)))

(@htdd State)
(define-struct st (scene selected))
;; State is (make-st Natural String)
;; interp. the state of the game, one of three states 0, 1, 2 (for start menu,
;;         main game, and game over screen) the current menu item selected (not
;;         relevant if in main game)
(define STATE0 (make-st 0 "play"))

(@htdd ScoreKeeper)
(define-struct sk (score lhit))
;; ScoreKeeper is (make-sk Natural Natural)
;; interp. the present score of a game with score and time of last hit
;;         (for bonuses)
(define SCORE0 (make-sk 0 (- COMBO-TIME)))

(@htdd GameState)
(define-struct gs (guns drockets lode planes bombs erockets bases
                        looe i st sk))
;; GameState is (make-gs (listof Gun) (listof DRocket) (listof DExplosion)
;;                       (listof Plane) (listof Bomb) (listof ERocket) 
;;                       (listof Base) (listof OExplosion) Instantiator)
(define START (make-gs (list GUN1 GUN2) empty empty empty
                       empty empty LOB1 empty INST1 STATE0 SCORE0))

(define (fn-for-gs gs)
  (... (gs-guns gs)
       (gs-drockets gs)
       (gs-lode gs)
       (gs-planes gs)
       (gs-bombs gs)
       (gs-erockets gs)
       (gs-bases gs)
       (gs-looe gs)
       (gs-i gs)
       (gs-st gs)
       (gs-score gs)))


;; =================
;; Functions:

(@htdf main)
(@signature GameState -> GameState)
;; start the world with (main START)

(@template htdw-main)

(define (main gs)
  (big-bang gs                    ; GameState
    (on-tick   next-gs)           ; GameState -> GameState
    (to-draw   render-gs)         ; GameState -> Image
    (on-key    handle-key)        ; GameState KeyEvent -> GameState
    (on-mouse  handle-mouse)      ; GameState Integer Integer MouseEvent
    ; -> GameState
    (stop-when last-state)
    (close-on-stop true)))


(@htdf next-gs)
(@signature GameState -> GameState)
;; produce next positions and check explosions/collisions on all gs objects

(@template fn-composition)

(define (next-gs gs)
  (cond [(zero? (st-scene (gs-st gs))) (handle-explosions
                                        (next-posn-gs gs))]
        [(= 1 (st-scene (gs-st gs))) (check-collisions
                                      (instantiate-new-go
                                       (handle-explosions
                                        (next-posn-gs gs))))]
        [(= 2 (st-scene (gs-st gs))) (instantiate-new-go
                                      (handle-explosions
                                       (next-posn-gs gs)))]))


(@htdf next-posn-gs)
(@signature GameState -> GameState)

(@template GameState)

(define (next-posn-gs gs)
  (local [(define nstate
            (if (and (= 1 (st-scene (gs-st gs))) (empty? (gs-bases gs)))
                (make-st 2 "play again")
                (gs-st gs)))]
    (make-gs (gs-guns gs)
             (next-drockets (gs-drockets gs))
             (gs-lode gs)
             (next-planes (gs-planes gs))
             (gs-bombs gs)
             (next-erockets (gs-erockets gs))
             (gs-bases gs)
             (gs-looe gs)
             (gs-i gs)
             nstate
             (gs-sk gs))))


(@htdf next-erockets)
(@signature (listof ERocket) -> (listof ERocket))
;; produce loer with next pos for all erockets

(@template (listof ERocket))

(define (next-erockets loer)
  (cond [(empty? loer) empty]
        [else
         (local [(define x (erock-x (first loer)))
                 (define y (erock-y (first loer)))
                 (define dx (erock-dx (first loer)))
                 (define dy (erock-dy (first loer)))
                 (define left-boundary (- 0 (/ EROCKET-OFFSET 2)))
                 (define right-boundary (+ WIDTH (/ EROCKET-OFFSET 2)))
                  
                 (define (next-pos r)
                   (make-erock (+ x dx) (+ y dy) dx dy (erock-speed r)
                               (erock-switch? r) (erock-target r)))
                 
                 (define (in-bounds? r)
                   (and (< left-boundary (erock-x r) WIDTH)
                        (< (erock-y r) right-boundary)))]
           (filter in-bounds?
                   (cons (next-pos (first loer))
                         (next-erockets (rest loer)))))]))


(@htdf next-drockets)
(@signature (listof DRocket) -> (listof DRocket))
;; produce lodr with next pos for all drockets

(@template (listof DRocket))

(define (next-drockets lodr)
  (cond [(empty? lodr) empty]
        [else
         (local [(define x (drocket-x (first lodr)))
                 (define y (drocket-y (first lodr)))
                 (define dx (drocket-dx (first lodr)))
                 (define dy (drocket-dy (first lodr)))
                 (define (next-pos r)
                   (make-drocket (+ x dx) (+ y dy) dx dy
                                 (drocket-ex r) (drocket-ey r)))
                 (define (in-bounds? dr)
                   (and (< 0 (drocket-x dr) WIDTH)
                        (< 0 (drocket-y dr) HEIGHT)))]
           (filter in-bounds?
                   (cons (next-pos (first lodr))
                         (next-drockets (rest lodr)))))]))

(@htdf next-planes)
(@signature (listof Plane) -> (listof Plane))
;; produce lop with nextr x/y pos for each plane

(@template (listof Plane))

(define (next-planes lop)
  (cond [(empty? lop) empty]
        [else
         (local [(define x (plane-x (first lop)))
                 (define dx (plane-dx (first lop)))
                 (define nx (- x dx))

                 (define (next-one p)
                   (make-plane nx (plane-y p) (plane-dx p) (plane-target p)
                               (plane-targets p)))
                 (define (in-scene? p) (> (plane-x p) 0))]
           (filter in-scene?
                   (cons (next-one (first lop)) (next-planes (rest lop)))))]))

(@htdf handle-explosions)
(@signature GameState -> GameState)
;; produce gamestate with new explosion values

(@template GameState)

(define (handle-explosions gs)
  (local [(define (explode-dr? dr)
            (and (< (- (drocket-ex dr) RANGE) (drocket-x dr)
                    (+ (drocket-ex dr) RANGE))
                 (< (- (drocket-ey dr) RANGE) (drocket-y dr)
                    (+ (drocket-ey dr) RANGE))))
          
          (define (explode-dr lodr)
            (filter (λ (dr) (not (explode-dr? dr))) lodr))
          
          (define (create-dex lodr)
            (map (λ (dr) (make-dexpl (drocket-x dr) (drocket-y dr) 0 EXPL-RATE))
                 (filter explode-dr? lodr)))

          (define (next-dexplosion de rnr)
            (cons (make-dexpl (dexpl-x de) (dexpl-y de)
                              (+ (dexpl-r de) (dexpl-rate de)) (dexpl-rate de))
                  rnr))

          (define (max? de)
            (< (dexpl-r de) MAX-EXPL-SIZE))
          
          (define (next-dexplosions lode)
            (filter max? (foldr next-dexplosion empty lode)))

          (define (explode-er? er)
            (local [(define ex (posn-x (erock-target er)))
                    (define ey (posn-y (erock-target er)))]
              (and (< (- ex E-RANGE) (erock-x er)
                      (+ ex E-RANGE))
                   (< (- ey E-RANGE) (erock-y er)
                      (+ ey E-RANGE)))))
          
          (define (explode-er loer)
            (filter (λ (er) (not (explode-er? er))) (gs-erockets gs)))

          (define (create-oex loer)
            (map (λ (er) (make-oexpl (erock-x er) (erock-y er) 0 O-EXPL-RATE))
                 (filter explode-er? loer)))

          (define new-oex
            (create-oex (gs-erockets gs)))

          (define (next-oexplosion oe rnr)
            (cons (make-oexpl (oexpl-x oe) (oexpl-y oe)
                              (+ (oexpl-rate oe) (oexpl-r oe))
                              (oexpl-rate oe))
                  rnr))
          
          (define (o-max? ex)
            (< (oexpl-r ex) O-MAX-EXPL-SIZE))
          
          (define (next-oexplosions loer)
            (filter o-max? (foldr next-oexplosion empty loer)))

          (define (hit? expl b)
            (> BASE-RANGE (distance (oexpl-x expl) (oexpl-y expl)
                                    (base-x b) (base-y b))))
          
          (define (check-bases lob)
            (local [(define (check-hits b)
                      (if (ormap (λ (expl) (hit? expl b)) new-oex)
                          (make-base (base-x b) (base-y b) (- (base-hp b) DMG))
                          b))]
              (map check-hits lob)))

          (define all-bases (check-bases (gs-bases gs)))

          (define nbases (filter (λ (b) (> (base-hp b) 0)) all-bases))

          (define base-explosions
            ;; creates explosions for all the destroyed bases
            (map (λ (b) (make-dexpl (base-x b) (base-y b) 0 EXPL-RATE))
                 (filter (λ (b) (<= (base-hp b) 0)) all-bases)))]
    
    (make-gs (gs-guns gs)
             (explode-dr (gs-drockets gs))
             (append (create-dex (gs-drockets gs))
                     base-explosions
                     (next-dexplosions (gs-lode gs)))
             (gs-planes gs) (gs-bombs gs)
             (explode-er (gs-erockets gs))
             nbases
             (append new-oex
                     (next-oexplosions (gs-looe gs)))
             (gs-i gs) (gs-st gs) (gs-sk gs))))


(@htdf instantiate-new-go)
(@signature GameState -> GameState)
;; produce new enemy game objects in accordance with instantiator and set new
;; random spawn times when new game objects are instantiated

(@template GameState)

(define (instantiate-new-go gs)
  (local [(define ins (gs-i gs))
          (define time-step (round (+ (/ (sk-score (gs-sk gs)) SPAWN-FACTOR)
                                      TIME-STEP-MIN)))
          
          (define new-time (+ time-step (inst-timer ins)))
               
          (define new-pt (if (>= new-time (inst-pt ins))
                             (+ new-time (+ PLANE-SPAWN-MIN
                                            (random (- PLANE-SPAWN-MAX
                                                       PLANE-SPAWN-MIN))))
                             (inst-pt ins)))
          
          (define new-ert (if (>= new-time (inst-ert ins))
                              (+ new-time (+ ER-SPAWN-MIN
                                             (random (- ER-SPAWN-MAX
                                                        ER-SPAWN-MIN))))
                              (inst-ert ins)))

          (define (get-slope x y tx ty)
            (if (zero? (- x tx))
                (/ (- y ty) (+ 0.1 (- x tx)))
                (/ (- y ty) (- x tx))))
          
          (define (get-factor slp x tx speed)
            (if (negative? (- x tx))
                (- (/ speed (sqrt (+ 1 (sqr slp)))))
                (/ speed (sqrt (+ 1 (sqr slp))))))

          ;; !!! you might want to be able to select the closest base as target
          (define (gen-plane lob)
            (cons (make-plane (+ WIDTH (image-width PLANE-IMG))
                              (random (round (* HEIGHT 0.4)))
                              PLANE-SPEED
                              (make-posn 1 1) empty)
                  (gs-planes gs)))

          (define score (sk-score (gs-sk gs)))
          
          (define nspeed (+ (/ score FACTOR) EROCKET-SPEED))
          
          (define (gen-erocket lob)
            (local [(define x-start (- (random (+ WIDTH EROCKET-OFFSET))
                                       (/ EROCKET-OFFSET 2)))
                    (define random-target
                      (if (not (empty? lob))
                          (local [(define random-base
                                    (list-ref lob (random (length lob))))]
                            (make-posn (base-x random-base)
                                       (base-y random-base)))
                          (make-posn (random WIDTH)
                                     BASE-Y)))
                    (define rbase-x (posn-x random-target))
                    (define rbase-y (posn-y random-target))
                    (define slope (get-slope x-start EROCKET-Y-START
                                             rbase-x rbase-y))
                    (define factor (get-factor slope x-start rbase-x nspeed))]
              
              (cons (make-erock x-start EROCKET-Y-START
                                (- factor) (- (* slope factor))
                                (+ SPLIT-Y (random (- HEIGHT SPLIT-Y)))
                                true random-target)
                    (gs-erockets gs))))

          (define nplanes
            (if (>= new-time (inst-pt ins))
                (gen-plane (gs-bases gs))
                (gs-planes gs)))

          (define nerockets
            (if (>= new-time (inst-ert ins))
                (gen-erocket (gs-bases gs))
                (gs-erockets gs)))]
    
    (make-gs (gs-guns gs) (gs-drockets gs) (gs-lode gs)
             nplanes
             (gs-bombs gs)
             nerockets
             (gs-bases gs) (gs-looe gs)
             (make-inst new-time new-pt new-ert)
             (gs-st gs) (gs-sk gs))))


(@htdf check-collisions)
(@signature GameState -> GameState)
;; check collisions--reduce base hp, explosions for hit planes and erockets

(@template use-abstract-fn)

(define (check-collisions gs)
  (local [(define lode (gs-lode gs))
          (define lop (gs-planes gs))
          (define loer (gs-erockets gs))

          (define (collision? x y)
            (ormap (λ (de) (< (distance x y (dexpl-x de) (dexpl-y de))
                              (dexpl-r de))) lode))

          (define new-planes
            (filter (λ (p) (not (collision? (plane-x p) (plane-y p)))) lop))
          
          (define new-erockets
            (filter (λ (er) (not (collision? (erock-x er) (erock-y er)))) loer))

          (define hit-planes
            (map (λ (p) (make-posn (plane-x p) (plane-y p)))
                 (filter (λ (p) (collision? (plane-x p) (plane-y p))) lop)))
          
          (define hit-erockets
            (map (λ (er) (make-posn (erock-x er) (erock-y er)))
                 (filter (λ (er) (collision? (erock-x er) (erock-y er))) loer)))

          (define new-oexpl
            (map (λ (pos) (make-oexpl (posn-x pos) (posn-y pos) 0 O-EXPL-RATE))
                 (append hit-planes hit-erockets)))

          (define npoints
            (foldr + 0 (append (map (λ (p) PLANE-SCORE) hit-planes)
                               (map (λ (r) ROCKET-SCORE) hit-erockets))))]
    
    (make-gs (gs-guns gs) (gs-drockets gs)
             (gs-lode gs) new-planes (gs-bombs gs)
             new-erockets (gs-bases gs)
             (append new-oexpl (gs-looe gs))
             (gs-i gs) (gs-st gs)
             (make-sk (+ npoints (sk-score (gs-sk gs)))
                      (sk-lhit (gs-sk gs))))))


(@htdf render-gs)
(@signature GameState -> Image)
;; produce image of all items within gs rendered onto MTS

(@template fn-composition)

(define (render-gs gs)
  (render-text (gs-st gs) (gs-sk gs)
               (render-oexpl
                (gs-looe gs)
                (render-planes
                 (gs-planes gs)
                 (render-bases
                  (gs-bases gs)
                  (render-erockets
                   (gs-erockets gs)
                   (render-dexpl
                    (gs-lode gs)
                    (render-drockets
                     (gs-drockets gs)
                     (render-guns (gs-guns gs))))))))))


(@htdf render-text)
(@signature State ScoreKeeper Image -> Image)
;; render text over game MTS

(@template State)

(define (render-text st sk i)
  (cond [(zero? (st-scene st))
         (local [(define menu
                   (if (string=? (st-selected st) "play")
                       (above/align "center"
                                    (make-text "PLAY" MENU-TEXT-SIZE
                                               HIGHLIGHTED-TEXT-COLOR true)
                                    (make-text "QUIT" MENU-TEXT-SIZE
                                               MENU-TEXT-COLOR false))
                       (above/align "center" (make-text "PLAY" MENU-TEXT-SIZE
                                                        MENU-TEXT-COLOR false)
                                    (make-text "QUIT" MENU-TEXT-SIZE
                                               HIGHLIGHTED-TEXT-COLOR true))))
                 (define instructions (make-text INSTRUCTIONS INSTRUCTIONS-SIZE
                                                 INSTRUCTIONS-COLOR false))]
           (place-image (make-text GAME-TITLE TITLE-TEXT-SIZE
                                   TITLE-TEXT-COLOR false)
                        CENTER TITLE-Y 
                        (place-image menu CENTER MENU1-Y
                                     (place-image instructions CENTER
                                                  INSTRUCTIONS-Y i))))]
        [(= 1 (st-scene st))
         (overlay/align "right" "top"
                        (score-text (number->string (sk-score sk)))
                        i)]
        [(= 2 (st-scene st))
         (local [(define go-menu
                   (if (string=? "play again" (st-selected st))
                       (above/align "center"
                                    (make-text "MAIN MENU" MENU-TEXT-SIZE
                                               HIGHLIGHTED-TEXT-COLOR true)
                                    (make-text "QUIT" MENU-TEXT-SIZE
                                               MENU-TEXT-COLOR false))
                       (above/align "center"
                                    (make-text "MAIN MENU" MENU-TEXT-SIZE
                                               MENU-TEXT-COLOR false)
                                    (make-text "QUIT" MENU-TEXT-SIZE
                                               HIGHLIGHTED-TEXT-COLOR true))))]
           (place-image (make-text "GAME OVER" TITLE-TEXT-SIZE
                                   TITLE-TEXT-COLOR false)
                        CENTER (/ TITLE-Y 2)
                        (place-image
                         (above (make-text "YOUR SCORE:" MENU-TEXT-SIZE
                                           HIGHLIGHTED-TEXT-COLOR false)
                                (make-text (number->string (sk-score sk))
                                           MENU-TEXT-SIZE
                                           HIGHLIGHTED-TEXT-COLOR false))
                         CENTER TITLE-Y
                         (place-image go-menu
                                      CENTER MENU1-Y i))))]))


(@htdf render-oexpl)
(@signature (listof Plane) Image -> Image)
;; render plane img for all planes in gs

(define (render-oexpl looe img)
  (local [(define (render-one oe i)
            (local [(define r (oexpl-r oe))
                    (define points (if (< r 20) 5 (quotient r 4)))]
              (place-image (radial-star points (* r 0.4) r "solid" "red")
                           (oexpl-x oe) (oexpl-y oe) i)))]
    (cond [(empty? looe) img]
          [else
           (render-one (first looe) (render-oexpl (rest looe) img))])))


(@htdf render-planes)
(@signature (listof Plane) Image -> Image)
;; render plane img for all planes in gs

(define (render-planes lop img)
  (local [(define (render-one p i)
            (place-image PLANE-IMG (plane-x p) (plane-y p) i))]
    (cond [(empty? lop) img]
          [else
           (render-one (first lop) (render-planes (rest lop) img))])))


(@htdf render-bases)
(@signature (listof Base) Image -> Image)
;; render erocket img for all erockets in gs

(define (render-bases lob img)
  (local [(define (render-one b i)
            (local [(define hp-x (base-x b))
                    (define hp-y (+ (base-y b) BASE-TEXT-OFFSET))]
              (place-image BASE-IMG (base-x b) (base-y b)
                           (place-image
                            (display-hp (number->string (base-hp b)))
                            hp-x hp-y i))))]
    (cond [(empty? lob) img]
          [else
           (render-one (first lob) (render-bases (rest lob) img))])))


(@htdf render-erockets)
(@signature (listof ERocket) Image -> Image)
;; render erocket img for all erockets in gs

(define (render-erockets loer img)
  (local [(define (render-one er i)
            (local [(define dx (erock-dx er))
                    (define dy (erock-dy er))
                    (define r (if (negative? dx)
                                  (+ (rad->deg (- (atan (/ dy dx)))) 90)
                                  (- (rad->deg (- (atan (/ dy dx)))) 90)))]
              (place-image (rotate r ER-IMG) (erock-x er) (erock-y er) i)))]
    (cond [(empty? loer) img]
          [else
           (render-one (first loer) (render-erockets (rest loer) img))])))


(@htdf render-dexpl)
(@signature (listof DExplosion) Image -> Image)
;; produce rendered image of explosions over MTS

(@template (listof DExplosion))

(define (render-dexpl lode img)
  (local [(define (render-one de i)
            (place-image (circle (dexpl-r de) "solid" "orange")
                         (dexpl-x de) (dexpl-y de) i))]
    (cond [(empty? lode) img]
          [else
           (render-one (first lode) (render-dexpl (rest lode) img))])))


(@htdf render-drockets)
(@signature (listof DRocket) Image -> Image)
;; produce image of drockets on MTS
(check-expect (render-drockets empty MTS) MTS)
(check-expect (render-drockets (list (make-drocket 100 100 5 -4 50 50))
                               MTS)
              (place-image DR-IMG 100 100 MTS))


(@template (listof DRocket))

(define (render-drockets lodr img)
  (local [(define (render-rocket r img)
            (place-image DR-IMG (drocket-x r) (drocket-y r) img))]
    (cond [(empty? lodr) img]
          [else
           (render-rocket (first lodr) (render-drockets (rest lodr) img))])))


(@htdf render-guns)
(@signature (listof Gun) -> Image)
;; render image of game guns with rotation r

(@template (listof Gun))

(define (render-guns log)
  (local [(define (render-gun g img)
            (place-image (rotate (gun-r g) GUN-IMG)
                         (gun-x g) (gun-y g) img))]
    (cond [(empty? log) MTS]
          [else
           (render-gun (first log) (render-guns (rest log)))])))

(@htdf handle-mouse)
(@signature GameState Integer Integer MouseEvent -> GameState)


(@template MouseEvent)

(define (handle-mouse gs x y me)
  (cond [(mouse=? me "button-down")
         (if (gun-active? (first (gs-guns gs)))
             (fire-drocket gs x y 0)
             (fire-drocket gs x y 1))]
        [(mouse=? me "move")
         (make-gs (update-guns (gs-guns gs) x y)
                  (gs-drockets gs) (gs-lode gs) (gs-planes gs) (gs-bombs gs) 
                  (gs-erockets gs) (gs-bases gs) (gs-looe gs) (gs-i gs)
                  (gs-st gs) (gs-sk gs))]
        [else gs]))


(@htdf fire-drocket)
(@signature GameState Number Number Natural -> GameState)
;; produce new rocket headed toward mouse x/y pos, left gun if 0, right if 1
(check-expect (fire-drocket START 250 GUN1-Y 0)
              (make-gs (gs-guns START)
                       (list (make-drocket GUN1-X GUN1-Y
                                           ROCKET-SPEED 0 250 GUN1-Y))
                       (gs-lode START) (gs-planes START) (gs-bombs START)
                       (gs-erockets START) (gs-bases START) (gs-looe START)
                       (gs-i START) (gs-st START) (gs-sk START)))

(@template GameState)

(define (fire-drocket gs x y g)
  (local [(define gun-x (if (zero? g) GUN1-X GUN2-X))
          (define gun-y (if (zero? g) GUN1-Y GUN2-Y))
          (define slope (if (zero? (- x gun-x))
                            (/ (- y gun-y) (+ 0.1 (- x gun-x)))
                            (/ (- y gun-y) (- x gun-x))))
          (define (get-factor slp)
            (if (negative? (- x gun-x))
                (- (/ ROCKET-SPEED (sqrt (+ 1 (sqr slp)))))
                (/ ROCKET-SPEED (sqrt (+ 1 (sqr slp))))))
          (define nrocket (make-drocket gun-x gun-y
                                        (get-factor slope)
                                        (* (get-factor slope) slope)
                                        x y))]
    (make-gs (gs-guns gs)
             (cons nrocket (gs-drockets gs))
             (gs-lode gs) (gs-planes gs) (gs-bombs gs) (gs-erockets gs)
             (gs-bases gs) (gs-looe gs) (gs-i gs) (gs-st gs) (gs-sk gs))))


(@htdf update-guns)
(@signature (listof Gun) Number Number -> (listof Gun))
;; produce log with new rotation depending on mouse x/y

(@template (listof Gun))

(define (update-guns log x y)
  (local [(define (update g)
            (cond [(or (zero? (- x (gun-x g))) (zero? (- (gun-y g) y))) g]
                  [else
                   (make-gun (gun-x g) (gun-y g)
                             (rad->deg (atan (/ (- (gun-y g) y)
                                                (- x (gun-x g)))))
                             (gun-ammo g) (gun-hp g) (gun-active? g))]))]
    (cond [(empty? log) empty]
          [else
           (cons (update (first log))
                 (update-guns (rest log) x y))])))


(@htdf handle-key)
(@signature GameState KeyEvent -> GameState)
;; on spacebar, produce new gs with active gun changed
;; ! program can only currently handle 2 guns

(@template KeyEvent)

(define (handle-key gs ke)
  (cond [(and (or (key=? ke "up") (key=? ke "down"))
              (zero? (st-scene (gs-st gs))))
         (local [(define next-item (if (string=? (st-selected (gs-st gs))
                                                 "play")
                                       "quit"
                                       "play"))]
           (make-gs (gs-guns gs) (gs-drockets gs) (gs-lode gs) (gs-planes gs)
                    (gs-bombs gs) (gs-erockets gs) (gs-bases gs) (gs-looe gs)
                    (gs-i gs)
                    (make-st 0 next-item) (gs-sk gs)))]
        
        [(and (key=? ke "\r") (zero? (st-scene (gs-st gs)))
              (string=? (st-selected (gs-st gs)) "play"))
         (make-gs (gs-guns gs) (gs-drockets gs) (gs-lode gs) (gs-planes gs)
                  (gs-bombs gs) (gs-erockets gs) LOB1 (gs-looe gs)
                  (gs-i gs)
                  (make-st 1 "play") (gs-sk gs))]
        
        [(and (or (key=? ke "up") (key=? ke "down"))
              (= 2 (st-scene (gs-st gs))))
         (local [(define next-item (if (string=? (st-selected (gs-st gs))
                                                 "play again")
                                       "quit"
                                       "play again"))]
           (make-gs (gs-guns gs) (gs-drockets gs) (gs-lode gs) (gs-planes gs)
                    (gs-bombs gs) (gs-erockets gs) (gs-bases gs) (gs-looe gs)
                    (gs-i gs)
                    (make-st 2 next-item) (gs-sk gs)))]
        
        [(and (key=? ke "\r") (= 2 (st-scene (gs-st gs)))
              (string=? (st-selected (gs-st gs))
                        "play again"))
         (make-gs (gs-guns gs) (gs-drockets gs) (gs-lode gs) (gs-planes gs)
                  (gs-bombs gs) (gs-erockets gs) (gs-bases gs) (gs-looe gs)
                  (gs-i gs) (make-st 0 "play") (make-sk 0 COMBO-TIME))]

        [(and (key=? ke "\r") (string=? (st-selected (gs-st gs)) "quit"))
         (make-gs (gs-guns gs) (gs-drockets gs) (gs-lode gs) (gs-planes gs)
                  (gs-bombs gs) (gs-erockets gs) LOB1 (gs-looe gs)
                  (gs-i gs)
                  (make-st 3 "quit") (gs-sk gs))]
        
        [(key=? ke " ")
         (local [(define (switch g)
                   (make-gun (gun-x g) (gun-y g) (gun-r g) (gun-ammo g)
                             (gun-hp g) (not (gun-active? g))))]
           (local [(define guns (gs-guns gs))
                   (define gun1 (first guns))
                   (define gun2 (first (rest guns)))
                   (define nlog (list (switch gun1) (switch gun2)))]
             (make-gs nlog
                      (gs-drockets gs) (gs-lode gs) (gs-planes gs) (gs-bombs gs)
                      (gs-erockets gs) (gs-bases gs) (gs-looe gs) (gs-i gs)
                      (gs-st gs) (gs-sk gs))))]
        
        [else gs]))


(@htdf last-state)
(@signature GameState -> GameState)
;; the termination condition of the program, the last GameState

(define (last-state gs)
  (= 3 (st-scene (gs-st gs))))



(@htdf rad->deg deg->rad)
(@signature Number -> Number)
(@signature Number -> Number)
;; produce degrees from radians
(check-expect (rad->deg (/ pi 4)) 45)
(check-expect (rad->deg (/ pi 2)) 90)
(check-within (deg->rad 90) (/ pi 2) 0.001)
(check-within (deg->rad 45) (/ pi 4) 0.001)

(@template Number)

(define (rad->deg n)
  (round (inexact->exact (/ (* 180 n) pi))))

(@template Number)

(define (deg->rad n)
  (/ (* pi n) 180))

(@htdf distance)
(@signature Number Number Number Number -> Number)
;; produce distance between point x1/y1 and x2/y2
(check-expect (distance 1 0 0 0) 1)
(check-within (distance 1 0 0 1) (sqrt 2) 0.001)

(@template Number)

(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))


(@htdf make-text)
(@signature String Natural Color Boolean -> Image)
;; produce image of text with size and color (2nd and 3rd arguments) and
;; #t/#f for underlined or not

(define (make-text txt s c u)
  (text/font txt s c FONT "default" "normal" "normal" u))


(@htdf score-text)
(@signature String -> Image)
;; produce image of score count

(define (score-text txt)
  (text/font txt SCORE-TEXT-SIZE SCORE-TEXT-COLOR
             SCORE-FONT "default" "normal" "normal" false))


(@htdf display-hp)
(@signature String -> Image)
;; produce image of base hp

(define (display-hp txt)
  (text/font txt BASE-TEXT-SIZE BASE-TEXT-COLOR
             SCORE-FONT "default" "normal" "normal" false))


;; MAIN FUNCTION CALL

(main START)