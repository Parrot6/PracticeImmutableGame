;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname alien) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Matthew Martin
; hw03 - handling lists
; ITEC380
; http://www.radford.edu/~itec380/2017fall-ibarland/Homeworks/lists/lists.html
(require 2htdp/image)

#|
;Data Def'n a team is:
; a name (non-empty string), AND
; an offense rating (real number), AND
; a defense rating (real number).

"john"
"mark"
3.4
6.4
0

(define-struct team (name offense defense))
;#Default Functions#
;make-team : string number real-number
;team-name
;team-offense
;team-defense
;team?

(define (func-for-team a-team)
  (... (team-name a-team)
   ... (team-offense a-team)
   ... (team-defense a-team)))

(define t1 (make-team "Radford Rioters" 3.1 5.5))
(define t2 (make-team "Christiansburg Criminals" 3.0 5.9))
(define t3 (make-team "Blacksburg Bandits" 1.0 0))

;team>> : team team -> boolean
;one team is “greater” than another if its offense is higher than the other’s defense, and its defense is higher than the other’s offense.
(define (team>?  a-team b-team)(and (> (team-offense a-team) (team-defense b-team)) (> (team-defense a-team) (team-offense b-team)) ))

(check-expect (team>? t1 t2) #false)
(check-expect (team>? (make-team "Radford Rioters" 0 0) (make-team "Radford Rioppers" 0 0)) #false)
(check-expect (team>? (make-team "Radford Rioters" 7 7) (make-team "Radford Rioppers" 1 1)) #true)
(check-expect (team>? (make-team "Radford Rioters" 7 1) (make-team "Radford Rioppers" 7 1)) #false)
(check-expect (team>? (make-team "Radford Rioters" 1 7) (make-team "Radford Rioppers" 1 7)) #false)
(check-expect (team>? (make-team "Radford Rioters" 7 7) (make-team "Radford Rioppers" 7 7)) #false)
|#



#|##################################################################################################
###################################### ALIENS GAME #################################################
###########################################################################################parrot6|#


#|####################### MISSILE #########################################################parrot6|#

;Data Def'n a missile is:
; a xLoc (real number), AND
; a yLoc (real number), AND
; a xvelocity (real number), AND
; a yvelocity (real number)
(define-struct missile (xLoc yLoc xVel yVel))

(define (func-for-missile a-missile)
  (... (missile-xLoc a-missile)
       ... (missile-yLoc a-missile)
       ... (missile-xVel a-missile)
       ... (missile-yVel a-missile)))

;Data Def'n a list-of-missiles is:
; a missile , AND
; a list

(define (func-forlistofmissiles missilesList) (... (make-missile (missile-xLoc (first missilesList))
                                                                 (missile-yLoc (first missilesList))
                                                                 (missile-xSpeed (first missilesList))
                                                                 (missile-ySpeed (first missilesList)) (func-forlistofmissiles (cdr missilesList)))))
(define mis1 (cons (make-missile 460 370 100 270) '()))
(cons (make-missile 0 0 100 270) (cons (make-missile 100 100 100 270) (cons (make-missile 200 200 100 270) '())))
(define missilefleet1 (cons (make-missile 50 100 100 270) (cons (make-missile 150 100 100 270) (cons (make-missile 200 100 100 270) (cons (make-missile 250 100 100 270) (cons (make-missile 300 100 100 270) (cons (make-missile 350 100 100 270) (cons (make-missile 400 100 100 270) '()))))))))
(define missilefleet2 (cons (make-missile 50 200 100 270) (cons (make-missile 150 200 100 270) (cons (make-missile 200 200 100 270) (cons (make-missile 250 100 200 270) (cons (make-missile 300 200 100 270) (cons (make-missile 350 200 100 270) (cons (make-missile 400 200 100 270) '()))))))))
(define parkedmissilefleet1 (cons (make-missile 50 100 0 0) (cons (make-missile 150 100 0 0) (cons (make-missile 200 100 0 0) (cons (make-missile 250 100 0 0) (cons (make-missile 300 100 0 0) (cons (make-missile 350 100 0 0) (cons (make-missile 400 100 0 0) '()))))))))
(define parkedmissilefleet2 (cons (make-missile 50 200 0 0) (cons (make-missile 150 200 0 0) (cons (make-missile 200 200 0 0) (cons (make-missile 250 200 0 0) (cons (make-missile 300 200 0 0) (cons (make-missile 350 200 0 0) (cons (make-missile 400 200 0 0) '()))))))))


;draw-missile : missile, image -> image
(define (draw-missile a-missile background) (place-image missile-graphic (missile-xLoc a-missile) (missile-yLoc a-missile) background))

(check-expect (draw-missile (make-missile 200 200 0 50) GameBoard) (place-image missile-graphic  200 200 GameBoard))
(check-expect (draw-missile (make-missile 0 0 0 50) GameBoard) (place-image missile-graphic  0 0 GameBoard))
(check-expect (draw-missile (make-missile 600 800 0 50) GameBoard) (place-image missile-graphic  600 800 GameBoard))
(check-expect (draw-missile (make-missile 605 805 0 50) GameBoard) (place-image missile-graphic  605 805 GameBoard))
(check-expect (draw-missile (make-missile -2 -2 0 50) GameBoard) (place-image missile-graphic  -2 -2 GameBoard))
(check-expect (draw-missile (make-missile 800 600 0 50) GameBoard) (place-image missile-graphic  800 600 GameBoard))

;draw-aliens : list-of-missiles, image → image
(define (draw-missiles missilesList background) (cond [(empty? missilesList) background]
                                                      [true (draw-missile (first missilesList) (draw-missiles (cdr missilesList) background))]))
       
(check-expect (draw-missiles (cons (make-missile 0 0 0 50) '()) GameBoard) (place-image missile-graphic  0 0 GameBoard))
(check-expect (draw-missiles '() GameBoard) GameBoard)
(check-expect (draw-missiles missilefleet1 GameBoard) (place-image missile-graphic  50 100 (place-image missile-graphic  150 100 (place-image missile-graphic  200 100 (place-image missile-graphic  250 100
                                                                                                                                                                                    (place-image missile-graphic  300 100 (place-image missile-graphic  350 100 (place-image missile-graphic  400 100 GameBoard))))))))

;move-missile : missile -> missile
(define (move-missile a-missile)
  (make-missile (+ (missile-xLoc a-missile) (missile-xVel a-missile))
                (+ (missile-yLoc a-missile) (missile-yVel a-missile))
                (missile-xVel a-missile)
                (missile-yVel a-missile)))

(check-expect (move-missile (make-missile 0 0 100 270)) (make-missile 100 270 100 270))
(check-expect (move-missile (make-missile 200 200 -10 10)) (make-missile 190 210 -10 10))
(check-expect (move-missile (make-missile 200 200 0 0)) (make-missile 200 200 0 0))

;move-missiles : list-of-missile → list-of-missile
(define (move-missiles missilesList) (cond [(empty? missilesList) '()]
                                           [#true (cons (move-missile (first missilesList)) (move-missiles (cdr missilesList)))]))

(check-expect (move-missiles (cons (make-missile 0 0 100 270) '())) (cons (make-missile 100 270 100 270) '()))
(check-expect (move-missiles (cons (make-missile 0 0 100 270) (cons (make-missile 100 100 100 270) (cons (make-missile 200 200 100 270) '())))) (cons (make-missile 100 270 100 270) (cons (make-missile 200 370 100 270) (cons (make-missile 300 470 100 270) '()))))

;missiles-remaining : a-alien, list-of-missile → list-of-missile
(define (missiles-remaining a-alien list-of-missile) (if (not (empty? list-of-missile))
                                                    (if (alien-collide-missile? a-alien (first list-of-missile))
                                                        (missiles-remaining a-alien (cdr list-of-missile))
                                                        (cons (first list-of-missile) (missiles-remaining a-alien (cdr list-of-missile))))
                                                    '() ))

;missiles-remaining* : list-of-alien, list-of-missile → list-of-missile
(define (missiles-remaining* list-of-alien list-of-missile)(if (not (empty? list-of-alien))
                                                             (missiles-remaining*  (cdr list-of-alien) (missiles-remaining (first list-of-alien) list-of-missile))
                                                             list-of-missile))

(check-expect (missiles-remaining* fleet1 missilefleet1) empty)
(check-expect (missiles-remaining* (cons (make-alien 500 500 0 0) fleet1) missilefleet1) empty)
(check-expect (missiles-remaining* fleet1 (list (make-missile 50 100 100 270))) empty)
(check-expect (missiles-remaining* fleet1 (list (make-missile 50 100 100 270) (make-missile 400 400 100 270))) (cons (make-missile 400 400 100 270) empty))



#|####################### ALIENS #########################################################parrot6|#
;Data Def'n a alien is:
; a Health Points (natural), AND
; a xLoc (real number), AND
; a yLoc (real number), AND
; a speed (real number), AND
; a diving? (boolean)

(define-struct alien (xLoc yLoc xSpeed ySpeed))

(make-alien 0 0 100 #true)
(make-alien 100 100 270 #false)
(make-alien 150 150 90 #true)

(define (func-for-alien a-alien)
  (   ... (alien-xLoc a-alien)
          ... (alien-yLoc a-alien)
          ... (alien-xSpeed a-alien)
          ... (alien-ySpeed a-alien)))

;Data Def'n a list-of-alien is:
; a alien , AND
; a list
;alien : alien?
;list : list?
(define (func-forlistofaliens alienList) (... (make-alien (alien-xLoc (first alienList))
                                                          (alien-yLoc (first alienList))
                                                          (alien-xSpeed (first alienList))
                                                          (alien-ySpeed (first alienList)) (func-forlistofaliens (cdr alienList)))))

(cons (make-alien 0 0 100 270) '())
(cons (make-alien 0 0 100 270) (cons (make-alien 100 100 100 270) (cons (make-alien 200 200 100 270) '())))
(define fleet1 (cons (make-alien 50 100 100 270) (cons (make-alien 150 100 100 270) (cons (make-alien 200 100 100 270) (cons (make-alien 250 100 100 270) (cons (make-alien 300 100 100 270) (cons (make-alien 350 100 100 270) (cons (make-alien 400 100 100 270) '()))))))))
(define fleet3 (cons (make-alien 550 100 100 270) (cons (make-alien 450 100 100 270) (cons (make-alien 200 100 100 270) (cons (make-alien 150 100 100 270) (cons (make-alien 100 100 100 270) (cons (make-alien 50 100 100 270) (cons (make-alien 0 100 100 270) '()))))))))
(define fleet2 (cons (make-alien 50 200 100 270) (cons (make-alien 150 200 100 270) (cons (make-alien 200 200 100 270) (cons (make-alien 250 100 200 270) (cons (make-alien 300 200 100 270) (cons (make-alien 350 200 100 270) (cons (make-alien 400 200 100 270) '()))))))))
(define parkedfleet1 (cons (make-alien 50 100 0 0) (cons (make-alien 150 100 0 0) (cons (make-alien 200 100 0 0) (cons (make-alien 250 100 0 0) (cons (make-alien 300 100 0 0) (cons (make-alien 350 100 0 0) (cons (make-alien 400 100 0 0) '()))))))))
(define parkedfleet2 (cons (make-alien 50 200 0 0) (cons (make-alien 150 200 0 0) (cons (make-alien 200 200 0 0) (cons (make-alien 250 200 0 0) (cons (make-alien 300 200 0 0) (cons (make-alien 350 200 0 0) (cons (make-alien 400 200 0 0) '()))))))))
(define alienxVel 20)
(define alienyVel 0)
(define NA (make-alien 200 200 alienxVel alienyVel))
(define (make-alienX xLoc yLoc) (make-alien xLoc yLoc alienxVel alienyVel))

;move-alien : alien -> alien
(define (move-alien a-alien)
  (make-alien (+ (alien-xLoc a-alien) (alien-xSpeed a-alien))
              (+ (alien-yLoc a-alien) (alien-ySpeed a-alien))
              (alien-xSpeed a-alien)
              (alien-ySpeed a-alien)))

(check-expect (move-alien (make-alien 0 0 100 270)) (make-alien 100 270 100 270))
(check-expect (move-alien (make-alien 200 200 -10 10)) (make-alien 190 210 -10 10))
(check-expect (move-alien (make-alien 200 200 0 0)) (make-alien 200 200 0 0))

;move-aliens : list-of-alien → list-of-alien
(define (move-aliens alienList) (cond [(empty? alienList) '()]
                                      [true (cons (move-alien (first alienList)) (move-aliens (cdr alienList)))]))

(check-expect (move-aliens (cons (make-alien 0 0 100 270) '())) (cons (make-alien 100 270 100 270) '()))
(check-expect (move-aliens (cons (make-alien 0 0 100 270) (cons (make-alien 100 100 100 270) (cons (make-alien 200 200 100 270) '())))) (cons (make-alien 100 270 100 270) (cons (make-alien 200 370 100 270) (cons (make-alien 300 470 100 270) '()))))

;draw-alien :alien, image -> image
(define (draw-alien a-alien background) (place-image alien-graphic (alien-xLoc a-alien) (alien-yLoc a-alien) background))

(check-expect (draw-alien (make-alien 0 0 0 50) GameBoard) (place-image alien-graphic  0 0 GameBoard))

;draw-aliens : list-of-alien, image → image
(define (draw-aliens alienList background) (cond [(empty? alienList) background]
                                                 [true (draw-alien (first alienList) (draw-aliens (cdr alienList) background))]))
       
(check-expect (draw-aliens (cons (make-alien 0 0 0 50) '()) GameBoard) (place-image alien-graphic  0 0 GameBoard))
(check-expect (draw-aliens '() GameBoard) GameBoard)
(check-expect (draw-aliens fleet1 GameBoard) (place-image alien-graphic  50 100 (place-image alien-graphic  150 100 (place-image alien-graphic  200 100 (place-image alien-graphic  250 100 (place-image alien-graphic  300 100 (place-image alien-graphic  350 100 (place-image alien-graphic  400 100 GameBoard))))))))

;alien-collide-missile?
(define (alien-collide-missile? a-alien a-missile) (overlap? (alien-xLoc a-alien) (alien-yLoc a-alien) AlienWidth AlienHeight  (missile-xLoc a-missile) (missile-yLoc a-missile) MissileWidth MissileHeight))

(check-expect (alien-collide-missile? (make-alien 0 0 100 200) (make-missile 0 0 100 100)) #true)

(check-expect (alien-collide-missile? (make-alien 300 300 100 200) (make-missile 100 100 100 100)) #false)

;aliens-remaining : list-of-alien, missile → list-of-alien
(define (aliens-remaining list-of-alien a-missile) (if (not (empty? list-of-alien))
                                                    (if (alien-collide-missile? (first list-of-alien) a-missile)
                                                        (aliens-remaining (cdr list-of-alien) a-missile)
                                                        (cons (first list-of-alien) (aliens-remaining (cdr list-of-alien) a-missile)))
                                                    '() ))
;aliens-remaining* : list-of-alien, list-of-missile → list-of-alien
(define (aliens-remaining* list-of-alien list-of-missile)(if (not (empty? list-of-missile))
                                                             (aliens-remaining* (aliens-remaining list-of-alien (first list-of-missile))(cdr list-of-missile))
                                                             list-of-alien))

(check-expect (aliens-remaining* fleet1 missilefleet1) empty)
(check-expect (aliens-remaining* (cons (make-alien 500 500 0 0) fleet1) missilefleet1) (cons (make-alien 500 500 0 0) empty))

(check-expect (aliens-remaining* fleet1 (list (make-missile 50 100 100 270))) (cons (make-alien 150 100 100 270) (cons (make-alien 200 100 100 270) (cons (make-alien 250 100 100 270) (cons (make-alien 300 100 100 270) (cons (make-alien 350 100 100 270) (cons (make-alien 400 100 100 270) '()))))))
)
;fleetchange? list-of-alien -> boolean
(define (fleetchange? list-of-aliens) ...)

;xlocminimum : list-of-alien -> alien
;returns lowest xLoc value alien from list
(define (xlocminimum listAlien alien)
  (cond
    [(empty? listAlien) alien]
    [(alien<alien? (first listAlien)  alien) (xlocminimum (rest listAlien) (first listAlien))]
    [else (xlocminimum (rest listAlien) alien)]))

;xlocmaximum : list-of-alien -> alien
;returns highest xLoc value alien from list
(define (xlocmaximum listAlien alien)
  (cond
    [(empty? listAlien) alien]
    [(alien<alien? (first listAlien) alien) (xlocmaximum (rest listAlien) alien) ]
    [else (xlocmaximum (rest listAlien) (first listAlien))]))


(define (aliencompare< alienlist)
  (if (null? alienlist)
      empty
      (xlocminimum (rest alienlist) (first alienlist))))

(define (aliencompare> alienlist)
  (if (null? alienlist)
      empty
      (xlocmaximum (rest alienlist) (first alienlist))))

;alien<alien? alien alien -> boolean
;return if first alien is smaller xLoc than the second alien
(define (alien<alien? alien1 alien2) (if (< (alien-xLoc alien1) (alien-xLoc alien2)) #true #false))


(check-expect (aliencompare< fleet1) (make-alien 50 100 100 270))
(check-expect (aliencompare< empty) empty)
(check-expect (aliencompare< (cons (make-alien -50 100 100 270) fleet1)) (make-alien -50 100 100 270))
(check-expect (aliencompare< fleet3) (make-alien 0 100 100 270))
(check-expect (aliencompare> fleet1) (make-alien 400 100 100 270))
(check-expect (aliencompare> empty) empty)
(check-expect (aliencompare> (cons (make-alien -50 100 100 270) fleet1)) (make-alien 400 100 100 270))
(check-expect (aliencompare> fleet3) (make-alien 550 100 100 270))

(check-expect (alien<alien? (make-alien 0 0 100 270) (make-alien -20 0 100 270)) #false)
(check-expect (alien<alien? (make-alien -70 0 100 270) (make-alien -20 0 100 270)) #true)

;fleet-change? list-of-alien -> boolean
(define (fleet-change? list-of-aliens)(if (or (not (onscreen? (aliencompare< list-of-aliens)))
                                              (not (onscreen? (aliencompare> list-of-aliens))))
                                          #true
                                          #false))
;(check-expect (fleet-change? fleet1) #false)
;(check-expect (fleet-change? (cons (make-alien -21 300 30 0) fleet1)) #true)

(define (fleet-change list-of-aliens) (if (not (empty? list-of-aliens))
                                         (cons (make-alien
                                            (alien-xLoc (first list-of-aliens))
                                            (alien-yLoc (first list-of-aliens))
                                            (- (alien-xSpeed (first list-of-aliens)))
                                            (alien-ySpeed (first list-of-aliens))) (fleet-change (rest list-of-aliens)))
                                         list-of-aliens))
  
(check-expect (fleet-change fleet1) (cons (make-alien 50 100 -100 270) (cons (make-alien 150 100 -100 270) (cons (make-alien 200 100 -100 270) (cons (make-alien 250 100 -100 270) (cons (make-alien 300 100 -100 270) (cons (make-alien 350 100 -100 270) (cons (make-alien 400 100 -100 270) '()))))))))

(define (onscreen? alien) (overlap? (alien-xLoc alien) (alien-yLoc alien) AlienWidth AlienHeight 0 0 GameBoardWidth GameBoardHeight))


;(check-expect (onscreen? (make-alien -21 300 30 0)) #false)
;(check-expect (onscreen? (make-alien -201 300 30 0)) #false)
;(check-expect (onscreen? (make-alien 1000 300 30 0)) #false)
;(check-expect (onscreen? (make-alien 300 300 30 0)) #true)

#|####################### SHIP #########################################################parrot6|#
;Data Def'n a ship is:
; a xLoc (real number), AND
; a yLoc (real number), AND
; a speed (real number)
; a Health Points (natural), AND

(define-struct ship (xLoc yLoc speed hp))

(make-ship 0 0 100 270)
(make-ship 500 400 25 270)
(define startship (make-ship 150 150 50 90))


(define (func-for-ship a-ship)
  (... (ship-xLoc a-ship)
       ... (ship-yLoc a-ship)
       ... (ship-speed a-ship)
       ... (ship-hp a-ship)))

;draw-ship : ship, image -> image
(define (draw-ship a-ship background) (place-image ship-graphic (ship-xLoc a-ship) (ship-yLoc a-ship) background))


(check-expect (draw-ship (make-ship 0 0 0 50) GameBoard) (place-image ship-graphic  0 0 GameBoard))
;ship-handle-key : ship, key-event? -> ship
;moves the ship in reaction to key-press
(define (ship-handle-key a-ship keyPress)
  (make-ship
   (+ (ship-xLoc a-ship) (if (string=? keyPress "d") (ship-speed a-ship) (if (string=? keyPress "a") (* -1 (ship-speed a-ship)) 0)))
   (+ (ship-yLoc a-ship) (if (string=? keyPress "w") (ship-speed a-ship) (if (string=? keyPress "s") (* -1 (ship-speed a-ship)) 0)))
   (ship-speed a-ship)
   (ship-hp a-ship)))

(check-expect (ship-handle-key (make-ship 0 0 100 270) "d") (make-ship 100 0 100 270))
(check-expect (ship-handle-key (make-ship 0 0 100 270) "s") (make-ship 0 -100 100 270))
(check-expect (ship-handle-key (make-ship 0 0 100 270) "w") (make-ship 0 100 100 270))
(check-expect (ship-handle-key (make-ship 0 0 100 270) "a") (make-ship -100 0 100 270))

;ship-collide-missile? ship, missile -> boolean
(define (ship-collide-missile? ship a-missile) (overlap? (ship-xLoc ship) (ship-yLoc ship) ShipWidth ShipHeight (missile-xLoc a-missile) (missile-yLoc a-missile) MissileWidth MissileHeight))

(check-expect (ship-collide-missile? (make-ship 0 0 100 100) (make-missile 0 0 100 100)) #true)
(check-expect (ship-collide-missile? (make-ship 300 300 100 100) (make-missile 100 100 100 100)) #false)

;ship-remaining? : ship, missile → ship/#false
(define (ship-remaining? ship a-missile)(if (ship-collide-missile? ship a-missile)
                                                        (if (> (- (ship-hp ship) 100) 0)
                                                            (make-ship (ship-xLoc ship) (ship-yLoc ship) (ship-speed ship) (- (ship-hp ship) 100))
                                                            #false)
                                                        ship))

(check-expect (ship-remaining? (make-ship 0 0 100 100) (make-missile 0 0 100 100)) #false)
(check-expect (ship-remaining? (make-ship 300 300 100 100) (make-missile 100 100 100 100)) (make-ship 300 300 100 100))


#|####################### WORLD ###########################################################parrot6|#

;Data Def'n a world is:
; a SHIP, AND
; a list of missiles,AND
; a list of aliens, AND
; a “fleet direction”

(define-struct world (PlayerOne list-of-missiles list-of-aliens goRight?))
;ship, list-of-missiles, list-of-aliens, boolean

;func-for-world: world -> world
(define (func-for-world a-world)
  (... (world-PlayerOne a-world)
       ... (world-list-of-missiles a-world)
       ... (world-list-of-aliens a-world)
       ... (world-goRight? a-world)))

;(check-expect (make-world missilefleet2 fleet1 #true)(place-image (draw-aliens fleet1 GameBoardRegion) (draw-missiles missilefleet2 GameBoard)))
(define w1 (make-world startship missilefleet1 fleet2 #false))
(make-world startship missilefleet2 fleet1 #true)

;update-world : world → world
;returns a new world one “tick” later. 
(define (update-world a-world) (make-world (world-PlayerOne a-world)
                                           (missiles-remaining* (move-aliens (world-list-of-aliens a-world)) (move-missiles (world-list-of-missiles a-world)))
                                           (aliens-remaining* (world-list-of-aliens a-world) (move-missiles (world-list-of-missiles a-world)))
                                           (world-goRight? a-world)))

(check-expect (update-world (make-world startship parkedmissilefleet2 parkedfleet1 #true)) (make-world startship parkedmissilefleet2 parkedfleet1 #true))
(check-expect (update-world (make-world startship parkedmissilefleet2 parkedfleet2 #true)) (make-world startship '() '() #true))
;(check-expect (update-world (make-world startship missilefleet1 fleet2 #true)) (make-world startship (move-missiles missilefleet1) (move-aliens fleet2) #true))
;(check-expect (update-world (make-world startship missilefleet1 fleet1 #true)) (make-world startship '() '() #true))

;world-handle-key : world, keypress → world
;which returns a new world updated to handle the keypress
(define (world-handle-key a-world keypress)
  (make-world (ship-handle-key (world-PlayerOne a-world) keypress)
       (if (string=? keypress " ") (cons (make-missile (ship-xLoc startship)(ship-yLoc startship) 0 20) (world-list-of-missiles a-world)) (world-list-of-missiles a-world))
       (world-list-of-aliens a-world)
       (world-goRight? a-world)))

(check-expect (world-handle-key (make-world startship missilefleet1 fleet2 #true) "d") (make-world (ship-handle-key startship "d") missilefleet1 fleet2 #true))
(check-expect (world-handle-key (make-world startship missilefleet1 fleet2 #true) " ") (make-world startship (cons (make-missile (ship-xLoc startship)(ship-yLoc startship) 0 20) missilefleet1) fleet2 #true))
(check-expect (world-handle-key (make-world startship missilefleet1 fleet2 #true) "a") (make-world (ship-handle-key startship "a") missilefleet1 fleet2 #true))

;draw-world : world -> image
(define (draw-world a-world) (draw-aliens (world-list-of-aliens a-world) (draw-missiles (world-list-of-missiles a-world) (draw-ship (world-PlayerOne a-world) GameBoard))))

(check-expect (draw-world w1) (draw-aliens fleet2 (draw-missiles missilefleet1 (draw-ship startship GameBoard))))


#|###################  GRAPHICS BELOW  ###################################################parrot6|#

;(define missile-graphic (overlay/offset (triangle 20 "solid" "green") 0 15 (rectangle 10 20 "solid" "green")))
(define MissileWidth 15)
(define MissileHeight 15)
(define AlienWidth 20)
(define AlienHeight 20)
(define ShipWidth 60)
(define ShipHeight 47)
(define GameBoardHeight 600)
(define GameBoardWidth 800)

(define missile-graphic (scale .3 (rotate 90 (polygon (list (make-posn 0 0)
                                                            (make-posn -15 20)
                                                            (make-posn 40 0)
                                                            (make-posn -15 -20))
                                                      "solid"
                                                      "green"))))
(define missile-explode (scale .5 (overlay/offset
                                   (overlay/offset (circle 30 'solid (color 0 150 0 127))
                                                   26 0
                                                   (circle 30 'solid (color 0 0 255 127)))
                                   0 26
                                   (circle 30 'solid (color 200 0 0 127)))))
(define ship-graphic (rotate 90 (polygon (list (make-pulled-point 1/2 20 0 0 1/2 -20)
                                               (make-posn -7 (/ ShipWidth 2))
                                               (make-pulled-point 1/2 -20 40 0 1/2 20)
                                               (make-posn -7 (- (/ ShipWidth 2))))
                                         "solid"
                                         "green")))
(define alien-graphic (scale (/ AlienWidth 60) (overlay (ellipse 10 10 "solid" "red")
                                         (ellipse 20 20 "solid" "black")
                                         (ellipse 30 30 "solid" "red")
                                         (ellipse 40 40 "solid" "black")
                                         (ellipse 50 50 "solid" "red")
                                         (ellipse 60 60 "solid" "black"))
                             ))
(define alien-graphic2 (scale (/ AlienWidth 60) (overlay (ellipse 10 10 'solid "red")
                                          (ellipse 20 20 'solid "Fuchsia")
                                          (ellipse 30 30 170 "red")
                                          (ellipse 40 40 170 "Fuchsia")
                                          (ellipse 50 50 100 "red")
                                          (ellipse 60 60 100 "Fuchsia"))
                              ))
(define GameBoard (rectangle GameBoardHeight GameBoardWidth "solid" "black"))
(define GameBoardRegion (rectangle 800 600 0 "black"))

(make-missile 0 0 100 270)
(make-missile 100 100 25 270)
(make-missile 150 150 50 90)






#|###################  Non Game Funcs  ########################################parrot6|#


;count-bigs : real, list-of-real → natnum
;takes in a threshold and a list of numbers, and returns how many of them are larger than the threshold
(define ex1 (cons 1 (cons 2 (cons 3 empty))))
(define ex2 (cons 1 empty))
                  
(define (count-bigs threshold ListNums)(cond [(empty? ListNums) 0]
                                             [(> (first ListNums) threshold) (+ 1 (count-bigs threshold (cdr ListNums)))]
                                             [(<= (first ListNums) threshold) (+ 0 (count-bigs threshold (cdr ListNums)))]
                                             [ else "Call HQ the ship's code is corrupte%!AiDa..2w,36141 kjk9*-+e25 a5wd2 "]))

(check-expect (count-bigs 7 empty) 0)
(check-expect (count-bigs 0 empty) 0)
(check-expect (count-bigs 7 (cons 8 empty)) 1)
(check-expect (count-bigs 0 (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 empty)))))))))) 8)
(check-expect (count-bigs 1 (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 empty)))))))))) 7)
(check-expect (count-bigs 8 (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 empty)))))))))) 0)

;map-sqr : list-of-number → list-of-number
;squares each number in a list
(define (map-sqr ListNums) (cond [(empty? ListNums) '()]
                                 [true (cons (* (first ListNums) (first ListNums)) (map-sqr (cdr ListNums)))]))
(check-expect (map-sqr '()) '())
(check-expect (map-sqr (cons 8 empty)) (cons 64 empty))
(check-expect (map-sqr (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 empty)))))))))) (cons 0 (cons 1 (cons 4 (cons 9 (cons 16 (cons 25 (cons 36 (cons 49 (cons 64 empty))))))))))
(check-expect (map-sqr (cons 0 (cons 1 (cons 2 (cons 3 empty))))) (cons 0 (cons 1 (cons 4 (cons 9 empty)))))





#|###################  iBarland Material  ########################################iBarland|#
 ;https://www.radford.edu/~itec380/2019fall-ibarland/Homeworks/lists/overlap.rkt

; overlap? : real,real,real,real, real,real,real,real -> boolean
; Does one rectangle overlap another?
; We represent a rectangle as four reals: the center x,y,  width, height.
; (The units can be considered pixels, although it doesn't actually matter 
;  so long as they're all in the *same* units, of course.)
;
; For barely-touching rectangles, we use the rather odd convention that
; a rectangle is closed along its top and left sides, but open on its
; bottom and right.
; (This way, the squares (i,j,1,1) perfectly tile the plane, for i,j in N.)
;
; A line is a 0-width rectangle, and it can overlap other rectangles/lines.
; (This is a slightly non-standard(?) interpretation of half-open of width 0;
;  usually that'd be the empty set,
;  since there are no points x which satisfy the constraints   a <= x < a.
;  But that'd mean an empty-rectangle could never overlap another, and
;  we wouldn't get lines.)
;
(define (overlap? x1 y1 w1 h1  x2 y2 w2 h2)
  ; Check dist-btwn-centers, in each dimension:
  (and (overlap-1d? x1 w1 x2 w2)
       (overlap-1d? y1 h1 y2 h2)))
; overlap-1d? : real? real?  real? real?  -> boolean
; Given the centers (x1,x2) and widths (w1,w2) of two half open intervals,
; do they overlap?
(define (overlap-1d? x1 w1  x2 w2)
  (or (< (abs (- x1 x2)) (/ (+ w1 w2) 2))
      ; two intervals overlap if the centers are closer than the sum of the 2 radii.
      ; Use strictly-< to capture open intervals.
      ; For half-open, the case that the above formula doesn't work for is
      ; one (or both) of the rectangles being 0-width.  So check for the 0-width
      ; being on the left(closed) side:
      (= x1 (- x2 (/ w2 2))) ; r1=0 and x1 was on left edge of interval-2
      (= x2 (- x1 (/ w1 2))) ; r2=0 and x2 was on left edge of interval-1
      ))
  ; If I had `let*`, I might have written:
  #;(let* {[r1 (/ w1 2)]  ; the radius of the first interval
           [r2 (/ w2 2)]}
      ...)
(check-expect (overlap-1d? 5 30  10 40) #true)
(check-expect (overlap-1d? 5 2  10 2) #false) ; entirely separate
(check-expect (overlap-1d? 5 100 4 6) #true)
(check-expect (overlap-1d? 5 10  4 6) #true)
(check-expect (overlap-1d? 5 6  10 4) #false) ; borderline / tiling   [2,8)  [8,10)
(check-expect (overlap-1d? 5 6   0 4) #false)
(check-expect (overlap-1d? 5 0 7 1) #false)
(check-expect (overlap-1d? 5 0 7 5) #true)
(check-expect (overlap-1d? 5 0 7 4) #true)    ; 0-width at left  edge of another rect
(check-expect (overlap-1d? 5 4 7 0) #false)   ; 0-width at right edge of another rect
; these checks are redundant after re-factoring into `overlap-1d?`,
; but hey no reason to remove them:
(check-expect (overlap?  5  5 10 10   6  6  2  2) #true)
(check-expect (overlap?  5  5 10 10  15  6 20  2) #true)
(check-expect (overlap?  5  5 10 10   6 15  2 20) #true)
(check-expect (overlap?  5  5 10 10  15 15 20 20) #true)
(check-expect (overlap?  5  5 10 10  16 16  2  2) #false)
(check-expect (overlap?  5  5 10 10  25 25 20 20) #false)
(check-expect (overlap?  5  5 10 10  -4 -4  2  2) #false)
(check-expect (overlap?  5  5 10 10 -10 -10 20 20) #false)
(check-expect (overlap?  5  5 10 10   5 15  10 10) #false)

(check-expect (overlap?    5   5 10 10  -10 -10 20 20) #false)
(check-expect (overlap?  -10 -10 20 20    5   5 10 10) #false)

(check-expect (overlap?    5 5 10 10    5 5 0 8) #true) ; a 0-width rectangle inside a "fat" rectangle
(check-expect (overlap?    0 0  0 10    0 0 0 5) #true) ; two line-segments 0-width rect) overlapping
(check-expect (overlap? -200 -300 20 20 1 1 800 600) #false)