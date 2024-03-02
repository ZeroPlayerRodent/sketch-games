(defpackage :ufo (:use :cl :sketch))
(in-package :ufo)

(defclass bad-ship ()
  (
    (x :initarg :x :initform 0 :accessor x)
    (y :initarg :y :initform 0 :accessor y)
    (s :initarg :s :initform 0 :accessor s)
  )
)

(defparameter *enemies* (list
(make-instance 'bad-ship :x 0 :y 100 :s 1)
(make-instance 'bad-ship :x 0 :y 200 :s 2)
(make-instance 'bad-ship :x 0 :y 300 :s 3) 
(make-instance 'bad-ship :x 0 :y 400 :s 4)))

(defparameter *mx* (random 350 (make-random-state t)))
(defparameter *my* (random 550 (make-random-state t)))

(defparameter *px* 200)
(defparameter *py* 600)
(defparameter *gravity* 0.2)
(defparameter *movement* 0)
(defparameter *walk* 0)
(defparameter *lost* 0)
(defparameter *score* 0)

(defun if-collided (posx posy)
  (if (and(and (<= posx (+ *px* 32))(>= (+ posx 64) (+ *px* 32))) (and (<= posy (+ *py* 32))(>= (+ posy 64) (+ *py* 32))))
  1
  ()
  )
)

(defsketch ufo
  (
   (title "UFO")
   (width 400)
   (height 650)
   (player (load-resource "img/player.png"))
   (enemy (load-resource "img/enemy.png"))
   (bg (load-resource "img/bg.png"))
   (money (load-resource "img/money.png"))
  )
  
  (defun over ()
     (draw bg :x 0 :y 0 :width 400 :height 650)
     (rect 100 150 200 200)
     (text "YOU LOSE!!!" 100 150)
     (text (write-to-string *score*) 100 200)
     (text "Press R to restart." 100 250)
   )
  
  (draw bg :x 0 :y 0 :width 400 :height 650)
  (draw player :x *px* :y *py* :width 64 :height 64)
  (draw money :x *mx* :y *my* :width 64 :height 64)
  
  (if (and (if-collided *mx* *my*) (= *lost* 0))(
  (lambda ()
    (setf *mx* (random 350 (make-random-state t)))
    (setf *my* (random 550 (make-random-state t)))
    (setf *score* (+ *score* 25))
  ))())
  
  (let ((i 0))
    (loop
      (if (= i (length *enemies*))(return)())
      (draw enemy :x (x (elt *enemies* i)) :y (y (elt *enemies* i)) :width 64 :height 64)
      (setf (x (elt *enemies* i)) (+ (x (elt *enemies* i)) (s (elt *enemies* i))))
      (if (> (x (elt *enemies* i)) 400)
      ((lambda ()
        (setf (x (elt *enemies* i)) (- 64))
        (setf (y (elt *enemies* i)) (random 625 (make-random-state t)))
      ))
      ())
      (if (if-collided (x (elt *enemies* i)) (y (elt *enemies* i)))(setf *lost* 1)())
      (setf i (+ i 1))
    )
  )
  
  (rect 0 0 100 25)
  (text (write-to-string *score*) 0 0)

  (setf *movement* (+ *movement* *gravity*))
  (setf *py* (+ *py* *movement*))
  (setf *px* (+ *px* *walk*))
  (if (> *py* 600)(setf *movement* 0)())
  (if (> *py* 600)(setf *py* 600)())
  (if (> 0 *px*)(setf *px* 0)())
  (if (> *px* 350)(setf *px* 350)())
  (if (> *movement* 8)(setf *movement* 8)())
  (if (< *movement* (- 8))(setf *movement* (- 8))())
  (if (> 0 *py*)(setf *movement* 0)())
  (if (> 0 *py*)(setf *py* 0)())
  
  (if (= *lost* 1)(over)())
  
)

(defun thrusters ()
  (if (= *gravity* 0.2)(setf *gravity* (- 0.2))(setf *gravity* 0.2))
)

(defun reset ()
  (setf *lost* 0)
  (setf *px* 200)
  (setf *py* 600)
  (setf *score* 0)
  (setf *movement* 0)
  (setf *gravity* 0.2)
  (setf *enemies* (list
(make-instance 'bad-ship :x 0 :y 100 :s 1)
(make-instance 'bad-ship :x 0 :y 200 :s 2)
(make-instance 'bad-ship :x 0 :y 300 :s 3) 
(make-instance 'bad-ship :x 0 :y 400 :s 4)))
)

(defmethod on-key ((sketch ufo) key state)
  (if (and(eq key :W)(eq state :DOWN))(if (= *lost* 0)(thrusters)())())
  (if (and(eq key :R)(eq state :DOWN))(if (= *lost* 1)(reset)())())
  
  (if (and(eq key :D)(eq state :DOWN))(setf *walk* 6)())
  (if (and(eq key :D)(eq state :UP))(if (= *walk* 6)(setf *walk* 0)())())
  
  (if (and(eq key :A)(eq state :DOWN))(setf *walk* (- 6))())
  (if (and(eq key :A)(eq state :UP))(if (= *walk* (- 6))(setf *walk* 0)())())
)

(make-instance 'ufo)
