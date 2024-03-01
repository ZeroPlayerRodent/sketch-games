(defpackage :cops-vs-robbers (:use :cl :sketch))
(in-package :cops-vs-robbers)

(defclass zombie ()
  (
    (x :initarg :x :initform 0 :accessor x)
    (y :initarg :y :initform 0 :accessor y)
    (d :initarg :d :initform 3 :accessor d)
    (a :initarg :a :initform 100 :accessor a)
  )
)

(defparameter *enemies* (list
(make-instance 'zombie :x 0 :y 0 :d 6 :a 100)
(make-instance 'zombie :x 100 :y 0 :d 6 :a 100)
(make-instance 'zombie :x 200 :y 0 :d 6 :a 100)
(make-instance 'zombie :x 300 :y 0 :d 6 :a 100)
(make-instance 'zombie :x 400 :y 0 :d 6 :a 100)
))

(defparameter *mx* (random 350 (make-random-state t)))
(defparameter *my* (random 550 (make-random-state t)))

(defparameter *px* 200)
(defparameter *py* 350)
(defparameter *dir* 6)
(defparameter *speed* 2)

(defparameter *score* 0)
(defparameter *wait* 50)

(defparameter *lost* 0)

(defun if-collided (posx posy)
  (if (and(and (<= posx (+ *px* 32))(>= (+ posx 64) (+ *px* 32))) (and (<= posy (+ *py* 32))(>= (+ posy 64) (+ *py* 32))))
  1
  ()
  )
)

(defsketch cops-vs-robbers
  (
   (title "Cops VS. Robbers")
   (width 650)
   (height 650)
   (player (load-resource "img/player.png"))
   (zombie (load-resource "img/cop.png"))
   (money (load-resource "img/money.png"))
  )
  
  (background (rgb 0.349 0.529 0.357))
  
  (defun over ()
     (rect 0 0 650 650)
     (rect 100 150 200 200)
     (text "YOU LOSE!!!" 100 150)
     (text (write-to-string *score*) 100 200)
     (text "Press R to restart." 100 250)
   )
  
  (rect 0 0 100 25)
  (text (write-to-string *score*) 0 0)
  
  (draw player :x *px* :y *py* :width 64 :height 64)
  
  (draw money :x *mx* :y *my* :width 64 :width 64)

  (let ((i 0))
    (loop
      (if (= i (length *enemies*))(return)())
      (draw zombie :x (x (elt *enemies* i)) :y (y (elt *enemies* i)) :width 64 :height 64)
      (cond ((= (d (elt *enemies* i)) 6)(setf (x (elt *enemies* i)) (+ (x (elt *enemies* i)) *speed*)))
            ((= (d (elt *enemies* i)) 4)(setf (x (elt *enemies* i)) (- (x (elt *enemies* i)) *speed*)))
            ((= (d (elt *enemies* i)) 2)(setf (y (elt *enemies* i)) (- (y (elt *enemies* i)) *speed*)))
            ((= (d (elt *enemies* i)) 8)(setf (y (elt *enemies* i)) (+ (y (elt *enemies* i)) *speed*)))
      )
      (setf (a (elt *enemies* i)) (- (a (elt *enemies* i)) 1))
      (if (<= (a (elt *enemies* i)) 0)(
      (lambda()
        (setf (a (elt *enemies* i)) (random 100 (make-random-state t)))
        (setf (d (elt *enemies* i)) (swap-dir (d (elt *enemies* i))))
      )
      )())
      (if (> (x (elt *enemies* i)) 650)(setf (x (elt *enemies* i)) (- 64))())
      (if (< (x (elt *enemies* i)) (- 64))(setf (x (elt *enemies* i)) 650)())
      (if (> (y (elt *enemies* i)) 650)(setf (y (elt *enemies* i)) (- 64))())
      (if (< (y (elt *enemies* i)) (- 64))(setf (y (elt *enemies* i)) 650)())
      
      (if (if-collided (x (elt *enemies* i)) (y (elt *enemies* i)))(setf *lost* 1)())
      (setf i (+ i 1))
    )
  )
  
  (cond ((= *dir* 6)(setf *px* (+ *px* *speed*)))
        ((= *dir* 4)(setf *px* (- *px* *speed*)))
        ((= *dir* 2)(setf *py* (- *py* *speed*)))
        ((= *dir* 8)(setf *py* (+ *py* *speed*)))
  )

  (if (> *px* 650)(setf *px* (- 64))())
  (if (< *px* (- 64))(setf *px* 650)())
  (if (> *py* 650)(setf *py* (- 64))())
  (if (< *py* (- 64))(setf *py* 650)())
  
  (if (and(if-collided *mx* *my*)(= *lost* 0))(
  (lambda ()
    (setf *score* (+ *score* 125))
    (setf *mx* (random 600 (make-random-state t)))
    (setf *my* (random 600 (make-random-state t)))
    (setf *speed* (+ *speed* 0.2))
  )
  )())
  
  (if (= *lost* 1)(over)())
)

(defun swap-dir (thing)
  (cond ((= thing 6)8)
        ((= thing 4)2)
        ((= thing 2)6)
        ((= thing 8)4)  (rect 0 0 100 25)
  (text (write-to-string *score*) 0 0)
  )
)

(defun reset ()
  (setf *lost* 0)
  (setf *px* 200)
  (setf *py* 350)
  (setf *score* 0)
  (setf *speed* 2)
  (setf *enemies* (list
(make-instance 'zombie :x 0 :y 0 :d 6 :a 100)
(make-instance 'zombie :x 100 :y 0 :d 6 :a 100)
(make-instance 'zombie :x 200 :y 0 :d 6 :a 100)
(make-instance 'zombie :x 300 :y 0 :d 6 :a 100)
(make-instance 'zombie :x 400 :y 0 :d 6 :a 100)
))
)

(defmethod on-key ((sketch cops-vs-robbers) key state)
  (if (and(eq key :W)(eq state :DOWN))(setf *dir* 2)())
  (if (and(eq key :A)(eq state :DOWN))(setf *dir* 4)())
  (if (and(eq key :S)(eq state :DOWN))(setf *dir* 8)())
  (if (and(eq key :D)(eq state :DOWN))(setf *dir* 6)())
  (if (and(eq key :R)(eq state :DOWN))(if (= *lost* 1)(reset)())())
)

(make-instance 'cops-vs-robbers)
