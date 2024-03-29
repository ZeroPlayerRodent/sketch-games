(defpackage :quality-control (:use :cl :sketch))
(in-package :quality-control)

(defparameter *input* :X)
(defparameter *player* '(0 0 0 0))
(defparameter *enemy* '(0 0 0 0))
(defparameter *distance* 400)
(defparameter *lost* 0)
(defparameter *score* 0)
(defparameter *speed* 2)

(defsketch quality-control
   (
    (title "Quality Control")
    (shit (load-resource "img/shit.png"))
    (burger (load-resource "img/burger.png"))
    (fire (load-resource "img/fire.png"))
    (conveyor (load-resource "img/conveyor.png"))
   )
   
   (defun render-enemy ()
     (let ((i 0))
       (loop
         (if (= i 4)(return)())
         (if (= (elt *enemy* i) 0)(draw burger :x (+ 32 (* i 100)) :y *distance* :width 32 :height 32)
                                  (draw shit :x (+ 32 (* i 100)) :y *distance* :width 32 :height 32))
         (setf i (+ i 1))
       )
     )
   )
   
   (defun render-player ()
     (let ((i 0))
       (loop
         (if (= i 4)(return)())
         (if (= (elt *player* i) 1)(draw fire :x (+ 32 (* i 100)) :y 0 :width 32 :height 32)())
         (setf i (+ i 1))
       )
     )
   )
   (defun new-enemy () ;
     (let ((i 0))
       (loop
         (if (= (length *enemy*) i)(return)())
         (setf (elt *enemy* i) (random 2 (make-random-state t)))
         (setf i (+ i 1))
       )
     )
   )
   
   (defun render ()
     (let ((i 0))
       (loop
         (if (> i 15)(return)())
         (draw conveyor :x 16 :y (* 32 i) :width 64 :height 32)
         (draw conveyor :x (+ 16 100) :y (* 32 i) :width 64 :height 32)
         (draw conveyor :x (+ 16 200) :y (* 32 i) :width 64 :height 32)
         (draw conveyor :x (+ 16 300) :y (* 32 i) :width 64 :height 32)
         (setf i (+ i 1))
       )
     )
     (render-enemy)
     (render-player)
     (rect 0 350 400 50)
     (text (write-to-string *score*) 175 350)
   )
   
   (defun over ()
     (rect 0 0 400 400)
     (text "YOU LOSE!!" 150 100)
     (text (write-to-string *score*) 150 150)
     (text "Press R to restart." 150 200)
   )
   
   (defun reset ()
     (new-enemy)
     (setf *distance* 400)
     (setf *score* (+ *score* 125))
     (if (> 10 *speed*)(setf *speed* (+ *speed* 0.1))())
   )
   
   (defun new-game ()
     (setf *lost* 0)
     (setf *enemy* '(0 0 0 0))
     (setf *player* '(0 0 0 0))
     (setf *distance* 400)
     (setf *score* 0)
     (setf *speed* 2)
   )
   
   (defun tick ()
     (setf *distance* (- *distance* *speed*))
     (if (> 0 *distance*)(if (equal *player* *enemy*)(reset)(setf *lost* 1))())
     (render)
   )
   
   (if (= *lost* 0)(tick)(over))
)

(defun flip-bit (to-flip)
  (if(= (elt *player* to-flip) 0)(setf (elt *player* to-flip) 1)(setf (elt *player* to-flip) 0))
)

(defmethod on-key ((sketch quality-control) key state)
  (if (and(eq key :1)(eq state :DOWN))(flip-bit 0)())
  (if (and(eq key :2)(eq state :DOWN))(flip-bit 1)())
  (if (and(eq key :3)(eq state :DOWN))(flip-bit 2)())
  (if (and(eq key :4)(eq state :DOWN))(flip-bit 3)())
  (if (and(eq key :R)(eq state :DOWN))(new-game)())
)

(make-instance 'quality-control)
