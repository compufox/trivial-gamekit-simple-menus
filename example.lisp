;;;; example.lisp

(defpackage :trivial-gamekit-simple-menus-example
  (:use :cl :gamekit :gamekit.simple-menus)
  (:nicknames :gamekit.simple-menus.example)
  (:export :run))

(in-package :gamekit.simple-menus.example)

(defgame menu-example () ()
  (:viewport-title "Simple Menu Example"))

(defparameter *start-menu*
  (make-menu "Example" (list (cons "Start" :start)
                             (cons "Credits" :credits)
                             (cons "Exit" :exit))
             (lambda (selection)
               (case selection
                 (:start (format t "game started!~%"))
                 (:credits (format t "displaying credits!~%"))
                 (:exit (gamekit:stop))))
             :position (vec2 350 450)))

(defmethod gamekit:draw ((this menu-example))
  (draw-rect (vec2 0 0) 800 600 :fill-paint (vec4 0 0 0 1))
  (draw-menu *start-menu*))

(defun run ()
  (gamekit:start 'menu-example)
  (initialize-menu *start-menu*))
