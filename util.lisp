;;;; util.lisp

(in-package :gamekit.simple-menus)

(defun text-center (text position &optional (font gamekit::*font*))
  (let ((dim (text-dimensions text font)))
    (gamekit:vec2 (- (gamekit:x position) (/ (car dim) 2))
                  (+ (gamekit:y position) (/ (cadr dim) 2)))))

(defmacro draw-text (text pos &key (font gamekit::*font*) (color (gamekit:vec4 0 0 0 1)) (center t))
  `(let ((dimensions (text-dimensions ,text ,font)))
     (declare (ignorable dimensions))
     (gamekit:draw-text ,text ,(if center `(gamekit:vec2 (- (gamekit:x ,pos) (/ (car dimensions) 2))
                                                         (gamekit:y ,pos))
                                   pos)
                        :fill-color ,color :font ,font)))

(defmacro l (&body body)
  `(lambda (&optional _ __)
     (declare (ignorable _ __))
     ,@body))
