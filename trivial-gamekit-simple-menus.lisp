;;;; trivial-gamekit-simple-menus.lisp

(in-package #:gamekit.simple-menus)

(defclass menu ()
  ((heading :initarg :heading
            :accessor menu-heading)
   (options :initarg :options
            :accessor menu-options)
   (position :initarg :position
             :accessor menu-position)
   (heading-font :initarg :heading-font
                 :reader menu-heading-font)
   (option-font :initarg :option-font
                :reader menu-option-font)
   (orientation :initarg :orientation
                :accessor menu-orientation)
   (type :initarg :type
         :reader menu-type)
   (fill-color :initarg :fill-color
               :accessor menu-fill-color)
   (hover-color :initarg :hover-color
                :accessor menu-hover-color)
   (callback :initarg :callback
             :accessor menu-callback)
   (selected :initform 0
             :accessor menu-selected)))

(defun make-menu (heading options callback &key (position (gamekit:vec2 0 0)) (orientation :veritcal) (type :keyboard) (fill-color (gamekit:vec4 1 1 1 1)) (hover-color (gamekit:vec4 0 0 0 .5)) heading-font option-font)
  "creates and returns a menu object with HEADING and OPTIONS. calls CALLBACK when an option is selected.

HEADING is a string
OPTIONS is an alist with each pair taking the form of a string to display in CAR and a value in CDR to pass to CALLBACK when selected
CALLBACK is a function that accepts a single parameter. gets called when a menu option is selected

POSITION is the position where the top of the menu should start rendering
ORIENTATION is either :HORIZONTAL or :VERTICAL. defaults to :VERTICAL
TYPE is either :KEYBOARD or :MOUSE. defaults to :KEYBOARD
FILL-COLOR is a gamekit:vec4. defaults to white
HOVER-COLOR is only valud if TYPE is :MOUSE. the color to display underneath the option when the mouse is hovering over it
HEADING-FONT is the font to be used to draw HEADING. defaults to gamekit::*font*
OPTION-FONT is the font to be used to draw OPTIONS. defaults to gamekit::*font*"
  (make-instance 'menu :heading heading :options options :callback callback :position position
                       :hover-color hover-color :fill-color fill-color :type type :orientation orientation
                       :heading-font (or heading-font (cl-bodge.canvas:make-default-font))
                       :option-font (or option-font (cl-bodge.canvas:make-default-font))))

(defmethod draw-menu ((this menu))
  "draws THIS menu"
  (with-slots (heading options position orientation type
               heading-font option-font fill-color hover-color) this
    (let ((heading-dimensions (text-dimensions heading heading-font)))
    
      (draw-text heading position :color fill-color :font heading-font)

      ;; if we dont have any options we dont even need to enter this
      (when options
        (loop with full-length = (apply #'+ 100 (mapcar (l (text-width (car _) option-font)) options))
              for opt in options
              for i from 0

              for x-val = (- (+ (gamekit:x position)
                                (/ (text-width (car opt) option-font) 2)
                                (* i 30)
                                (apply #'+ (mapcar (l (text-width (car _) option-font))
                                                   (subseq options 0 i))))
                             (/ full-length 2))
              for y-val = (- (gamekit:y position) (* 1.5 (cadr heading-dimensions)))
              
              when (and (eq :keyboard type)
                        (= (menu-selected this) i))
                do (if (eq :horizontal orientation)
                       (draw-text ">" (gamekit:vec2 (- x-val 13) y-val)
                                  :color fill-color :font option-font :center nil)
                       (draw-text ">" (gamekit:vec2 (- (gamekit:x position)
                                                       (/ (text-width (car opt) option-font) 1.75)
                                                       10)
                                                    (- y-val (* i (* 1.5 (text-height (car opt) option-font)))))
                                  :color fill-color :font option-font))
                          
              if (eq :horizontal orientation) do
                (draw-text (car opt) (gamekit:vec2 x-val y-val)
                           :color fill-color :font option-font :center nil)
              else do
                (draw-text (car opt) (gamekit:vec2 (gamekit:x position)
                                                   (- y-val (* i (* 1.5 (text-height (car opt) option-font)))))
                           :color fill-color :font option-font))))))
    
(defmethod initialize-menu ((this menu))
  "initalizes bindings for the type of THIS menu 

only call after gamekit:start has been called"
  (with-slots (selected options callback) this
    (when (eq (menu-type this) :keyboard)
      (gamekit:bind-button (if (eq (menu-orientation this) :horizontal)
                               :left :up)
                           :pressed (l (setf selected (mod (1- selected) (length options)))))
      (gamekit:bind-button (if (eq (menu-orientation this) :horizontal)
                               :right :down)
                           :pressed (l (setf selected (mod (1+ selected) (length options)))))
      (gamekit:bind-button :enter :pressed
                           (l (funcall callback (cdr (nth selected options))))))))
    ;(when (eq (menu-type this) :mouse)
    ; ;; this is where i need to set up logic to determine if the cursor is over a menu item
    ;  (gamekit:bind-cursor (l (

(defmethod uninitialize-menu ((this menu))
  "removes all keybindings and resets the selected value of THIS menu"
  (with-slots (selected options callback) this
    (setf selected 0)
    (when (eq (menu-type this) :keyboard)
      (gamekit:bind-button (if (eq (menu-orientation this) :horizontal)
                               :left :up)
                           :pressed nil)
      (gamekit:bind-button (if (eq (menu-orientation this) :horizontal)
                               :right :down)
                           :pressed nil)
      (gamekit:bind-button :enter :pressed nil))
    (when (eq (menu-type this) :mouse)
      (gamekit:bind-cursor nil))))
