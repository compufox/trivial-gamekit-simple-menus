;;;; trivial-gamekit-simple-menus.lisp

(in-package #:gamekit.simple-menus)

(defclass menu ()
  ((heading :initarg :heading
            :accessor menu-heading)
   (options :initarg :options
            :accessor menu-options)
   (position :initarg :position
             :accessor menu-position)
   (orientation :initarg :orientation
                :accessor menu-orientation)
   (type :initarg :type
         :reader menu-type)
   (callback :initarg :callback
             :accessor menu-callback)
   (selected :initform 0
             :accessor menu-selected)
   (widgets :initarg :widgets
            :reader menu-widgets)))

(defun make-menu2 (heading options callback &key (position (vec2 0 0)) (heading-font :default) (option-font :default) (heading-size 24)
                                             (option-size 18) (type :keyboard) (orientation :vertical) (fill-color gamekit.colors:+white+)
                                             (btn-color gamekit.colors:+white+) (hover-color gamekit.colors:+black+)
                                             (pressed-color gamekit.colors:+black+) panel)
  (let ((w (list (gamekit.ui:make-label heading position :font heading-font :size heading-size :color fill-color)
                 (if (eq type :mouse)
                     (loop for (k . v) in options
                           for l = (make-label k position :font option-font :size option-size :color fill-color)
                           collect (make-button position :label l :color fill-color
                                                         :fill-color btn-color :hover-color hover-color
                                                         :pressed-color pressed-color
                                                         :rounding 7
                                                         :on-click (lambda () (funcall callback v))))
                     (loop for (k . v) in options
                           collect (make-label k position :font option-font :size option-size :color fill-color))))))
    (set-widget-positions w)
    (when panel (setf (children panel) w))
    (make-instance 'menu :widgets (if panel panel w))))

(defun set-widget-positions (widgets)
  "set the widget's positions here"
  )


(defmethod draw-menu2 ((this menu))
  (with-slots (position widgets) this
    (dolist (w widgets)
      (draw-widget w :offset position))))
  

(defun make-menu (heading options callback &key (position (gamekit:vec2 0 0)) (orientation :veritcal) (type :keyboard) (fill-color (gamekit:vec4 1 1 1 1)) (hover-color (gamekit:vec4 0 0 0 .5)) panel (panel-color (gamekit:vec4 0 0 0 1)) (stroke-color (gamekit:vec4 0 0 0 0)) (stroke-thickness 1) panel-position heading-font option-font)
  "creates and returns a menu object with HEADING and OPTIONS. calls CALLBACK when an option is selected.

HEADING is a string
OPTIONS is an alist with each pair taking the form of a string to display in CAR and a value in CDR to pass to CALLBACK when selected
CALLBACK is a function that accepts a single parameter. gets called when a menu option is selected

POSITION is the position where the top of the menu should start rendering
ORIENTATION is either :HORIZONTAL or :VERTICAL. defaults to :VERTICAL
TYPE is either :KEYBOARD or :MOUSE. defaults to :KEYBOARD
FILL-COLOR is a gamekit:vec4. defaults to white
HOVER-COLOR is only valid if TYPE is :MOUSE. the color to display underneath the option when the mouse is hovering over it
PANEL is any one of: NIL, T, VEC2, or a symbol denoting an image to draw via GAMEKIT:DRAW-IMAGE. if NIL, no panel is drawn. if T a panel is drawn and automatically sized. if VEC2, the elements are used as width and height for drawing. if SYMBOL, the image is drawn.
PANEL-COLOR is a VEC4 that denotes the color of the panel. only used if PANEL is T or VEC2
PANEL-POSITION is a VEC2 that is used to position the panel. if NIL, it is placed automatically
STROKE-COLOR is a VEC4 that is used to color the stroke on the panel. defaults to transparent
STROKE-THICKNESS is a NUMBER. used to set the panel's stroke thickness. defaults to 1
HEADING-FONT is the font to be used to draw HEADING. defaults to gamekit::*font*
OPTION-FONT is the font to be used to draw OPTIONS. defaults to gamekit::*font*"
  (make-instance 'menu :heading heading :options options :callback callback :position position
                       :hover-color hover-color :fill-color fill-color :type type :orientation orientation
                       :panel panel :panel-color panel-color :panel-position panel-position
                       :stroke-color stroke-color :stroke-thickness stroke-thickness
                       :heading-font (or heading-font (cl-bodge.canvas:make-default-font))
                       :option-font (or option-font (cl-bodge.canvas:make-default-font))))
  

(defmethod draw-menu ((this menu))
  "draws THIS menu"
  (with-slots (heading options position orientation type
               heading-font option-font fill-color hover-color
               panel panel-color panel-position stroke-color
               stroke-thickness) this
    (let ((heading-dimensions (text-dimensions heading heading-font)))

      ;; TODO: all of the calculations for drawing the options
      ;; suck a lot and should be completely redone.
      ;;  this goes for all horizontal code and also for all panel calculations
      
      (when panel
        (let* ((full-x (apply #'+ (mapcar (l (text-width (car _) option-font)) options)))
               (full-y (+ (* 1.5 (length options) (text-height (car (car options)) option-font))
                          (* 1.5 (text-height heading heading-font))))
               (longest (max (text-width (car (first (sort options #'> :key (l (length (car _)))))) option-font)
                             (text-width heading heading-font)))
               (pos (or panel-position (if (eq orientation :horizontal)
                                          (gamekit:vec2 (- (gamekit:x position) (/ full-x 1.5))
                                                        (- (gamekit:y position)
                                                           (text-height (car (car options)) option-font)
                                                           (text-height heading heading-font)
                                                           25))
                                          (gamekit:vec2 (- (gamekit:x position) (/ longest 2) 10)
                                                        (- (gamekit:y position) full-y))))))
        (cond
          ((and (symbolp panel) (not (eq panel t)))
           (gamekit:draw-image pos panel))
          ((typep panel 'gamekit:vec2)
           (gamekit:draw-rect pos (gamekit:x panel) (gamekit:y panel)
                              :fill-paint panel-color :stroke-paint stroke-color
                              :thickness stroke-thickness))
          (t (gamekit:draw-rect pos
                                (if (eq orientation :horizontal)
                                    (+ (* full-x 1.3) 10) (+ longest 20))
                                (if (eq orientation :horizontal)
                                    (+ (* 2 (text-height (car (car options)) option-font))
                                       (* 2 (text-height heading heading-font)))
                                    (+ full-y (text-height heading heading-font) 10))
                                :fill-paint panel-color :stroke-paint stroke-color
                                :thickness stroke-thickness)))))

             
      (draw-text heading position :color fill-color :font heading-font)

      ;; if we dont have any options we dont even need to enter this
      (when options
        (loop with full-length = (apply #'+ 100 (mapcar (l (text-width (car _) option-font)) options))
              for opt in options
              for i from 0

              for x-val = (- (+ (gamekit:x position)
                                (/ (text-width (car opt) option-font) 2)
                                (* i 90)
                                (apply #'+ (mapcar (l (text-width (car _) option-font))
                                                   (subseq options 0 i))))
                             (/ full-length 1.5))
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
