;;;; package.lisp

(defpackage #:trivial-gamekit-simple-menus
  (:use #:cl)
  (:nicknames :gamekit.simple-menus)
  (:export :menu-heading :menu-options :menu-position
           :menu-orientation :menu-fill-color 
           :menu-hover-color :menu-callback

           :make-menu :draw-menu
           :initialize-menu :uninitialize-menu))
