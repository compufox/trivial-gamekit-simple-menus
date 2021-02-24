;;;; package.lisp

(defpackage #:trivial-gamekit-simple-menus
  (:use #:cl)
  (:nicknames :gamekit.simple-menus)
  (:export :menu-heading :menu-options :menu-position
           :menu-orientation :menu-fill-color 
           :menu-hover-color :menu-callback
           :menu-panel-color :menu-panel-position
           :menu-panel :menu-stroke-color 
           :menu-stroke-thickness

           :make-menu :draw-menu
           :initialize-menu :uninitialize-menu))
