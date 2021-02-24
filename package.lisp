;;;; package.lisp

(defpackage #:trivial-gamekit-simple-menus
  (:use #:cl #:gamekit.ui)
  (:nicknames :gamekit.simple-menus)
  (:import-from :gamekit :vec2 :x :y)
  (:export :menu-heading :menu-options :menu-position
           :menu-orientation :menu-fill-color 
           :menu-hover-color :menu-callback
           :menu-panel-color :menu-panel-position
           :menu-panel :menu-stroke-color 
           :menu-stroke-thickness

           :make-menu :draw-menu
           :initialize-menu :uninitialize-menu))
