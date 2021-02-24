;;;; trivial-gamekit-simple-menus.asd

(asdf:defsystem #:trivial-gamekit-simple-menus
  :description "trivial-gamekit module for creating simple menus"
  :author "ava fox"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit #:trivial-gamekit-ui)
  :components ((:file "package")
               (:file "util")
               (:file "trivial-gamekit-simple-menus")))
