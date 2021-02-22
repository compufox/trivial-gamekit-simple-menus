;;;; trivial-gamekit-simple-menus-example.asd

(asdf:defsystem #:trivial-gamekit-simple-menus-example
  :description "Describe trivial-gamekit-simple-menus here"
  :author "ava fox"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit #:trivial-gamekit-simple-menus)
  :components ((:file "example")))
