# trivial-gamekit-simple-menus
### _ava fox_

module for trivial-gamekit

provides a way to quickly create simple menus, either mouse (WIP) or keyboard driven

## Installation

```shell
$ git clone https://github.com/compufox/trivial-gamekit-simple-menus ~/common-lisp/
$ ros run # (or run whichever lisp you use)
* ;; install the quicklisp distro that houses trivial-gamekit
* (ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")
* ;; ready to go!
```

## Example 

load example into lisp:
```lisp
* ;; load example
* (ql:quickload :trivial-gamekit-simple-menus-example)
* (gamekit.simple-menus.example:run)
```

source code at: `example.lisp`

## Usage

define a menu

```lisp
(defvar *menu* (make-menu "My Menu!" '(("Option1" . :opt1) ("Option2" . :opt2))
                          (lambda (x) (if (eq x :opt1) (format t "option 1 picked") (format t "option 2 picked")))
                          :position (vec2 100 100)))
```

after you start your game, call the initializer

```lisp
(defun run ()
    (gamekit:start 'your-game)
    (initialize-menu *menu*))
```

in your custom `gamekit:draw` function add 

```lisp
(defmethod gamekit:draw ((this your-game))
    ...
    (draw-menu *menu*)
    ...)
```

see `example.lisp` for the full example

## API

`(make-menu heading options callback &key (position (gamekit:vec2 0 0)) (orientation :veritcal) (type :keyboard) (fill-color (gamekit:vec4 1 1 1 1)) (hover-color (gamekit:vec4 0 0 0 .5)) heading-font option-font)`

creates and returns a menu object with HEADING and OPTIONS. calls CALLBACK when an option is selected.

HEADING is a string

OPTIONS is an alist with each pair taking the form of a string to display in CAR and a value in CDR to pass to CALLBACK when selected

CALLBACK is a function that accepts a single parameter. gets called when a menu option is selected

POSITION is the position where the top of the menu should start rendering

ORIENTATION is either :HORIZONTAL or :VERTICAL. defaults to :VERTICAL

TYPE is either :KEYBOARD or :MOUSE. defaults to :KEYBOARD

FILL-COLOR is a gamekit:vec4. defaults to white

HOVER-COLOR is only valud if TYPE is :MOUSE. the color to display underneath the option when the mouse is hovering over it

HEADING-FONT is the font to be used to draw HEADING. defaults to gamekit::\*font\*

OPTION-FONT is the font to be used to draw OPTIONS. defaults to gamekit::\*font\*

---

`(draw-menu this)`

draws THIS menu

---

`(initialize-menu this)`

initalizes bindings for the type of THIS menu 

only call after gamekit:start has been called

---

`(uninitialize-menu this)`

removes all keybindings and resets the selected value of THIS menu

---

menu accessors exported: `menu-heading`, `menu-options`, `menu-position`,
    `menu-orientation`, `menu-fill-color`,`menu-hover-color`, `menu-callback`
    
---

## License

MIT

