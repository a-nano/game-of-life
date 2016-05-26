# Game-of-life

## Usage

![Game-of-life](image/game-of-life.png)

### Start the game of life

```common-lisp
(game-of-life:start-game)
```

### Key operations

* Return: To next step
* Space : Enable/disable auto step
* q     : Quit game
* s     : Save board to 'save/YYYYMMDDhhmmss.lisp'
* d     : Refresh board
* r     : Invert the quarter of all cells' state.

### Set a frame-rate (default is 10)

```common-lisp
(game-of-life:start-game :frame-rate 30)
```

### Set a window size (default is 640)

```common-lisp
(game-of-life:start-game :window-size 480)
```

### Set a number of one-side cells (default is 80)

```common-lisp
(game-of-life:start-game :one-size-cells 100)
```

### Load a initial state of board

```common-lisp
(game-of-life:start-game :read-path "examples/glider-gun.lisp")
```

## Installation

### Requirements

```common-lisp
(push #P"path-to-game-of-life" asdf:*central-registry*)
(ql:quickload :game-of-life)
```

* [SDL](https://www.libsdl.org/)

## Author

* akeo_quick

## Copyright

Copyright (c) 2016 akeo_quick