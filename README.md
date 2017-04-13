# Springer Challenge

### A simple drawing application

A command line application that let's you draw simple stuff like lines and rectangles.

**Architeture considerations:**

This codes strives to use:

1. Functional programming
2. Pure functions
3. Immutable structures

##### Installing

```shell
$ sbt run
enter command:
```

##### Test

To run tests

```shell
$ sbt test
```

### The problem

You're given the task of writing a simple console version of a drawing program. The functionality of the program is
quite limited but should be extensible. The program should work as follows:

1. create a new canvas.
2. start drawing on the canvas by issuing various commands.
3. quit.

The program should support the following commands:

| Command | Description |
| ------- | ----------- |
| `C w  h` | Should create a new canvas of width w and height h. |
| `L x1 y1 x2 y2` | Should create a new line from (x1,y1) to (x2,y2). Currently only horizontal or vertical lines are supported. Horizontal and vertical lines will be drawn using the xcharacter. |
| `R x1 y1 x2 y2` | Should create a new rectangle, whose upper left corner is (x1,y1) and lower right corner is (x2,y2). Horizontal and vertical lines will be drawn using the xcharacter. |
| `B x y c` | Should fill the entire area connected to (x,y) with colour 'c'. The behaviour of this is the same as that of the "bucket fill" tool in paint programs. |
| `Q` | Should quit the program. |

### Sample I/O

Below is a sample of the output your program should produce. User input is prefixed with `enter command`:.

```
enter command: C 20 4

----------------------
|                    |
|                    |
|                    |
|                    |
----------------------

enter command: L 1 2 6 2

----------------------
|                    |
|xxxxxx              |
|                    |
|                    |
----------------------

enter command: L 6 3 6 4

----------------------
|                    |
|xxxxxx              |
|     x              |
|     x              |
----------------------

enter command: R 16 1 20 3

----------------------
|               xxxxx|
|xxxxxx         x   x|
|     x         xxxxx|
|     x              |
----------------------

enter command: B 10 3 o

----------------------
|oooooooooooooooxxxxx|
|xxxxxxooooooooox   x|
|     xoooooooooxxxxx|
|     xoooooooooooooo|
----------------------

enter command: Q
```
