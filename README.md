# Catch the spy

This repository solves the following puzzle from the book "Algorithmic
Puzzles" by Anany Levitin and Maria Levitin.

> In a computer game, a spy is located on a one-dimensional line. At
> time `0`, the spy is at location `a`. With each time interval, the
> spy moves `b` units to the right if `b≥0`, and `|b|` units to the
> left if `b<0`.  Both `a` and `b` are fixed integers, but they are
> unknown to you. Your goal is to identify the spy’s location by
> asking at each time interval (starting at time 0) whether the spy is
> currently at some location of your choosing. For example, you can
> ask whether the spy is currently at location 19, to which you will
> receive a truthful yes/no answer. If the answer is “yes,” you reach
> your goal; if the answer is “no,” you can ask the next time whether
> the spy is at the same or another location of your choice. Devise an
> algorithm that will find the spy after a finite number questions.

# Solution

Since the only interaction with the spy is through an interface that
asks if the spy is at _some_ location, the spy is implemented as a
server and the solvers are implemented as clients -- one each in
Common Lisp and Python.

## Server

I wanted to play with a Common Lisp server so the server is written
using the popular [Hunchentoot](https://edicl.github.io/hunchentoot/)
server.  It is a simple Web Server that both shows the results on a
browser and also returns its state via HTTP Headers.  The automated
solvers use the HTTP Headers.

### Setup

There are two options to run this game: (1) Docker or (2) install lisp
and dependencies.

#### Docker

```shell
docker build -t spy .
```

##### Running

```shell
docker run -p 4242:4242 -i spy
```

#### Install Dependencies

* Install [sbcl](http://www.sbcl.org/).
* Install [quicklisp](https://www.quicklisp.org/beta/).

##### Running

The following will start the spy game server on port 4242.

```shell
sbcl --load spy.lisp
```

You can navigate to it by visiting http://localhost:4242.

## Python client

The Python client, written in Python 2, should work as easily as
follows

```shell
./solve.py
```

## Lisp client

To run the lisp client

1. Install [sbcl](http://www.sbcl.org/).
2. Install [quicklisp](https://www.quicklisp.org/beta/).

### Running

```shell
sbcl --load solve.lisp --quit
```

# License

GNU Affero General Public License v3.0
