Naive Unpickler for Clojure
===========================

Naive unpickler for clojure, note that some features aren't implemented for now.

* `EXTn` opcodes
* `OBJ` and other related opcodes

thus this library is now only be able to unpickle data consisted of `int`, `float`, `long`, `str`, `unicode`, `list`, `tuple`, `dict`

Usage
-----

See `-main` under core.clj

Misc
----

Data types are mapped in this way

| Python          | Clojure    |
| --------------- | ----------:|
| `int` `long`    | `int`      |
| `str` `unicode` | `String`   |
| `list` `tuple`  | `vector`   |
| `dict`          | `hash-map` |


contact me: chsc4698@gmail.com

p.s. nrepl is a great tool for clojure developing, many thanks to the contributors to this project


