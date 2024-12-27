# platel

Low level (C level) PLATform introspection with ELisp.

That is, reach down to the C dungeon to get information about the
current platform.

This library is really an exercise in compiling and loading
a *dynamic Emacs module* on one of the three main platforms: UN*X,
Mac OS and Windows.  The library (either a `.so`, a `.dylib`, or a
`.dll`) is compiles and loaded from the main `platel.el` file with
everything kept in the `c` sub-directory.

The library exports, for the time being, one command and two functions.

* `platel-endianness`: a command that shows a message saying whether
  the current platform is little or big endian.
* `platel-is-little-endian`: a function that returns `T` if the current
  platform is little-endian.
* `platel-is-big-endian`: a function that returns `T` if the current
  platform is little-endian.


Enjoy

Marco Antoniotti, Milan, Italy, (c) 2024-2025
