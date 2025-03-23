platel
======

Low level (C level) **PLAT**form introspection with **EL**isp.

That is, reach down to the C dungeon to get information about the
current platform.

This library is really an exercise in compiling and loading
a *dynamic Emacs module* on one of the three main platforms: UN\*X,
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


## Compiling and Installing

The process is clear. Compile the library that is the *dynamic Emacs
module* and `load` it in Emacs.

You can do it manually, but that would limit how the module (read:
**any module**) can be distributed and loaded, especially via a
package distribution system like `elpa`.

Let's see what this entailed.

### Manual Build

Just use `make`?  Not so fast.  There are (at least) three platforms
to consider, and "reinvent the wheel" plus NIH syndrome are very
powerful motivators.

As a result, `platel` can be built "manually" in two ways.

1. Use a three flavors of `Makefile`.
2. Use [`cmake`](https://cmake.org).

Moreover, `platel` can be - automatically - (re)built directly from
Emacs using the `emc` library.

To start, note that the actual Emacs module code in the the **c**
sub-directory.  Apart from the `CMakeLists.txt` file which will be used
for the `cmake` building process, there are a few **`make`** files.
* `platel.make`: a generic **UN\*X** make file (tested on
  [Ubuntu](https://ubuntu.com).
* `platel-darwin.make`: a Mac OS specific make file (tested on an
  Intel Macbook Pro laptop running Sequoia).
* `platel.nmake`: a Microsoft `nmake`make file (tested on W11).
* `Makefile` : earlier version of `platel-darwin.make`.

All `make` files have targets `all` (the default one), `clean`,
`install`, and `uninstall`.


#### Building and Installing on **UN\*X**

To build and install on **UN\*X** just issue:

<pre>
    $ <b>cd /path/to/platel/c</b>
    $ <b>make -f platel.make</b>
    $ <b>make -f platel.make install</b>
</pre>

The `install` target starts the building as weel, so you really just
need the last command.

At this point you can check that the library file
`platel_emacs_module.so` is present in the top `platel` directory.

<pre>
    $ <b>ls /path/to/platel/*.so</b>
    platel_emacs_module.so
</pre>

Invoking the following will clean up the installation.

<pre>
    $ <b>cd /path/to/platel/c</b>
    $ <b>make -f platel.make uninstall</b>
</pre>



#### Building and Installing on **Mac OS**

To build and install on Mac Os just issue:

<pre>
    $ <b>cd /path/to/platel/c</b>
    $ <b>make -f platel-darwin.make</b>
    $ <b>make -f platel-darwin.make install</b>
</pre>

Again, the `install` target starts the building as weel, so you really just
need the last command.

At this point you can check that the library file
`platel_emacs_module.dylib` is present in the top `platel` directory.

<pre>
    $ <b>ls /path/to/platel/*.dylib</b>
    platel_emacs_module.dylib
</pre>

Invoking the following will clean up the installation.

<pre>
    $ <b>cd /path/to/platel/c</b>
    $ <b>make -f platel.make uninstall</b>
</pre>


#### Building and Installing on **Windows**

To build and install on Windows just issue:

<pre>
    W:\Path\to\platel\c> <b>nmake /F platel.nmake</b>
    W:\Path\to\platel\c> <b>nmake /F platel.nmake install</b>
</pre>

Of course, this assumes that you have started `cmd` or **Power Shell**
from the proper [**MSVC**](https://visualstudio.microsoft.com/)
startup item, in order to have the overall Visual Studio completely
setup.

As before, the `install` target starts the building as weel, so you really just
need the last command.

At this point you can check that the library file
`platel_emacs_module.dll` is present in the top `platel` directory.

<pre>
    W:\Path\to\platel\c> <b>dir /b ..\*.dll</b>
    platel_emacs_module.dylib
</pre>

The `/b` switch is for "bare" display by `dir`.

Note that the Windows build process creates also `.exp` and `.lib`
(and `.obj`) files supporting the `.dll` creation.  The
`platel_emacs_module.def` file is necessary to fully control the
`.dll` creation and it is part of the source distribution.

Invoking the following will clean up the installation.

<pre>
    W:\Path\to\platel\c> <b>make -f platel.make uninstall</b>
</pre>


Conclusion
==========

All in all, this exercise falls in the catogory of "let me just add a
small feature to Emacs".  The result being the construction of several
supporting pieces of code and the learing of several tools and their
idioti... idiosyncrasies (cf., `cmake` and `nmake`).

It has bee a trip.  Yes, it has been a waste of time, but seriously:
if you are reading this, admit it!  You would have done the same.

Enjoy

Marco Antoniotti, Milan, Italy, (c) 2024-2025
