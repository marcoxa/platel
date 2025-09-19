# platel `C` code

This has been tested only on Mac OS Sonoma and Sequoia (Intel; I don't
have a M* available) and on Windows 11 (with the Visual Studio
toolchain and `nmake`).

You can manually test this code without loading `platel.el` from the
top directory.

The `cmake` setup works on UN*X, but not yet on Windows.  It is left in
for future reference.

On UN*X you should be able to just run `make -f platel.make` from this
directory and then **`load`** `platel_emacs_module.dylyb` or
`platel_emacs_module.so`.

On Windows you should be able to just run `nmake /F platel.nmake` from this
directory and then **`load`** `platel_emacs_module.dll`.  Note that
the Windows build process requires more ancillary files; morevoer, you
need to run `nmake` in a `cmd` or `PowerShell` where you have the full
development toolchain at hand - i.e., at a minimum you need to run
`vcvarsall.bat` beforehand (see the MS documentation for details).

Finally, issuing `M-x load-library` should work; and it does.  The
functions `platel-is-big-endian` and `platel-is-little-endian` are now
working as expected; you can test them in `ielm`.  The command
`platel-endiannes` is also available from the top level `platel.el`
file.


## `emc`

While working with `platel` several ideas percolated to the library `emc`.

Now you can call `emc-make` to build the system and `emc-install` to
install it.  Eventually, you will be able to choose between build
systems, e.g., `cmake` instead of the vanilla `Makefile` based setup.


## Issues

The code is convoluted mainly because of two issues.

1. Emacs does not expose anything like `emacs-includes-dir`, therefore
   guessing the location of `include` directory in the Emacs
   installation cannot be programmatically done in a reliable way.
2. Windows `nmake` is much more limited than UN*X `make` in all its
   variants; e.g., it does not have the equivalent of `$(shell ...)`,
   which makes `platel.nmake` rather rigid.


# Acknowledgements

Thanks to Eli Zaretskii and the helpful Emacs developers crowd.
