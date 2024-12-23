# platel `C` code

This has been tested only on Mac OS Sonoma and Sequoia (Intel; I don't
have a M* available) and on Windows 11 (with the Visual Studio
toolchain and `nmake`).

You can manually test this code without loading `platel.el` from the
top directory.

The `cmake` setup works on UN*X, but yet on Windows.  It is left in
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

Thanks to Eli Zaretskii and the helpful Emacs developers crowd.
