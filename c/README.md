# platel `C` code

This works only on Mac OS Sonoma (Intel; I don't have a M* available).

You should be able to just run `make` and then move the
`platel_emacs_module.dylyb` somewhere in `load-path`.

Issuing `M-x load-library` should work; and it does.  The functions
`platel-is-big-endian` and `platel-is-little-endian` are now forking
as expected; you can test them in `ielm`.

Thanks to Eli Zaretskii and the helpful Emacs developers crowd.
