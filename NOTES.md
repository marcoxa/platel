# Notes

## 2024-02-10

This is a first attempt to use Emacs dynamic module facility.
The top `platel.el` file is empty and all the action is in the `c`
folder.

The code in the `c` folder should be straightforward and it is very
very bare bone.  FTTB it is just a test to see how to make things
work; plus it is built with only Mac OS Sonoma (Intel) in
mind. YMMV, etc etc etc.


## 2024-11-24

Tested on Sequoia and it works as well.


## 2024-12-04

Started work to make code portable to UN*X (Linux) and Windows.


## 2024-12-26

Compiling and loading working on Mac OS, and, most likey, other UN\*X
(some more tweaking necessary).  Manual compilation works also on
Windows 11 with MS Visual Studio `nmake`.  `cmake` setup works from
Mac OS and UN*X, but not yet for Windows 11.


## 2025-01-21

The library and the Emacs Module are now built using the `emc`
library.


## 2025-03-21

Almost working `cmake` setup; changes required also for `emc`.
