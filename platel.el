;;; platel --- Low level (C level) PLATform introspection with ELisp -*- Mode: Emacs-Lisp; lexical-binding: t; -*-


;;; platel.el
;;;
;;; See the file COPYING in the top directory for copyright and
;;; licensing information.

;; Author: Marco Antoniotti <marcoxa [at] gmail.com>
;; Maintainer: Marco Antoniotti <marcoxa [at] gmail.com>
;;
;; Summary: Low level (C level) PLATform introspection with ELisp.
;;
;; Created: 2024-02-07
;; Timestamp: 2025-09-22
;; Version: 0.42
;;
;; Keywords: languages, operating systems, binary platform.


;;; Commentary:
;;
;; Low level (C level) PLATform introspection with ELisp.
;;
;; That is, reach down to the C dungeon to get information about the
;; current platform.
;;
;; This library is really an exercise in compiling and loading
;; a *dynamic Emacs module* on one of the three main platforms: UN*X,
;; Mac OS and Windows.  The library (either a `.so`, a `.dylib`, or a
;; `.dll`) is compiles and loaded from the main `platel.el` file with
;; everything kept in the `c` subdirectory.
;;
;; The library exports, for the time being, one command and two functions.
;;
;; * `platel-endianness': a command that shows a message saying whether
;;   the current platform is little or big endian.
;; * `platel-is-little-endian': a function that returns `T' if the current
;;   platform is little-endian.
;; * `platel-is-big-endian': a function that returns `T' if the current
;;   platform is little-endian.


;; Notes:
;;
;; The reason to do this?  There is NFW to check the endiannnes of the
;; platform where Emacs is running on.  And I want it.
;;
;; 2024-02-09: Of course, I was not able to make the module machinery
;; work, so FTTB, this is just guesswork.  The two functions will
;; eventually be replaced by the proper C ones.
;;
;; 2024-12-02: The C module works.  Alas, "it works on my machine".
;; Now: to make it run and install on Windows *and* UN*X it is another
;; can of worms, and it appears that the infrastructure to eventually
;; distribute this with elpa/melpa ain't quite there.  The following is
;; therefore kludgy,
;;
;; 2025-01-20: Of course, working on this made me fall in another
;; rabbit hole, where I dug out a simple library that allows us to
;; invoke a 'make' toolchain (possibly a 'CMake' one) in a reasonalble
;; way.  See the `require' for the 'emc' library.


;;; Code:

(require 'cl-lib)
(require 'emc)


(defvar platel-path (file-name-directory (or load-file-name "./"))
  "The location PLATEL is loaded from.")


(defvar platel--emacs-module-include-dir
  (if (eq system-type 'darwin)
      (let ((resource-dir
	     (file-name-as-directory
	      (expand-file-name
	       "Resources"
	       (file-name-parent-directory
		invocation-directory)))
	     ))
	(file-name-as-directory
	 (expand-file-name "include" resource-dir)))
  
    (file-name-as-directory
     (expand-file-name "include"
                       (file-name-parent-directory
			invocation-directory))))
  
  ;; 'invocation-directory' is standard.
  "The Emacs installation \\='include\\=' directory.

This is where, if Emacs has been installed in a non-funky way, the
\\='emacs-module.h\\=' file resides.

The guessing is based on `invocation-directory'.  On W11 and
UN*X/Linux things are straightforward.  On Mac OS, it is really
guessing, as Emacs may or may not have been installed as \"Emacs.app\"
under \"/Applications/\".


Notes:

The Emacs developer crowd is sometimes rather obtuse on really minor
things.  Having Emacs export a `dyn-module-include-dir' or something
like that would really make life much easier; alas, the response has
been \"you should install Emacs in a non-funky way\", which is ok, but
what about the \"free\" in \"free-not-as-in-beer\"?")


;; Unused FTTB.

(defvar platel-*platel-lib-dir*
  (file-name-as-directory (expand-file-name "lib" platel-path))
  "The directory which will hold the Platel Emacs Module.")


(defvar platel-*platel-c-src-dir*
  (file-name-as-directory (expand-file-name "c" platel-path))
  "The directory which holds the Platel Emacs Module C sources.")


(defvar platel-*platel-emacs-module*
  (expand-file-name (concat "platel_emacs_module" module-file-suffix)
		    platel-*platel-c-src-dir*) ; FTTB.  Fix the 'lib' business.
  "Platel Emacs Module file in local `lib` subdirectory.")


(defun platel--emacs-module-exists ()
  "Check whether the resulting Emacs module library exists."
  (file-exists-p platel-*platel-emacs-module*))


(defvar platel--*system-dependent-makefiles*
  '((windows-nt . "platel.nmake")
    (darwin     . "platel-darwin.make")
    )
  "An a-list containg associations between `system-type' and \"makefile\"s.")


(defun platel--select-makefile ()
  "Select the proper, platform-dependent \\='Makefile\\='."

  (let ((mkf (assq system-type platel--*system-dependent-makefiles*)))
    (if mkf (cl-rest mkf) "Makefile")))


(cl-defun platel-build-emacs-module (&optional force)
  "Build the `platel' Emacs module.

The function checks whether the \\='platel\\=' Emacs module exists
and, if not, builds it (using \\='emc\\='.  If FORCE is non-nil the
Emacs module is forcibly rebuilt."

  (unless (file-exists-p platel--emacs-module-include-dir)
    (error "PLATEL: Error: Emacs \"include\" directory not found, %S"
	   platel--emacs-module-include-dir))
  
  (let* ((emacs-dir (concat "emacs-" emacs-version))
	 (make-evd-macro (format "EMACS_VERSION_DIR=%S" emacs-dir))
	 (make-emid-macro
	  (format "EMACS_MOD_INCLUDE=%S"
		  platel--emacs-module-include-dir))
	 (make-macros (format "%s %s"
			      make-evd-macro
			      make-emid-macro))
	 )
    (if (platel--emacs-module-exists)
	(when force
	  (emc-make :build-dir "c"
		    :makefile (platel--select-makefile)
		    :targets "clean all"
		    :make-macros make-macros
		    :wait t))
      (emc-make :build-dir "c"
		:makefile (platel--select-makefile)
		:make-macros make-macros
		:wait t))
    ))


;; Exported functions.
;; -------------------

;; platel-is-little-endian
;; platel-is-big-endian
;; Defined in the dynamic module "c/platel_emacs_module.c.
;; The `declare-function' is there to placate `flycheck'.

(declare-function platel-is-little-endian nil ())

(declare-function platel-is-big-endian nil ())

(defun platel-endianness ()
  "Show a message saying whether the current platform is little or big endian."
  (interactive)
  (if (platel-is-little-endian)
      (message "PLATEL: platform is little endian.")
    (message "PLATEL: platform is big endian.")
    ))


;; Emacs Module Compiling and loading.

;; MA 20241227: The `eval-when-compile' below is lifted from
;; `emacs-libpq', but it seems a bit too much.  The `unless' below
;; does the job.

;; (eval-when-compile
;;   (unless (or (fboundp 'platel-is-little-endian)
;; 	      (and (fboundp 'macroexp-compiling-p)
;; 		   (not (macroexp-compiling-p))))
;;     (platel--build-emacs-module)
;;     (load platel-*platel-emacs-module*)
;;     ))


(unless (and (fboundp 'platel-is-little-endian)
	     (file-exists-p platel-*platel-emacs-module*))
  (platel-build-emacs-module)
  )

;; (load platel-*platel-emacs-module* nil nil)
(message "PLATEL: loading module %s" platel-*platel-emacs-module*)
(if (file-exists-p platel-*platel-emacs-module*)
    (load platel-*platel-emacs-module* nil nil)
  (error (concat "PLATEL: Error: cannot find emacs module %S"
                 "\nPLATEL: Error: build process went probably awry;"
                 "check and retry.")
         platel-*platel-emacs-module*))

    
;;; Attic.

;; Valid for Intel and ARM (usualy), possibly RISC-V.

;; (defun platel-is-big-endian ()
;;   "Returns non-NIL if we are running on a \"big endian\" architecture."
;;   nil)


;; (defun platel-is-little-endian ()
;;   "Returns non-NIL if we are running on a \"little endian\" architecture."
;;   t)


;;; Epilogue.

(provide 'platel)

;;; platel.el ends here
