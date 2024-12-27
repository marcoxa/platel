;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; platel --- Low level (C level) PLATform introspection with ELisp.

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
;; Version: 2024-12-27
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


;;; Code:

(require 'cl-lib)


(defvar platel-path (file-name-directory (or load-file-name "."))
  "The location PLATEL is loaded from.")


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


(defun platel--build-emacs-module ()
  "Build the `platel' Emacs module in a platform dependent way."
  (cl-flet ((do-compile (make-cmd)
	      (with-temp-buffer
		(let* ((default-directory platel-*platel-c-src-dir*)
		       (exit-code
			(call-process-shell-command make-cmd
						    nil
						    t)
			)
		       )
		  (if (zerop exit-code)
		      (message "PLATEL: build succeded.")
		    ;; The rest is somewhat lifted from 'emacs-libpq'.
		    (let ((result-msg (buffer-string)))
		      (if noninteractive
			  (message "PLATEL: build failed:\n%s\n"
				   result-msg)
			(with-current-buffer
			    (get-buffer-create "*platel-compile*")
			  (let ((default-directory
				 platel-*platel-c-src-dir*)
				(inhibit-read-only t)
				)
			    (erase-buffer)
			    (insert result-msg))
			  (compilation-mode)
			  (pop-to-buffer (current-buffer))
			  (error "PLATEL: build failed")))
		      ))))
	      )				; do-compile
	    )				; cl-flet
    (unless (platel--emacs-module-exists)
      (cl-case system-type
	(windows-nt
	 (message
	  "PLATEL: building the platel Emacs module (Windows).")
	 (do-compile "nmake /F platel.nmake"))
       
	(darwin
	 (message
	  "PLATEL: building the platel Emacs module (Mac OS X - Darwin).")
	 (do-compile "make -f platel-darwin.make"))
	
	(t
	 (message
	  "PLATEL: building the platel Emacs module (vanilla Unix/Linux).")
	 (do-compile "make -f platel.make"))
	)
      ))
  )


;; Exported functions.

;; platel-is-little-endian
;; platel-is-big-endian
;; Defined in the dynamic module "c/platel_emacs_module.c.
;; The `declare-function' is there to placate `flycheck'.

(declare-function platel-is-little-endian nil)

(declare-function platel-is-big-endian nil)

(defun platel-endiannes ()
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


(unless (fboundp 'platel-is-little-endian)
  (platel--build-emacs-module)
  (load platel-*platel-emacs-module*)
  )

    
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
