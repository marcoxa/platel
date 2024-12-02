;;; -*- Mode: Emacs-Lisp; lexical-binding: t; -*-
;;; platel --- Getting some more info about the underlying plaform.

;;; platel.el


;;; Commentary:
;;
;; Reach down to the C dungeon to get information abaot the current
;; platfrom.
;;
;; The reason to do this?  There is NFW to check the endiannnes of the
;; platform Emacs is running on.  And I want it.
;;
;; Notes:
;;
;; 2024-02-09: Of course, I was not able to make the module machinery
;; work, so FTTB, this is just guesswork.  The two functions will
;; eventually be replaced by the proper C ones.


;;; Code:

;; Valid for Intel and ARM (usualy), possibly RISC-V.

;; (defun platel-is-big-endian ()
;;   "Returns non-NIL if we are running on a \"big endian\" architecture."
;;   nil)


;; (defun platel-is-little-endian ()
;;   "Returns non-NIL if we are running on a \"little endian\" architecture."
;;   t)
  

(provide 'platel)

;;; platel.el ends here
