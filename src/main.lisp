(in-package :pts)

(defun main(&key
	      (path nil)
	      (debug nil)
	      (verbose nil)
	      (list-sievents nil)
	      (list-vaevents nil)
	      (extra-line nil))
  (format t "file: ~a~%" (pts:load-pts-file path)))

