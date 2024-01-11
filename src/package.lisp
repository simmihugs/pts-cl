(declaim (optimize (speed 3) (debug 0) (safety 0)))
	 
(defpackage :pts
  (:use :cl :clingon :cxml :cl-date-time-parser)
  (:export
   #:main
   ;; xml handling
   #:load-pts-file
   ;; data
   #:sievent 
   ;; test
   #:*test-path*
   #:*test-xml-nodes*
   #:*test-vaEvents*
   #:*test-siEvents*
   #:*test-layoutEvents*
   #:*test-logoEvents*))   
