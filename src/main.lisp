(declaim (optimize (speed 3) (debug 3) (safety 3)))
(in-package :pts)

(defun main(&key
	      (path nil)
	      (debug nil)
	      (verbose nil)
	      (list-sievents nil)
	      (list-vaevents nil)
	      (extra-line nil))

  (let* (;; PATH
	 (path
	   "C://Users//sgraetz//Documents//exporte//11-01-2024--05-02-2024//hdplus_20240111_29286.pts")
	 ;; NODES
	 (xml-nodes
	   (dom:child-nodes
	    (dom:document-element
	     (pts:load-pts-file path))))
	 ;; EVENTCOMMANDS
	 (test-eventCommands
	   (dom:item (filter-events xml-nodes "eventCommands") 0))
	 ;; DEFINES
	 (DEFINES
	   (filter-events
	    (dom:child-nodes *test-eventCommands*)
	    "DEFINE"
	    :get-child #'(lambda (define)
			   (dom:item (remove-whitespace-nodes (dom:child-nodes define)) 0)))))

    (format t "~a~%" (remove-if-not #'(lambda (x) (string= "siEvent" (dom:node-name x))) defines))))

