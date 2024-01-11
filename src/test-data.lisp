(in-package :pts)

(defparameter *test-path*
  "C://Users//sgraetz//Documents//exporte//11-01-2024--05-02-2024//hdplus_20240111_29286.pts")

(defparameter *test-xml-nodes*
  (dom:child-nodes
   (dom:document-element
    (pts:load-pts-file pts:*test-path*))))

(defparameter *test-eventCommands*
  (dom:item (filter-events pts:*test-xml-nodes* "eventCommands") 0))

(defparameter *test-DEFINES*
  (filter-events
   (dom:child-nodes *test-eventCommands*)
   "DEFINE"
   :get-child #'(lambda (define)
		  (dom:item (remove-whitespace-nodes (dom:child-nodes define)) 0)
		  )))

(defparameter *test-vaevents*
  (filter-events *test-DEFINES* "vaEvent"))

(defparameter *test-sievents*
  (filter-events *test-DEFINES* "siEvent"))

(defparameter *test-layoutEvents*
  (filter-events *test-DEFINES* "layoutEvent"))

(defparameter *test-logoEvents*
  (filter-events *test-DEFINES* "logoEvent"))

