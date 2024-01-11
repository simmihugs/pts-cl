(in-package :pts)

(defun is-whitespace(node)
  (eq :element (dom:node-type node)))

(defun remove-whitespace-nodes(nodes)
  (remove-if-not #'is-whitespace nodes))

(defun filter-events(nodes node-name &key get-child)
  (let ((new-nodes (remove-if-not
		    #'(lambda (x) (and (is-whitespace x)
				       (string= node-name (dom:node-name x))))
		    nodes)))
    (if get-child
	(map 'vector get-child new-nodes)
	new-nodes)))

(defclass event ()
  ((title
    :accessor :title
    :initarg  :title)
   (startTime
    :accessor :startTime
    :initarg  :startTime)
   (endTime
    :accessor :endTime
    :initarg  :endTime)
   (programId
    :accessor :programId
    :initarg  :programId)))

(defclass sievent (event)
  ((displayedStartTime
    :accessor :displayedStartTime
    :initarg  :displayedStartTime)
   (displayedEndTime
    :accessor :displayedEndTime
    :initarg  :displayedEndTime)
   (description
    :accessor :description
    :initarg :description )))  
