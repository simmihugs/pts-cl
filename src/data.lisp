(declaim (optimize (speed 3) (debug 0) (safety 0)))
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
    :initarg  :title
    :type string)
   (startTime
    :accessor :startTime
    :initarg  :startTime
    :type integer)
   (endTime
    :accessor :endTime
    :initarg  :endTime
    :type integer)
   (programId
    :accessor :programId
    :initarg  :programId
    :type integer)))

(defclass sievent (event)
  ((displayedStartTime
    :accessor :displayedStartTime
    :initarg  :displayedStartTime
    :type integer)
   (displayedEndTime
    :accessor :displayedEndTime
    :initarg  :displayedEndTime
    :type integer)
   (description
    :accessor :description
    :initarg :description
    :type string)))

(defmethod print-object ((obj sievent) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream
	    "{~a~a~a~a~a~a~a}"
	    (format nil "~%~atitle:              ~a"   #\SPACE (:title              obj))
	    (format nil "~%~astartTime:          ~a"   #\SPACE (:startTime          obj))
	    (format nil "~%~aendTime:            ~a"   #\SPACE (:endTime            obj))
	    (format nil "~%~aprogramId:          ~a"   #\SPACE (:programId          obj))
	    (format nil "~%~adisplayedStartTime: ~a"   #\SPACE (:displayedStartTime obj))
	    (format nil "~%~adisplayedEndTime:   ~a"   #\SPACE (:displayedEndTime   obj))
	    (format nil "~%~adescription:        ~a~%" #\SPACE (:description        obj)))))

(defclass duration ()
    ((hour
      :accessor :hour
      :initarg :hour)
     (minute
      :accessor :minute
      :initarg :minute)
     (second
      :accessor :second
      :initarg :second)
     (millisecond
      :accessor :millisecond
      :initarg :millisecond)))

(defmethod print-object ((obj duration) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream
	    "{hour: ~a minute: ~a second: ~a millisecond: ~a}"
	    (:hour        obj)
	    (:minute      obj)
	    (:second      obj)
	    (:millisecond obj))))

(defun create-duration(str)
  (destructuring-bind (h m s) (str:split ":" (second (str:split " " str)))
    (destructuring-bind (s ms) (str:split "." s)
      (make-instance 'duration
		     :hour (parse-integer h)
		     :minute (parse-integer m)
		     :second (parse-integer s)
		     :millisecond (parse-integer ms)))))

(defun create-time(str)
  (multiple-value-bind (seconds milliseconds) (cl-date-time-parser:parse-date-time str)
    (let ((nanoseconds (* 1000000 (floor (* 1000 milliseconds)))))      
      (local-time:timestamp+ (local-time:universal-to-timestamp seconds) nanoseconds :nsec))))

(defun add-time-and-duration(time duration)
  (setf time (local-time:timestamp+ time (:hour duration)   :hour))
  (setf time (local-time:timestamp+ time (:minute duration) :minute))
  (setf time (local-time:timestamp+ time (:second duration) :sec))
  (setf time
	(local-time:timestamp+
	 time
	 (* 1000000 (:millisecond duration)) :nsec))
  time)

(defun extract-child-node(element node-name)
  (dom:item
   (remove-if-not
    #'(lambda (x) (string= node-name (dom:node-name x)))
    (dom:child-nodes element)) 0))

(defun create-sievent(sievent)
  (let* ((title             (dom:get-attribute sievent "title"))
	 (starttime         (dom:get-attribute sievent "startTime"))
	 (duration          (dom:get-attribute sievent "duration"))
	 (programId         (dom:get-attribute sievent "programId"))
	 (sistandard        (extract-child-node sievent "siStandard"))
	 (displayedStart    (dom:get-attribute sistandard "displayedStart"))
	 (displayedDuration (dom:get-attribute sistandard "displayedDuration"))
	 (description       (dom:get-attribute
			     (extract-child-node
			      (extract-child-node siStandard "siDescriptions") "description")
			     "longDescription")))
    (let ((starttime (create-time starttime))
	  (duration  (create-duration duration))
	  (displayedStart (create-time displayedStart))
	  (displayedDuration (create-duration displayedDuration)))
    (make-instance 'sievent
		   :title              title
		   :startTime          starttime        
		   :endTime            (add-time-and-duration starttime duration)
		   :programId          programId        
		   :displayedStartTime displayedStart   
		   :displayedEndTime   (add-time-and-duration displayedStart displayedDuration)
		   :description        description))))


