;;(ql:quickload :cxml)

(in-package :pts)

(defun my-load-file(&optional path)
  "Load file - return file or `nil' if it does not exist."
  (when (uiop/filesystem:file-exists-p path)
    (cxml:parse-file path (cxml-dom:make-dom-builder))))
;; (handler-case (cxml:parse-file path (cxml-dom:make-dom-builder))
;;     (FILE-DOES-NOT-EXIST () nil)
;;     (FILE-EXISTS (file) file)))

(import 'serapeum:dict)

(defstruct duration hour minute second millisecond)

(defun create-duration (str)
  (let ((parts (cdr (cl-ppcre:split "( )|(:)|(\\.)" str))))
    (make-duration :hour (car parts) :minute (cadr parts) :second (caddr parts) :millisecond (cadddr parts))))


(defun create-time(str)
  (multiple-value-bind (seconds milliseconds) (cl-date-time-parser:parse-date-time str)
    (let ((nanoseconds (* 1000000 (floor (* 1000 milliseconds)))))      
      (local-time:timestamp+ (local-time:universal-to-timestamp seconds) nanoseconds :nsec))))


(defun add-time-and-duration(time duration)
  (setf time (local-time:timestamp+ time (parse-integer (duration-hour duration)) :hour))
  (setf time (local-time:timestamp+ time (parse-integer (duration-minute duration)) :minute))
  (setf time (local-time:timestamp+ time (parse-integer (duration-second duration)) :sec))
  (setf time (local-time:timestamp+ time (* 1000000 (parse-integer (duration-millisecond duration))) :nsec))
  time)


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


(defmethod print-object ((obj event) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream
	    "{title: ~a startTime: ~a endTime: ~a programId: ~a}"
	    (:title    obj)
	    (:startTime obj)
	    (:endTime   obj)
	    (:programId obj))))

(defclass logo (event)
  ((contentId
    :accessor :contentId
    :initarg  :contentId)))


(defun get-title (element)
  (dom:get-attribute element "title"))


(defun get-programId (element)
  (dom:get-attribute element "programId"))


(defun get-contentId (element)
  (dom:get-attribute element "contentId"))


(defun get-startTime (element)
  (create-time (dom:get-attribute element "startTime")))


(defun get-duration (element)
  (create-duration (dom:get-attribute element "duration")))



(defun make-event (element)
  (let* ((startTime (get-startTime element))
	 (duration (get-duration element))
	 (endTime (add-time-and-duration startTime duration)))
    (serapeum:dict
     :title     (get-title element)
     :programId (get-programId element)
     :startTime startTime
     :endTime   endTime)))


(defun make-logo (element)
  (let ((event (make-event element)))
    (make-instance 'logo
		   :title     (gethash :title event)
		   :startTime (gethash :startTime event)
		   :endTime   (gethash :endTime event)
		   :programId (gethash :programId event)
		   :contentId (get-contentId element))))

(defclass vaevent (event)
  ((contentId
    :accessor :contentId
    :initarg  :contentId)))

(defun make-vaevent (element)
  (let ((event (make-event element)))
    (make-instance 'vaevent
		   :title     (gethash :title event)
		   :startTime (gethash :startTime event)
		   :endTime   (gethash :endTime event)
		   :programId (gethash :programId event)
		   :contentId (get-contentId element))))


(defmethod print-object ((obj vaevent) stream)
  (let ((tab (lambda (x y) (format nil "~%~a~20a  ~a" #\TAB x y))))
    (print-unreadable-object (obj stream :type t)
      (format stream
	      "{~a ~a ~a ~a ~a~%}"
	      (funcall tab "title:"     (:title     obj))
	      (funcall tab "startTime:" (:startTime obj))
	      (funcall tab "endTime:"   (:endTime   obj))
	      (funcall tab "programId:" (:programId obj))
	      (funcall tab "contentId:" (:contentId obj))))))


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


(defmethod print-object ((obj sievent) stream)
  (let ((tab (lambda (x y) (format nil "~%~a~20a  ~a" #\TAB x y))))
    (print-unreadable-object (obj stream :type t)
      (format stream
	      "{~a ~a ~a ~a ~a ~a ~a~%}"
	      (funcall tab "title:"              (:title              obj))
	      (funcall tab "startTime:"          (:startTime          obj))
	      (funcall tab "endTime:"            (:endTime            obj))
	      (funcall tab "programId:"          (:programId          obj))
	      (funcall tab "displayedStartTime:" (:displayedStartTime obj))
	      (funcall tab "displayedEndTime:"   (:displayedEndTime   obj))
	      (funcall tab "description:"
		       (let ((description (:description obj)))
			 (if (and description (< 20 (length description)))
			     (concatenate
			      'string
			      (str:trim (subseq description 0 20))
			      "...")
			     "NIL")))))))



(defun make-sistandard (element)
  (let ((children (dom:child-nodes element))
	(result nil))
    (dom:do-node-list (child children)
      (when (and (eq :element (dom:node-type child))
		 (string= "siStandard" (dom:tag-name child)))
	(let* ((start    (create-time     (dom:get-attribute child "displayedStart")))
	       (duration (create-duration (dom:get-attribute child "displayedDuration")))
	       (end      (add-time-and-duration start duration))
	       (description "")
	       (xx (dom:child-nodes child)))

	  (dom:do-node-list (x xx)
	    (when (and (eq :element (dom:node-type x))
		       (string= "siDescriptions" (dom:tag-name x)))
	      (let ((yy (dom:child-nodes x)))
		(dom:do-node-list (y yy)
		  (when (and (eq :element (dom:node-type y))
			     (string= "description" (dom:tag-name y)))
		    (let ((jj (dom:get-attribute y "longDescription")))
		      (when (not (string= jj ""))
			(progn
			  ;; (format t "description: `~a'~%" jj)
			  (setf description jj)))))))))
	
	  (setf
	   result
	   (serapeum:dict
	    :start start
	    :end end
	    :description description)))))
    result))


(defun make-sievent (element)
  (let ((event (make-event element))
	(sistandard (make-sistandard element)))
    ;; (format t "ll: ~a~%" (gethash :description sistandard))
    (make-instance
     'sievent
     :title              (gethash :title event)
     :startTime          (gethash :startTime event)
     :endTime            (gethash :endTime event)
     :programId          (gethash :programId event)
     :displayedStartTime (gethash :start sistandard)
     :displayedEndTime   (gethash :end   sistandard)
     :description        (gethash :description sistandard))))


(defclass events()
  ((siEvents
    :accessor :siEvents
    :initarg  :siEvents)
   (vaEvents
    :accessor :vaEvents
    :initarg  :vaEvents)
   (logos
    :accessor :logos
    :initarg  :logos)))


(defun read-out-all-events(hdplus.xml.children)
  (let ((vaEvents (list))
	(siEvents (list))
	(logos    (list)))
    (dom:do-node-list (node hdplus.xml.children)
      (when (and (eq :element (dom:node-type node))
		 (string= "eventCommands" (dom:tag-name node)))
	(let ((defines (dom:child-nodes node)))
	  (dom:do-node-list (define defines)
	    (when (and (eq :element (dom:node-type define))
		       (string= "DEFINE" (dom:tag-name define)))
	      (let ((events (dom:child-nodes define)))
		(dom:do-node-list (event events)
		  (when (eq :element (dom:node-type event))
		    (cond
		      ;; vaEvent
		      ((string= "vaEvent" (dom:tag-name event))
		       (push (make-vaevent event) vaEvents))
		      ;; siEvent
		      ((string= "siEvent" (dom:tag-name event))
		       (push (make-sievent event) siEvents))
		      ;; logos
		      ((or (string= "logoEvent" (dom:tag-name event))
			   (string= "layoutEvent" (dom:tag-name event)))
		       (push (make-logo event) logos))
		      (t ()))))))))))
    (make-instance 'events
		   :sievents (reverse siEvents)
		   :vaevents (reverse vaEvents)
		   :logos (reverse logos))))

(defun print-sievents (event1 event2)
  (format t "~50a~50a~%"
	  (format nil "Title:            ~a" (:title event1))
	  (format nil "Title:            ~a" (:title event2)))

  (termcolor:fg :red)
  (format t "~50a" (format nil "Starttime:        ~a" (:starttime event1)))
  (termcolor:fg :reset)
  (format t "Starttime:        ~a~%" (:starttime event2))  

  (format t "~50a" (format nil "Endtime:          ~a" (:endtime event1)))
  (termcolor:fg :red)
  (format t "Endtime:          ~a~%" (:endtime event2))
  (termcolor:fg :reset)

  (format t "~50a~50a~%"
	  (format nil "ProgramId:        ~a"(:programid event1))
	  (format nil "ProgramId:        ~a" (:programid event2)))
  
  (termcolor:fg :red)
  (format t "~50a" (format nil "DisplayedStart:   ~a" (:displayedstarttime event1)))
  (termcolor:fg :reset)
  
  (format t "~50a~%" (format nil "DisplayedStart:   ~a" (:displayedstarttime event2)))
  
  (format t "~50a" (format nil "DisplayedEndtime: ~a" (:displayedendtime event1)))
  (termcolor:fg :red)
  (format t "~50a~%" (format nil "DisplayedEndtime: ~a" (:displayedendtime event2)))
  (termcolor:fg :reset))



(defun print-vaevents (event1 event2)
  (format t "~50a~50a~%"
	  (format nil "Title:            ~a" (:title event1))
	  (format nil "Title:            ~a" (:title event2)))

  (termcolor:fg :red)
  (format t "~50a" (format nil "Starttime:        ~a" (:starttime event1)))
  (termcolor:fg :reset)
  (format t "Starttime:        ~a~%" (:starttime event2))  

  (format t "~50a" (format nil "Endtime:          ~a" (:endtime event1)))
  (termcolor:fg :red)
  (format t "Endtime:          ~a~%" (:endtime event2))
  (termcolor:fg :reset)

  (format t "~50a~50a~%"
	  (format nil "ProgramId:        ~a"(:programid event1))
	  (format nil "ProgramId:        ~a" (:programid event2)))

  (format t "~50a~50a~%"
	  (format nil "ContentId:        ~a" (:contentid event1))
	  (format nil "ContentId:        ~a" (:contentid event2))))


(defun print-sievent (event &key (extra-line nil) (time nil))
  (format t "Title:            ~a~%" (:title event))
  (when time
    (termcolor:fg :red))
  (format t "Starttime:        ~a~%" (:starttime event))
  (format t "Endtime:          ~a~%" (:endtime event))
  (format t "DisplayedStart:   ~a~%" (:displayedstarttime event))
  (format t "DisplayedEndtime: ~a~%" (:displayedendtime event))
  (when time
    (termcolor:fg :reset))
  (format t "ProgramId:        ~a~%" (:programid event))
  (when extra-line
    (format t "~%")))

(defun print-vaevent (event &key (extra-line nil) (contentid nil) (time nil))
  (format t "Title:            ~a~%" (:title event))

  (when time
    (termcolor:fg :red))
  (format t "Starttime:        ~a~%" (:starttime event))
  (format t "Endtime:          ~a~%" (:endtime event))
  (when time
    (termcolor:fg :reset))

  (format t "ProgramId:        ~a~%" (:programid event))

  (when contentid
    (termcolor:fg :red))
  (format t "ContentId:        ~a~%" (:contentid event))
  (when contentid
    (termcolor:fg :reset))

  (when extra-line
    (format t "~%")))


(defmethod time-error((obj1 sievent) (obj2 sievent))
  (let ((end-first (:endtime obj1))
	(start-second (:starttime obj2))
	(end-disp  (:displayedendtime obj1))
	(start-disp (:displayedstarttime obj2)))
    (or (not (local-time:timestamp=  end-first start-second))
	(not (local-time:timestamp=  end-disp start-disp)))))


(defmethod print-event ((obj sievent) &key (extra-line nil) (contentid nil) (time nil))
  (print-sievent obj :extra-line extra-line :time time))


(defmethod print-events ((obj1 sievent) (obj2 sievent))
  (print-sievents obj1 obj2))

(defmethod print-events ((obj1 vaevent) (obj2 vaevent))
  (print-vaevents obj1 obj2))


(defmethod time-error ((obj1 event) (obj2 event))
  (not
   (local-time:timestamp=
    (:endtime   obj1)
    (:starttime obj2))))


(defmethod time-error ((obj1 sievent) (obj2 sievent))
  (or
  (not
   (local-time:timestamp=
    (:endtime   obj1)
    (:starttime obj2)))
  (not
   (local-time:timestamp=
    (:displayedendtime obj1)
    (:displayedstarttime obj2)))))

(defmethod id-error ((obj vaevent))
  (let ((contentId (:contentId obj)))
    (and (find #\- contentId)
	 (not (find "WERBUNG" contentId)))))


(defun list-id-errors(events &key (extra-line nil) (verbose nil))
  "Collect all id errors in the Events"
  (let ((id-errors (remove-if-not #'id-error events)))
    (when verbose
      (loop :for event :in id-errors
	    :do (format t "~a~%" event)
		(when extra-line
		  (format t "~%"))))
    (format t "~a id errors found.~%" (length id-errors))))


(defmethod text-error ((obj sievent))
  (let ((description (:description obj)))
    (cond
      (description
       (if (string= "" (str:trim description))
	   t
	   nil))
      (t nil))))

(defun list-text-errors(events &key (extra-line nil) (verbose nil))
  "Collect all id errors in the Events"
  (let ((text-errors (remove-if-not #'text-error events)))
    (when verbose 
	(loop :for event :in text-errors
	      :do (format t "~a~%" event)
		  (when extra-line
		    (format t "~%"))))
    (format t "~a text errors found.~%" (length text-errors))))


(defun list-events(events &key (extra-line nil))
  (loop :for event :in events
	:do (format t "~a" event)
	    (if extra-line
		(format t "~2%")
		(format t "~%"))))
  
(defun list-vaevents(events &key (extra-line nil))
  (list-events (:vaevents events) :extra-line extra-line))


(defun list-sievents(events &key (extra-line nil))
  (list-events (:sievents events)  :extra-line extra-line))



(defun list-time-errors(events &key (extra-line nil) (verbose nil))
  "Collect all time errors in the Events"
  (let ((before-last (- (length events) 2))
	(the-last (- (length events) 1))
	(store (list)))

    ;; Find errors
    (loop :for a :in (subseq events 0 before-last)
	  :for b :in (subseq events 1 the-last)
	  :do (when (time-error a b)
		(push (cons a b) store)))

    ;; List errors
    (when verbose
      (loop :for a :in store
	    :do (format t "~2%~a:~%~a~%~a~2%" "time error" (car a) (cdr a))
		(when extra-line
		  (format t "~%"))))
    
    (format t "~a time errors found.~%" (length store))))


