(in-package :pts)

(defun cli/options ()
  "Returns a list of options for our main command"
  (list
    (clingon:make-option
     :flag
     :description "short help."
     :short-name #\h
     :key :help)
    (clingon:make-option
     :flag
     :description "make verbose."
     :short-name #\v
     :key :verbose)
    (clingon:make-option
     :flag
     :description "extra line."
     :short-name #\e
     :long-name "extra-line"
     :key :extra-line)
    (clingon:make-option
     :flag
     :description "list vaevents."
     :short-name #\l
     :long-name "list-vaevents"
     :key :list-vaevents)
    (clingon:make-option
     :string            
     :description "file"
     :short-name #\f
     :long-name "file"
     :env-vars '("USER")
     :initial-value nil 
     :key :file)))

(defun deal-with-file (xml-file verbose listSievents listVaevents extra-line)
  (let* ((xml-file-nodes (dom:child-nodes (dom:document-element xml-file)))
	 (events (pts:read-out-all-events xml-file-nodes)))
    (format t "~a~%" "vaEvents:")
    (list-id-errors   (:vaevents events) :verbose verbose :extra-line extra-line)
    (pts:list-time-errors (:vaevents events) :verbose verbose :extra-line extra-line)
    
    (format t "~a~%" "siEvents:")
    (pts:list-time-errors (:sievents events) :verbose verbose :extra-line extra-line)
    (pts:list-text-errors (:sievents events) :verbose verbose :extra-line extra-line)
    
    (when listVaevents
      (progn
	(format t "~3%vaEvents:~%")
	(pts:list-vaevents events :extra-line extra-line)))

    (when listSievents
      (progn
	(format t "~3%siEvents:~%")
	(pts:list-sievents events :extra-line extra-line)))))

(defun cli/handler (cmd &optional debug-file verbose listSievents listVaevents extra-line)
  "The handler function of our top-level command"
  (let* ((free-args (clingon:command-arguments cmd))
	 (extra-line (if extra-line t (clingon:getopt cmd :extra-line)))
	 (file (my-load-file (clingon:getopt cmd :file)))
	 (verbose (if verbose t (clingon:getopt cmd :verbose)))
	 ;;(listVaevents (if listVaevents t (clingon:getopt cmd :list-vaevents)))))
         (help (clingon:getopt cmd :help)))
    (cond
      (file (deal-with-file file verbose listSievents listVaevents extra-line))
      (debug-file (let ((file (my-load-file debug-file)))
	      (when file (deal-with-file file verbose listSievents listVaevents extra-line))))
      (t (progn
	   (format t "~%~a~2%" "file does not exist")
	   (clingon:print-usage (cli/command) t))))))
 
(defun cli/command (&optional debug verbose listSievents listVaevents extra-line)
  "A command to say hello to someone"
  (clingon:make-command
   :name "pts2"
   :description "check a pts file"
   :version "0.1.0"
   :authors '("John Doe <john.doe@example.org")
   :license "BSD 2-Clause"
   :options (cli/options) ;; <-- our options
   :handler #'(lambda (x) (cli/handler x debug verbose listSievents listVaevents extra-line))))

(defun main(&key (debug nil) (verbose nil) (list-sievents nil) (list-vaevents nil) (extra-line nil))
  (clingon:run (cli/command debug verbose list-sievents list-vaevents extra-line)))

