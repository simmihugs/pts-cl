(declaim (optimize (speed 3) (debug 0) (safety 0)))
(in-package :pts)



(defun extract-node(nodes node-name)
  (loop :for node :across nodes
	:collect (when (string= node-name (dom:node-name node))
		   (return node))))

(defun load-pts-file(&optional path)
  "Load file - return file or `nil' if it does not exist."
  (handler-case (cxml:parse-file path (cxml-dom:make-dom-builder))
    (sb-ext:file-does-not-exist ()     nil)
    (sb-ext:file-exists         (path)
      (cxml:parse-file path (cxml-dom:make-dom-builder)))))

