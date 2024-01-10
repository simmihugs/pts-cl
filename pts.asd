(asdf:defsystem "pts"
  :description "A pts error checker"
  :depends-on ("termcolor"
	       "str"
	       "clingon"
	       "cxml"
	       "cl-date-time-parser"
	       "cl-ppcre"
	       "serapeum")
  :serial t  
  :build-operation "program-op"
  :build-pathname "build/pts2"
  :entry-point "pts:main"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
			     (:file "data")
			     (:file "main")))))


