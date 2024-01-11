(asdf:defsystem "pts"
  :description "A pts error checker"
  :depends-on ("clingon"
	       "cxml"
	       "cl-date-time-parser")
  :serial t  
  :build-operation "program-op"
  :build-pathname "build/pts2"
  :entry-point "pts:main"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
			     (:file "xml")
			     (:file "data")
			     (:file "test-data")
			     (:file "main")))))


