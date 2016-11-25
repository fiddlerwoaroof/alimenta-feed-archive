(asdf:defsystem #:alimenta-feed-archive
  :description "Dump a collection of feeds to a directory"
  :author "Fiddlerwoaroof <fiddlerwoaroof@howit.is>"
  :license "MIT"
  :depends-on (#:alexandria
	       #:alimenta
	       #:fwoar.lisputils
	       #:ironclad
	       #:local-time
	       #:serapeum
	       #:ubiquitous
	       #:uiop
	       #:yason)
  :serial t
  :components ((:file "tools")
	       (:file "yason-encoders")
	       (:file "feed-archive")))

