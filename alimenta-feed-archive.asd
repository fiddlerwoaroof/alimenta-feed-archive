(asdf:defsystem #:alimenta-feed-archive
  :description "Dump a collection of feeds to a directory"
  :author "Fiddlerwoaroof <fiddlerwoaroof@howit.is>"
  :license "MIT"
  :depends-on (#:alexandria
               #:alimenta
               #:fwoar.lisputils
               #:alimenta+patmatch
               #:ironclad
               #:local-time
               #:serapeum
               #:stream-provider
               #:trivia
               #:ubiquitous
               #:uiop
               #:vector-update-stream
               #:yason
               #:html-sanitizer)
  :serial t
  :components ((:file "package")
               (:file "tools")
               (:file "yason-encoders")
               (:file "encoders")
               (:file "feed-index-utils")
               (:file "feed-archive")))

