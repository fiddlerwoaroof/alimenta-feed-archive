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
               #:yason)
  :serial t
  :components ((:file "feed-archive")))

