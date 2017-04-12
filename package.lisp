(defpackage :alimenta.feed-archive.tools
  (:use :cl :alexandria :serapeum :fw.lu)
  (:shadow :->)
  (:export :fix-pathname :sha256-string :get-id :older-than-a-week :-> :get-feed-store-name
	   :store :get-item-store-name :restart-once :coerce-feed-link :with-retry
	   :older-than-a-month))

(defpackage :alimenta.feed-archive.encoders
  (:use :cl :alexandria :serapeum :fw.lu :alimenta.feed-archive.tools)
  (:shadowing-import-from :alimenta.feed-archive.tools :->)
  (:export :skip-item :the-condition :the-feed :feed-error
	   :unwrap-feed-errors))

(defpackage :alimenta.feed-archive
  (:use :cl :alexandria :serapeum :fw.lu :alimenta.feed-archive.tools)
  (:shadowing-import-from :alimenta.feed-archive.tools :->)
  (:export #:init-feeds #:archive-feeds #:command-line-main))

