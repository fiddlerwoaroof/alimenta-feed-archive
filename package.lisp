(defpackage :afa-user
  (:use cl)
  (:export #:define-package))
(in-package :afa-user)

(defmacro define-package (name &body arguments)
  (let ((uses (cons :use
		    (union '(:cl :alexandria :serapeum)
			   (cdr (assoc :use arguments))))))
    `(progn
       (defpackage ,name
	 ,uses
	 ,@(remove :use arguments :key 'car)))))

(define-package :alimenta.feed-archive.tools
  (:use :fw.lu)
  (:shadow :->)
  (:export :fix-pathname :sha256-string :get-id :older-than-a-week :-> :get-feed-store-name
	   :store :get-item-store-name :restart-once :coerce-feed-link :with-retry
	   :older-than-a-month))

(define-package :alimenta.feed-archive.encoders
  (:use :fw.lu :alimenta.feed-archive.tools)
  (:shadowing-import-from :alimenta.feed-archive.tools :->)
  (:export :skip-item :the-condition :the-feed :feed-error
           :unwrap-feed-errors :feed-stream-provider))

(define-package :alimenta.feed-archive
  (:use :fw.lu :alimenta.feed-archive.tools)
  (:shadowing-import-from :alimenta.feed-archive.tools :->)
  (:export #:init-feeds #:archive-feeds #:command-line-main))

