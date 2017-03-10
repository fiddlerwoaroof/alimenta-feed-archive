(defpackage :alimenta.feed-archive
  (:use :cl :alexandria :serapeum :fw.lu :alimenta.feed-archive.tools)
  (:shadowing-import-from :alimenta.feed-archive.tools :->)
  (:export #:init-feeds #:archive-feeds #:command-line-main))

(in-package :alimenta.feed-archive)


(defvar *feeds*)
(defvar *feed-base*)

(defparameter +dirname-format+
  '((:year 4) #\- (:month 2) #\- (:day 2) #\/ (:hour 2) #\- (:min 2) #\/))

(defclass feed-index ()
  ((%pull-time :initarg :pull-time :reader pull-time)
   ;; Why this slot? Won't the references duplicate this?
   (%feed-urls :initarg :feed-urls :reader feed-urls)
   (%feed-references :initarg :references :reader references)))

(defclass feed-reference ()
  ((%url :initarg :url :reader url)
   (%title :initarg :title :reader title :initform nil)
   (%path :initarg :path :reader path :initform nil)))

(defun make-feed-index (pull-time feeds paths)
  (make-instance 'feed-index
		 :pull-time pull-time
		 :feed-urls feeds
		 :references (mapcar (destructuring-lambda (url (title path))
				       (make-feed-reference url :title title :path path))
				     feeds
				     paths)))

(defun make-feed-reference (url &rest feed-data)
  (apply #'make-instance 'feed-reference
	 :url url
	 feed-data))

(defmethod yason:encode-slots progn ((object feed-reference))
  (let ((title (title object))
	(path (path object)))
    (yason:encode-object-element "url" (url object))
    (when title
      (yason:encode-object-element "title" title))
    (when path
      (yason:encode-object-element "path" path))))

(defmethod yason:encode-slots progn ((object feed-index))
  (with-accessors ((pull-time pull-time) (feeds feed-urls) (references references)) object
    (yason:encode-object-elements "pull-time" (local-time:format-timestring nil pull-time)
				  "feed-urls" feeds)
    (yason:with-object-element ("feeds")
      (yason:with-array ()
	(mapcar 'yason:encode-object references)))))

(defun get-store-directory-name (timestamp)
  (flet ((make-dirname (timestamp)
	   (-> (local-time:format-timestring nil (local-time:timestamp-minimize-part timestamp :sec)
					     :format +dirname-format+)
	       (merge-pathnames *feed-base*))))
    (-> (prog1-let ((result (make-dirname timestamp)))
	  (ensure-directories-exist result))
	(car))))

(defun test-feed-list ()
  (values '("http://feeds.feedburner.com/GamasutraFeatureArticles/"
	    "https://www.codinghorror.com/blog/index.xml"
	    "https://sancrucensis.wordpress.com/feed/")
	  #p"/tmp/feed-archive/"))

(defun init-feeds (&key feed-list archive-root)
  (ubiquitous:restore 'alimenta.feed-archiver)
  (let ((default-root (or archive-root
			  (merge-pathnames ".feed-archive/"
					   (truename "~/")))))
    (values (ubiquitous:defaulted-value feed-list :feeds)
	    (ubiquitous:defaulted-value default-root :archive :root))))

(defun add-feed (feed)
  (init-feeds)
  (pushnew feed
	   (ubiquitous:value :feeds)
	   :test #'equalp))

(defun safe-pull-feed (feed-url &aux (pop-times 0))
  "Handles date parsing errors in the feed: chronicity won't parse
   certain date formats, this catches the error and modifies the
   format to something chronicity can handle."
  (flet ((pop-50-tokens (c)
	   (declare (ignore c))
	   (when (find-restart 'alimenta:pop-token) 
	     (if (< pop-times 50)
		 (progn (incf pop-times)
			(format t "~&Processing error, trying to pop a token (popped ~d times)~%"
				pop-times)
			(alimenta:pop-token))
		 (continue)))))
    (handler-bind ((warning #'muffle-warning)
		   (error #'pop-50-tokens))
      (prog1 (alimenta.pull-feed:pull-feed feed-url)
	;; Why am I decf-ing here?
	(decf pop-times)))))

(defun log-pull (stream feed-url)
  (format stream "~&Trying to pull: ~a... " feed-url)
  (handler-bind ((error (lambda (c) (format stream "... Error ~a~%" c))))
      (prog1 (safe-pull-feed feed-url)
		  (format stream "... Success~%"))))

(defun skip-feed ()
  (when-let ((restart (find-restart 'skip-feed)))
    (invoke-restart restart)))


(defun pull-and-store-feeds (feeds pull-directory)
  (mapcar (lambda (feed-url)
	    (with-simple-restart (skip-feed "Skip ~a" feed-url)
	      (let ((feed (with-retry ("Pull feed again.")
			    (log-pull t feed-url))))
		(store (coerce-feed-link feed-url feed)
		       pull-directory))))
	  feeds))

(defun feed-index (index-stream pull-time paths)
  (yason:with-output (index-stream :indent t)
    (yason:encode-object
     (make-feed-index pull-time *feeds*
		      (mapcar (destructuring-lambda ((title path))
				(list title (uiop:enough-pathname path *feed-base*)))
			      paths)))))

(defun archive-feeds ()
  (let* ((pull-time (local-time:now))
	 (pull-directory (get-store-directory-name pull-time)) 
	 (paths (pull-and-store-feeds *feeds* pull-directory))
	 (index-path (merge-pathnames "index.json" pull-directory)))
    (with-open-file (index index-path :direction :output)
      (feed-index index pull-time paths))))

;; This is an ungodly mess, we need to avoid funneling everything through fix-pathname-or-skip
(defun command-line-main (&optional (feed-list-initializer #'init-feeds))
  (labels ((feed-type-unsupported (c &key (restart 'skip-feed))
	     (format t "~&Feed type unsupported: ~a for feed ~a~%"
		     (alimenta:feed-type c)
		     (alimenta:feed-link c))
	     (funcall restart))
	   (fix-pathname-or-skip (c &key (restart 'skip-feed) (wrapped-condition nil wc-p))
	     (typecase (or wrapped-condition c)
	       (alimenta:feed-type-unsupported (feed-type-unsupported c))
	       (otherwise
		(if (find-restart 'fix-pathname)
		    (fix-pathname)
		    (progn (unless (eq restart 'continue)
			     (format t "~&Skipping a feed... ~s~%"
				     (if wc-p
					 (alimenta.feed-archive.encoders:the-feed c)
					 "Unknown")))
			   (funcall restart)))))))

    (handler-bind ((alimenta.feed-archive.encoders:feed-error
		    (lambda (c)
		      (fix-pathname-or-skip c :wrapped-condition (alimenta.feed-archive.encoders:the-condition c))))
		   (alimenta:feed-type-unsupported #'feed-type-unsupported)
		   (error (lambda (c)
			    (fix-pathname-or-skip c :restart 'continue))))
      (multiple-value-bind (*feeds* *feed-base*) (funcall feed-list-initializer)
	(alimenta.pull-feed:with-user-agent ("Feed Archiver v0.1b")
	  (archive-feeds))))))
