(defpackage :alimenta.feed-archive
  (:use :cl :alexandria :serapeum :fw.lu :alimenta.feed-archive.tools)
  (:shadowing-import-from :alimenta.feed-archive.tools :->)
  (:export #:init-feeds #:archive-feeds #:command-line-main))

(in-package :alimenta.feed-archive)


(defvar *feeds*)
(defvar *feed-base*)

(defparameter +dirname-format+
  '((:year 4) #\- (:month 2) #\- (:day 2) #\/ (:hour 2) #\- (:min 2) #\/))

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

(defmacro with-progress-message ((stream before after &optional (error-msg " ERROR~%~4t~a~%")) &body body)
  (once-only (before after stream)
    `(handler-bind ((error (op (format ,stream ,error-msg _))))
       (format ,stream "~&~a . . ." ,before)
       (multiple-value-prog1 (progn
			       ,@body)
	 (format ,stream " ~a~%" ,after)))))

(defun skip-feed ()
  (when-let ((restart (find-restart 'skip-feed)))
    (invoke-restart restart)))

(defun save-feed (feed output-file &key (if-exists :supersede))
  (with-output-to-file (s output-file :if-exists if-exists)
    (plump:serialize (alimenta:doc feed) s)))

(defun pull-and-store-feeds (feeds pull-directory)
  (mapcar (op (pull-and-store-feed _ pull-directory))
	  feeds))

(defun pull-and-store-feed (feed-url pull-directory)
  (flet ((log-pull (stream)
	   (let ((before-message (format nil "Trying to pull: ~a" feed-url)))
	     (with-progress-message (stream before-message "Success")
	       (prog1 (safe-pull-feed feed-url)))))
	 (log-serialization (stream feed path)
	   (with-progress-message (stream "Serializing XML" (format nil "done with ~a" feed-url))
	     (save-feed feed (merge-pathnames "feed.xml" path)))))

    (with-simple-restart (skip-feed "Stop processing for ~a" feed-url)
      (let* ((feed (with-retry ("Pull feed again.")
		     (alimenta:filter-feed (coerce-feed-link feed-url
							     (log-pull t))
					   (complement #'older-than-a-month)
					   :key 'alimenta:date))))
	(multiple-value-bind (result url) (store feed pull-directory)
	  (destructuring-bind (title path) result
	    (log-serialization t feed path)
	    (make-feed-reference url :title title
				 :path (uiop:enough-pathname path *feed-base*))))))))

(defun feed-index (index-stream pull-time references)
  (yason:with-output (index-stream :indent t)
    (yason:encode-object
     (make-feed-index pull-time (remove-if 'null references)))))

(defun archive-feeds ()
  (let* ((pull-time (local-time:now))
	 (pull-directory (get-store-directory-name pull-time)) 
	 (references (pull-and-store-feeds *feeds* pull-directory))
	 (index-path (merge-pathnames "index.json" pull-directory)))
    (with-open-file (index index-path :direction :output)
      (feed-index index pull-time references))))

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

    (let ((error-count 0))
      (handler-bind ((alimenta.feed-archive.encoders:feed-error
		      (op (fix-pathname-or-skip _1 :wrapped-condition (alimenta.feed-archive.encoders:the-condition _1))))
		     (alimenta:feed-type-unsupported #'feed-type-unsupported)
		     ((or usocket:timeout-error
			  usocket:ns-error) (op (alimenta.pull-feed:skip-feed _)))
		     (error
		      (op
			(format t "~&Error signaled, ~a (count ~d)" _1 error-count)
			(incf error-count)
			(unless (< error-count 15)
			  (format t " continuing~%")
			  (fix-pathname-or-skip _1 :restart 'continue)))))
	(multiple-value-bind (*feeds* *feed-base*) (funcall feed-list-initializer)
	  (alimenta.pull-feed:with-user-agent ("Feed Archiver v0.1b")
	    (archive-feeds)))))))
