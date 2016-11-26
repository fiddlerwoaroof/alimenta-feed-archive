(defpackage :alimenta.feed-archive
  (:use :cl :alexandria :serapeum :fw.lu :alimenta.feed-archive.tools)
  (:shadowing-import-from :alimenta.feed-archive.tools :->))

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
	    "http://feeds.feedburner.com/GamasutraNews/"
	    "http://feeds.feedburner.com/GamasutraColumns/")
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

(defun safe-pull-feed (feed-url)
  "Handles date parsing errors in the feed: chronicity won't parse certain date formats, this catches the error 
and modifies the format to something chronicity can handle."
  (let ((pop-times 0))
    (flet ((pop-50-tokens (c)
	     (declare (ignore c))
	     (when (find-restart 'alimenta.rss::pop-token) 
	       (if (< pop-times 50)
		   (progn (incf pop-times)
			  (format t "~&Processing error, trying to pop a token (popped ~d times)~%"
				  pop-times)
			  (alimenta.rss::pop-token))
		   (continue)))))
      (handler-bind ((error #'pop-50-tokens))
	(prog1 (alimenta.pull-feed:pull-feed feed-url)
	  ;; Why am I decf-ing here?
	  (decf pop-times))))))

(defun skip-feed ()
  (when-let ((restart (find-restart 'skip-feed)))
    (invoke-restart restart)))

(defun archive-feeds ()
  (let ((pull-time (local-time:now)))
    (let* ((pull-directory (get-store-directory-name pull-time)) 
	   (paths (loop for feed-url in *feeds* collect
		       (with-simple-restart (skip-feed "Skip ~a" feed-url)
			 (let ((feed (safe-pull-feed feed-url)))
			   (setf (alimenta:feed-link feed)
				 feed-url)
			   (store feed pull-directory))))))
      (with-open-file (index (merge-pathnames "index.json" pull-directory) :direction :output)
	(yason:with-output (index :indent t)
	  (yason:with-object ()
	    (yason:encode-object-element "pull-time" (local-time:format-timestring nil pull-time))
	    (yason:encode-object-element "feed-urls" *feeds*)
	    (yason:with-object-element ("feeds")
	      (yason:with-array ()
		(mapcar (lambda (url feed-data)
			  (yason:with-object ()
			    (yason:encode-object-element "url" url)
			    (when feed-data
			      (destructuring-bind (title path) feed-data
				(yason:encode-object-element "title" title)
				(yason:encode-object-element "path"
							     (princ-to-string
							      (uiop:enough-pathname path *feed-base*)))))))
			*feeds*
			paths)))))))))


(defun command-line-main (&optional (feed-list-initializer #'init-feeds))
  (flet ((fix-pathname-or-continue (c)
	   (declare (ignorable c))
	   (format t "~&Received condition ~s~%" c)
	   (if (find-restart 'fix-pathname)
	       (progn (fix-pathname)
		      (format t "~&Fixing pathname...~%"))
	       (progn (format t "~&Skipping a feed...~%")
		      (continue)))))
    (handler-bind ((error (lambda (c) (fix-pathname-or-continue c))))
      (multiple-value-bind (*feeds* *feed-base*) (funcall feed-list-initializer)
	(alimenta.pull-feed::with-user-agent ("Feed Archiver v0.1b")
	  (archive-feeds))))))

