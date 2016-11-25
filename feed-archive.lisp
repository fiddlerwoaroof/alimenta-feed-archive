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
	   (merge-pathnames (local-time:format-timestring nil (local-time:timestamp-minimize-part timestamp :sec)
							  :format +dirname-format+)
			    *feed-base*)))
    (-> (prog1-let ((result (make-dirname timestamp)))
	  (ensure-directories-exist result))
	(car))))

(defun %encode-item (root-dir item)
  (let ((restarted nil))
    (destructuring-bind (title path) item
      (tagbody start
	 (format t "~&Restarted: ~a" restarted)
	 (when restarted
	   (format t " ~a~%"(namestring path)))
	 (restart-case
	     (progn (format t "~&encoding . . .~%")
		    (let ((pathname (uiop:enough-pathname path root-dir)))
		      (yason:with-object ()
			(yason:encode-object-element "title" title)
			(yason:encode-object-element "path" pathname))))
	   (fix-pathname ()
	     (setf path
		   (merge-pathnames path
				    (make-pathname :type :unspecific)))
	     (unless restarted
	       (setf restarted t)
	       (go start))))))))

(defun encode-feed-as-json (feed item-storage-info root-dir &optional stream)
  (with-accessors ((description alimenta:description)
		   (feed-link alimenta:feed-link)
		   (items alimenta:items)
		   (link alimenta:link)
		   (source-type alimenta:source-type)
		   (title alimenta:title)) feed)
  (yason:with-output (stream :indent t)
    (yason:with-object ()
      (yason:encode-object-element "metadata" feed)
      (yason:with-object-element ("items")
	(yason:with-array ()
	  (dolist (item item-storage-info)
	    (with-simple-restart (continue "Skip item ~s" (car item))
	      (%encode-item root-dir item))))))))

(defmethod store ((item alimenta:item) directory)
  (prog1-let ((item-title (alimenta:title item))
	      (fn (get-item-store-name item directory)))
    (with-open-file (item-f fn :direction :output)
      (yason:encode item item-f))))

(defmethod store ((feed alimenta:feed) directory)
  (with-accessors ((description alimenta:description)
		   (feed-link alimenta:feed-link)
		   (items alimenta:items)
		   (link alimenta:link)
		   (source-type alimenta:source-type)
		   (title alimenta:title)) feed
    (prog1-let ((feed-title title)
		(feed-store (get-feed-store-name feed directory)))
      (ensure-directories-exist feed-store)
      (with-open-file (index (merge-pathnames "index.json" feed-store) :direction :output)
	(encode-feed-as-json feed
			     (store items feed-store)
			     feed-store
			     index)))))

(defmethod store ((items sequence) directory)
  (map 'list (lambda (item) (store item directory))
       (stable-sort
         (sort (remove-if #'older-than-a-week items :key #'alimenta:date)
             #'string-lessp
             :key #'alimenta:title)
         #'local-time:timestamp>
         :key #'alimenta:date)))

(defmethod yason:encode ((item alimenta:item) &optional stream)
  (with-accessors ((author alimenta::author)
                   (content alimenta:content)
                   (date alimenta:date)
                   (id alimenta:id)
                   (link alimenta:link)
                   (title alimenta:title)) item
    (let* ((date (local-time:format-timestring nil date)))
      (yason:with-output (stream :indent t)
	(yason:with-object ()
	  (yason:encode-object-element "title" title)
	  (yason:encode-object-element "date" date)
	  (yason:encode-object-element "author" title)
	  (yason:encode-object-element "id" (princ-to-string id))
	  (yason:encode-object-element "link" link)
	  (yason:encode-object-element "content" content)))))
  item)


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
  (let ((pop-times 0))
    (handler-bind
      ((condition
         (lambda (c) c
           (when (find-restart 'alimenta.rss::pop-token) 
             (if (< pop-times 50)
               (progn (incf pop-times)
                      (format t "~&Processing error, trying to pop a token (popped ~d times)~%"
                              pop-times)
                      (alimenta.rss::pop-token))
               (progn
                 (break)
                 (continue)))))))
      (prog1 (alimenta.pull-feed:pull-feed feed-url)
        (decf pop-times)))))

(defun archive-feeds ()
  (let ((pull-time (local-time:now)))
    (alimenta.pull-feed::with-user-agent ("Feed Archiver v0.1b")
      (let* ((pull-directory (get-store-directory-name pull-time)) 
	     (paths (loop for feed-url in *feeds* collect
			 (with-simple-restart (continue "Skip ~a" feed-url)
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
			  paths))))))))))


(defun command-line-main (&optional (feed-list-initializer #'init-feeds))
  (handler-bind ((error (lambda (c)
		      c
		      (format t "~&CONDITION RECEIVED: ~S~%RESTARTS: ~s~%" c (compute-restarts c))
		      (if (find-restart 'fix-pathname)
			  (fix-pathname)
			  (progn (format t "~&Skip a feed...~%")
				 (continue))))))
    (multiple-value-bind (*feeds* *feed-base*) (funcall feed-list-initializer)
      (archive-feeds))))

