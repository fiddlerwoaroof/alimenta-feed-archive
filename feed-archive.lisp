(defpackage :alimenta.feed-archive
  (:use :cl :alexandria :serapeum :fw.lu))

(in-package :alimenta.feed-archive)

(defvar *feeds*)
(defvar *feed-base*)

(defparameter +dirname-format+
  '((:year 4) #\- (:month 2) #\- (:day 2) #\/ (:hour 2) #\- (:min 2) #\/))

(defun get-store-directory-name (timestamp)
  (car
    (prog1-let ((result (merge-pathnames
                          (local-time:format-timestring
                            nil
                            (local-time:timestamp-minimize-part timestamp :sec)
                            :format +dirname-format+)
                          *feed-base*)))
      (ensure-directories-exist result))))

(defmethod store ((feed alimenta:feed) directory)
  (with-accessors ((description alimenta:description)
                   (feed-link alimenta:feed-link)
                   (items alimenta:items)
                   (link alimenta:link)
                   (source-type alimenta:source-type)
                   (title alimenta:title)) feed
    (prog1-let ((feed-title title)
                (feed-store (merge-pathnames (concatenate 'string (puri:uri-host feed-link)
                                                         "/")
                                            directory)))
      (ensure-directories-exist feed-store)
      (with-open-file (index (merge-pathnames "index.json" feed-store) :direction :output)
        (yason:with-output (index :indent t)
          (yason:with-object ()
            (yason:encode-object-element "title" title)
            (yason:encode-object-element "fetch-url"
                                         (puri:render-uri feed-link nil))
            (yason:encode-object-element "link" link)
            ;(yason:encode-object-element "source-type" source-type)
            (yason:encode-object-element "description" description)
            (yason:with-object-element ("items")
              (yason:with-array ()
                (dolist (item (store items feed-store))
                  (destructuring-bind (title path) item
                    (yason:with-object ()
                      (yason:encode-object-element "title" title)
                      (yason:encode-object-element "path" path))))))))))))

(defun older-than-a-week (date)
  (let ((week-ago (local-time:timestamp- (local-time:now)
                                         7 :day)))
    (local-time:timestamp< date week-ago)))

(defmethod store ((items sequence) directory)
  (map 'list (lambda (item) (store item directory))
       (stable-sort
         (sort (remove-if #'older-than-a-week items :key #'alimenta:date)
             #'string-lessp
             :key #'alimenta:title)
         #'local-time:timestamp>
         :key #'alimenta:date)))

(defmethod get-id ((item alimenta:item))
  (let* ((digester (ironclad:make-digesting-stream :sha256))
         (digest-stream (flexi-streams:make-flexi-stream digester)))
    (princ (alimenta:id item) digest-stream)
    (concatenate 'string
                 (local-time:format-timestring nil (alimenta:date item))
                 "-"
                 (crypto:byte-array-to-hex-string (crypto:produce-digest digester))
                 ".json")))

(defmethod store ((item alimenta:item) directory)
  (with-accessors ((author alimenta::author)
                   (content alimenta:content)
                   (date alimenta:date)
                   (id alimenta:id)
                   (link alimenta:link)
                   (title alimenta:title)) item
    (let* ((date (local-time:format-timestring nil date)))
      (prog1-let ((item-title title)
                  (fn (get-id item)))
        (with-open-file (item-f (merge-pathnames fn directory) :direction :output)
          (yason:with-output (item-f :indent t)
            (yason:with-object ()
              (yason:encode-object-element "title" title)
              (yason:encode-object-element "date" date)
              (yason:encode-object-element "author" title)
              (yason:encode-object-element "id" (princ-to-string id))
              (yason:encode-object-element "link" link)
              (yason:encode-object-element "content" content))))) )))


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
  (declare (optimize (debug 3)))
  (multiple-value-bind (*feeds* *feed-base*) (init-feeds)
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
                                (destructuring-bind (title path) feed-data
                                  (yason:encode-object-element "title" title)
                                  (yason:encode-object-element "path"
                                                               (princ-to-string
                                                                 (uiop:enough-pathname path *feed-base*))))))
                            *feeds*
                            paths)))))))))))
