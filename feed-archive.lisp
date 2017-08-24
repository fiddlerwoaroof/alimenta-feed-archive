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
            "http://edwardfeser.blogspot.com/feeds/posts/default"
            "http://feeds.feedburner.com/undergroundthomist/yCSy"
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
      (prog1-bind (feed (alimenta.pull-feed:pull-feed feed-url))
        ;; Why am I decf-ing here?
        (alimenta:transform feed
                            (fw.lu:glambda (entity)
                              (:method (entity))
                              (:method ((entity alimenta:item))
                                (setf (alimenta:content entity)
                                      (html-sanitizer:sanitize (alimenta:content entity))))))
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

(defun log-pull (feed-puller feed-url stream)
  (let ((before-message (concatenate 'string "Trying to pull: " feed-url)))
    (with-progress-message (stream before-message "Success")
      (funcall feed-puller feed-url))))

(defun normalize-feed (feed-url feed)
  (alimenta:filter-feed (coerce-feed-link feed-url feed)
                        (complement #'older-than-a-month)
                        :key 'alimenta:date))

(defun log-serialization (feed-url stream feed path)
  (declare (ignorable feed-url stream feed path))
  (with-progress-message (stream "Serializing XML" (format nil "done with ~a" feed-url))
    (save-feed feed (merge-pathnames "feed.xml" path))))

(defun feed-relative-pathname (path &optional (feed-base *feed-base*))
  (uiop:enough-pathname path feed-base))

(defun feed-index (index-stream pull-time references)
  (yason:with-output (index-stream :indent t)
    (yason:encode-object
     (make-instance 'feed-index
                    :pull-time pull-time
                    :references (remove-if 'null references)))))


(defun pull-and-store-feed (feed-url stream-provider &optional (feed-puller #'safe-pull-feed))
  (declare (optimize (debug 3)))
  (flet ((log-pull (stream)
           (declare (inline) (dynamic-extent stream))
           (log-pull feed-puller feed-url stream))
         (log-serialization (stream feed path)
           (declare (inline) (dynamic-extent stream))
           (log-serialization feed-url stream feed
                              (merge-pathnames path
                                               (stream-provider:root stream-provider)))))
    (handler-bind ((cl+ssl:ssl-error-verify
                    (lambda (c)
                      (declare (ignore c))
                      (format *error-output* "~&SSL Error while pulling ~a~%"
                              feed-url))))
      (with-simple-restart (skip-feed "Stop processing for ~a" feed-url)
        (let* ((feed (with-retry ("Pull feed again.")
                       (normalize-feed feed-url (log-pull t)))))
          (trivia:match (store feed stream-provider)
            ((list title path)
             (log-serialization t feed path)
             (make-feed-reference (alimenta:feed-link feed)
                                  :title title
                                  :path (feed-relative-pathname
                                         (uiop:pathname-directory-pathname
                                          (merge-pathnames path
                                                           (stream-provider:root stream-provider))))))))))))

(defun archive-feeds (pull-time pull-directory index-stream)
  (prog1-bind (references (mapcar (op (pull-and-store-feed _ pull-directory))
                                  *feeds*))
    (feed-index index-stream pull-time references)))

(defun archive-feeds-nondeterm ()
  (let* ((pull-time (local-time:now))
         (pull-directory (get-store-directory-name pull-time)) 
         (index-path (merge-pathnames "index.json" pull-directory))
         (feed-stream-provider (make-instance 'alimenta.feed-archive.encoders:feed-stream-provider
                                              :if-exists :error
                                              :root pull-directory)))
    (with-open-file (index index-path :direction :output)
      (archive-feeds pull-time
                     feed-stream-provider
                     index))
    (format t "~&!! pull-directory ~a~%" (uiop:enough-pathname pull-directory *feed-base*))))

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
                     ((or usocket:timeout-error usocket:ns-error cl+ssl:ssl-error-verify)
                      (op (alimenta.pull-feed:skip-feed _)))
                     
                     (error
                      (op
                        (format t "~&Error signaled, ~a (count ~d)" _1 error-count)
                        (incf error-count)
                        (unless (< error-count 15)
                          (format t " continuing~%")
                          (fix-pathname-or-skip _1 :restart 'continue)))))
        (multiple-value-bind (*feeds* *feed-base*) (funcall feed-list-initializer)
          (alimenta.pull-feed:with-user-agent ("Feed Archiver v0.1b")
            (archive-feeds-nondeterm)))))))


(defpackage :alimenta.feed-archive/tests
  (:use :cl :should-test)
  (:export ))
(in-package :alimenta.feed-archive/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'alimenta.feed-archive::feed-index))

(defun hash-table= (ht1 ht2 &key (key-test 'equal) (value-test 'equal))
  (let ((ht1-keys (alexandria:hash-table-keys ht1))
        (ht2-keys (alexandria:hash-table-keys ht2)))
    (and (= (length ht1-keys)
            (length ht2-keys))
         (every key-test ht1-keys ht2-keys)
         (every value-test
                (alexandria:hash-table-values ht1)
                (alexandria:hash-table-values ht2)))))

(deftest feed-index ()
    (should be hash-table=
            (yason:parse
             (with-output-to-string (s)
               (feed-index s (local-time:encode-timestamp 0 0 0 0 1 1 1) '()))
             :object-as :hash-table :json-arrays-as-vectors nil)
            (alexandria:alist-hash-table
             '(("pull-time" . "0001-01-01T00:00:00.000000-08:00")
               ("feeds" . ())))))
