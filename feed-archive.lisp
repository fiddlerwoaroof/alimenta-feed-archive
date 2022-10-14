(in-package :alimenta.feed-archive)

(defvar *feeds*)
(defvar *feed-base*)

(defparameter +dirname-format+
  '((:year 4) #\- (:month 2) #\- (:day 2) #\/ (:hour 2) #\- (:min 2) #\/))


(defmacro lambda* ((&rest args) &body body)
  (let ((rest-arg (gensym "REST")))
    `(lambda (,@args &rest ,rest-arg)
       (declare (ignore ,rest-arg))
       ,@body)))

(defun safe-pull-feed (feed-url &aux (pop-times 0))
  "Handles date parsing errors in the feed: chronicity won't parse
   certain date formats, this catches the error and modifies the
   format to something chronicity can handle."
  (handler-bind ((warning #'muffle-warning)
                 (error (lambda* (c)
                          (when (find-restart 'alimenta:pop-token c)
                            (cond
                              ((< pop-times 50)
                               (incf pop-times)
                               (format t
                                       "~&Processing error, trying to pop a token (popped ~d times)~%"
                                       pop-times)
                               (alimenta:pop-token))
                              (t
                               (continue)))))))
    (prog1-bind (feed (alimenta.pull-feed:pull-feed feed-url))
      ;; Why am I decf-ing here?
      (alimenta:transform feed
                          (lambda (entity)
                            (typecase entity
                              (alimenta:item
                              (let ((v (alimenta:content entity)))
                                (when v
                                  (setf (alimenta:content entity)
                                         (html-sanitizer:sanitize v))))))))
      (decf pop-times))))

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


(defvar *error-client* nil)
(defgeneric record-error (client url)
  (:method ((client null) feed-url)))

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
      (restart-case
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
        (skip-feed ()
          :report (lambda (s)
                    (format s "Stop processing for ~a" feed-url))
          (record-error *error-client* feed-url))))))

(defun archive-feeds (pull-time pull-directory index-stream)
  (prog1-bind (references (mapcar (op (pull-and-store-feed _ pull-directory))
                                  *feeds*))
    (feed-index index-stream pull-time references)))

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
