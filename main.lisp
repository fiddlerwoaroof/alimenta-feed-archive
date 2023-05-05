(in-package :alimenta.feed-archive)

(defun directory-for-timestamp (timestamp)
  (let* ((rounded (local-time:timestamp-minimize-part timestamp :sec))
         (formatted (local-time:format-timestring nil rounded
                                                  :format +dirname-format+)))
    (values (parse-namestring formatted))))

(defun ensure-store-directory (timestamp base)
  (prog1-let ((result (merge-pathnames (directory-for-timestamp timestamp)
                                       base)))
    (ensure-directories-exist result)))

(defun test-feed-list ()
  (values '("http://feeds.feedburner.com/GamasutraFeatureArticles/"
            "http://feeds.feedburner.com/undergroundthomist/yCSy"
            "https://www.codinghorror.com/blog/index.xml")
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
(defun append-feed (feed)
  (init-feeds)
  (setf (ubiquitous:value :feeds)
        (append (remove feed (ubiquitous:value :feeds)
                        :test 'equalp)
                (list feed))))

(defun archive-feeds-nondeterm ()
  (let* ((pull-time (local-time:now))
         (pull-directory (ensure-store-directory pull-time *feed-base*))
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
  (labels ((feed-type-unsupported (c)
             (format t "~&Feed type unsupported: ~a for feed ~a~%"
                     (alimenta:feed-type c)
                     (alimenta:feed-link c))
             (skip-feed c))
           (fix-pathname-or-skip (c &key (restart 'skip-feed) (wrapped-condition nil wc-p))
             (typecase (or wrapped-condition c)
               (alimenta:feed-type-unsupported (feed-type-unsupported c))
               (t
                (if (find-restart 'fix-pathname c)
                    (fix-pathname)
                    (if (find-restart 'alimenta.pull-feed:skip-feed c)
                        (alimenta.pull-feed:skip-feed c)
                        (progn
                          (unless (eq restart 'continue)
                            (format t "~&Skipping a feed... ~s~%"
                                    (if wc-p
                                        (alimenta.feed-archive.encoders:the-feed c)
                                        "Unknown")))
                          (funcall restart))))))))

    (let ((error-count 0))
      (handler-bind
          ((alimenta.feed-archive.encoders:feed-error
             (op
               (format t "~&~2t~%Feed Error?~%")
               (fix-pathname-or-skip _1 :wrapped-condition
                                     (alimenta.feed-archive.encoders:the-condition _1))))
           (alimenta:feed-type-unsupported #'feed-type-unsupported)
           ((or usocket:timeout-error
                usocket:ns-error
                usocket:socket-error
                cl+ssl:ssl-error-verify
                storage-condition
                type-error
                plump-dom:invalid-xml-character)
             (op
               (format t "~&Error pulling feed, skipping: ~s~%" _1)
               (skip-feed _1)))

           (error
             (op
               (format t "~&Error signaled, ~s (count ~d)"
                       _1 error-count)
               (incf error-count)
               (when (< error-count 15)
                 (format t " continuing~%")
                 (skip-feed _1)))))
        (multiple-value-bind (*feeds* *feed-base*)
            (funcall feed-list-initializer)
          (alimenta.pull-feed:with-user-agent ("Feed Archiver v0.1b")
            (archive-feeds-nondeterm)))))))
