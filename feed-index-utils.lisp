(in-package :alimenta.feed-archive)

(defclass feed-index ()
  ((%pull-time :initarg :pull-time :reader pull-time)
   ;; Why this slot? Won't the references duplicate this?
   (%feed-references :initarg :references :reader references)))

(defclass feed-reference ()
  ((%url :initarg :url :reader url)
   (%title :initarg :title :reader title :initform nil)
   (%path :initarg :path :reader path :initform nil)))

(defun make-feed-index (pull-time references)
  (make-instance 'feed-index
                 :pull-time pull-time
                 :references (copy-seq references)))

(defun make-feed-reference (url &rest feed-data &key title path)
  (declare (ignore title path))
  (apply #'make-instance 'feed-reference
         :url url
         feed-data))

(defun directory-of (pathname)
  (make-pathname :directory (pathname-directory pathname)))

(defmethod yason:encode-slots progn ((object feed-reference))
  (let ((title (title object))
        (path (path object)))
    (yason:encode-object-element "url" (url object))
    (when title
      (yason:encode-object-element "title" title))
    (when path
      (yason:encode-object-element "path" (directory-of path)))))

(defmethod yason:encode-slots progn ((object feed-index))
  (with-accessors ((pull-time pull-time) (references references)) object
    (yason:encode-object-element "pull-time" (local-time:format-timestring nil pull-time))
    (yason:with-object-element ("feeds")
      (yason:with-array ()
        (mapcar 'yason:encode-object references)))))
