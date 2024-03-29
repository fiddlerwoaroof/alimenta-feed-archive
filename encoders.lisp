(in-package :alimenta.feed-archive.encoders)

(defun fix-path (path)
  ;; Work around some issue with pathnames
  (setf path (merge-pathnames path (make-pathname :type :unspecific))))

(defun skip-item ()
  (when-let ((restart (find-restart 'skip-item)))
    (invoke-restart restart)))

(define-condition feed-error (error)
  ((%feed :reader the-feed :initarg :feed :initform (error "We need a feed"))
   (%condition :reader the-condition :initarg :condition :initform (error "feed-error must wrap a condition"))))

(defun wrap-condition (condition feed)
  (error 'feed-error
         :feed feed
         :condition condition))

(defmacro unwrap-feed-errors (() &body body)
  `(handler-bind ((feed-error (op (error (the-condition _)))))
     ,@body))

(defun %encode-item (root-dir item)
  (destructuring-bind (title path) item
    (format t "~&Encoding ~a~%" title)
    (restart-once (fix-pathname () (fix-path path))
      (let ((pathname (uiop:enough-pathname path root-dir)))
        (yason:with-object ()
          (yason:encode-object-element "title" title)
          (yason:encode-object-element "path" pathname))))))

(defun %encode-feed-as-json (feed item-storage-info root-dir &optional stream)
  (with-accessors ((description alimenta:description)
                   (feed-link alimenta:feed-link)
                   (items alimenta:items)
                   (link alimenta:link)
                   (source-type alimenta:source-type)
                   (title alimenta:title)) feed
    (yason:with-output (stream :indent t)
      (yason:with-object ()
        (yason:encode-object-element "metadata" feed)
        (yason:with-object-element ("items")
          (yason:with-array ()
            (dolist (item item-storage-info)
              (with-simple-restart (skip-item "Skip item ~s" (car item))
                ;; (format t "~&I Store Info: ~a~%~4t~a~%" (uiop:unix-namestring (cadr item)) root-dir)
                (%encode-item root-dir item)
                #+null
                (yason:encode-array-element (uiop:unix-namestring (uiop:enough-pathname root-dir (cadr item))))
                ))))))))

(defmethod store ((items sequence) storage)
  (when (next-method-p)
    (format t "calling next...~%")
    (call-next-method))
  (map 'list (op (store _ storage))
       (stable-sort (sort items #'string-lessp
                          :key #'alimenta:title)
                    #'local-time:timestamp>
                    :key #'alimenta:date)))

(defmethod store ((feed alimenta:feed) (directory pathname))
  (flet ((get-feed-store-name (feed directory)
           (merge-pathnames (get-id feed)
                            directory)))

    (with-accessors ((description alimenta:description)
                     (feed-link alimenta:feed-link)
                     (items alimenta:items)
                     (link alimenta:link)
                     (source-type alimenta:source-type)
                     (title alimenta:title)) feed
                                        ; We wrap all errors with our own condition
      (handler-bind ((error (lambda (c) (error 'feed-error :feed feed :condition c))))
        (values (multiple-value-list
                 (prog1-let ((feed-title title)
                             (feed-store (get-feed-store-name feed directory)))
                   (ensure-directories-exist feed-store)
                   (with-open-file (index (merge-pathnames "index.json" feed-store) :direction :output)
                     (%encode-feed-as-json feed
                                           (store (copy-seq items) feed-store)
                                           feed-store
                                           index))))
                feed-link)))))

(defmethod store ((feed alimenta:feed) (stream stream))
  (handler-bind ((error (lambda (c)
                          (typecase c
                            (feed-error c)
                            (t (wrap-condition c feed))))))
    (yason:with-output (stream :indent t)
      (yason:with-object ()
        (yason:with-object-element ("metadata")
          (yason:encode-object feed))
        (yason:with-object-element ("items")
          (yason:with-array ()
            (for:for ((item over feed))
              (store item stream))))))
    (list (alimenta:title feed)
          stream)))

(defmethod store ((item alimenta:item) (directory pathname))
  (flet ((get-item-store-name (item directory)
           (let ((id (get-id item)))
             (merge-pathnames (make-pathname :name id :version nil :type "json") directory))))

    (multiple-value-list
     (prog1-let ((item-title (alimenta:title item))
                 (fn (get-item-store-name item directory)))
       (with-open-file (item-f fn :direction :output)
         (yason:encode item item-f))))))

(defmethod store ((item alimenta:item) (stream stream))
  (yason:with-output (stream :indent t)
    (yason:with-object ()
      (yason:encode-slots item)))
  (list (alimenta:title item)
        stream))

;; The feed is always index.json
(defmethod stream-provider:stream-key (provider (feed alimenta:feed))
  (pathname
   (string-join
    (list (get-id feed)
          "index.json")
    "/")))

(defmethod stream-provider:stream-key :around ((provider stream-provider:file-provider)
                                               (feed alimenta:feed))
  (prog1-bind (result (call-next-method))
    (ensure-directories-exist (merge-pathnames result
                                               (stream-provider:root provider)))))

(defmethod stream-provider:stream-key (provider (item alimenta:item))
  (let ((id (get-id item)))
    (make-pathname :name id :version nil :type "json")))

(defclass feed-stream-item-provider ()
  ((%item-providers :accessor item-providers :initform (make-hash-table :test 'equal))))

(defclass feed-stream-provider (stream-provider:file-provider feed-stream-item-provider)
  ())

(defclass feed-stream-string-provider (stream-provider:string-provider feed-stream-item-provider)
  ())

(defmethod stream-provider:get-nested-provider ((provider stream-provider:stream-provider) (streamable alimenta:feed))
  (with (items-root (uiop:merge-pathnames* (uiop:pathname-directory-pathname (stream-provider:stream-key provider streamable))
                                           (stream-provider:root provider)))
    (ensure-gethash items-root
                    (item-providers provider)
                    (make-instance 'stream-provider:file-provider :root items-root))))

(defmethod store :around ((item alimenta:feed-entity) (stream-provider stream-provider:stream-provider))
  (call-next-method)
  (list (alimenta:title item)
        (stream-provider:stream-key stream-provider item)))

(defmethod store ((item alimenta:item) (stream-provider stream-provider:stream-provider))
  (stream-provider:with-storage-stream (s item stream-provider)
    (store item s)))

(defmethod store :around ((item alimenta:item) (dest stream-provider:stream-provider))
  (with-simple-restart (skip-item "Skip item ~s" (car item))
    (call-next-method)))

(defun map-coalesce (fun &rest seqs)
  (apply #'mappend
         (compose #'unsplice
                  fun)
         seqs))

(defmethod store ((feed alimenta:feed) (stream-provider stream-provider:stream-provider))
  (stream-provider:with-storage-stream (s feed stream-provider)
    (with-accessors ((description alimenta:description)
                     (feed-link alimenta:feed-link)
                     (items alimenta:items)
                     (link alimenta:link)
                     (source-type alimenta:source-type)
                     (title alimenta:title)) feed
      (let* ((item-provider (stream-provider:get-nested-provider stream-provider feed))
             (item-storage-info (map-coalesce (op (store _ item-provider))
                                              items)))
        (let ((yason::*json-output*
                (make-instance 'yason::json-output-stream
                               :output-stream s
                               :indent t)))
          (with-collection (item "items" item-storage-info "metadata" feed)
            (destructuring-bind (title path) item
              (yason:with-object ()
                (yason:encode-object-elements "title" title "path" path)))))))))
