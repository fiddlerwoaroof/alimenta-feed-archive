(in-package :alimenta.feed-archive.encoders)

(defmethod yason:encode-slots progn ((feed alimenta:feed))
  (prog1 feed
    (patmatch:let-pat* (((alimenta:feed :title title
                                        :feed-link feed-link
                                        :link link
                                        :description description)
                         feed))
      (yason:encode-object-element "title" title)
      (yason:encode-object-element "fetch-url" (puri:render-uri feed-link nil))
      (yason:encode-object-element "link" link)
      ;;(yason:encode-object-element "source-type" source-type)
      (yason:encode-object-element "description" description))))


(defmethod yason:encode-slots progn ((item alimenta:item))
  (patmatch:let-pat* (((alimenta:item :author author
                                      :content content
                                      :date date
                                      :id id
                                      :link link
                                      :title title)
                       item))
    (let* ((date (local-time:format-timestring nil date)))
      (yason:encode-object-element "title" title)
      (yason:encode-object-element "date" date)
      (yason:encode-object-element "author" author)
      (yason:encode-object-element "id" (princ-to-string id))
      (yason:encode-object-element "link" link)
      (yason:encode-object-element "content" content))))

(defmethod yason:encode ((object pathname) &optional stream)
  (prog1 object
    (yason:encode (princ-to-string (uiop:native-namestring object))
                  stream)))

(defmethod yason:encode ((object puri:uri) &optional stream)
  (prog1 object
    (yason:encode (puri:render-uri object nil)
                  stream)))

(defmethod yason:encode ((feed-entity alimenta:feed-entity) &optional stream)
  (prog1 feed-entity
    (yason:with-output (stream :indent t)
      (yason:encode-object feed-entity))))

(defun encode-collection-object (other-pairs collection-key collection-value element-encoder)
  (yason:with-object ()
    (loop for (key value) on other-pairs by #'cddr
       do
         (yason:encode-object-element key value))
    (yason:with-object-element (collection-key)
      (yason:with-array ()
        (dolist (item collection-value)
          (funcall element-encoder item))))))

(defmacro with-collection ((item-sym key collection &rest other-pairs) &body encoder)
  (once-only (key collection)
    `(encode-collection-object (list ,@other-pairs)
                               ,key ,collection
                               (lambda (,item-sym)
                                 ,@encoder))))
