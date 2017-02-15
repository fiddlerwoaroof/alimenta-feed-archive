(defmethod yason:encode ((object pathname) &optional stream)
  (yason:encode (princ-to-string (uiop:native-namestring object))
		       stream)
  object)

(defmethod yason:encode ((object puri:uri) &optional stream)
  (yason:encode (puri:render-uri object nil)
		stream)
  object)

(defmethod yason:encode-slots progn ((feed alimenta:feed))
  (with-accessors ((description alimenta:description)
		   (feed-link alimenta:feed-link)
		   (items alimenta:items)
		   (link alimenta:link)
		   (source-type alimenta:source-type)
		   (title alimenta:title)) feed
    (yason:encode-object-element "title" title)
    (yason:encode-object-element "fetch-url"
				 (puri:render-uri feed-link nil))
    (yason:encode-object-element "link" link)
    ;;(yason:encode-object-element "source-type" source-type)
    (yason:encode-object-element "description" description))
  feed)


(defmethod yason:encode ((feed alimenta:feed) &optional stream)
  (yason:with-output (stream :indent t)
    (yason:encode-object feed)))

(defmethod yason:encode-slots progn ((item alimenta:item))
  (with-accessors ((author alimenta::author)
		   (content alimenta:content)
		   (date alimenta:date)
		   (id alimenta:id)
		   (link alimenta:link)
		   (title alimenta:title)) item
    (let* ((date (local-time:format-timestring nil date)))
      (yason:with-object ()
	(yason:encode-object-element "title" title)
	(yason:encode-object-element "date" date)
	(yason:encode-object-element "author" title)
	(yason:encode-object-element "id" (princ-to-string id))
	(yason:encode-object-element "link" link)
	(yason:encode-object-element "content" content)))))

(defmethod yason:encode ((item alimenta:item) &optional stream)
  (yason:with-output (stream :indent t)
    (yason:encode-slots item))
  item)

