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

