(defpackage :alimenta.feed-archive.tools
  (:use :cl :alexandria :serapeum :fw.lu)
  (:shadow :->)
  (:export :fix-pathname :sha256-string :get-id :older-than-a-week :-> :get-feed-store-name
	   :store :get-item-store-name))

(in-package :alimenta.feed-archive.tools)

(defgeneric store (item directory)
  (:documentation "Store an item in a directory"))

(defmacro -> (&rest forms)
  (let ((forms (mapcar (lambda (form)
			 (typecase form
			   (list form)
			   (t (list form))))
		       forms)))
    (loop with result = (car forms)
       for form in (cdr forms)
       do
	 (setf result `(,(car form)
			 ,result
			 ,@(cdr form)))
       finally
	 (return result))))

(defun fix-pathname ()
  (let ((restart (find-restart 'fix-pathname)))
    (when restart
      (invoke-restart restart))))

(defun sha256-string (string)
  (let* ((digester (ironclad:make-digesting-stream :sha256))
	 (digest-stream (flexi-streams:make-flexi-stream digester)))
    (princ string digest-stream)
    (crypto:byte-array-to-hex-string
     (crypto:produce-digest
      digester))))

(defgeneric get-id (feed)
  (:documentation "Get an identifier for a feed"))

(defmethod get-id ((feed alimenta:feed))
  (let* ((link (alimenta:feed-link feed))
	 (host (puri:uri-host link)))
    (concat host "-" (sha256-string link) "/")))

(defmethod get-id ((item alimenta:item))
  (concatenate 'string
	       (local-time:format-timestring nil (alimenta:date item))
	       "-"
	       (sha256-string (alimenta:id item))
	       ".json"))

(defun get-item-store-name (item directory)
  (let ((id (get-id item)))
    (merge-pathnames (make-pathname :name id) directory)))

(defun get-feed-store-name (feed directory)
  (merge-pathnames (get-id feed)
                   directory))

(defun older-than-a-week (date)
  (let ((week-ago (local-time:timestamp- (local-time:now)
                                         7 :day)))
    (local-time:timestamp< date week-ago)))

