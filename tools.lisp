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
	       (sha256-string (alimenta:id item))))

(defun older-than-a-month (date)
  (let ((month-ago (local-time:timestamp- (local-time:now)
					  31 :day)))
    (local-time:timestamp< date month-ago)))

(defun older-than-a-week (date)
  (let ((week-ago (local-time:timestamp- (local-time:now)
                                         7 :day)))
    (local-time:timestamp< date week-ago)))

(defmacro restart-once ((restart-name (&rest restart-args) &body handler) &body body)
  "Defines a restart that, the first time it's executed, runs a chunk of code and then,
next time, it re-raises the exception."
  (with-gensyms (start restarted)
    `(let ((,restarted nil))
       (tagbody ,start
	  (restart-case
	      (progn ,@body)
	    (,restart-name ,restart-args
	      ,@handler
	      (unless ,restarted
		(setf ,restarted t)
		(go ,start))))))))


(defun coerce-feed-link (link feed)
  (prog1 feed
    (unless (alimenta:feed-link feed)
      (setf (alimenta:feed-link feed) link))))

(defmacro with-retry ((&optional (message "retry the operation")) &body body)
  `(loop
      (restart-case (return (progn ,@body))
	(retry ()
	  :report (lambda (s)
		    (format s "~@<~a~@:>" ,message))))))
