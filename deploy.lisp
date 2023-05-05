(handler-case (logical-pathname-translations "SYS")
  (error ()
    (setf (logical-pathname-translations "SYS") nil)))

(pushnew
 `(#p"SYS:SITE;**;*.*.*"
     ,(merge-pathnames
       (make-pathname :directory (list
                                  :relative ".sbcl" "site"
                                  :wild-inferiors)
                      :name :wild
                      :type :wild)
       (user-homedir-pathname)))
 (logical-pathname-translations "SYS")
 :test #'equal
 :key (lambda (it) (namestring (car it))))

(mapcar (lambda (it)
          (with-open-file (s it :direction :input
                                :element-type 'character)
            (setf (logical-pathname-translations (string-upcase
                                                  (pathname-name it)))
                  (read s))))
        (directory #p"SYS:SITE;*.translations"))

(load (truename #p"QL:setup.lisp"))

(asdf:load-asd #p"PROJECTS:COLLECTION-CLASSES;COLLECTION-CLASS.ASD")
(asdf:load-asd #p"PROJECTS:ALIMENTA;ALIMENTA.ASD")

(push (truename #p"PROJECTS:alimenta-feed-archive;")
      asdf:*central-registry*)

(ql:quickload :alimenta-feed-archive)

(load #p"PROJECTS:alimenta-feed-archive;main.lisp")

#+(or)
(setf sb-alien::*shared-objects* nil)

#+sbcl
(defun do-sbcl ()
  (save-lisp-and-die "feed-archiver"
                     :executable t
                     :toplevel #'alimenta.feed-archive:command-line-main))

#+ccl
(defun do-ccl ()
  (ccl:save-application
   "feed-archiver"
   :prepend-kernel t
   :toplevel-function #'alimenta.feed-archive:command-line-main))

(#+sbcl do-sbcl
 #+ccl do-ccl)
