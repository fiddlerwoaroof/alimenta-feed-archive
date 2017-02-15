(load (truename "~/quicklisp/setup.lisp"))
(push "/home/edwlan/github_repos/alimenta-feed-archive/" asdf:*central-registry*)
(ql:quickload :alimenta-feed-archive)
(#+ccl ccl:save-application
 #+sbcl save-lisp-and-die
 "feed-archiver"
 #+sbcl :executable  #+ccl :prepend-kernel t
 #+sbcl :toplevel #+ccl :toplevel-function #'alimenta.feed-archive::command-line-main)
