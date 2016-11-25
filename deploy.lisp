(load (truename "~/quicklisp/setup.lisp"))
(push "/home/edwlan/github_repos/alimenta-feed-archive/" asdf:*central-registry*)
(ql:quickload :alimenta-feed-archive)
(save-lisp-and-die "feed-archiver" :executable t :toplevel #'alimenta.feed-archive::command-line-main)
