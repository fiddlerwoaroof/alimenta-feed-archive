all: sbcl

debug:
	sbcl --load deploy.lisp
sbcl:
	sbcl --no-userinit --disable-debugger --load deploy.lisp
ccl:
	ccl --load deploy.lisp
