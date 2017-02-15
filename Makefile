all: ccl

sbcl:
	sbcl --load deploy.lisp
ccl:
	ccl --load deploy.lisp
