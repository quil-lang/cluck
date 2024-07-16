
.PHONY:
test:
	sbcl --non-interactive --disable-debugger --load 'scripts/run-tests.lisp'
