test:
	rlwrap sbcl --script ./run-tests.lisp

develop:
	while true; do \
		make --silent test; \
		inotifywait -qre close_write .; \
	done
