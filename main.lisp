#!/usr/bin/sbcl --script

(load "./lib.lisp")

(main (cdr *posix-argv*)
      (read-from-file "../db/sources.lisp")
      (read-from-file "../db/packages.lisp"))
