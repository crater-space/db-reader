(use-package :fiveam)

(def-suite* lib-tests)

(def-suite* lib-tests-concatenation
    :in lib-tests)

(test concatenates-on-same-line
      (is (string-equal (concatenate-on-same-line "a" "b")
                        "ab")
          "two strings are concatenated without a delimiter")
      (is (string-equal (concatenate-on-same-line "a" nil)
                        "a")
          "the only argument is returned"))

(test concatenates-by-spaces
      (is (string-equal (concatenate-by-spaces "a" "b")
                        "a b")
          "two strings are concatenated with a space as a delimiter")
      (is (string-equal (concatenate-by-spaces "a" nil)
                        "a ")
          "the only argument is returned, followed by a space"))
