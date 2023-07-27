(use-package :fiveam)

(def-suite* lib-tests)

(def-suite* lib-tests-concatenation
    :in lib-tests)

(test concatenates-on-same-line
      (is (string-equal (concatenate-on-same-line "a" "b")
                        "ab")
          "two strings are concatenated without a delimiter"))
