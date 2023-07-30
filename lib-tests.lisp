(use-package :fiveam)

(def-suite* lib-tests)

(def-suite* lib-tests-string
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

(test splits-strings
      (is (equal (split-string "a,b")
                 '("a" "b"))
          "splits string around comma, as default")
      (is (equal (split-string "a:b" ":")
                 '("a" "b"))
          "splits string around a specific separator"))

(def-suite* lib-tests-files
    :in lib-tests)

(test reads-files
      (labels ((write-to-file (file-path data)
                 (with-open-file (file-stream file-path
                                              :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
                   (write-sequence (write-to-string data)
                                   file-stream))))
        (write-to-file "/tmp/crater-db-reader-test-file"
                       '(1 2 3))
        (is (equal (read-from-file "/tmp/crater-db-reader-test-file")
                   '(1 2 3))
            "reads content from file")))
