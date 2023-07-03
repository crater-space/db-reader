#!/usr/bin/sbcl --script

(defun file-to-string (file-path)
  "Reads and returns the contents of a text file as a string."
  (with-open-file (file-stream file-path)
    (read-from-string (reduce (lambda (a b)
                                (concatenate 'string a b))
                              (loop for i from 0
                                    for line = (read-line file-stream nil nil)
                                    while line
                                    collect line)))))

(defun command-get-available-source-names (known-sources)
  "Gathers a list of available package sources."
  (labels ((get-command-name-for-source (known-source)
             (cadar (remove-if-not (lambda (property-pair)
                                     (and (listp property-pair)
                                          (eql (car property-pair) :COMMAND)
                                          (cadr property-pair)))
                                   (cdr known-source))))
           (generate-if-ladder-for-sources (sources-and-commands)
             (reduce (lambda (a b)
                       (concatenate 'string a b))
                     (mapcar (lambda (pair)
                               (concatenate 'string
                                            "if [ $(command -v " (second pair) ") ]; then "
                                            "available_sources+=(" (first pair) "); "
                                            "fi
"))
                             sources-and-commands)
                     :initial-value "")))
    (let* ((sources-and-commands (mapcar (lambda (known-source)
                                           `(,(car known-source)
                                              ,(or (get-command-name-for-source known-source)
                                                   (car known-source))))
                                         known-sources)))
      (princ (concatenate 'string
                          "available_sources=();
"
                          (generate-if-ladder-for-sources sources-and-commands)
                          "result=$(IFS=,; echo \"${available_sources[*]}\")
"
                          "echo $result")))))

(defun command-search-packages (term)
  "Searches for packages matching the supplied search-term."
  ;; TODO: Implement command
  )

(defun command-list-packages ()
  "Lists installed packages."
  ;; TODO: Implement command
  )

(defun command-install-packages (packages)
  "Installs the specified packages."
  ;; TODO: Implement command
  )

(defun command-uninstall-packages (packages)
  "Uninstalls the specified packages."
  ;; TODO: Implement command
  )

(defun command-update-packages (packages)
  "Updates the specified (or all) packages."
  ;; TODO: Implement command
  )

(defun main ()
  "The entry-point to the script."
  (let* ((arguments (cdr *posix-argv*))
         (command-name (car arguments))
         (known-sources (file-to-string "../db/sources.lisp")))
    (cond ((string-equal command-name "get_sources") (command-get-available-source-names known-sources))
          ((string-equal command-name "search") (princ "Not implemented!"))
          ((string-equal command-name "list") (princ "Not implemented!"))
          ((string-equal command-name "install") (princ "Not implemented!"))
          ((string-equal command-name "uninstall") (princ "Not implemented!"))
          ((string-equal command-name "update") (princ "Not implemented!"))
          (t (princ "Command not specified!")))))

(main)
