#!/usr/bin/sbcl --script

(defun read-from-file (file-path)
  "Reads a file as string and parses it."
  (with-open-file (file-stream file-path)
    (read-from-string (reduce (lambda (a b)
                                (concatenate 'string a b))
                              (loop for i from 0
                                    for line = (read-line file-stream nil nil)
                                    while line
                                    collect line)))))

;; Courtesy: https://gist.github.com/siguremon/1174988/babcbdcbbfcb9f42df34f000f9326a26caa64be4
(defun split-string (string &optional (separator ",") (r nil))
  "Splits a string around the (optionally) specified operator."
  (let ((n (position separator string
		             :from-end t
		             :test #'(lambda (x y)
			                   (find y x :test #'string=)))))
    (if n
	    (split-string (subseq string 0 n)
                 separator
                 (cons (subseq string (1+ n)) r))
        (cons string r))))

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

(defun command-list-packages (known-sources sources-query)
  "Lists installed packages from the supplied sources, using the known sources."
  (labels ((get-source-by-name (source-name)
             (find source-name known-sources :key #'car :test #'string-equal))
           (get-list-command-for-source (source)
             (cadar (remove-if-not (lambda (property-pair)
                                     (and (listp property-pair)
                                          (eql (car property-pair) :LIST)))
                                   (cdr source)))))
    (let* ((relevant-source-names (split-string sources-query ",")))
      (princ (reduce (lambda (a b)
                       (concatenate 'string
                                    a
                                    "
"
                                    b))
                     (mapcar (lambda (source-name)
                               (get-list-command-for-source (get-source-by-name source-name)))
                             relevant-source-names)
                     :initial-value "")))))

(defun command-install-packages (known-sources known-packages sources-query package-names)
  "Installs the specified packages from among the supplied sources, using the known sources and known packages."
  (labels ((get-package-by-name (package-name)
             (find package-name known-packages :key #'car :test #'string-equal))
           (get-suitable-source-for-package (package-ref source-names)
             (car (remove-if-not (lambda (source-info)
                                   (member (string-downcase (symbol-name (car source-info)))
                                           source-names
                                           :test #'string-equal))
                                 (cadr package-ref))))
           (get-source-by-name (source-name)
             (find source-name known-sources :key #'car :test #'string-equal))
           (get-install-command-for-source (source)
             (cadar (remove-if-not (lambda (property-pair)
                                     (and (listp property-pair)
                                          (eql (car property-pair) :INSTALL)))
                                   (cdr source))))
           (get-install-command-for-package (package-ref source-names)
             (let* ((source-ref (get-suitable-source-for-package package-ref source-names))
                    (install-command (get-install-command-for-source
                                      (get-source-by-name (string-downcase (symbol-name (car source-ref)))))))
               (concatenate 'string
                            install-command
                            (reduce (lambda (a b)
                                      (concatenate 'string a " " b))
                                    (cdr source-ref)
                                    :initial-value ""))
               )))
    (let* ((relevant-source-names (split-string sources-query ",")))
      (princ (reduce (lambda (a b)
                       (concatenate 'string
                                    a
                                    "
"
                                    b))
                     (mapcar (lambda (package-name)
                               (get-install-command-for-package (get-package-by-name package-name)
                                                                relevant-source-names))
                             package-names)
                     :initial-value "")))))

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
         (known-sources (read-from-file "../db/sources.lisp"))
         (known-packages (read-from-file "../db/packages.lisp")))
    (cond ((string-equal command-name "get_sources") (command-get-available-source-names known-sources))
          ((string-equal command-name "search") (princ "Not implemented!"))
          ((string-equal command-name "list") (command-list-packages known-sources (cadr arguments)))
          ((string-equal command-name "install") (command-install-packages known-sources
                                                                           known-packages
                                                                           (cadr arguments)
                                                                           (cddr arguments)))
          ((string-equal command-name "uninstall") (princ "Not implemented!"))
          ((string-equal command-name "update") (princ "Not implemented!"))
          (t (princ "Command not specified!")))))

(main)
