#!/usr/bin/sbcl --script

(defun concatenate-on-same-line (a b)
  "Concatenates the two supplied strings one after the other."
  (concatenate 'string a b))

(defun concatenate-by-spaces (a b)
  "Concatenates the two supplied strings separated by spaces."
  (concatenate 'string a " " b))

(defun concatenate-on-next-line (a b)
  "Concatenates the two supplied strings on two different lines."
  (concatenate 'string
               a
               "
"
               b))

(defun read-from-file (file-path)
  "Reads a file as string and parses it."
  (with-open-file (file-stream file-path)
    (read-from-string (reduce #'concatenate-on-same-line
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

(defun get-source-by-name (known-sources source-name)
  "Find a source by name from within known sources."
  (find source-name known-sources :key #'car :test #'string-equal))

(defun get-package-by-name (known-packages package-name)
  "Find a package by name from within known packages."
  (find package-name
        known-packages
        :test (lambda (package-name package)
                "Looks for a package with other known names."
                (or (string-equal package-name
                                  (car package))
                    (member package-name (third package) :test #'string-equal)))))

(defun get-suitable-source-for-package (package-ref source-names)
  "Determine suitable source for a package from within relevant sources."
  (find source-names
        (second package-ref)
        :test (lambda (source-names source-info)
                "Looks for sources from within source names."
                (member (string-downcase (symbol-name (car source-info)))
                        source-names
                        :test #'string-equal))))

(defun source-external-p (source)
  "Determines whether a source is external."
  (find :external
        (cdr source)
        :test (lambda (prop property-pair)
                "Looks for a property named ':external'."
                (and (atom property-pair)
                     (eql property-pair prop)))))

(defun command-get-available-source-names (known-sources)
  "Gathers a list of available package sources."
  (labels ((get-command-name-for-source (known-source)
             (cadar (remove-if-not (lambda (property-pair)
                                     (and (listp property-pair)
                                          (eql (car property-pair) :COMMAND)
                                          (cadr property-pair)))
                                   (cdr known-source))))
           (generate-if-ladder-for-sources (sources-and-commands)
             (reduce #'concatenate-on-same-line
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
  (labels ((get-list-command-for-source (source)
             (cadar (remove-if-not (lambda (property-pair)
                                     (and (listp property-pair)
                                          (eql (car property-pair) :LIST)))
                                   (cdr source)))))
    (let* ((relevant-source-names (split-string sources-query ",")))
      (princ (reduce #'concatenate-on-next-line
                     (mapcar (lambda (source-name)
                               (get-list-command-for-source (get-source-by-name known-sources
                                                                                source-name)))
                             relevant-source-names)
                     :initial-value "")))))

(defun command-install-packages (known-sources known-packages sources-query package-names)
  "Installs the specified packages from among the supplied sources, using the known sources and known packages."
  (labels ((get-install-command-for-source (source)
             (cadar (remove-if-not (lambda (property-pair)
                                     (and (listp property-pair)
                                          (eql (car property-pair) :INSTALL)))
                                   (cdr source))))
           (get-install-command-for-package (package-name source-names)
             (let* ((package-ref (get-package-by-name known-packages
                                                      package-name))
                    (source-ref (get-suitable-source-for-package package-ref source-names))
                    (source-name (if source-ref
                                     (string-downcase (symbol-name (car source-ref)))
                                     (car source-names)))
                    (source (get-source-by-name known-sources
                                                source-name)))
               (if (source-external-p source)
                   (concatenate 'string
                                "echo \"Skipping installation of "
                                package-name
                                " using "
                                source-name
                                "\"!")
                   (concatenate 'string
                                (get-install-command-for-source source)
                                (reduce #'concatenate-by-spaces
                                        (or (cdr source-ref)
                                            (if package-ref
                                                `(,(car package-ref))
                                                `(,package-name)))
                                        :initial-value ""))))))
    (let* ((relevant-source-names (split-string sources-query ",")))
      (princ (reduce #'concatenate-on-next-line
                     (mapcar (lambda (package-name)
                               (get-install-command-for-package package-name
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
