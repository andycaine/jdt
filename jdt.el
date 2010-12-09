;; Global config variables
(defvar jdt-jdk-location "/usr/lib/jvm/java-6-openjdk/")

;; Set case sensitive regexes
(setq case-fold-search nil)

;; Utilities
(defun time-funcall (fn &rest args)
  (let ((start (time-to-seconds (current-time))))
    (apply fn args)
    (- (time-to-seconds (current-time))
       start)))

(defun longer (x y)
  (labels ((compare (x y)
                    (and (consp x)
                         (or (null y)
                             (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
      (> (length x) (length y)))))

(defun string-starts-with (prefix str)
  (if (longer prefix str)
      nil
    (string-match-p (format "^%s.*" prefix) str)))

(defun expand-file-names (paths base-dir)
  (mapcar (lambda (x) (expand-file-name x basedir)) paths))

(defun join (sep seq)
  (mapconcat 'identity seq sep))

(defun directory-files-recursive (dir &optional full match)
  (let ((matched '()))
    (dolist (file (directory-files dir t) matched)
      (unless (string-match-p "/\\.\\.?$" file)
        (if (file-directory-p file)
            (setq matched (append matched (directory-files-recursive file t match)))
          (when (or (not match)
                    (string-match-p match (file-relative-name file dir)))
            (push file matched)))))
    (if full
        matched
      (mapcar (lambda (f) (file-relative-name f dir))
              matched))))

(defun jdt-get-list-using-buffer-cache (lst-fn cache-buffer-name)
  (with-current-buffer (get-buffer-create cache-buffer-name)
    (if (= (buffer-size) 0)
        (let ((lst (funcall lst-fn)))
          (print lst (current-buffer))
          lst)
      (goto-char (point-min))
      (read (current-buffer)))))

;; Projects
(defvar jdt-projects '())

(defun jdt-list-jars (dir)
  "Returns a list of jar files in the given directory."
  (directory-files dir t "\\.jar$"))

(defun jdt-prj-register (name basedir &optional src-dirs class-dirs lib-dirs)
  (add-to-list 'jdt-projects (jdt-make-prj name basedir src-dirs class-dirs lib-dirs)))

(defun jdt-make-prj (name basedir &optional src-dirs class-dirs lib-dirs)
  (let ((class-dirs (expand-file-names class-dirs basedir))
        (jars (when lib-dirs
                (mapcan 'jdt-list-jars
                        (expand-file-names lib-dirs basedir)))))
    (list (cons 'name name)
          (cons 'basedir (expand-file-name basedir))
          (cons 'src-dirs
                (expand-file-names src-dirs basedir))
          (cons 'class-dirs class-dirs)
          (cons 'jars (cons (concat jdt-jdk-location "jre/lib/rt.jar") jars)))))

(defun jdt-prj-property (prj prop)
  (cdr (assoc prop prj)))

(defun jdt-prj-name (prj)
  (jdt-prj-property prj 'name))

(defun jdt-prj-basedir (prj)
  (jdt-prj-property prj 'basedir))

(defun jdt-prj-classpath-entries (prj)
  (append (jdt-prj-property prj 'jars)
          (jdt-prj-property prj 'class-dirs)))

(defun jdt-prj-classes-on-path (prj)
  "Returns a list of all the classes available on this projects path."
  (jdt-get-list-using-buffer-cache (lambda () (mapcan 'jdt-class-repos-get-classes
                                                      (jdt-prj-classpath-entries prj)))
                                   (format "*%s classes*" (jdt-prj-name prj))))

(defun jdt-prj-classpath (prj)
  (join ":" (jdt-prj-classpath-entries prj)))

(defun jdt-prj-owning (file-name projects)
  "Returns the project owning file-name"
  (let ((prjs (remove-if-not (lambda (p)
                               (string-starts-with (jdt-prj-basedir p) file-name))
                             projects)))
    (when (not (= 1 (length prjs)))
      (error (format "Unable to find unique project owning file name: %s"
                     file-name)))
    (car prjs)))

(defun jdt-prj-owning-current-buffer ()
  (jdt-prj-owning (buffer-file-name) jdt-projects))

(defun jdt-compile-project ()
  (interactive)
  (let ((cur-dir default-directory))
    (cd-absolute (jdt-prj-basedir (jdt-prj-owning-current-buffer)))
    (cond ((file-exists-p "build.xml")
           (compile "ant compile"))
          ((file-exists-p "pom.xml")
           (compile "mvn compile")))
    (cd-absolute cur-dir)))

(defun jdt-run-mvn-target ()
  (interactive)
  (jdt-funcall-in-prj-base
   (lambda ()
    (compile (concat "mvn " (completing-read "Target: "
                                             '("compile" "install" "test" "clean")))))))
     
(defun jdt-funcall-in-prj-base (fn)
  (let ((cur-dir default-directory))
    (cd-absolute (jdt-prj-basedir (jdt-prj-owning-current-buffer)))
    (funcall fn)
    (cd-absolute cur-dir)))

(defun jdt-compile-current-buffer ()
  (interactive)
  (jdt-funcall-in-prj-base
   (lambda ()
     (compile (format "javac -cp %s -d target/classes %s"
                      (jdt-prj-classpath (jdt-prj-owning-current-buffer))
                      (buffer-file-name))))))

;; Class names
(defun jdt-class-file-p (file-name)
  "Returns true if file-name represents a class file."
  (string-match "\\(^\\([a-z]+/\\)*\\)\\([A-Z]+[A-Za-z0-9]*\\.\\(java\\|class\\)$\\)" file-name))

(defun jdt-make-class-name-from-file-name (file-name)
  (when (jdt-class-file-p file-name)
    (jdt-make-class-name (replace-regexp-in-string "\\.$" ""
                                                   (replace-regexp-in-string "/" "."
                                                                             (match-string 1 file-name)))
                         (replace-regexp-in-string "\\.\\(class\\|java\\)$" ""
                                                   (match-string 3 file-name)))))

(defun jdt-make-class-name (package base-name)
  (cons package base-name))

(defun jdt-class-name-base-name (class)
  (cdr class))

(defun jdt-class-name-package (class)
  (car class))

(defun jdt-class-name-as-string (class-name)
  (format "%s.%s"
          (jdt-class-name-package class-name)
          (jdt-class-name-base-name class-name)))

;; Class repos
(defun jdt-class-repos-cache-buf-name (repo-name)
  (format "*%s*" repo-name))

(defun jdt-class-repos-classes-in-jar (jar-file)
  (jdt-get-list-using-buffer-cache (lambda () (mapcar 'jdt-make-class-name-from-file-name
                                                      (remove-if-not 'jdt-class-file-p
                                                                     (process-lines "jar" "tf" jar-file))))
                                   (format "*%s*" jar-file)))

(defun jdt-class-repos-classes-in-dir (dir)
  (mapcar 'jdt-make-class-name-from-file-name
          (directory-files-recursive dir nil
                                     "^[A-Z][A-Za-z0-9]*\\.class$")))

(defun jdt-class-repos-get-classes (repo)
  "Returns all the classes in the given class repository (either a jar file or a class directory)."
  (if (string-match-p "\\.jar$" repo)
      (jdt-class-repos-classes-in-jar repo)
    (jdt-class-repos-classes-in-dir repo)))

(defvar jdt-java-lang-classes-cache '())
(defun jdt-classes-in-java-lang ()
  (unless jdt-java-lang-classes-cache
    (setq jdt-java-lang-classes-cache
          (remove-if-not (lambda (class) (string= (jdt-class-name-package class) "java.lang"))
                         (jdt-class-repos-classes-in-jar (concat jdt-jdk-location
                                                                 "jre/lib/rt.jar")))))
  jdt-java-lang-classes-cache)

;; Import
(defun jdt-import-import-class (package base-name)
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (insert (format "import %s.%s;\n" package base-name))
    (message (format "%s.%s imported." package base-name))))

(defun jdt-import-class-imported-p (package base-name)
  (save-excursion
    (goto-char (point-min))
    (or (re-search-forward (format "import %s.%s;" package base-name) nil t)
        (re-search-forward (format "import %s.*;" package) nil t))))

(defun jdt-package-buffer (buf)
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "package \\([a-zA-Z0-9.]+\\);" nil t)
          (match-string-no-properties 1)
        ""))))

(defun jdt-package-current-buffer ()
  (jdt-package-buffer (current-buffer)))

(defun jdt-import-class-requires-import (package base-name)
  (or (string= "java.lang" package)
      (string= (jdt-package-current-buffer) package)
      (jdt-import-class-imported-p package base-name)))

;; Auto-completion
(require 'auto-complete)

(defun jdt-ac-class-candidates-1 (prj)
  (mapcar (lambda (class) (format "%s - %s"
                                  (jdt-class-name-base-name class)
                                  (jdt-class-name-package class)))
          (jdt-prj-classes-on-path prj)))
;;test
;(let ((project (car jdt-projects)))
;  (jdt-ac-class-candidates-1 project))

(defun jdt-ac-class-candidates ()
  (jdt-ac-class-candidates-1 (jdt-prj-owning-current-buffer)))

(defun jdt-ac-complete-class ()
  (let* ((package-end (point))
         (package-start (1+ (re-search-backward "\s")))
         (class-end (re-search-backward "\s-"))
         (class-start (1+ (re-search-backward "\s[A-Z]")))
         (package (buffer-substring-no-properties package-start package-end))
         (class (buffer-substring-no-properties class-start class-end)))
    (delete-region class-end package-end)
    (goto-char class-end)
    (when (jdt-import-class-requires-import package class)
      (jdt-import-import-class package class))))

(ac-define-source jdt-classes
  '((candidates . jdt-ac-class-candidates)
    (requires . 3)
    ;(document . jdt-class-documentation)
    (action . jdt-ac-complete-class)
    (symbol . "c")))

(defun jdt-name-of-object-being-called-at-point()
  (save-excursion
    (search-backward ".")
    (word-at-point)))

(defun jdt-class-in-java-lang-p (simple-name)
  (find simple-name
        (jdt-classes-in-java-lang)
        :test (lambda (sname class-name) (string= (jdt-class-name-base-name class-name) sname))))

(defun jdt-class-of-object-being-called-at-point ()
  (save-excursion
    (re-search-backward (concat "\\([A-Z]+[A-Za-z0-9_]?\\)\s+"
                                (jdt-name-of-object-being-called-at-point)))
    (let* ((simple-name (word-at-point))
           (java-lang-name (jdt-class-in-java-lang-p simple-name)))
      (or java-lang-name
          (if (progn (goto-char (point-min))
                     (re-search-forward (concat "^import +\\(.*\\)\\." simple-name ";") nil 't))
              (jdt-make-class-name (match-string-no-properties 1) simple-name))
          (jdt-make-class-name (jdt-package-current-buffer) simple-name)))))

;(with-current-buffer (get-buffer "LoadFileCreatorImpl.java")
;  (jdt-class-methods (jdt-class-of-object-being-called-at-point)))

(defun jdt-class-methods (class)
  (let ((res (process-lines "listmembers"
                            (jdt-prj-classpath (jdt-prj-owning-current-buffer))
                            (jdt-class-name-as-string class))))
    res))

(jdt-class-name-as-string '("java.io." . "BufferedReader"))

;(with-current-buffer (get-buffer "LoadFileCreatorImpl.java")
;  (jdt-class-of-object-being-called-at-point))
;  (jdt-class-methods (jdt-class-of-object-being-called-at-point)))

;  (jdt-class-methods '("java.util" . "List")))


(defun jdt-ac-method-candidates ()
  (jdt-class-methods (jdt-class-of-object-being-called-at-point)))

(defun jdt-ac-complete-method ()
  (zap-to-char -1 ?\:))

;; (defun jdt-member-candidates ()
;;   (let ((class (jdt-fq-class-name-of-object-at-point)))
;;     (jdt-list-members class)))

(ac-define-source jdt-methods
  '((candidates . jdt-ac-method-candidates)
    (requires . 0)
    (prefix . c-dot)
    (action . jdt-ac-complete-method)
    (symbol . "m")))

(defun jdt-auto-complete-setup ()
  (setq ac-sources (list 'ac-source-jdt-classes 'ac-source-jdt-methods))
  (jdt-prj-classes-on-path (jdt-prj-owning (buffer-file-name (current-buffer))
                                           jdt-projects)))

(add-hook 'java-mode-hook 'jdt-auto-complete-setup)
(add-hook 'java-mode-hook 'auto-complete-mode)



(provide 'jdt)

;; (progn
;;   (setq prj 
;;         (jdt-make-prj "bookingdesk" "~/development/bookingdesk/"
;;                       '("src/main/java") '("target/classes") '("lib")))
;;   (unless (string= "bookingdesk" (jdt-prj-name prj))
;;     (error "Unexpected name"))
;;   (unless (string= "/home/acaine/development/bookingdesk/" (jdt-prj-basedir prj))
;;     (error "Unexpected base dir"))
;;   (unless (= 24003 (length (jdt-prj-classes-on-path prj)))
;;     (error "Unexpected class count"))
;;   (unless (jdt-prj-owning "/home/acaine/development/bookingdesk/README" (list prj)))
;;   )


;(time-funcall 'jdt-prj-classes-on-path prj)
;(time-funcall 'jdt-ac-class-candidates1 prj)
;(time-funcall 'test-read-buffer)
;(time-funcall 'test-eval-buffer)

;(length (listp (test-eval-buffer)))
;(length (test-read-buffer))
;(jdt-prj-classes-on-path prj)

;; (defun test-read-buffer ()
;;   (with-current-buffer "*bookingdesk classes*"
;;     (split-string (buffer-string) "\n" t)))

;; (defun test-eval-buffer ()
;;   (with-current-buffer "*bookingdesk lisp classes*"
;;     (goto-char (point-min))    
;;     (read (current-buffer))))

;; (with-current-buffer (get-buffer-create "*bookingdesk lisp classes*")
;;   (message (buffer-name))
;;   (erase-buffer)
;;   (goto-char (point-min))
;;   (print (test-read-buffer) (current-buffer)))


;(format "'(%s)" (join " " (test-read-buffer)))
;(eval "'(1 2 3)")
;(jdt-class-name-package "uk.co.cnm.BaseClass")
;(jdt-class-name-base-name "uk.co.cnm.BaseClass")



;; testing
;(let ((prj (jdt-make-prj "bookingdesk" "~/development/bookingdesk/" '("src/main/java") '("target/classes") '("lib"))))
;  (unless (string= "bookingdesk" (jdt-prj-name prj))
;    (error "Unexpected name"))
;  (unless (string= "~/development/bookingdesk/" (jdt-prj-basedir prj))
;    (error "Unexpected base dir")))

;;(progn
;;  (setq jdt-projects nil)
;;  (jdt-prj-register "bookingdesk" "~/development/bookingdesk/")
;;  (unless (= (length jdt-projects) 1) (error "Project not registered")))







;; (require 'auto-complete)








;; (defvar jdt-class-cache nil)
;; (defvar jdt-class-candidate-cache nil)
;; (defvar jdt-projects '())
 
;; (defun jdt-jdk-rt-jar-location ()
;;   (concat jdt-jdk-location "jre/lib/rt.jar"))

;; (defun jdt-find-maven-base-dir ()
;;   "If the current buffer is part of a maven project, return the base directory
;; of that project.  Otherwise return nil."
;;   (defun find-pom (dir)
;;     (cond ((string= "/" dir) nil)
;;           ((find "pom.xml" (directory-files dir) :test 'string=) dir)
;;           ('t (find-pom (parent-dir dir)))))
;;   (find-pom (file-name-directory (buffer-file-name))))

;; (defun jdt-cd-to-maven2-project-root ()
;;   (while (not (file-exists-p "pom.xml"))
;;     (if (string= "Directory /../" (pwd))
;;         (error "pom.xml not found")
;;       (cd ".."))))

;; (defun jdt-run-junit-test (test-class)
;;   (interactive "sTest class: ")
;;   (save-excursion
;;     (jdt-cd-to-maven2-project-root)
;;     (cd "target")
;;     (let ((classpath (join ":" (append '("test-classes" "classes")
;;                                        (directory-files "dependency")))))
;;       (start-process "junit-run"
;;                      "junit"
;;                      "java"
;;                      "-cp"
;;                      classpath
;;                      " org.junit.runner.JUnitCore "
;;                      test-class))))

;; (defun jdt-classpath-entries ()
;;   "Return a list of classpath entries for the maven project that the current
;; buffer belongs to."
;;   (let ((maven-target-dir (concat (jdt-find-maven-base-dir) "target/")))
;;     (append (mapcar (lambda (x) (concat maven-target-dir x))
;;                     '("classes" "test-classes"))
;;             (directory-files (concat maven-target-dir "dependency")
;;                              't
;;                              ".*\\.jar"
;;                              nil)
;;             (list (jdt-jdk-rt-jar-location)))))

;; (defun parent-dir (path)
;;   (if (not (string-starts-with "/" path)) (error "Path must be absolute"))
;;   (if (string= path "/") (error "Path is the root directory"))
;;   (string-match "\\(.*\\)/[^/]+/?$" path)
;;   (concat (match-string 1 path) "/"))

;; (defun jdt-import-class ()
;;   (interactive)
;;   (jdt-insert-import (completing-read "Class name: "
;;                                      (jdt-class-names-on-build-path))))

;; (defun jdt-class-names-on-build-path ()
;;   (jdt-list-classes (jdt-get-build-path)))

;; (defun jdt-get-build-path ()
;;   (cons (jdt-jdk-rt-jar-location) (jdt-classpath-entries)))

;; (defun jdt-insert-import (class-name)
;;   (save-excursion
;;     (beginning-of-buffer)
;;     (end-of-line)
;;     (newline)
;;     (newline)
;;     (insert (concat "import " class-name ";"))
;;     (message (concat class-name " imported."))))

;; (defun jdt-complete-class ()
;;   (interactive)
;;   (let* ((end (point))
;;          (beg (re-search-backward "\\b[A-Z]"))
;;          (initial-input (buffer-substring beg end)))
;;     (kill-line)
;;     (goto-char beg)
;;     (insert-string (completing-read "Class: "
;;                                     (jdt-class-names-available)
;;                                     () () initial-input () () ()))))

;; (defun jdt-class-names-available ()
;;   "Return a list of class names that have either been imported or are available 
;; without importing (e.g. java.lang classes and classes in the same package)"
;;   (append (jdt-class-names-in-java-lang)
;;           (jdt-imported-classes)))

;; (defun jdt-imported-classes ()
;;   (save-excursion
;;     (beginning-of-buffer)
;;     (let ((classes '()))
;;       (while (re-search-forward "import \\([a-z]+\\.\\)*\\([A-Z][A-Za-z0-9]+\\);"
;;                                 () 't ())
;;         (setq classes (cons (match-string 2) classes)))
;;       classes)))

;; (defun jdt-class-names-in-java-lang ()
;;   (mapcar 'jdt-base-class-name (jdt-fq-class-names-in-java-lang)))

;; (defun jdt-fq-class-names-in-java-lang ()
;;   (remove-if-not (lambda (x) (string-match-p "java.lang." x))
;;                  (jdt-list-classes (list (jdt-jdk-rt-jar-location)))))

;; (defun jdt-base-class-name (fq-class-name)
;;   (string-match "[A-Z][a-z0-9]+$" fq-class-name)
;;   (match-string 0 fq-class-name))

;; (defun jdt-package-name (fq-class-name)
;;   (string-match "\\(\\([a-z]+\\.\\)*[a-z]+\\)\\." fq-class-name)
;;   (match-string 1 fq-class-name))

;; (defun jdt-list-classes ()
;;   "List all classes on this projects classpath."
;;   (if (null jdt-class-cache)
;;       (jdt-build-class-cache))
;;   jdt-class-cache)

;; (defun jdt-build-class-cache ()
;;   (message "building class cache")
;;   (setq jdt-class-cache
;;         (process-lines "listclasses" (join ":" (jdt-classpath-entries)))))

;; (defun jdt-project (name root)
;;   (setq jdt-projects (cons (cons name root) jdt-projects)))

;; (defun jdt-open-java-type ()
;;   (interactive)
;;   (if (null jdt-projects)
;;       (message "No java projects defined")
;;     (message (completing-read "Open java type: "
;;                               (jdt-java-types jdt-projects)))))

;; (defun jdt-java-types (projects)
;;   "Return a list of all java types declared in projects."
;;   (cond ((null projects) nil)
;;         ('t (append (mapcar (lambda (file) (cons (jdt-java-type file) file))
;;                             (jdt-find-files (cdr (car projects))
;;                                             ".*\\.java"))
;;                     (jdt-java-types (cdr projects))))))

;; (defun jdt-java-type (file)
;;   (if (string-match "\\([A-Za-z0-9]+\\)\\.java$" file)
;;       (match-string 1 file)
;;     nil))

;; (defun jdt-find-files (dir pattern)
;;   "Return all files under dir that match the given pattern."
;;   (let ((result))
;;     (dolist (file (directory-files dir 't "^[^.]+") result)
;;       (cond ((file-directory-p file) (setq result
;;                                            (append result
;;                                                    (jdt-find-files file pattern))))
;;             ((string-match-p pattern file) (add-to-list 'result file))))))



;; ;; auto-completion
;; (defun jdt-class-candidates ()
;;   (if (null jdt-class-candidate-cache)
;;       (jdt-build-class-candidate-cache))
;;   (insert (join ":" (jdt-class-candidate-cache)))
;;   (jdt-class-candidate-cache))

;; (defun jdt-build-class-candidate-cache ()
;;   (message "building candidate cache")
;;   (setq jdt-class-candidate-cache
;;         (mapcar (lambda (fq-class-name) (format "%s - %s"
;;                                                 (jdt-base-class-name fq-class-name)
;;                                                 (jdt-package-name fq-class-name)))
;;                 (jdt-list-classes))))  

;; (defun jdt-class-documentation (class)
;;   "TODO: add documentation")

;; (ac-define-source jdt-classes
;;   '((candidates . jdt-class-candidates)
;;     (requires . 3)
;;     (document . jdt-class-documentation)
;;     (action . jdt-complete)
;;     (symbol . "c")))

;; (defun jdt-list-members (class)
;;   (process-lines "listmembers" (join ":" (jdt-classpath-entries)) class))

;; (defun jdt-member-candidates ()
;;   (let ((class (jdt-fq-class-name-of-object-at-point)))
;;     (jdt-list-members class)))

;; (defun jdt-object-name-at-point ()
;;   (save-excursion
;;     (re-search-backward "\\.")
;;     (word-at-point)))

;; (defun jdt-base-class-name-of-object-at-point ()
;;   (save-excursion
;;     (re-search-backward (concat "[A-Za-z0-9] +" (jdt-object-name-at-point)))
;;     (word-at-point)))

;; (defun jdt-fq-class-name-of-object-at-point ()
;;   (save-excursion
;;     (let ((base-name (jdt-base-class-name-of-object-at-point)))
;;       (cond ((re-search-backward (concat "^import +\\(.*" base-name "\\);") nil 't)
;;              (match-string 1))
;;             ((find (concat "java.lang." base-name)
;;                    (jdt-fq-class-names-in-java-lang) :test 'string=)
;;              (concat "java.lang." base-name))
;;             ('t (concat (jdt-current-package) "." base-name))))))

;; (defun jdt-current-package ()
;;   "Returns the package name of the current buffer."
;;   (save-excursion
;;     (beginning-of-buffer)
;;     (re-search-forward "package \\(.*\\);")
;;     (match-string 1)))

;; (defun test ()
;;   (interactive)
;;   (message (join ":" (jdt-list-classes))))


;; (ac-define-source jdt-members
;;   '((candidates . jdt-member-candidates)
;;     (requires . 0)
;;     (prefix . c-dot)
;;     (action . jdt-member-complete)
;;     (symbol . "m")))

;; (defun test-candidates ()
;;   '("test" "test2"))

;; (ac-define-source test
;;   '((candidates . test-candidates)
;;     (requires . 3)
;;     (symbol . "t")))

;; (defun jdt-auto-complete-setup () 
;; ;  (setq ac-sources (list 'ac-source-jdt-classes 'ac-source-jdt-members 'ac-source-test)))
;;   (setq ac-sources (list 'ac-source-test)))

;; (add-hook 'java-mode-hook 'jdt-auto-complete-setup)
;; (add-hook 'java-mode-hook 'auto-complete-mode)
;; ;(add-hook 'java-mode-hook 'jdt-build-class-cache)
;; ;(add-hook 'java-mode-hook 'jdt-build-class-candidate-cache)

;; (defun jdt-complete ()
;;   (let* ((package-end (point))
;;          (package-start (+ 3 (search-backward " - ")))
;;          (package (buffer-substring package-start package-end))
;;          (class-end (point))
;;          (class-start (re-search-backward "\\b[A-Z]"))
;;          (class (buffer-substring class-start class-end)))
;;     (delete-region (- package-start 3) package-end)
;;     (jdt-insert-import (concat package "." class))
;;     (forward-word)))


;; (setq ac-completing-map
;;   (let ((map (make-sparse-keymap)))
;;     ;(define-key map "\t" 'ac-expand)
;;     (define-key map "\r" 'ac-complete)
;;     (define-key map (kbd "M-TAB") 'auto-complete)
;;     (define-key map "\C-s" 'ac-isearch)

;;     (define-key map "\M-n" 'ac-next)
;;     (define-key map "\M-p" 'ac-previous)
;;     (define-key map [down] 'ac-next)
;;     (define-key map [up] 'ac-previous)

;;     (define-key map [f1] 'ac-help)
;;     (define-key map [M-f1] 'ac-persist-help)
;;     (define-key map (kbd "C-?") 'ac-help)
;;     (define-key map (kbd "C-M-?") 'ac-persist-help)

;;     (define-key map [C-down] 'ac-quick-help-scroll-down)
;;     (define-key map [C-up] 'ac-quick-help-scroll-up)
;;     (define-key map "\C-\M-n" 'ac-quick-help-scroll-down)
;;     (define-key map "\C-\M-p" 'ac-quick-help-scroll-up)

;;     (dotimes (i 9)
;;       (let ((symbol (intern (format "ac-complete-%d" (1+ i)))))
;;         (fset symbol
;;               `(lambda ()
;;                  (interactive)
;;                  (when (and (ac-menu-live-p) (popup-select ac-menu ,i))
;;                    (ac-complete))))
;;         (define-key map (read-kbd-macro (format "M-%s" (1+ i))) symbol)))

;;     map))


;; (provide 'jdt)

