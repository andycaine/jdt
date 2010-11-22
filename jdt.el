
(defun jdt-cd-to-maven2-project-root ()
  (while (not (file-exists-p "pom.xml"))
    (if (string= "Directory /../" (pwd))
        (error "pom.xml not found")
      (cd ".."))))

(defun jdt-run-junit-test (test-class)
  (interactive "sTest class: ")
  (save-excursion
    (jdt-cd-to-maven2-project-root)
    (cd "target")
    (let ((classpath (join ":" (append '("test-classes" "classes")
                                       (directory-files "dependency")))))
      (start-process "junit-run"
                     "junit"
                     "java"
                     "-cp"
                     classpath
                     " org.junit.runner.JUnitCore "
                     test-class))))

(defun jdt-import-class ()
  (interactive)
  (jdt-insert-import (completing-read "Class name: "
                                     (jdt-class-names-on-build-path))))

(defun jdt-class-names-on-build-path ()
  (jdt-list-classes (jdt-get-build-path)))

(defun jdt-get-build-path ()
  (save-excursion
    (jdt-cd-to-maven2-project-root)
    (append '("src/main/java" "src/test/java")
            (directory-files "target/dependency" 't))))

(defun jdt-insert-import (class-name)
  (save-excursion
    (beginning-of-buffer)
    (end-of-line)
    (newline)
    (newline)
    (insert (concat "import " class-name ";"))
    (message (concat class-name " imported."))))


(defun jdt-complete-class ()
  (interactive)
  (let* ((end (point))
         (beg (re-search-backward "\\b[A-Z]"))
         (initial-input (buffer-substring beg end)))
    (kill-line)
    (goto-char beg)
    (insert-string (completing-read "Class: "
                                    (jdt-class-names-available)
                                    () () initial-input () () ()))))

(defun jdt-class-names-available ()
  "Return a list of class names that have either been imported or are available 
without importing (e.g. java.lang classes and classes in the same package)"
  (append (jdt-class-names-in-java-lang)
          (jdt-imported-classes)))

(defun jdt-imported-classes ()
  (save-excursion
    (beginning-of-buffer)
    (let ((classes '()))
      (while (re-search-forward "import \\([a-z]+\\.\\)*\\([A-Z][A-Za-z0-9]+\\);"
                                () 't ())
        (setq classes (cons (match-string 2) classes)))
      classes)))

(defun jdt-class-names-in-java-lang ()
  (mapcar 'jdt-base-class-name (jdt-fq-class-names-in-java-lang)))

(defun jdt-fq-class-names-in-java-lang ()
  (filter (lambda (x) (string-match "java.lang." x))
          (jdt-list-classes '("/usr/lib/jvm/java-6-openjdk/jre/lib/rt.jar"))))

(defun jdt-base-class-name (fq-class-name)
  (string-match "[A-Z][a-z0-9]+$" fq-class-name)
  (match-string 0 fq-class-name))

(defun jdt-list-classes (path)
  (process-lines "listclasses" (join ":" path)))


;; utils
(defun join (sep seq)
  (mapconcat 'identity seq sep))

(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(provide 'jdt)
