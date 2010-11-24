(require 'auto-complete)

(defvar jdt-jdk-location "/usr/lib/jvm/java-6-openjdk/")

(defun jdt-jdk-rt-jar-location ()
  (concat jdt-jdk-location "jre/lib/rt.jar"))

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
    (append (list "src/main/java" "src/test/java" (jdt-jdk-rt-jar-location))
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
          (jdt-list-classes (jdt-jdk-rt-jar-location))))

(defun jdt-base-class-name (fq-class-name)
  (string-match "[A-Z][a-z0-9]+$" fq-class-name)
  (match-string 0 fq-class-name))

(defun jdt-package-name (fq-class-name)
  (string-match "\\(\\([a-z]+\\.\\)*[a-z]+\\)\\." fq-class-name)
  (match-string 1 fq-class-name))

(defun jdt-list-classes (path)
  (process-lines "listclasses" (join ":" path)))


;; utils
(defun join (sep seq)
  (mapconcat 'identity seq sep))

(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;; auto-complete
(defvar jdt-class-cache nil)
(defun jdt-class-candidates ()
  (if (not jdt-class-cache)
      (jdt-init-class-cache))
  jdt-class-cache)

(defun jdt-class-documentation (class)
  "TODO: add documentation")

(defun jdt-init-class-cache ()
  (message "Initializing class cache")
  (setq jdt-class-cache (mapcar (lambda (x) (concat (jdt-base-class-name x)
                                                    " - "
                                                    (jdt-package-name x)))
                                (jdt-class-names-on-build-path))))

(ac-define-source jdt-classes
  '((init . 'jdt-init-class-cache)
    (candidates . jdt-class-candidates)
    (requires . 3)
    (document . jdt-class-documentation)
    (action . jdt-complete)
    (symbol . "c")))

(defun jdt-auto-complete-setup () 
  (setq ac-sources (list 'ac-source-jdt-classes)))

(add-hook 'java-mode-hook 'jdt-auto-complete-setup)
(add-hook 'java-mode-hook 'auto-complete-mode)

(defun jdt-complete ()
  (let* ((package-end (point))
         (package-start (+ 3 (search-backward " - ")))
         (package (buffer-substring package-start package-end))
         (class-end (point))
         (class-start (re-search-backward "\\b[A-Z]"))
         (class (buffer-substring class-start class-end)))
    (delete-region (- package-start 3) package-end)
    (jdt-insert-import (concat package "." class))
    (forward-word)))

(provide 'jdt)
