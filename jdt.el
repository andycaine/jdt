(require 'auto-complete)

(defvar jdt-jdk-location "/usr/lib/jvm/java-6-openjdk/")
(defvar jdt-class-cache nil)
(defvar jdt-class-candidate-cache nil)
(defvar jdt-projects '())
 
(defun jdt-jdk-rt-jar-location ()
  (concat jdt-jdk-location "jre/lib/rt.jar"))

(defun jdt-find-maven-base-dir ()
  "If the current buffer is part of a maven project, return the base directory
of that project.  Otherwise return nil."
  (defun find-pom (dir)
    (cond ((string= "/" dir) nil)
          ((find "pom.xml" (directory-files dir) :test 'string=) dir)
          ('t (find-pom (parent-dir dir)))))
  (find-pom (file-name-directory (buffer-file-name))))

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

(defun jdt-classpath-entries ()
  "Return a list of classpath entries for the maven project that the current
buffer belongs to."
  (let ((maven-target-dir (concat (jdt-find-maven-base-dir) "target/")))
    (append (mapcar (lambda (x) (concat maven-target-dir x))
                    '("classes" "test-classes"))
            (directory-files (concat maven-target-dir "dependency")
                             't
                             ".*\\.jar"
                             nil)
            (list (jdt-jdk-rt-jar-location)))))

(defun parent-dir (path)
  (if (not (string-starts-with "/" path)) (error "Path must be absolute"))
  (if (string= path "/") (error "Path is the root directory"))
  (string-match "\\(.*\\)/[^/]+/?$" path)
  (concat (match-string 1 path) "/"))

(defun string-starts-with (prefix str)
  (if (> (length prefix) (length str))
      nil
    (string-match (format "^%s.*" prefix) str)))

(defun jdt-import-class ()
  (interactive)
  (jdt-insert-import (completing-read "Class name: "
                                     (jdt-class-names-on-build-path))))

(defun jdt-class-names-on-build-path ()
  (jdt-list-classes (jdt-get-build-path)))

(defun jdt-get-build-path ()
  (cons (jdt-jdk-rt-jar-location) (jdt-classpath-entries)))

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
  (remove-if-not (lambda (x) (string-match-p "java.lang." x))
                 (jdt-list-classes (list (jdt-jdk-rt-jar-location)))))

(defun jdt-base-class-name (fq-class-name)
  (string-match "[A-Z][a-z0-9]+$" fq-class-name)
  (match-string 0 fq-class-name))

(defun jdt-package-name (fq-class-name)
  (string-match "\\(\\([a-z]+\\.\\)*[a-z]+\\)\\." fq-class-name)
  (match-string 1 fq-class-name))

(defun jdt-list-classes ()
  "List all classes on this projects classpath."
  (if (null jdt-class-cache)
      (jdt-build-class-cache))
  jdt-class-cache)

(defun jdt-build-class-cache ()
  (message "building class cache")
  (setq jdt-class-cache
        (process-lines "listclasses" (join ":" (jdt-classpath-entries)))))

(defun jdt-project (name root)
  (setq jdt-projects (cons (cons name root) jdt-projects)))

(defun jdt-open-java-type ()
  (interactive)
  (if (null jdt-projects)
      (message "No java projects defined")
    (message (completing-read "Open java type: "
                              (jdt-java-types jdt-projects)))))

(defun jdt-java-types (projects)
  "Return a list of all java types declared in projects."
  (cond ((null projects) nil)
        ('t (append (mapcar (lambda (file) (cons (jdt-java-type file) file))
                            (jdt-find-files (cdr (car projects))
                                            ".*\\.java"))
                    (jdt-java-types (cdr projects))))))

(defun jdt-java-type (file)
  (if (string-match "\\([A-Za-z0-9]+\\)\\.java$" file)
      (match-string 1 file)
    nil))

(defun jdt-find-files (dir pattern)
  "Return all files under dir that match the given pattern."
  (let ((result))
    (dolist (file (directory-files dir 't "^[^.]+") result)
      (cond ((file-directory-p file) (setq result
                                           (append result
                                                   (jdt-find-files file pattern))))
            ((string-match-p pattern file) (add-to-list 'result file))))))



;; utils
(defun join (sep seq)
  (mapconcat 'identity seq sep))

(defun filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;; auto-completion
(defun jdt-class-candidates ()
  (if (null jdt-class-candidate-cache)
      (jdt-build-class-candidate-cache))
  (insert (join ":" (jdt-class-candidate-cache)))
  (jdt-class-candidate-cache))

(defun jdt-build-class-candidate-cache ()
  (message "building candidate cache")
  (setq jdt-class-candidate-cache
        (mapcar (lambda (fq-class-name) (format "%s - %s"
                                                (jdt-base-class-name fq-class-name)
                                                (jdt-package-name fq-class-name)))
                (jdt-list-classes))))  

(defun jdt-class-documentation (class)
  "TODO: add documentation")

(ac-define-source jdt-classes
  '((candidates . jdt-class-candidates)
    (requires . 3)
    (document . jdt-class-documentation)
    (action . jdt-complete)
    (symbol . "c")))

(defun jdt-list-members (class)
  (process-lines "listmembers" (join ":" (jdt-classpath-entries)) class))

(defun jdt-member-candidates ()
  (let ((class (jdt-fq-class-name-of-object-at-point)))
    (jdt-list-members class)))

(defun jdt-object-name-at-point ()
  (save-excursion
    (re-search-backward "\\.")
    (word-at-point)))

(defun jdt-base-class-name-of-object-at-point ()
  (save-excursion
    (re-search-backward (concat "[A-Za-z0-9] +" (jdt-object-name-at-point)))
    (word-at-point)))

(defun jdt-fq-class-name-of-object-at-point ()
  (save-excursion
    (let ((base-name (jdt-base-class-name-of-object-at-point)))
      (cond ((re-search-backward (concat "^import +\\(.*" base-name "\\);") nil 't)
             (match-string 1))
            ((find (concat "java.lang." base-name)
                   (jdt-fq-class-names-in-java-lang) :test 'string=)
             (concat "java.lang." base-name))
            ('t (concat (jdt-current-package) "." base-name))))))

(defun jdt-current-package ()
  "Returns the package name of the current buffer."
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "package \\(.*\\);")
    (match-string 1)))

(defun test ()
  (interactive)
  (message (join ":" (jdt-list-classes))))


(ac-define-source jdt-members
  '((candidates . jdt-member-candidates)
    (requires . 0)
    (prefix . c-dot)
    (action . jdt-member-complete)
    (symbol . "m")))

(defun test-candidates ()
  '("test" "test2"))

(ac-define-source test
  '((candidates . test-candidates)
    (requires . 3)
    (symbol . "t")))

(defun jdt-auto-complete-setup () 
;  (setq ac-sources (list 'ac-source-jdt-classes 'ac-source-jdt-members 'ac-source-test)))
  (setq ac-sources (list 'ac-source-test)))

(add-hook 'java-mode-hook 'jdt-auto-complete-setup)
(add-hook 'java-mode-hook 'auto-complete-mode)
;(add-hook 'java-mode-hook 'jdt-build-class-cache)
;(add-hook 'java-mode-hook 'jdt-build-class-candidate-cache)

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


(setq ac-completing-map
  (let ((map (make-sparse-keymap)))
    ;(define-key map "\t" 'ac-expand)
    (define-key map "\r" 'ac-complete)
    (define-key map (kbd "M-TAB") 'auto-complete)
    (define-key map "\C-s" 'ac-isearch)

    (define-key map "\M-n" 'ac-next)
    (define-key map "\M-p" 'ac-previous)
    (define-key map [down] 'ac-next)
    (define-key map [up] 'ac-previous)

    (define-key map [f1] 'ac-help)
    (define-key map [M-f1] 'ac-persist-help)
    (define-key map (kbd "C-?") 'ac-help)
    (define-key map (kbd "C-M-?") 'ac-persist-help)

    (define-key map [C-down] 'ac-quick-help-scroll-down)
    (define-key map [C-up] 'ac-quick-help-scroll-up)
    (define-key map "\C-\M-n" 'ac-quick-help-scroll-down)
    (define-key map "\C-\M-p" 'ac-quick-help-scroll-up)

    (dotimes (i 9)
      (let ((symbol (intern (format "ac-complete-%d" (1+ i)))))
        (fset symbol
              `(lambda ()
                 (interactive)
                 (when (and (ac-menu-live-p) (popup-select ac-menu ,i))
                   (ac-complete))))
        (define-key map (read-kbd-macro (format "M-%s" (1+ i))) symbol)))

    map))


(provide 'jdt)

(defun directory-files-recursive (dir)
  (mapcan (lambda (file)
            (if (file-directory-p file)
                (all-files file)
              (list file)))
          (directory-files dir t "^[a-zA-Z].*")))

