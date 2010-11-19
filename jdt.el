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
  (process-lines "listclasses" (jdt-get-build-path)))

(defun jdt-get-build-path ()
  (save-excursion
    (jdt-cd-to-maven2-project-root)
    (join ":" (append '("src/main/java" "src/test/java")
                      (directory-files "target/dependency" 't)))))

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
    (completing-read "Class: " (jdt-class-names-on-build-path))))
                     
;; delete
(defun jdt-complete-type-name (start)
  (interactive "sType name: ")
  (find-file tags-file-name)
  (beginning-of-buffer)
  (let ((matches '()))
    (while (re-search-forward (concat "\\(class\\|interface\\) " start)
                              (buffer-end 1)
                              't
                              1)
      (setq matches (cons (symbol-at-point) matches)))
    (message "Matches: %s" matches)))


(defun join (sep seq)
  (mapconcat 'identity seq sep))
