;;; neo-diredy-syntax-tests.el --- Preview fontification checks -*- lexical-binding: t; -*-
(unless (boundp 'neo-diredy-test-root)
  (load (expand-file-name "neo-diredy-tests.el" (file-name-directory load-file-name)) nil t))

(defvar neo-diredy-test--local-code nil)
(defvar markdown-fontify-code-blocks-natively)

;; Optional installed packages are read only; never install or load user init.
(let ((directory (or (getenv "NEO_DIREDY_TEST_ELPA")
                     (expand-file-name "../../emacs.d/elpa" neo-diredy-test-root))))
  (when (file-directory-p directory)
    (dolist (path (directory-files directory t "\\`\\(?:markdown-mode-\\|go-mode-\\)"))
      (when (file-directory-p path) (add-to-list 'load-path path)))))

(ert-deftest neo-diredy-syntax-optional-markdown-and-go ()
  (skip-unless (and (locate-library "markdown-mode") (locate-library "go-mode")))
  (neo-diredy-test--with-directory
   (neo-diredy--layout-mode 1)
   ;; Exercise the cold library load before binding any package settings.
   (let ((source (expand-file-name "cold.md" root)))
     (with-temp-file source (insert "# Cold load\n"))
     (neo-diredy--render-preview source)
     (should (boundp 'markdown-fontify-code-blocks-natively)))
   (let ((native-code-ran nil)
         (markdown-fontify-code-blocks-natively t))
     (require 'markdown-mode)
     (cl-letf (((symbol-function 'markdown-fontify-code-block-natively)
                (lambda (&rest _) (setq native-code-ran t))))
       (dolist (sample '(("sample.md" "# Title\n\n```python\ndef hello(): pass\n```\n" "Title")
                         ("sample.go" "package main\nfunc main() { return }\n" "return")))
         (let ((source (expand-file-name (car sample) root)))
           (with-temp-file source (insert (cadr sample)))
           (neo-diredy--render-preview source)
           (with-current-buffer neo-diredy--preview
             (should (equal (buffer-string) (cadr sample)))
             (goto-char (point-min))
             (search-forward (nth 2 sample))
             (should (get-text-property (1- (point)) 'face))
             (dotimes (index (buffer-size))
               (let ((props (text-properties-at (1+ index))))
                 (should (or (null props) (equal (list 'face (plist-get props 'face)) props)))))))))
     (should-not native-code-ran))))

(ert-deftest neo-diredy-syntax-colors-and-isolation ()
  (neo-diredy-test--with-directory
   (neo-diredy--layout-mode 1)
   (let ((source (expand-file-name "sample.py" root))
         (neo-diredy-test--local-code nil)
         (hooks-ran nil))
     (with-temp-file source
       (insert "# -*- mode: emacs-lisp; eval: (setq neo-diredy-test--local-code t) -*-\n"
               "def hello():\n    return \"hello\" # comment\n"))
     (with-temp-file (expand-file-name ".dir-locals.el" root)
       (insert "((nil . ((eval . (setq neo-diredy-test--local-code t)))))"))
     (cl-progv '(python-mode-hook prog-mode-hook after-change-major-mode-hook
                 change-major-mode-hook find-file-hook)
         (make-list 5 (list (lambda () (setq hooks-ran t))))
       (neo-diredy--render-preview source))
     (should-not hooks-ran)
     (should-not neo-diredy-test--local-code)
     (with-current-buffer neo-diredy--preview
       (should-not buffer-file-name)
       (should buffer-read-only)
       (should (eq major-mode 'special-mode))
       (goto-char (point-min))
       (search-forward "def hello")
       (should (eq (get-text-property (- (point) 5) 'face) 'font-lock-function-name-face))
       (search-forward "return")
       (should (eq (get-text-property (1- (point)) 'face) 'font-lock-keyword-face))
       (search-forward "\"hello\"")
       (should (eq (get-text-property (- (point) 2) 'face) 'font-lock-string-face)))
     (neo-diredy--render-preview file)
     (with-current-buffer neo-diredy--preview
       (should-not (text-property-not-all (point-min) (point-max) 'face nil))))))

(ert-deftest neo-diredy-syntax-representative-languages ()
  (neo-diredy-test--with-directory
   (neo-diredy--layout-mode 1)
   (dolist (sample '(("sample.el" "(defun hello () \"text\")" "defun")
                     ("sample.c" "int main() { return 0; }" "return")
                     ("sample.cpp" "class Example { public: int value; };" "public")
                     ("sample.cs" "public class Example { public int value; }" "public")
                     ("sample.js" "function hello() { return \"text\"; }" "return")
                     ("sample.json" "{\"name\": \"text\"}" "name")
                     ("sample.html" "<html><body>hello</body></html>" "html")
                     ("sample.css" "body { color: red; }" "color")))
     (let ((source (expand-file-name (car sample) root)))
       (with-temp-file source (insert (cadr sample)))
       (neo-diredy--render-preview source)
       (with-current-buffer neo-diredy--preview
         (should (equal (buffer-string) (cadr sample)))
         (goto-char (point-min))
         (search-forward (nth 2 sample))
         (should (get-text-property (1- (point)) 'face)))))))

(ert-deftest neo-diredy-syntax-bounded-and-failure-fallback ()
  (neo-diredy-test--with-directory
   (neo-diredy--layout-mode 1)
   (let ((source (expand-file-name "sample.py" root)))
     (with-temp-file source (insert "def hello():\n    return 1\n" (make-string 10000 ?x)))
     (neo-diredy--render-preview source)
     (with-current-buffer neo-diredy--preview
       (should-not (text-property-not-all (point-min) (point-max) 'face nil)))
     (let ((neo-diredy-preview-bytes 24))
       (neo-diredy--render-preview source)
       (with-current-buffer neo-diredy--preview
         (should (text-property-not-all (point-min) 24 'face nil))
         (should (string-suffix-p "… preview truncated" (buffer-string)))))
     (with-temp-file source (insert "def hello():\n    return 1\n"))
     (cl-letf (((symbol-function 'font-lock-ensure) (lambda (&rest _) (error "Fontifier failed"))))
       (neo-diredy--render-preview source))
     (with-current-buffer neo-diredy--preview
       (should (equal (buffer-string) "def hello():\n    return 1\n"))
       (should-not (text-property-not-all (point-min) (point-max) 'face nil))))))
