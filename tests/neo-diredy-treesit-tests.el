;;; neo-diredy-treesit-tests.el --- Native preview checks -*- lexical-binding: t; -*-
(load (expand-file-name "neo-diredy-syntax-tests.el" (file-name-directory load-file-name)) nil t)
(require 'treesit)

;; Explicit fixture path; never install grammars or load the user's init.
(when-let* ((directory (getenv "NEO_DIREDY_TEST_GRAMMARS")))
  (add-to-list 'treesit-extra-load-path directory))

(ert-deftest neo-diredy-syntax-treesit-real-grammars ()
  (skip-unless (cl-every #'treesit-language-available-p
                         '(typescript tsx rust yaml toml lua go c-sharp c cpp python javascript jsdoc json css ruby bash)))
  (neo-diredy-test--with-directory
   (neo-diredy--layout-mode 1)
   (let ((fontify (symbol-function 'font-lock-ensure)) seen
         (treesit-auto-install-grammar t))
     (cl-letf (((symbol-function 'font-lock-ensure)
                (lambda (&rest args)
                  (setq seen (mapcar #'treesit-parser-language (treesit-parser-list)))
                  (should-not treesit-auto-install-grammar)
                  (apply fontify args)))
               ((symbol-function 'treesit-install-language-grammar)
                (lambda (&rest _) (ert-fail "Preview must not install grammars"))))
       (dolist (sample '(("sample.ts" "const value: number = 1;\n" "const" typescript)
                         ("sample.tsx" "const value = <div />;\n" "const" tsx)
                         ("sample.rs" "fn main() { let value = 1; }\n" "fn" rust)
                         ("sample.yaml" "name: hello\n" "name" yaml)
                         ("sample.toml" "name = \"hello\"\n" "hello" toml)
                         ("sample.lua" "local value = true\n" "local" lua)
                         ("sample.go" "package main\nfunc main() { return }\n" "return" go)
                         ("sample.cs" "public class Example {}\n" "public" c-sharp)
                         ("sample.c" "int main() { return 0; }\n" "return" c)
                         ("sample.cpp" "class Example { public: int value; };\n" "public" cpp)
                         ("sample.py" "def hello(): return 1\n" "return" python)
                         ("sample.js" "function hello() { return 1; }\n" "return" javascript)
                         ("sample.json" "{\"name\": 1}\n" "name" json)
                         ("sample.css" "body { color: red; }\n" "color" css)
                         ("sample.rb" "def hello; return 1; end\n" "return" ruby)
                         ("sample.sh" "#!/bin/bash\nif true; then echo ok; fi\n" "if" bash)))
         (ert-info ((car sample))
           (setq seen nil)
           (neo-diredy-test--assert-preview-face root (car sample) (nth 1 sample) (nth 2 sample))
           (should (memq (nth 3 sample) seen))))))))

(ert-deftest neo-diredy-syntax-treesit-missing-grammar-fallback ()
  (neo-diredy-test--with-directory
   (neo-diredy--layout-mode 1)
   (cl-letf (((symbol-function 'treesit-language-available-p) (lambda (&rest _) nil)))
     (neo-diredy-test--assert-preview-face root "sample.cs" "public class Example {}\n" "public"))))

(ert-deftest neo-diredy-syntax-treesit-unavailable-fallback ()
  (neo-diredy-test--with-directory
   (neo-diredy--layout-mode 1)
   (cl-letf (((symbol-function 'treesit-available-p) (lambda () nil)))
     (neo-diredy-test--assert-preview-face root "sample.cs" "public class Example {}\n" "public"))))

(ert-deftest neo-diredy-syntax-treesit-long-line ()
  (skip-unless (treesit-language-available-p 'json))
  (neo-diredy-test--with-directory
   (neo-diredy--layout-mode 1)
   (neo-diredy-test--assert-preview-face root "long.json"
                                        (concat "{\"name\": \"" (make-string 5000 ?x) "\"}\n")
                                        "name")))

(ert-deftest neo-diredy-syntax-treesit-broken-parser-fallback ()
  (skip-unless (treesit-language-available-p 'c-sharp))
  (require 'csharp-mode)
  (neo-diredy-test--with-directory
   (neo-diredy--layout-mode 1)
   (cl-letf (((symbol-function 'csharp-ts-mode) (lambda () (error "Broken grammar"))))
     (neo-diredy-test--assert-preview-face root "sample.cs" "public class Example {}\n" "public"))))
