;;; neo-diredy-diff-tests.el --- Diff presentation checks -*- lexical-binding: t; -*-
(require 'ert)
(load (expand-file-name "../neo-diredy.el" (file-name-directory load-file-name)) nil t)

(defun neo-diredy-test-diff (text)
  (insert text)
  (special-mode)
  (neo-diredy--decorate-diff))

(defun neo-diredy-test-prefix (line)
  (goto-char (point-min))
  (search-forward line)
  (get-text-property (line-beginning-position) 'line-prefix))

(ert-deftest neo-diredy-diff-display-preserves-text-and-numbers ()
  (with-temp-buffer
    (let ((text "diff --git a/x b/x\n--- a/x\n+++ b/x\n@@ -9,3 +19,4 @@ function\n same\n-old value\n+new value\n+extra\n tail\n"))
      (neo-diredy-test-diff text)
      (should (equal text (buffer-substring-no-properties (point-min) (point-max))))
      (should (equal (neo-diredy-test-prefix "@@") "old new │ "))
      (should (equal (neo-diredy-test-prefix " same") "  9  19 │ "))
      (should (equal (neo-diredy-test-prefix "-old") " 10     │ "))
      (should (equal (neo-diredy-test-prefix "+new") "     20 │ "))
      (should (equal (neo-diredy-test-prefix "+extra") "     21 │ "))
      (should (equal (neo-diredy-test-prefix " tail") " 11  22 │ "))
      (should (equal (get-text-property (point) 'wrap-prefix) "        │ "))
      (should (assq 'diff-added face-remapping-alist))
      (let ((remapping (copy-tree face-remapping-alist))
            (position (point)))
        (neo-diredy--decorate-diff)
        (should (= position (point)))
        (should (equal remapping face-remapping-alist))
        (should (equal text (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest neo-diredy-diff-display-zero-counts-markers-and-file-boundaries ()
  (with-temp-buffer
    (neo-diredy-test-diff
     (concat "diff --git a/x b/x\n--- /dev/null\n+++ b/x\n@@ -0,0 +1,2 @@\n"
             "+one\n+++literal plus\n\\ No newline at end of file\n"
             "diff --git a/y b/y\n--- a/y\n+++ /dev/null\n@@ -8,2 +0,0 @@\n"
             "-two\n---literal minus\n\\ No newline at end of file\n"
             "diff --git a/z b/z\nBinary files a/z and b/z differ\n"))
    (should (equal (neo-diredy-test-prefix "+one") "      1 │ "))
    (should (equal (neo-diredy-test-prefix "+++literal") "      2 │ "))
    (should (equal (neo-diredy-test-prefix "-two") "  8     │ "))
    (should (equal (neo-diredy-test-prefix "---literal") "  9     │ "))
    (should-not (neo-diredy-test-prefix "--- /dev/null"))
    (should-not (neo-diredy-test-prefix "No newline"))
    (should-not (neo-diredy-test-prefix "Binary files"))))

(ert-deftest neo-diredy-diff-display-large-line-numbers-and-truncated-input ()
  (with-temp-buffer
    (neo-diredy-test-diff "@@ -9999,2 +10000,2 @@\n-before\n+after\n context\n+not a hunk\n@@ -2 +2 @@\n-partial")
    (should (equal (neo-diredy-test-prefix "-before") " 9999       │ "))
    (should (equal (neo-diredy-test-prefix " context") "10000 10001 │ "))
    (should-not (neo-diredy-test-prefix "+not a hunk"))
    (should (equal (neo-diredy-test-prefix "-partial") "    2       │ "))))

(ert-deftest neo-diredy-diff-display-unsupported-formats-stay-readable ()
  (dolist (text '("diff --cc x\n@@@ -1,1 -1,1 +1,1 @@@\n++merged\n"
                  "*** a/x\n--- b/x\n***************\n*** 1 ****\n-old\n"
                  "[Diff truncated at 1 MiB]\n"))
    (with-temp-buffer
      (neo-diredy-test-diff text)
      (should (equal text (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (text-property-not-all (point-min) (point-max) 'line-prefix nil)))))

(ert-deftest neo-diredy-diff-display-refines-unicode-and-keeps-common-text ()
  (with-temp-buffer
    (let ((text "@@ -1,2 +1,2 @@\n-共通 old 値\n-unchanged\n+共通 new 値\n+unchanged\n"))
      (neo-diredy-test-diff text)
      (dotimes (_ 2)
        (goto-char (point-min))
        (search-forward "old")
        (should (memq 'diff-refine-removed (get-text-property (1- (point)) 'face)))
        (search-forward "new")
        (should (memq 'diff-refine-added (get-text-property (1- (point)) 'face)))
        (search-forward "値")
        (should (eq (get-text-property (1- (point)) 'face) 'diff-added))
        (search-forward "unchanged")
        (should (eq (get-text-property (1- (point)) 'face) 'diff-added))
        (neo-diredy--decorate-diff))
      (should (equal text (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest neo-diredy-diff-display-does-not-pair-unequal-blocks ()
  (with-temp-buffer
    (neo-diredy-test-diff "@@ -1 +1,2 @@\n-before\n+inserted\n+after\n")
    (goto-char (point-min))
    (search-forward "+after")
    (should (eq (get-text-property (1- (point)) 'face) 'diff-added))))
