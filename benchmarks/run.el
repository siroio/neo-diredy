;;; run.el --- Reproducible Dired workload -*- lexical-binding: t; -*-
;; emacs -Q --batch -l benchmarks/run.el
;; Optional: NEO_DIREDY_SOURCE (baseline .el), NEO_DIREDY_RESULT (JSON).
(require 'benchmark)
(require 'json)
(require 'cl-lib)
(defconst neo-diredy-bench-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(defconst neo-diredy-bench-source
  (or (getenv "NEO_DIREDY_SOURCE") (expand-file-name "neo-diredy.el" neo-diredy-bench-root)))
(load neo-diredy-bench-source nil t)
(neo-diredy-mode -1)

(defun neo-diredy-bench-measure (count function)
  "Measure COUNT repetitions of FUNCTION with normal garbage collection."
  (garbage-collect)
  (let ((result (benchmark-call function count)))
    `((seconds . ,(nth 0 result)) (gcs . ,(nth 1 result)))))

(let* ((store (expand-file-name ".bench" neo-diredy-bench-root))
       (_ (make-directory store t))
       (root (make-temp-file (expand-file-name "fixture-" store) t))
       (sample (expand-file-name "file-0000.txt" root))
       results)
  (unwind-protect
      (progn
        (dotimes (index 2000)
          (with-temp-file (expand-file-name (format "file-%04d.txt" index) root)
            (insert "Preview text for the directory benchmark.\n")))
        (dotimes (index 100)
          (make-directory (expand-file-name (format "dir-%03d" index) root)))
        (dired root)
        (neo-diredy--layout-mode 1)
        (dired-goto-file sample)
        (neo-diredy--refresh)
        (push (cons 'cursor_200
                    (neo-diredy-bench-measure
                     200 (lambda () (dired-next-line 1) (neo-diredy--post-command)))) results)
        (push (cons 'status_1000
                    (neo-diredy-bench-measure 1000 #'neo-diredy--status)) results)
        (push (cons 'marks_40
                    (neo-diredy-bench-measure
                     40 (lambda () (dired-mark 1) (neo-diredy--style)))) results)
        (push (cons 'listing_10
                    (neo-diredy-bench-measure
                     10 (lambda () (with-temp-buffer (neo-diredy--listing root))))) results)
        (dired-goto-file sample)
        (neo-diredy--refresh)
        (push (cons 'repeat_preview_100
                    (neo-diredy-bench-measure 100 #'neo-diredy--refresh)) results)
        (neo-diredy--layout-mode -1)
        (push (cons 'large_tree_open_close_3
                    (neo-diredy-bench-measure
                     3 (lambda () (neo-diredy--layout-mode 1)
                          (neo-diredy--layout-mode -1)))) results)
        ;; Real source-tree workload, read-only; no user configuration is loaded.
        (let ((source-dir (expand-file-name "../emacs.d/lisp" neo-diredy-bench-root)))
          (when (file-directory-p source-dir)
            (dired source-dir)
            (push (cons 'source_tree_open_close_10
                        (neo-diredy-bench-measure
                         10 (lambda () (neo-diredy--layout-mode 1)
                              (neo-diredy--layout-mode -1)))) results)))
        (let ((json-encoding-pretty-print t)
              (report `((emacs . ,emacs-version)
                        (source_sha256 . ,(with-temp-buffer
                                           (insert-file-contents-literally neo-diredy-bench-source)
                                           (secure-hash 'sha256 (current-buffer))))
                        (entries . 2100) (results . ,(nreverse results)))))
          (princ (json-encode report)) (terpri)
          (when-let* ((output (getenv "NEO_DIREDY_RESULT")))
            (with-temp-file output (insert (json-encode report)) (insert "\n")))))
    (when neo-diredy--layout-mode (neo-diredy--layout-mode -1))
    (dolist (buffer (buffer-list))
      (when (with-current-buffer buffer
              (string-prefix-p (file-name-as-directory root) default-directory))
        (kill-buffer buffer)))
    (delete-directory root t)))
