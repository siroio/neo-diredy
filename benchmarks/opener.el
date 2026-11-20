;;; opener.el --- GUI opener display latency -*- lexical-binding: t; -*-
;; Run in a fresh GUI: emacs -Q -l benchmarks/opener.el (exits when done).
;; Optional environment: NEO_DIREDY_SOURCE, NEO_DIREDY_RESULT,
;; NEO_DIREDY_FONT, NEO_DIREDY_THEME, NEO_DIREDY_THEME_DIRECTORY.
;; Measures command entry through the first redisplay, excluding human input.
(setq native-comp-jit-compilation nil create-lockfiles nil make-backup-files nil
      inhibit-startup-screen t frame-inhibit-implied-resize t)
(require 'cl-lib)
(require 'json)
(defconst neo-diredy-opener-bench-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(defconst neo-diredy-opener-bench-source
  (or (getenv "NEO_DIREDY_SOURCE") (expand-file-name "neo-diredy.el" neo-diredy-opener-bench-root)))
(defconst neo-diredy-opener-bench-store
  (expand-file-name ".bench/opener-latency/" neo-diredy-opener-bench-root))
(make-directory neo-diredy-opener-bench-store t)
(setq default-directory neo-diredy-opener-bench-root
      temporary-file-directory neo-diredy-opener-bench-store
      frame-title-format "neo-diredy opener benchmark")
(load neo-diredy-opener-bench-source nil t)
(neo-diredy-mode -1)
(set-frame-size nil 140 46)
(when-let* ((font (getenv "NEO_DIREDY_FONT"))) (set-frame-font font nil t))
(when-let* ((directory (getenv "NEO_DIREDY_THEME_DIRECTORY")))
  (add-to-list 'custom-theme-load-path directory))
(when-let* ((theme (getenv "NEO_DIREDY_THEME"))) (load-theme (intern theme) t))

(defun neo-diredy-opener-bench-run ()
  "Measure twelve menu openings without starting an external application."
  (let* ((fixture (make-temp-file (expand-file-name "fixture-" neo-diredy-opener-bench-store) t))
         (file (expand-file-name "sample.txt" fixture))
         (neo-diredy-git-status nil) (neo-diredy-directory-sizes nil)
         (create (symbol-function 'make-frame)) (frames-created 0) samples)
    (unwind-protect
        (progn
          (with-temp-file file (insert "Opener benchmark fixture\n"))
          (dired fixture)
          (neo-diredy--layout-mode 1)
          (dired-goto-file file)
          (neo-diredy--post-command)
          (sit-for 0.3)
          (cl-letf (((symbol-function 'make-frame)
                     (lambda (&rest args) (cl-incf frames-created) (apply create args))))
            (dotimes (_ 12)
              (let ((start (float-time)))
                (cl-letf (((symbol-function 'read-key)
                           (lambda (&rest _)
                             (push (* 1000 (- (float-time) start)) samples)
                             'escape)))
                  (neo-diredy-open)))
              (sit-for 0.1)))
          (setq samples (nreverse samples))
          (with-temp-file (or (getenv "NEO_DIREDY_RESULT")
                              (expand-file-name "result.json" neo-diredy-opener-bench-store))
            (let ((json-encoding-pretty-print t))
              (insert (json-encode
                       `((emacs . ,emacs-version)
                         (source_sha256 . ,(with-temp-buffer (insert-file-contents-literally neo-diredy-opener-bench-source)
                                                             (secure-hash 'sha256 (current-buffer))))
                         (font . ,(frame-parameter nil 'font)) (theme . ,(getenv "NEO_DIREDY_THEME"))
                         (frames_created . ,frames-created)
                         (cold_ms . ,(car samples))
                         (warm_median_ms . ,(nth 5 (sort (copy-sequence (cdr samples)) #'<)))
                         (display_ms . ,(vconcat samples))))))))
      (neo-diredy-mode -1)
      (dolist (buffer (buffer-list))
        (when (with-current-buffer buffer (string-prefix-p (file-name-as-directory fixture) default-directory))
          (kill-buffer buffer)))
      (delete-directory fixture t))))

(run-at-time 1 nil (lambda ()
                    (condition-case err
                        (progn (neo-diredy-opener-bench-run) (kill-emacs 0))
                      (error (message "Opener benchmark failed: %S" err) (kill-emacs 1)))))
