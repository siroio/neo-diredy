;;; neo-diredy-reveal-tests.el --- Grouped Windows Reveal -*- lexical-binding: t; -*-
(load (expand-file-name "neo-diredy-actions-tests.el" (file-name-directory load-file-name)) nil t)

(ert-deftest neo-diredy-reveal-groups-by-parent-with-unicode-payload ()
  (neo-diredy-actions--with-files
   (let* ((other (expand-file-name "日本語 & %name% 😀.txt" root))
          (nested (expand-file-name "two.txt" child))
          (groups (neo-diredy--reveal-groups (list file other nested)))
          (payload (json-parse-string (neo-diredy--reveal-payload groups)
                                      :object-type 'alist :array-type 'list)))
     (should (= (length groups) 2))
     (should (equal (cdr (car groups)) (list file other)))
     (should (equal (alist-get 'files (car payload)) (list file other)))
     (should (equal (alist-get 'files (cadr payload)) (list nested))))))

(ert-deftest neo-diredy-reveal-results-distinguish-group-failure ()
  (neo-diredy-actions--with-files
   (let* ((nested (expand-file-name "two.txt" child))
          (groups (neo-diredy--reveal-groups (list file nested)))
          (json (json-encode (vector `((directory . ,(car (car groups))) (ok . t))
                                    `((directory . ,(car (cadr groups))) (ok . :json-false) (error . "Shell failure"))))))
     (neo-diredy--reveal-report groups json nil)
     (should (equal (mapcar #'cadr neo-diredy--action-results) '(done failed)))
     (should (equal (nth 3 (cadr neo-diredy--action-results)) "Shell failure")))))

(ert-deftest neo-diredy-reveal-malformed-output-is-not-success ()
  (neo-diredy-actions--with-files
   (dolist (output '("invalid JSON" "1" "{}" "[1]" "[\"invalid\"]"))
     (neo-diredy--reveal-report (neo-diredy--reveal-groups (list file)) output nil)
     (should (equal (mapcar #'cadr neo-diredy--action-results) '(failed))))))

(ert-deftest neo-diredy-reveal-real-missing-item-reports-and-cleans-up ()
  (skip-unless (eq system-type 'windows-nt))
  (neo-diredy-actions--with-files
   (let ((process-environment (copy-sequence process-environment))
         (missing (expand-file-name "missing-日本語😀.txt" root)))
     (setenv "TEMP" root) (setenv "TMP" root)
     (setq default-directory (file-name-as-directory (expand-file-name "作業😀" root)))
     (make-directory default-directory)
     (unwind-protect
         (progn
           (cl-letf (((symbol-function 'neo-diredy--action-targets) (lambda (&rest _) (list missing))))
             (neo-diredy-reveal))
           (let ((deadline (+ (float-time) 35)))
             (while (and neo-diredy--reveal-process (< (float-time) deadline)) (accept-process-output nil 0.05)))
           (should-not neo-diredy--reveal-process)
           (should (equal (mapcar #'cadr neo-diredy--action-results) '(failed)))
           (should-not (string-match-p "Spawning child process" (nth 3 (car neo-diredy--action-results))))
           (should-not (seq-find (lambda (buffer) (string-prefix-p " *neo-diredy-reveal" (buffer-name buffer)))
                                 (buffer-list))))
       (neo-diredy--reveal-stop)))))

(ert-deftest neo-diredy-reveal-real-helper-compiles-without-opening-explorer ()
  (skip-unless (eq system-type 'windows-nt))
  (neo-diredy-actions--with-files
   (let* ((process-environment (copy-sequence process-environment))
          (temporary-file-directory root)
          (command (neo-diredy--reveal-command))
          (output (generate-new-buffer " *reveal-helper-test*"))
          (errors (generate-new-buffer " *reveal-helper-test-errors*")) process)
     (setenv "TEMP" root) (setenv "TMP" root)
     (unwind-protect
         (progn
           (setq process (make-process :name "reveal-helper-test" :buffer output :stderr errors
                                       :command command :coding 'utf-8-unix :connection-type 'pipe :noquery t))
           ;; Empty input still compiles the actual helper; it never opens Explorer.
           (process-send-string process "[]") (process-send-eof process)
           (let ((deadline (+ (float-time) 30)))
             (while (and (process-live-p process) (< (float-time) deadline))
               (accept-process-output process 0.05)))
           (should-not (process-live-p process))
           (should (= (process-exit-status process) 0))
           (should (string-match-p "\\[\\]" (with-current-buffer output (buffer-string)))))
       (when (and process (process-live-p process)) (delete-process process))
       (kill-buffer output) (kill-buffer errors)))))

(ert-deftest neo-diredy-reveal-native-unicode-item-ids-without-opening-explorer ()
  (skip-unless (eq system-type 'windows-nt))
  (neo-diredy-actions--with-files
   (let* ((process-environment (copy-sequence process-environment))
          (directory (expand-file-name "日本語😀/" root))
          (files (mapcar (lambda (name) (expand-file-name name directory)) '("a.txt" "日本語 & % 😀.txt")))
          ;; Replace only the UI side effect. COM, JSON, Unicode name parsing,
          ;; PIDLs and native resource cleanup still use the real helper/APIs.
          (declaration "[DllImport(\"shell32.dll\")]\n  static extern int SHOpenFolderAndSelectItems(IntPtr folder, uint count, IntPtr[] children, uint flags);")
          (neo-diredy--reveal-script
           (replace-regexp-in-string
            (regexp-quote declaration)
            "static int SHOpenFolderAndSelectItems(IntPtr folder, uint count, IntPtr[] children, uint flags) { return count == 2 ? 0 : -2147024809; }"
            neo-diredy--reveal-script t t)))
     (should (not (string-match-p (regexp-quote declaration) neo-diredy--reveal-script)))
     (setenv "TEMP" root) (setenv "TMP" root)
     (make-directory directory)
     (dolist (path files) (with-temp-file path (insert "test")))
     (unwind-protect
         (progn
           (cl-letf (((symbol-function 'neo-diredy--action-targets) (lambda (&rest _) files)))
             (neo-diredy-reveal))
           (let ((deadline (+ (float-time) 35)))
             (while (and neo-diredy--reveal-process (< (float-time) deadline)) (accept-process-output nil 0.05)))
           (should (equal (mapcar #'cadr neo-diredy--action-results) '(done done))))
       (neo-diredy--reveal-stop)))))
