;;; neo-diredy-art-tests.el --- Art preview tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'sqlite)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(require 'neo-diredy)
(neo-diredy-mode -1)
(defconst neo-diredy-art-test-root (file-name-directory load-file-name))
(defmacro neo-diredy-art-test--with-files (&rest body)
  `(let* ((temporary-file-directory neo-diredy-art-test-root)
          (neo-diredy-git-status nil)
          (neo-diredy-directory-sizes nil)
          (directory (make-temp-file "yazi-test-art-" t)))
     (unwind-protect (progn ,@body)
       (when (fboundp 'neo-diredy--art-stop) (neo-diredy--art-stop))
       (delete-directory directory t))))
(defconst neo-diredy-art-test-png
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAYAAABytg0kAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAAQSURBVBhXY/jPwPCfARkAAB7zAf+x9MCaAAAAAElFTkSuQmCC"))

(ert-deftest neo-diredy-art-cleanup-retries-transient-lock ()
  (neo-diredy-art-test--with-files
   (let* ((owned (make-temp-file (expand-file-name "worker-" directory) t))
          (process (make-pipe-process :name "art-cleanup-test" :noquery t))
          (remove-directory (symbol-function 'delete-directory))
          (calls 0))
     (process-put process 'directory owned)
     (unwind-protect
         (cl-letf (((symbol-function 'delete-directory)
                    (lambda (path &rest args)
                      (if (and (equal path owned) (= (cl-incf calls) 1))
                          (signal 'file-error '("Simulated transient Windows lock"))
                        (apply remove-directory path args)))))
           (neo-diredy--art-cleanup process)
           (let ((deadline (+ (float-time) 3)))
             (while (and (file-directory-p owned) (< (float-time) deadline))
               (accept-process-output nil 0.05)))
           (should-not (file-directory-p owned))
           (should (= 2 calls)))
       (delete-process process)))))

(defun neo-diredy-art-test--clip-file (file directory)
  (let* ((db-file (expand-file-name "image.db" directory))
         (db (sqlite-open db-file))
         (png (copy-sequence neo-diredy-art-test-png)))
    (unwind-protect
        (progn
          (sqlite-execute db "CREATE TABLE CanvasPreview (ImageData BLOB)")
          (put-text-property 0 1 'coding-system 'binary png)
          (sqlite-execute db "INSERT INTO CanvasPreview VALUES (?)" (list png)))
      (sqlite-close db))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert "CSFCHUNK" (make-string 24 0))
      (insert-file-contents-literally db-file)
      (let ((coding-system-for-write 'no-conversion))
        (write-region (point-min) (point-max) file nil 'silent)))))

(ert-deftest neo-diredy-art-worker-clip ()
  (skip-unless (sqlite-available-p))
  (neo-diredy-art-test--with-files
   (let ((file (expand-file-name "日本語 image.clip" directory)) result)
     (neo-diredy-art-test--clip-file file directory)
     (neo-diredy--art-request file (lambda (value) (setq result value)))
     (neo-diredy-art-test--wait)
     (should (eq (car result) 'image))
     (should (equal (cdr result) neo-diredy-art-test-png)))))

(ert-deftest neo-diredy-art-worker-timeout ()
  (neo-diredy-art-test--with-files
   (let ((file (expand-file-name "image.psd" directory)) result
         (start-timer (symbol-function 'run-at-time)))
     (neo-psd-check-file file nil nil nil nil)
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (_seconds repeat callback &rest args)
                  (apply start-timer 0.001 repeat callback args))))
       (neo-diredy--art-request file (lambda (value) (setq result value))))
     (neo-diredy-art-test--wait)
     (should (eq (car result) 'error))
     (should (string-match-p "timed out" (cdr result)))
     ;; A transient failure must not poison retries of an unchanged file.
     (setq result nil)
     (neo-diredy--art-request file (lambda (value) (setq result value)))
     (should (processp neo-diredy--art-process))
     (neo-diredy-art-test--wait)
     (should (eq (car result) 'image)))))

(ert-deftest neo-diredy-art-worker-source-changed ()
  (neo-diredy-art-test--with-files
   (let ((file (expand-file-name "image.psd" directory)) result)
     (neo-psd-check-file file nil nil nil nil)
     (neo-diredy--art-request file (lambda (value) (setq result value)))
     (set-file-times file (time-add (current-time) 5))
     (neo-diredy-art-test--wait)
     (should (eq (car result) 'error))
     (should (string-match-p "File changed" (cdr result)))
     (should-not neo-diredy--art-cache))))

(ert-deftest neo-diredy-art-preview-real-images ()
  (skip-unless (display-images-p))
  (neo-diredy-art-test--with-files
   (let ((original (current-window-configuration)))
     (unwind-protect
         (progn
           (delete-other-windows)
           (dired directory)
           (neo-diredy--layout-mode 1)
           (dolist (extension '("psd" "psb" "clip"))
             (let ((file (expand-file-name (concat "image." extension) directory)))
               (if (equal extension "clip")
                   (neo-diredy-art-test--clip-file file directory)
                 (neo-psd-check-file file (equal extension "psb") t nil nil))
               (neo-diredy--render-preview file)
               (neo-diredy-art-test--wait)
               (with-current-buffer neo-diredy--preview
                 (let ((image (get-text-property (point-min) 'display)))
                   (should (eq (car image) 'image))
                   (should (equal (image-size image t) '(2 . 2))))))))
       (neo-diredy--layout-mode -1)
       (set-window-configuration original)
       (dolist (buffer (buffer-list))
         (when (with-current-buffer buffer (equal default-directory (file-name-as-directory directory)))
           (kill-buffer buffer)))))))
(defun neo-diredy-art-test--wait ()
  (let ((deadline (+ (float-time) 15)))
    (while (and neo-diredy--art-process (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should-not neo-diredy--art-process)))

(ert-deftest neo-diredy-art-worker-cache-and-change ()
  (neo-diredy-art-test--with-files
   (let ((file (expand-file-name "日本語 image.PSB" directory)) result)
     (neo-psd-check-file file t t nil nil)
     (neo-diredy--art-request file (lambda (value) (setq result value)))
     (should (processp neo-diredy--art-process))
     (neo-diredy-art-test--wait)
     (should (eq (car result) 'image))
     (should (string-prefix-p "P6\n2 2\n255\n" (cdr result)))
     (setq result nil)
     (neo-diredy--art-request file (lambda (value) (setq result value)))
     (should-not neo-diredy--art-process)
     (should (eq (car result) 'image))
     (set-file-times file (time-add (current-time) 3))
     (setq result nil)
     (neo-diredy--art-request file (lambda (value) (setq result value)))
     (should (processp neo-diredy--art-process))
     (neo-diredy-art-test--wait)
     (should (eq (car result) 'image)))))

(ert-deftest neo-diredy-art-worker-cancel-and-error ()
  (neo-diredy-art-test--with-files
   (let ((file (expand-file-name "bad.psd" directory)) result stale worker-directory)
     (with-temp-file file (insert "broken"))
     (neo-diredy--art-request file (lambda (value) (setq stale value)))
     (setq worker-directory (process-get neo-diredy--art-process 'directory))
     (neo-diredy--art-cancel)
     (should-not neo-diredy--art-process)
     (should-not stale)
     (neo-diredy--art-request file (lambda (value) (setq result value)))
     (neo-diredy-art-test--wait)
     (should (eq (car result) 'error))
     (should (string-match-p "Truncated" (cdr result)))
     (let ((deadline (+ (float-time) 3)))
       (while (and (file-directory-p worker-directory) (< (float-time) deadline))
         (accept-process-output nil 0.05)))
     (should-not (file-directory-p worker-directory)))))

(ert-deftest neo-diredy-art-revert-clears-cache ()
  (neo-diredy-art-test--with-files
   (let ((original (current-window-configuration)) buffer)
     (unwind-protect
         (progn
           (delete-other-windows)
           (dired directory)
           (setq buffer (current-buffer))
           (neo-diredy--layout-mode 1)
           (setq neo-diredy--art-cache '((key image . "old")))
           (revert-buffer)
           (should-not neo-diredy--art-cache))
       (neo-diredy--layout-mode -1)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (should-not (memq #'neo-diredy--after-readin dired-after-readin-hook)))
         (kill-buffer buffer))
       (set-window-configuration original)))))
(ert-deftest neo-diredy-art-composites ()
  (neo-diredy-art-test--with-files
   (dolist (psb '(nil t))
     (dolist (rle '(nil t))
       (dolist (gray '(nil t))
         (dolist (alpha '(nil t))
           (let ((file (expand-file-name "日本語 image.psd" directory)))
             (neo-psd-check-file file psb rle gray alpha)
             (let* ((image (neo-diredy--art-psd file))
                    (pixels (substring image (length "P6\n2 2\n255\n"))))
               (should (string-prefix-p "P6\n2 2\n255\n" image))
               (should (= (length pixels) 12))
               (should (= (aref pixels 0) (if alpha 192 (if gray 0 255))))
               (should (= (aref pixels 4) (if gray 64 255)))
               (should (= (aref pixels 11) (if alpha 223 255)))))))))))

(ert-deftest neo-diredy-art-malformed ()
  (dolist (bad '((0 1) (255) (3 1 2 3 4)))
    (should-error (neo-diredy--art-packbits (apply #'unibyte-string bad) 0 (length bad) 2)))
  (neo-diredy-art-test--with-files
   (let ((file (expand-file-name "bad.psd" directory)))
     (with-temp-file file (insert "truncated"))
     (should-error (neo-diredy--art-psd file)))))

(ert-deftest neo-diredy-art-clip-overflow-pages ()
  (skip-unless (sqlite-available-p))
  (neo-diredy-art-test--with-files
   (let* ((db-file (expand-file-name "fixture.db" directory))
          (file (expand-file-name "日本語 image.clip" directory))
          (png (concat (unibyte-string 137 80 78 71 13 10 26 10)
                       (make-string 20000 ?a)))
          (db (sqlite-open db-file)))
     (unwind-protect
         (progn (sqlite-execute db "CREATE TABLE CanvasPreview (ImageData BLOB)")
                (put-text-property 0 1 'coding-system 'binary png)
                (sqlite-execute db "INSERT INTO CanvasPreview VALUES (?)" (list png))
                (should (equal (caar (sqlite-select db "SELECT typeof(ImageData) FROM CanvasPreview")) "blob")))
       (sqlite-close db))
     (with-temp-buffer
       (set-buffer-multibyte nil)
       (insert (make-string (- (* 1024 1024) 8) 0))
       (insert-file-contents-literally db-file)
       (let ((coding-system-for-write 'no-conversion))
         (write-region (point-min) (point-max) file nil 'silent)))
     (let ((actual (neo-diredy--art-clip file)))
       (should (= (length actual) (length png)))
       (should (equal (substring actual 0 12) (substring png 0 12)))
       (should (equal (secure-hash 'sha256 actual) (secure-hash 'sha256 png))))
     (should-not (directory-files directory nil "^neo-clip-")))))

(ert-deftest neo-diredy-art-thumbnail-and-bounds ()
  (neo-diredy-art-test--with-files
   (dolist (psb '(nil t))
     (let* ((file (expand-file-name "thumbnail.psd" directory))
            (jpeg (unibyte-string 255 216 255 217))
            (thumbnail (concat (neo-psd-check-uint 1 4) (make-string 16 0)
                               (neo-psd-check-uint 4 4) (make-string 4 0) jpeg))
            (resource (concat "8BIM" (neo-psd-check-uint 1036 2) (make-string 2 0)
                              (neo-psd-check-uint (length thumbnail) 4) thumbnail)))
       (neo-psd-check-file file psb nil nil nil)
       (let ((header (neo-diredy--art-read file 0 30)))
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (insert header (neo-psd-check-uint (length resource) 4) resource)
           (let ((coding-system-for-write 'no-conversion))
             (write-region (point-min) (point-max) file nil 'silent))))
       ;; No layer/composite section exists: the thumbnail alone must suffice.
       (should (equal (neo-diredy--art-psd file) jpeg))
       (should-error (neo-diredy--art-read file 0 (* 33 1024 1024)))))))

(ert-deftest neo-diredy-art-no-sqlite ()
  (cl-letf (((symbol-function 'sqlite-available-p) (lambda () nil)))
    (should-error (neo-diredy--art-clip "unused.clip") :type 'error)))

(ert-deftest neo-diredy-art-preview-dispatch-and-stale-result ()
  (neo-diredy-art-test--with-files
   (let* ((file (expand-file-name "large.CLIP" directory))
          (neo-diredy--preview (generate-new-buffer " *art-ui-test*"))
          (neo-diredy--right (selected-window))
          (neo-diredy--preview-key '(first))
          (original (window-buffer))
          callback)
     (unwind-protect
         (progn
           (with-temp-file file (insert "not interpreted as text"))
           (set-window-buffer neo-diredy--right neo-diredy--preview)
           (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
                     ((symbol-function 'neo-diredy--art-request)
                      (lambda (_file done) (setq callback done))))
             (neo-diredy--insert-preview file (file-attributes file)))
           (should callback)
           (with-current-buffer neo-diredy--preview
             (should (string-match-p "Loading" (buffer-string))))
           (setq neo-diredy--preview-key '(second))
           (funcall callback '(error . "stale"))
           (with-current-buffer neo-diredy--preview
             (should-not (string-match-p "stale" (buffer-string))))
           (setq neo-diredy--preview-key '(first))
           (funcall callback '(error . "unsupported"))
           (with-current-buffer neo-diredy--preview
             (should (string-match-p "Preview unavailable: unsupported" (buffer-string)))))
       (set-window-buffer neo-diredy--right original)
       (kill-buffer neo-diredy--preview)))))

(ert-deftest neo-diredy-art-cache-is-bounded ()
  (neo-diredy-art-test--with-files
   (dotimes (i 10)
     (let ((file (expand-file-name (format "%d.psd" i) directory)))
       (neo-psd-check-file file nil nil nil nil)
       (neo-diredy--art-request file #'ignore)
       (neo-diredy-art-test--wait)))
   (should (= 8 (length neo-diredy--art-cache)))
   (neo-diredy--art-stop)
   (should-not neo-diredy--art-cache)))
(defun neo-psd-check-uint (value length)
  (apply #'unibyte-string
         (mapcar (lambda (i)
                   (logand 255 (ash value (- (* i 8)))))
                 (reverse (number-sequence 0 (1- length))))))

(defun neo-psd-check-file (file psb rle grayscale alpha)
  (let* ((planes (if grayscale
                     (list (unibyte-string 0 64 128 255))
                   (list (unibyte-string 255 0 0 255) (unibyte-string 0 255 0 255)
                         (unibyte-string 0 0 255 255))))
         (length-size (if psb
                          8
                        4))
         (channels (+ (length planes) (if alpha
                                          1
                                        0)))
         (layers (if alpha
                     (concat (neo-psd-check-uint 2 length-size)
                             (unibyte-string 255 255))
                   ""))
         (coding-system-for-write 'no-conversion)
         rows)
    (when alpha
      (setq planes (append planes (list (unibyte-string 0 255 255 128)))))
    (dolist (plane planes)
      (dotimes (y 2)
        (push (concat (unibyte-string 128 1) (substring plane (* 2 y) (+ 2 (* 2 y)))) rows)))
    (setq rows (nreverse rows))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert "8BPS" (neo-psd-check-uint (if psb
                                             2
                                           1) 2) (make-string 6 0)
                                           (neo-psd-check-uint channels 2) (neo-psd-check-uint 2 4) (neo-psd-check-uint 2 4)
                                           (neo-psd-check-uint 8 2) (neo-psd-check-uint (if grayscale
                                                                                            1
                                                                                          3) 2)
                                           (make-string 8 0) (neo-psd-check-uint (length layers) length-size) layers
                                           (neo-psd-check-uint (if rle
                                                                   1
                                                                 0) 2)
                                           (if rle
                                               (concat (apply #'concat (mapcar (lambda (row)
                                                                                 (neo-psd-check-uint (length row) (if psb
                                                                                                                      4
                                                                                                                    2))) rows))
                                                       (apply #'concat rows))
                                             (apply #'concat planes))))))
