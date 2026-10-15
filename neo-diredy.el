;;; neo-diredy.el --- A small three-column Dired view -*- lexical-binding: t; -*-

;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: files, convenience
;; URL: https://github.com/siroio/neo-diredy
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Loading this package enables Yazi-style Dired automatically.
;; Dired and all other required libraries are bundled with Emacs.
;; One layout at a time.  The center remains an ordinary Dired buffer.
;; Auxiliary buffers never visit files or run their local variables/hooks.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'dired)
(require 'hl-line)
(require 'image)
(require 'sqlite)
(require 'tab-bar)
(declare-function dired-preview-mode "dired-preview" (&optional arg))
(declare-function wdired-get-filename "wdired" (&optional no-dir old))

(defgroup neo-diredy nil "Three-column Dired view." :group 'dired)
(defcustom neo-diredy-preview-bytes (* 64 1024)
  "Maximum bytes read for a text preview."
  :type 'natnum :group 'neo-diredy)
(defcustom neo-diredy-image-bytes (* 8 1024 1024)
  "Maximum file size for inline image previews."
  :type 'natnum :group 'neo-diredy)
(defcustom neo-diredy-directory-sizes t
  "Calculate the selected directory's total size in a background Emacs."
  :type 'boolean :group 'neo-diredy)
(defcustom neo-diredy-git-status t
  "Read Git status asynchronously when Git is available."
  :type 'boolean :group 'neo-diredy)
(defface neo-diredy-selection
  '((t (:inherit highlight :extend t))) "Selected file row.")

(defvar neo-diredy--layout-mode nil)
(defvar neo-diredy-mode nil)
(defvar neo-diredy--center nil)
(defvar neo-diredy--left nil)
(defvar neo-diredy--right nil)
(defvar neo-diredy--parent nil)
(defvar neo-diredy--preview nil)
(defvar neo-diredy--configuration nil)
(defvar neo-diredy--windows nil)
(defvar neo-diredy--handoff nil
  "Selected window, buffer and point held until navigation after a handoff.")
(defvar neo-diredy--buffers nil)
(defvar neo-diredy--overlays nil)
(defvar neo-diredy--timer nil)
(defvar neo-diredy--key nil)
(defvar neo-diredy--directory nil)
(defvar neo-diredy--decorated nil)
(defvar neo-diredy--preview-key nil)
(defconst neo-diredy--library (or load-file-name buffer-file-name))
(defvar neo-diredy--art-process nil)
(defvar neo-diredy--art-cache nil)
(defvar neo-diredy--metadata-jobs nil)
(defvar neo-diredy--size-cache nil)
(defvar neo-diredy--git-location nil)
(defvar neo-diredy--git-state nil)
(defvar neo-diredy--margins nil)
(defvar-local neo-diredy--file-sizes nil)
(defvar-local neo-diredy--listing-dirty t)
(defvar-local neo-diredy--mark-tick nil)
(defvar-local neo-diredy--mark-count 0)
(defvar w32-get-true-file-attributes)
(defconst neo-diredy--layout-commands
  '(tab-bar-new-tab tab-bar-select-tab tab-bar-close-tab neo-git--layout)
  "Functions that must capture a clean window configuration.
Neo Git creates its layout asynchronously, so its entry command is too early.
Advising its layout function does not load or require the optional package.")
(defconst neo-diredy--variables
  '(header-line-format mode-line-format truncate-lines cursor-type
		       display-line-numbers hl-line-face dired-hide-details-mode hl-line-mode))

(defun neo-diredy--icon (_file directory)
  "Return a simple label for a file of type DIRECTORY."
  (propertize (if directory "▸ " "· ")
              'face (if directory 'dired-directory 'shadow)))

(defun neo-diredy--changed (begin end old-length)
  "Invalidate decoration for listing edits, but not single-column marks."
  (unless (and (<= old-length 1) (<= (- end begin) 1)
               (= begin (save-excursion (goto-char begin) (line-beginning-position)))
               (not (and (> end begin) (eq (char-before end) ?\n))))
    (setq neo-diredy--listing-dirty t
          neo-diredy--preview-key nil
          neo-diredy--directory nil)))

(defun neo-diredy--after-readin ()
  "Invalidate anchors after a whole listing is replaced, including WDired."
  (setq neo-diredy--decorated nil neo-diredy--listing-dirty t)
  (neo-diredy--art-stop)
  (neo-diredy--metadata-stop))

(defun neo-diredy--original-file ()
  "Return the file on this row, retaining its original name during edits."
  (if (derived-mode-p 'wdired-mode)
      (ignore-errors (wdired-get-filename nil t))
    (dired-get-filename nil t)))

(defun neo-diredy--style ()
  "Temporarily style the current Dired buffer, saving its original state."
  (unless (assq (current-buffer) neo-diredy--buffers)
    (push (list (current-buffer)
                (mapcar (lambda (variable)
                          (list variable (local-variable-p variable)
                                (symbol-value variable)))
                        neo-diredy--variables)
                (bound-and-true-p dired-preview-mode))
          neo-diredy--buffers)
    (setq neo-diredy--listing-dirty t)
    (add-hook 'after-change-functions #'neo-diredy--changed nil t)
    (add-hook 'dired-after-readin-hook #'neo-diredy--after-readin nil t)
    (when (bound-and-true-p dired-preview-mode) (dired-preview-mode -1)))
  (setq-local truncate-lines t
              cursor-type (when (derived-mode-p 'wdired-mode) (or cursor-type t))
              display-line-numbers nil
              hl-line-face 'neo-diredy-selection
              header-line-format '(:eval (concat "  " (abbreviate-file-name default-directory)))
              mode-line-format
              (if (derived-mode-p 'wdired-mode)
                  '("  WDIRED   " (:eval (neo-diredy--status)) "   C-c C-c: finish   C-c ESC: cancel")
                '("  DIRED   " (:eval (neo-diredy--status)) "   M-x neo-diredy-mode: off")))
  (dired-hide-details-mode 1)
  (when (window-live-p neo-diredy--center)
    (let ((width (if (>= (window-total-width neo-diredy--center) 32) 16 0)))
      (unless (= (or (cdr (window-margins neo-diredy--center)) 0) width)
        (set-window-margins neo-diredy--center (car neo-diredy--margins) width))))
  (hl-line-mode 1)
  (hl-line-highlight)
  (when (overlayp hl-line-overlay)
    (overlay-put hl-line-overlay 'face 'neo-diredy-selection))
  (when (and (or neo-diredy--listing-dirty
                 (not (eq (car neo-diredy--decorated) (current-buffer))))
             ;; Keep filename anchors while editing, including an empty name.
             ;; Rebuild from the completed listing after finish or abort.
             (or (not (derived-mode-p 'wdired-mode))
                 (not (eq (car neo-diredy--decorated) (current-buffer)))))
    (setq neo-diredy--decorated
          (list (current-buffer))
          neo-diredy--listing-dirty nil)
    (mapc #'delete-overlay neo-diredy--overlays)
    (setq neo-diredy--overlays nil)
    (setq neo-diredy--file-sizes (make-hash-table :test #'equal))
    (let ((directories (make-hash-table :test #'equal)))
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (let ((file (neo-diredy--original-file)))
            (if (and file (not (member (file-name-nondirectory file) '("." ".."))))
		(when (or (dired-move-to-filename)
                          (and (derived-mode-p 'wdired-mode) (progn (end-of-line) t)))
		  (let ((overlay (make-overlay (point) (point))))
                    (neo-diredy--read-sizes (file-name-directory file) directories)
                    (overlay-put overlay 'neo-diredy-file file)
                    (overlay-put overlay 'neo-diredy-directory
				 (eq (gethash file neo-diredy--file-sizes) 'directory))
                    (overlay-put overlay 'before-string
				 (neo-diredy--icon file (eq (char-after (+ (line-beginning-position) 2)) ?d)))
                    (push overlay neo-diredy--overlays)))
              (let ((overlay (make-overlay (line-beginning-position)
					   (min (point-max) (1+ (line-end-position))))))
		(overlay-put overlay 'invisible t)
		(push overlay neo-diredy--overlays))))
	  (forward-line 1))))
    (neo-diredy--metadata-paint)))

(defun neo-diredy--status ()
  "Return the current filename and marked entry count."
  (unless (eq neo-diredy--mark-tick (buffer-chars-modified-tick))
    (setq neo-diredy--mark-count 0
          neo-diredy--mark-tick (buffer-chars-modified-tick))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*" nil t) (cl-incf neo-diredy--mark-count))))
  (let ((file (neo-diredy--original-file)))
    (format "%s  %s  %s  |  %d marked%s"
            (or (dired-get-filename 'no-dir t) "")
            (neo-diredy--size-label file) (neo-diredy--git-label file)
            neo-diredy--mark-count
            (if (car neo-diredy--git-state)
                (concat "  |  Git: " (car neo-diredy--git-state)) ""))))

;;; Size and Git metadata (display strings never enter the Dired listing)

(defun neo-diredy--read-sizes (directory seen)
  "Cache attributes for DIRECTORY once per listing pass, tracked by SEEN."
  (unless (gethash directory seen)
    (puthash directory t seen)
    (let ((w32-get-true-file-attributes nil))
      (dolist (entry (ignore-errors
                       (directory-files-and-attributes directory t nil t 'integer)))
        (puthash (car entry)
                 (pcase (file-attribute-type (cdr entry))
                   ('t 'directory)
                   ((pred stringp) 'link)
                   (_ (file-attribute-size (cdr entry))))
                 neo-diredy--file-sizes)))))

(defun neo-diredy--size-label (file)
  "Return cached size text for FILE; never perform I/O during redisplay."
  (let* ((job (cdr (assq 'size neo-diredy--metadata-jobs)))
         (value (and neo-diredy--file-sizes (gethash file neo-diredy--file-sizes)))
         (size (if (eq value 'directory)
                   (or (cdr (assoc file neo-diredy--size-cache))
                       (if (and job (equal file (process-get job 'key))) "…" "—"))
                 value)))
    (cond ((numberp size) (if (< size 1024) (format "%d B" size)
                            (file-size-human-readable size 'iec " ")))
          ((stringp size) size)
          ((eq size 'link) "link")
          (t "—"))))

(defun neo-diredy--git-label (file)
  "Return the cached two-column Git state for FILE."
  (or (and (cdr neo-diredy--git-state)
           (or (gethash file (cdr neo-diredy--git-state))
               ;; Git's normal mode collapses wholly untracked directories.
               (let ((parent file) found)
                 (while (and parent (not found))
                   (let ((next (directory-file-name (file-name-directory parent))))
                     (setq parent (unless (equal parent next) next)))
                   (when (and parent
                              (equal (gethash (file-name-as-directory parent)
                                              (cdr neo-diredy--git-state)) "??"))
                     (setq found "??")))
                 found))) "  "))

(defun neo-diredy--git-conflict-p (xy)
  "Return non-nil for an unmerged porcelain state XY."
  (member xy '("DD" "AU" "UD" "UA" "DU" "AA" "UU")))

(defun neo-diredy--metadata-paint ()
  "Update only our display strings, preserving WDired edits and anchors."
  (dolist (overlay neo-diredy--overlays)
    (when-let* ((buffer (overlay-buffer overlay))
                (file (overlay-get overlay 'neo-diredy-file)))
      (with-current-buffer buffer
        (let* ((git (neo-diredy--git-label file))
               (label (concat (propertize (format "%11s " (neo-diredy--size-label file)) 'face 'shadow)
                              (propertize git 'face (if (neo-diredy--git-conflict-p git) 'error 'font-lock-warning-face))
                              "  ")))
          (overlay-put overlay 'before-string
                       (concat (propertize " " 'display `((margin right-margin) ,label))
                               (neo-diredy--icon file (overlay-get overlay 'neo-diredy-directory))))))))
  (force-mode-line-update))

(defun neo-diredy--metadata-cancel (kind)
  "Cancel and release the job of KIND without invoking its callback."
  (when-let* ((process (cdr (assq kind neo-diredy--metadata-jobs))))
    (setq neo-diredy--metadata-jobs (assq-delete-all kind neo-diredy--metadata-jobs))
    (when-let* ((timer (process-get process 'timer))) (cancel-timer timer))
    (set-process-sentinel process #'ignore)
    (when (process-live-p process) (delete-process process))
    (when (buffer-live-p (process-buffer process)) (kill-buffer (process-buffer process)))))

(defun neo-diredy--metadata-stop ()
  "Cancel jobs and invalidate metadata after rereading or closing a layout."
  (mapc #'neo-diredy--metadata-cancel '(size git))
  (setq neo-diredy--size-cache nil neo-diredy--git-location nil neo-diredy--git-state nil))

(defun neo-diredy--metadata-start (kind key command callback)
  "Run COMMAND for KIND and KEY, passing bounded output or nil to CALLBACK."
  (neo-diredy--metadata-cancel kind)
  (let ((buffer (generate-new-buffer " *neo-diredy-metadata*")))
    (condition-case nil
        (let ((process
               (make-process
                :name "neo-diredy-metadata" :buffer buffer :command command
                :coding 'utf-8-unix :connection-type 'pipe :noquery t
                :filter (lambda (process text)
                          (with-current-buffer (process-buffer process)
                            (if (> (+ (buffer-size) (length text)) (* 8 1024 1024))
                                (progn (process-put process 'failed t) (delete-process process))
                              (goto-char (point-max)) (insert text))))
                :sentinel
                (lambda (process _event)
                  (when (and (memq (process-status process) '(exit signal))
                             (eq process (cdr (assq kind neo-diredy--metadata-jobs))))
                    (let ((output (when (and (zerop (process-exit-status process))
                                             (not (process-get process 'failed)))
                                    (with-current-buffer buffer (buffer-string)))))
                      (neo-diredy--metadata-cancel kind)
                      (funcall callback output)))))))
          (process-put process 'key key)
          (push (cons kind process) neo-diredy--metadata-jobs)
          (process-put process 'timer
                       (run-at-time 30 nil
                                    (lambda ()
                                      (when (process-live-p process)
                                        (process-put process 'failed t)
                                        (delete-process process))))))
      (error (when (buffer-live-p buffer) (kill-buffer buffer)) (funcall callback nil)))))

(defun neo-diredy--directory-total (directory)
  "Sum regular file bytes below DIRECTORY without following symbolic links.
Any unreadable entry fails the total instead of reporting a misleading sum."
  (let ((pending (list directory)) (total 0) (w32-get-true-file-attributes nil))
    (while pending
      (let ((next (pop pending)))
        (unless (file-symlink-p next)
          (dolist (entry (directory-files-and-attributes next t nil t 'integer))
            (unless (member (file-name-nondirectory (car entry)) '("." ".."))
              (unless (cdr entry) (error "Cannot read file attributes"))
              (pcase (file-attribute-type (cdr entry))
                ('t (push (car entry) pending))
                ('nil (when (file-regular-p (car entry))
                        (cl-incf total (file-attribute-size (cdr entry)))))))))))
    total))

(defun neo-diredy--size-worker ()
  "Print a directory total from the private child-process environment."
  (condition-case nil
      (princ (neo-diredy--directory-total (getenv "NEO_DIREDY_SIZE_DIRECTORY")))
    (error (kill-emacs 1))))

(defun neo-diredy--git-aggregate (states root path xy)
  "Add XY for PATH and its ancestors below ROOT into STATES."
  (while (and path (not (equal path (directory-file-name root))))
    (let ((old (gethash path states "  ")) (merged (copy-sequence xy)))
      (dotimes (i 2)
        (aset merged i (cond ((= (aref old i) ?\s) (aref xy i))
                             ((= (aref xy i) ?\s) (aref old i))
                             ((= (aref old i) (aref xy i)) (aref xy i))
                             (t ?*))))
      (puthash path (cond ((neo-diredy--git-conflict-p old) old)
                          ((neo-diredy--git-conflict-p xy) xy)
                          ;; Independent additions must not fabricate AA, etc.
                          ((neo-diredy--git-conflict-p merged) "**")
                          (t merged)) states))
    (let ((parent (directory-file-name (file-name-directory path))))
      (setq path (unless (equal parent path) parent)))))

(defun neo-diredy--git-parse (root output)
  "Parse Git porcelain v1 NUL OUTPUT relative to ROOT, aggregating parents."
  (let ((records (split-string output "\0" t))
        (states (make-hash-table :test #'equal)) branch)
    (when (and records (string-prefix-p "## " (car records)))
      (setq branch (substring (pop records) 3)))
    (while records
      (let* ((record (pop records)) (xy (substring record 0 2))
             (name (substring record 3))
             (path (directory-file-name (expand-file-name name root))))
        ;; A slash key identifies a truly collapsed untracked directory, as
        ;; opposed to a parent summary containing some untracked descendants.
        (when (and (equal xy "??") (string-suffix-p "/" name))
          (puthash (file-name-as-directory path) "??" states))
        ;; With -z, destination precedes source and neither name is quoted.
        (when (string-match-p "[RC]" xy)
          (let ((source (pop records)))
            (when (and source (string-match-p "R" xy))
              (neo-diredy--git-aggregate
               states root (directory-file-name (file-name-directory
                                                 (expand-file-name source root))) xy))))
        (neo-diredy--git-aggregate states root path xy)))
    (cons branch states)))

(defun neo-diredy--metadata-refresh (file)
  "Start required background jobs for the current directory and selected FILE."
  (unless (equal default-directory neo-diredy--git-location)
    (neo-diredy--metadata-cancel 'git)
    (setq neo-diredy--git-location default-directory neo-diredy--git-state nil)
    (neo-diredy--metadata-paint)
    (when-let* ((git (and neo-diredy-git-status (executable-find "git"))))
      (let ((directory default-directory)
            (process-environment (copy-sequence process-environment)))
        (setenv "GIT_OPTIONAL_LOCKS" "0")
        (neo-diredy--metadata-start
         'git directory (list git "-C" directory "rev-parse" "--show-toplevel")
         (lambda (root)
           (when root
             (setq root (string-trim-right root "[\r\n]+"))
             (let ((process-environment (copy-sequence process-environment)))
               (setenv "GIT_OPTIONAL_LOCKS" "0")
               (neo-diredy--metadata-start
                'git directory
                (list git "-C" root "status" "--porcelain=v1" "-z" "--branch" "--untracked-files=normal")
                (lambda (output)
                  (when output (setq neo-diredy--git-state (neo-diredy--git-parse root output)))
                  (neo-diredy--metadata-paint))))))))))
  (let ((job (cdr (assq 'size neo-diredy--metadata-jobs))))
    (when (and job (not (equal file (process-get job 'key))))
      (neo-diredy--metadata-cancel 'size))
    (when (and neo-diredy-directory-sizes file
               (eq (gethash file neo-diredy--file-sizes) 'directory)
               (not (assoc file neo-diredy--size-cache))
               (not (assq 'size neo-diredy--metadata-jobs)))
      (let ((process-environment (copy-sequence process-environment)))
        (setenv "NEO_DIREDY_SIZE_DIRECTORY" file)
        (neo-diredy--metadata-start
         'size file (list (expand-file-name invocation-name invocation-directory)
                          "-Q" "--batch" "-l" neo-diredy--library
                          "--eval" "(neo-diredy--size-worker)")
         (lambda (output)
           (push (cons file (if (and output (string-match-p "\\`[0-9]+\\'" output))
                                (string-to-number output) "!")) neo-diredy--size-cache)
           (when (> (length neo-diredy--size-cache) 64)
             (setcdr (nthcdr 63 neo-diredy--size-cache) nil))
           (neo-diredy--metadata-paint))))
      (neo-diredy--metadata-paint))))

(defun neo-diredy--listing (directory &optional selected)
  "Insert a compact listing of DIRECTORY, highlighting SELECTED."
  (let ((files (let ((w32-get-true-file-attributes nil))
                 (directory-files-and-attributes directory t nil t 'integer)))
        entries)
    (dolist (entry files)
      (let* ((file (car entry)) (type (file-attribute-type (cdr entry)))
             (name (file-name-nondirectory file)))
        (unless (member name '("." ".."))
          (push (list file (or (eq type t) (and (stringp type) (file-directory-p file)))
                      (downcase name)) entries))))
    (setq entries (sort entries (lambda (a b)
                                  (if (eq (cadr a) (cadr b))
                                      (string-lessp (caddr a) (caddr b))
                                    (cadr a)))))
    (dolist (entry (seq-take entries 500))
      (let* ((file (car entry)) (start (point)))
        (insert (neo-diredy--icon file (cadr entry)) (file-name-nondirectory file) "\n")
        (when (cadr entry) (add-face-text-property start (1- (point)) 'dired-directory))
        (when (and selected (equal (directory-file-name selected) file))
          (add-face-text-property start (point) 'neo-diredy-selection)
          (set-window-point neo-diredy--left start))))
    (when (> (length entries) 500) (insert "… first 500 entries\n"))))

(defun neo-diredy--file-info (file)
  "Return resolved name and target attributes for local FILE."
  (when (and file (not (file-remote-p file)))
    (let ((attributes (file-attributes file 'integer)))
      (if (stringp (file-attribute-type attributes))
          (let ((target (file-truename file)))
            (cons target (file-attributes target 'integer)))
        (cons file attributes)))))

(defun neo-diredy--render-preview (file)
  "Reuse FILE's preview until metadata, size limits or window size change."
  (let* ((info (ignore-errors (neo-diredy--file-info file)))
         (attributes (cdr info))
         (key (list file (car info) (file-attribute-modification-time attributes)
                    (file-attribute-status-change-time attributes)
                    (file-attribute-size attributes)
                    (window-body-width neo-diredy--right t)
                    (window-body-height neo-diredy--right t)
                    neo-diredy-preview-bytes neo-diredy-image-bytes)))
    (unless (equal key neo-diredy--preview-key)
      (neo-diredy--art-cancel)
      (setq neo-diredy--preview-key key)
      (neo-diredy--insert-preview file attributes))))

(defun neo-diredy--insert-preview (file attributes)
  "Render FILE without visiting it or evaluating file-local code."
  (with-current-buffer neo-diredy--preview
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq header-line-format (concat "  " (if file (file-name-nondirectory file) "Preview")))
      (condition-case error-data
          (cond
           ((null file) (insert "No file selected"))
           ((file-remote-p file) (insert "Remote preview disabled"))
           ((file-directory-p file) (neo-diredy--listing file))
           ((not (file-regular-p file)) (insert "No preview for this file type"))
           ((member (downcase (or (file-name-extension file) "")) '("clip" "psd" "psb"))
            (if (not (display-images-p))
                (insert "Art preview requires a graphical Emacs frame")
              (insert "Loading art preview…")
              (let ((buffer (current-buffer))
                    (window neo-diredy--right)
                    (key neo-diredy--preview-key))
                (neo-diredy--art-request
                 file
                 (lambda (result)
                   (when (and (equal key neo-diredy--preview-key)
                              (buffer-live-p buffer) (window-live-p window)
                              (eq (window-buffer window) buffer))
                     (with-current-buffer buffer
                       (let ((inhibit-read-only t))
                         (erase-buffer)
                         (condition-case err
                             (if (eq (car result) 'image)
                                 (insert-image
                                  (create-image (cdr result) nil t
                                                :max-width (window-body-width window t)
                                                :max-height (window-body-height window t)))
                               (insert "Preview unavailable: " (cdr result)))
                           (error (insert "Preview unavailable: " (error-message-string err))))
                         (goto-char (point-min))
                         (set-buffer-modified-p nil)))))))))
           (t
            (let* ((size (file-attribute-size attributes))
                   (type (ignore-errors (image-type file))))
              (cond
               ((and type (display-images-p) (image-type-available-p type)
                     (<= size neo-diredy-image-bytes))
                (insert-image (create-image file type nil
                                            :max-width (window-body-width neo-diredy--right t)
                                            :max-height (window-body-height neo-diredy--right t))))
               (type (insert (format "Image: %s (%s)\nInline preview unavailable or over size limit"
                                     type (file-size-human-readable size))))
               (t
                (insert-file-contents file nil 0 (min size neo-diredy-preview-bytes))
                (if (save-excursion (goto-char (point-min)) (search-forward "\0" nil t))
                    (progn (erase-buffer) (insert (format "Binary file (%s)" (file-size-human-readable size))))
                  (when (> size neo-diredy-preview-bytes)
                    (goto-char (point-max)) (insert "\n… preview truncated"))))))))
        (error (setq neo-diredy--preview-key nil)
               (erase-buffer) (insert (error-message-string error-data)))))
    (goto-char (point-min))
    (set-buffer-modified-p nil))
  (set-window-start neo-diredy--right 1)
  (set-window-point neo-diredy--right 1))

(defun neo-diredy--refresh ()
  "Refresh the active layout; do nothing in another frame or tab."
  (when (and neo-diredy--layout-mode (neo-diredy--layout-intact-p)
             (eq (window-frame neo-diredy--center) (selected-frame)))
    (with-current-buffer (window-buffer neo-diredy--center)
      (when (and (derived-mode-p 'dired-mode 'wdired-mode)
                 (not (file-remote-p default-directory)))
        (neo-diredy--style)
        (unless (equal default-directory neo-diredy--directory)
          (setq neo-diredy--directory default-directory)
          (let ((directory default-directory))
            (with-current-buffer neo-diredy--parent
              (let ((inhibit-read-only t))
                (erase-buffer)
                (setq header-line-format "  Parent")
                (condition-case err
                    (neo-diredy--listing (file-name-directory (directory-file-name directory)) directory)
                  (error (insert (error-message-string err))))
                (set-buffer-modified-p nil)))))
        (neo-diredy--metadata-refresh (neo-diredy--original-file))
        (neo-diredy--render-preview (neo-diredy--original-file))
        (force-mode-line-update)))))

(defun neo-diredy--layout-intact-p ()
  "Return non-nil while the saved layout still belongs to neo-diredy."
  (and (window-live-p neo-diredy--center)
       (let ((windows (window-list (window-frame neo-diredy--center) 'no-minibuffer)))
         (and (= (length windows) (length neo-diredy--windows))
              (cl-every
               (lambda (entry)
                 (and (memq (car entry) windows)
                      (if (eq (car entry) neo-diredy--center)
                          (with-current-buffer (window-buffer (car entry))
                            (and (derived-mode-p 'dired-mode 'wdired-mode)
                                 (not (file-remote-p default-directory))))
                        (eq (window-buffer (car entry)) (cdr entry)))))
               neo-diredy--windows)))))

(defun neo-diredy--post-command ()
  "Follow ordinary Dired navigation, debouncing cursor preview updates."
  (when neo-diredy--layout-mode
    (cond
     ((not (neo-diredy--layout-intact-p))
      ;; Foreign windows and buffers are authoritative, including popups that
      ;; leave the center selected and replacements of existing windows.
      (setq neo-diredy--configuration nil)
      (neo-diredy--layout-mode -1))
     ((eq (selected-window) neo-diredy--center)
      ;; Mode/state switches can alter presentation without moving point or
      ;; changing buffer text.  Repair styling before every redisplay, while
      ;; retaining the dirty flag for the expensive listing decorations.
      (neo-diredy--style)
      (let ((key (list (current-buffer) major-mode (point) (buffer-chars-modified-tick))))
        (unless (equal key neo-diredy--key)
          (neo-diredy--art-cancel)
          (let ((job (cdr (assq 'size neo-diredy--metadata-jobs))))
            (when (and job (not (equal (neo-diredy--original-file) (process-get job 'key))))
              (neo-diredy--metadata-cancel 'size)))
          (setq neo-diredy--key key)
          (when (timerp neo-diredy--timer) (cancel-timer neo-diredy--timer))
          (if (or (not (eq (current-buffer) (car neo-diredy--decorated)))
                  (not (equal default-directory neo-diredy--directory)))
              ;; Directory styling must precede command-loop redisplay.
              (neo-diredy--refresh)
            (setq neo-diredy--timer
                  (run-with-idle-timer 0.12 nil #'neo-diredy--refresh)))))))))

(defun neo-diredy--before-layout-change (&rest _)
  "Close the layout before another interface saves our temporary panes."
  (when (and neo-diredy--layout-mode (window-live-p neo-diredy--center)
             (eq (window-frame neo-diredy--center) (selected-frame)))
    (neo-diredy--layout-mode -1)))

(defun neo-diredy--stop ()
  "Restore saved presentation and dispose only of owned preview buffers."
  (unless (neo-diredy--layout-intact-p)
    (setq neo-diredy--configuration nil)
    (when (assq (window-buffer) neo-diredy--buffers)
      (setq neo-diredy--handoff
            (list (selected-window) (window-buffer) (window-point)))))
  (neo-diredy--art-stop)
  (neo-diredy--metadata-stop)
  (when (and (window-live-p neo-diredy--center)
             (assq (window-buffer neo-diredy--center) neo-diredy--buffers))
    (set-window-margins neo-diredy--center (car neo-diredy--margins) (cdr neo-diredy--margins)))
  (remove-hook 'post-command-hook #'neo-diredy--post-command)
  (dolist (command neo-diredy--layout-commands)
    (advice-remove command #'neo-diredy--before-layout-change))
  (when (timerp neo-diredy--timer) (cancel-timer neo-diredy--timer))
  (mapc #'delete-overlay neo-diredy--overlays)
  (dolist (entry neo-diredy--buffers)
    (when (buffer-live-p (car entry))
      (with-current-buffer (car entry)
        (remove-hook 'after-change-functions #'neo-diredy--changed t)
        (remove-hook 'dired-after-readin-hook #'neo-diredy--after-readin t)
        (kill-local-variable 'neo-diredy--file-sizes)
        (hl-line-mode (if (caddr (assq 'hl-line-mode (cadr entry))) 1 -1))
        (dired-hide-details-mode (if (caddr (assq 'dired-hide-details-mode (cadr entry))) 1 -1))
        (dolist (saved (cadr entry))
          (if (cadr saved) (set (make-local-variable (car saved)) (caddr saved))
            (kill-local-variable (car saved))))
        (when (overlayp hl-line-overlay)
          (overlay-put hl-line-overlay 'face hl-line-face)))))
  (when (and neo-diredy--configuration
             (frame-live-p (window-configuration-frame neo-diredy--configuration)))
    (let* ((window neo-diredy--center)
           (buffer (and (window-live-p window) (window-buffer window)))
           (position (and buffer (window-point window))))
      (with-selected-frame (window-configuration-frame neo-diredy--configuration)
        (set-window-configuration neo-diredy--configuration)
        (when (buffer-live-p buffer)
          (set-window-buffer (selected-window) buffer t)
          (set-window-point (selected-window) position)))))
  ;; A plugin may reuse an auxiliary window after removing its dedication.
  ;; Release our navigation flag, but never delete a window it has taken over.
  (dolist (entry (list (cons neo-diredy--left neo-diredy--parent)
                       (cons neo-diredy--right neo-diredy--preview)))
    (let ((window (car entry)) (buffer (cdr entry)))
      (when (window-live-p window)
        (set-window-parameter window 'no-other-window nil)
        (when (eq (window-buffer window) buffer)
          (set-window-dedicated-p window nil)
          (unless (one-window-p t (window-frame window))
            (delete-window window))))))
  (dolist (buffer (list neo-diredy--parent neo-diredy--preview))
    (when (buffer-live-p buffer) (kill-buffer buffer)))
  (dolist (entry neo-diredy--buffers)
    (when (and (caddr entry) (buffer-live-p (car entry)))
      (with-current-buffer (car entry) (dired-preview-mode 1))))
  (setq neo-diredy--buffers nil neo-diredy--overlays nil
        neo-diredy--windows nil neo-diredy--center nil
        neo-diredy--left nil neo-diredy--right nil neo-diredy--margins nil
        neo-diredy--configuration nil neo-diredy--key nil
        neo-diredy--directory nil neo-diredy--timer nil
        neo-diredy--decorated nil neo-diredy--preview-key nil))

;;;###autoload
(define-minor-mode neo-diredy--layout-mode
  "Toggle a Yazi-style Dired layout for the selected window.
Only one layout is active at once.  Disable to restore the previous layout.
Opening a file exits the layout.
All ordinary Dired operations remain available."
  :global t :group 'neo-diredy
  (if (not neo-diredy--layout-mode)
      (neo-diredy--stop)
    (condition-case err
        (unless neo-diredy--configuration
          (unless (derived-mode-p 'dired-mode 'wdired-mode) (user-error "Enable this mode in Dired"))
          (when (file-remote-p default-directory) (user-error "Use this layout in a local directory"))
          (when (< (window-total-width) 72) (user-error "The Dired window needs at least 72 columns"))
          (progn
            (setq neo-diredy--handoff nil
                  neo-diredy--configuration (current-window-configuration)
                  neo-diredy--center (selected-window)
                  neo-diredy--margins (window-margins))
            (dolist (symbol '(neo-diredy--parent neo-diredy--preview))
              (set symbol (generate-new-buffer (format " *%s*" symbol)))
              (with-current-buffer (symbol-value symbol)
                (special-mode)
                (setq-local truncate-lines t cursor-type nil mode-line-format nil
                            display-line-numbers nil)))
            (let ((width (window-total-width)))
              (setq neo-diredy--left (split-window neo-diredy--center (- (max 16 (/ width 5))) 'left)
                    neo-diredy--right (split-window neo-diredy--center nil 'right)))
            (set-window-buffer neo-diredy--left neo-diredy--parent)
            (set-window-buffer neo-diredy--right neo-diredy--preview)
            (dolist (window (list neo-diredy--left neo-diredy--right))
              (set-window-dedicated-p window t)
              (set-window-parameter window 'no-other-window t))
            (setq neo-diredy--windows
                  (mapcar (lambda (window) (cons window (window-buffer window)))
                          (window-list nil 'no-minibuffer)))
            (add-hook 'post-command-hook #'neo-diredy--post-command)
            (dolist (command neo-diredy--layout-commands)
              (advice-add command :before #'neo-diredy--before-layout-change))
            (neo-diredy--refresh)))
      (error (setq neo-diredy--layout-mode nil) (neo-diredy--stop)
             (signal (car err) (cdr err))))))

(defun neo-diredy--auto-enable (&rest entry-arguments)
  "Activate a layout for the selected, local Dired window."
  ;; A nonselecting popup leaves Dired selected.  Do not immediately recreate
  ;; its panes later in the same post-command hook, or on unrelated commands.
  ;; Moving point, changing windows/buffers, or explicitly entering Dired
  ;; resumes normal automatic activation without special-casing keymaps.
  (when (and (not (active-minibuffer-window))
             (or entry-arguments
                 (not (equal neo-diredy--handoff
                             (list (selected-window) (window-buffer) (window-point))))))
    (setq neo-diredy--handoff nil))
  (when (and neo-diredy-mode (not neo-diredy--layout-mode)
             (not neo-diredy--handoff)
             (not (active-minibuffer-window))
             (eq (current-buffer) (window-buffer (selected-window)))
             (derived-mode-p 'dired-mode 'wdired-mode)
             (not (file-remote-p default-directory))
             (not (window-dedicated-p))
             (>= (window-total-width) 72))
    (condition-case err
        (neo-diredy--layout-mode 1)
      (error (message "neo-diredy: %s" (error-message-string err))))))

;;;###autoload
(define-minor-mode neo-diredy-mode
  "Use Yazi-style columns automatically in local Dired windows.
Disable to restore normal Dired.  Narrow and remote windows are left alone."
  :global t :group 'neo-diredy
  (setq neo-diredy--handoff nil)
  (if neo-diredy-mode
      (progn
        (add-hook 'post-command-hook #'neo-diredy--auto-enable 90)
        (dolist (command '(dired dired-other-window dired-other-frame))
          (advice-add command :after #'neo-diredy--auto-enable))
        (neo-diredy--auto-enable))
    (remove-hook 'post-command-hook #'neo-diredy--auto-enable)
    (dolist (command '(dired dired-other-window dired-other-frame))
      (advice-remove command #'neo-diredy--auto-enable))
    (neo-diredy--layout-mode -1)
    (setq neo-diredy--handoff nil)))

(defun neo-diredy-unload-function ()
  "Remove active hooks, advice and windows before unloading the package."
  (neo-diredy-mode -1)
  nil)

;;; Asynchronous art extraction

(defun neo-diredy--art-signature (file)
  "Identify FILE's current content without reading its layers."
  (let* ((info (neo-diredy--file-info file)) (attributes (cdr info)))
    (list file (car info) (file-attribute-size attributes)
          (file-attribute-modification-time attributes)
          (file-attribute-status-change-time attributes)
          neo-diredy-image-bytes)))

(defun neo-diredy--art-cleanup (process)
  "Dispose PROCESS's timer, log and private temporary directory."
  (let ((timer (process-get process 'timer))
        (directory (process-get process 'directory)))
    (when (timerp timer) (cancel-timer timer))
    (when (buffer-live-p (process-buffer process))
      (kill-buffer (process-buffer process)))
    ;; Windows may report process termination before releasing its working
    ;; directory handle.  Retry owned cleanup without blocking Dired input.
    (let ((attempts 0))
      (cl-labels ((cleanup ()
                    (when (and directory (file-directory-p directory))
                      (condition-case err
                          (delete-directory directory t)
                        (file-error
                         (if (< (cl-incf attempts) 20)
                             (run-at-time 0.1 nil #'cleanup)
                           (message "neo-diredy temporary cleanup: %s"
                                    (error-message-string err))))))))
        (cleanup)))))

(defun neo-diredy--art-cancel ()
  "Cancel the owned extraction without publishing a stale result."
  (when neo-diredy--art-process
    (let ((process neo-diredy--art-process))
      (setq neo-diredy--art-process nil
            neo-diredy--preview-key nil)
      (set-process-sentinel process #'ignore)
      (when (process-live-p process) (delete-process process))
      (neo-diredy--art-cleanup process))))

(defun neo-diredy--art-stop ()
  "Stop extraction and release the session's bounded image cache."
  (neo-diredy--art-cancel)
  (setq neo-diredy--art-cache nil neo-diredy--preview-key nil))

(defun neo-diredy--art-finished (process _event)
  "Publish PROCESS's result only while its input remains unchanged."
  (when (and (memq (process-status process) '(exit signal))
             (eq process neo-diredy--art-process))
    (setq neo-diredy--art-process nil)
    (let* ((file (process-get process 'file))
           (key (process-get process 'key))
           (callback (process-get process 'callback))
           (output (expand-file-name "preview.image" (process-get process 'directory)))
           (result
            (condition-case err
                (if (and (eq (process-status process) 'exit)
                         (zerop (process-exit-status process)))
                    (let ((size (file-attribute-size (file-attributes output))))
                      (unless (and size (<= size neo-diredy-image-bytes))
                        (error "Extracted image exceeds neo-diredy-image-bytes"))
                      (cons 'image (neo-diredy--art-read output 0 size)))
                  (cons 'error
                        (if (process-get process 'timeout) "Art extraction timed out (30 seconds)"
                          (with-current-buffer (process-buffer process)
                            (string-trim (buffer-string))))))
              (error (cons 'error (error-message-string err))))))
      (neo-diredy--art-cleanup process)
      (if (not (equal key (ignore-errors (neo-diredy--art-signature file))))
          (funcall callback '(error . "File changed during preview; select it again"))
        (when (eq (car result) 'image)
          (push (cons key result) neo-diredy--art-cache))
        ;; At most eight entries and 32 MiB of decoded image bytes.
        (let ((bytes 0) (count 0))
          (setq neo-diredy--art-cache
                (seq-take-while
                 (lambda (entry)
                   (setq count (1+ count) bytes (+ bytes (length (cddr entry))))
                   (and (<= count 8) (<= bytes (* 32 1024 1024))))
                 neo-diredy--art-cache)))
        (funcall callback result)))))

(defun neo-diredy--art-request (file callback)
  "Extract FILE asynchronously and call CALLBACK with (image/error . data)."
  (neo-diredy--art-cancel)
  (setq file (expand-file-name file))
  (let* ((key (neo-diredy--art-signature file))
         (cached (assoc key neo-diredy--art-cache)))
    (if cached
        (progn
          (setq neo-diredy--art-cache (cons cached (delq cached neo-diredy--art-cache)))
          (funcall callback (cdr cached)))
      (let* ((directory (make-temp-file "neo-diredy-art-" t))
             (default-directory (file-name-as-directory directory))
             (process-environment (copy-sequence process-environment))
             (buffer (generate-new-buffer " *neo-diredy-art-worker*")))
        (setenv "NEO_DIREDY_ART_FILE" (expand-file-name file))
        (setenv "NEO_DIREDY_ART_DIRECTORY" directory)
        (condition-case err
            (let ((process
                   (make-process
                    :name "neo-diredy-art" :buffer buffer :noquery t
                    :connection-type 'pipe :coding 'utf-8-unix
                    :command (list (expand-file-name invocation-name invocation-directory)
                                   "-Q" "--batch" "-l" neo-diredy--library
                                   "--eval" "(neo-diredy--art-worker)")
                    :sentinel #'neo-diredy--art-finished)))
              (setq neo-diredy--art-process process)
              (process-put process 'directory directory)
              (process-put process 'file file)
              (process-put process 'key key)
              (process-put process 'callback callback)
              (process-put process 'timer
                           (run-at-time
                            30 nil (lambda ()
                                     (when (eq process neo-diredy--art-process)
                                       (process-put process 'timeout t)
                                       (delete-process process))))))
          (error
           (kill-buffer buffer)
           (delete-directory directory t)
           (funcall callback (cons 'error (error-message-string err)))))))))

(defun neo-diredy--art-worker ()
  "Extract a single preview in an isolated -Q batch Emacs."
  (let* ((file (getenv "NEO_DIREDY_ART_FILE"))
         (temporary-file-directory (file-name-as-directory (getenv "NEO_DIREDY_ART_DIRECTORY"))))
    (condition-case err
        (let ((data (if (equal (downcase (or (file-name-extension file) "")) "clip")
                        (neo-diredy--art-clip file)
                      (neo-diredy--art-psd file)))
              (coding-system-for-write 'no-conversion))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert data)
            (write-region (point-min) (point-max)
                          (expand-file-name "preview.image" temporary-file-directory) nil 'silent)))
      (error (princ (error-message-string err)) (kill-emacs 1)))))

;;; Bounded binary reads

(defun neo-diredy--art-read (file offset length)
  "Read exactly LENGTH bytes at OFFSET, without decoding FILE."
  (unless (and (<= 0 offset) (<= 0 length (* 32 1024 1024)))
    (error "Invalid preview byte range"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil offset (+ offset length))
    (unless (= (buffer-size) length)
      (error "Truncated preview data"))
    (buffer-string)))

(defun neo-diredy--art-uint (bytes offset length)
  "Read a bounded big-endian integer from BYTES."
  (unless (<= 0 offset (+ offset length) (length bytes))
    (error "Truncated integer"))
  (let ((value 0))
    (dotimes (i length value)
      (setq value (+ (ash value 8) (aref bytes (+ offset i)))))))

;;; PSD resources and composite decoding

(defun neo-diredy--art-psd (file)
  "Extract a JPEG thumbnail, or decode the saved PSD/PSB composite."
  (let* ((header (neo-diredy--art-read file 0 30))
         (offset (+ 30 (neo-diredy--art-uint header 26 4)))
         (size (file-attribute-size (file-attributes file)))
         end
         thumbnail)
    (unless (and (equal (substring header 0 4) "8BPS")
                 (memq (neo-diredy--art-uint header 4 2) '(1 2)))
      (error "Not a PSD/PSB file"))
    (setq end (+ offset 4 (neo-diredy--art-uint (neo-diredy--art-read file offset 4) 0 4))
          offset (+ offset 4))
    (unless (<= end size)
      (error "Truncated PSD resources"))
    (while (and (< offset end) (not thumbnail))
      (unless (<= (+ offset 7) end)
        (error "Truncated PSD resource header"))
      (let* ((resource (neo-diredy--art-read file offset 7))
             (id (neo-diredy--art-uint resource 4 2))
             (name-size (1+ (aref resource 6)))
             (size-offset (+ offset 6 name-size (% name-size 2)))
             (length (neo-diredy--art-uint (neo-diredy--art-read file size-offset 4) 0 4))
             (data-offset (+ size-offset 4)))
        (unless (and (equal (substring resource 0 4) "8BIM")
                     (<= (+ data-offset length (% length 2)) end))
          (error "Invalid PSD resource"))
        (when (= id 1036)
          (let* ((data (neo-diredy--art-read file data-offset length))
                 (jpeg-size (neo-diredy--art-uint data 20 4)))
            (unless (and (>= length 28) (= (neo-diredy--art-uint data 0 4) 1)
                         (<= 2 jpeg-size (- length 28)))
              (error "Unsupported PSD thumbnail"))
            (setq thumbnail (substring data 28 (+ 28 jpeg-size)))))
        (setq offset (+ data-offset length (% length 2)))))
    (if (and thumbnail (string-prefix-p (unibyte-string 255 216) thumbnail))
        thumbnail
      (neo-diredy--art-psd-composite file header end))))

(defun neo-diredy--art-packbits (bytes start end width)
  "Decode one bounded PackBits row; reject truncated or overflowing runs."
  (let ((row (make-string width 0))
        (out 0))
    (while (< start end)
      (let* ((control (aref bytes start))
             (count (if (< control 128)
                        (1+ control)
                      (- 257 control))))
        (setq start (1+ start))
        (unless (= control 128) ; -128 is a no-op.
          (unless (and (<= (+ out count) width)
                       (<= (+ start (if (< control 128)
                                        count
                                      1)) end))
            (error "Invalid PSD PackBits run"))
          (store-substring row out
                           (if (< control 128)
                               (substring bytes start (+ start count))
                             (make-string count (aref bytes start))))
          (setq out (+ out count)
                start (+ start (if (< control 128)
                                   count
                                 1))))))
    (unless (= out width)
      (error "Truncated PSD PackBits row"))
    row))

(defun neo-diredy--art-psd-composite (file header offset)
  "Decode raw/RLE 8-bit RGB or grayscale composites as a bounded PPM.
Transparency is shown on a checkerboard.  Layer blending is not performed."
  ;; ponytail: saved raw/RLE composites only; add other codecs when a file needs them.
  (let* ((psb (= (neo-diredy--art-uint header 4 2) 2))
         (channels (neo-diredy--art-uint header 12 2))
         (height (neo-diredy--art-uint header 14 4))
         (width (neo-diredy--art-uint header 18 4))
         (depth (neo-diredy--art-uint header 22 2))
         (mode (neo-diredy--art-uint header 24 2))
         (colors (if (= mode 1)
                     1
                   3))
         (length-size (if psb
                          8
                        4))
         (layer-size (neo-diredy--art-uint (neo-diredy--art-read file offset length-size) 0 length-size))
         (layer-start (+ offset length-size))
         (image-start (+ layer-start layer-size))
         (file-size (file-attribute-size (file-attributes file)))
         alpha)
    (unless (and (= depth 8) (memq mode '(1 3))
                 (<= colors channels 56) (<= 1 width 300000) (<= 1 height 300000)
                 (<= (* width height) (* 64 1024 1024)))
      (error "PSD composite requires 8-bit RGB/grayscale, at most 64 megapixels"))
    (unless (<= (+ image-start 2) file-size)
      (error "PSD has no saved composite image"))
    ;; Only a negative layer count marks the first extra channel as transparency.
    (when (>= layer-size (+ length-size 2))
      (let ((info-size (neo-diredy--art-uint (neo-diredy--art-read file layer-start length-size) 0 length-size)))
        (unless (<= (+ length-size info-size) layer-size)
          (error "Invalid PSD layer section"))
        (when (>= info-size 2)
          (setq alpha (and (> channels colors)
                           (>= (neo-diredy--art-uint (neo-diredy--art-read file (+ layer-start length-size) 2) 0 2) 32768))))))
    (let* ((compression (neo-diredy--art-uint (neo-diredy--art-read file image-start 2) 0 2))
           (row-size (if psb
                         4
                       2))
           (data-start (+ image-start 2))
           (counts (when (= compression 1)
                     (prog1 (neo-diredy--art-read file data-start (* row-size height channels))
                       (setq data-start (+ data-start (* row-size height channels))))))
           (used (+ colors (if alpha
                               1
                             0)))
           (scale (max 1.0 (/ (float (max width height)) 768)))
           (w (max 1 (floor (/ width scale))))
           (h (max 1 (floor (/ height scale))))
           (planes (make-vector used nil))
           (cursor 0)
           data)
      (unless (memq compression '(0 1))
        (error "PSD composite compression %d is unsupported (raw/RLE supported)" compression))
      (setq data (neo-diredy--art-read file data-start
                                (if (= compression 0)
                                    (* width height used)
                                  (- file-size data-start))))
      (dotimes (channel used)
        (let ((plane (make-string (* w h) 0))
              (target-y 0))
          (dotimes (y height)
            (let* ((length (if counts
                               (neo-diredy--art-uint counts (* (+ (* channel height) y) row-size) row-size)
                             width))
                   (end (+ cursor length)))
              (unless (<= end (length data))
                (error "Truncated PSD composite row"))
              (when (and (< target-y h) (= y (/ (* target-y height) h)))
                (let ((row (if counts
                               (neo-diredy--art-packbits data cursor end width)
                             (substring data cursor end))))
                  (dotimes (x w)
                    (aset plane (+ (* target-y w) x) (aref row (/ (* x width) w)))))
                (setq target-y (1+ target-y)))
              (setq cursor end)))
          (aset planes channel plane)))
      (let ((pixels (make-string (* 3 w h) 0)))
        (dotimes (i (* w h))
          (let ((a (if alpha
                       (aref (aref planes colors) i)
                     255))
                (background (if (zerop (% (+ (/ (% i w) 8) (/ (/ i w) 8)) 2))
                                192
                              224)))
            (dotimes (c 3)
              (aset pixels (+ (* i 3) c)
                    (/ (+ (* (aref (aref planes (if (= colors 1)
                                                    0
                                                  c)) i) a)
                          (* background (- 255 a))) 255)))))
        (concat (format "P6\n%d %d\n255\n" w h) pixels)))))

;;; CLIP canvas preview

(defun neo-diredy--art-sqlite-offset (file)
  "Find the embedded SQLite header using overlapping bounded reads."
  (let ((signature "SQLite format 3\0")
        (size (file-attribute-size (file-attributes file)))
        (offset 0)
        found)
    (while (and (< offset size) (not found))
      (let* ((length (min (* 1024 1024) (- size offset)))
             (bytes (neo-diredy--art-read file offset length))
             (match (string-match (regexp-quote signature) bytes)))
        (if match
            (setq found (+ offset match))
          (setq offset (+ offset (max 1 (- length 15)))))))
    (or found (error "CLIP has no embedded SQLite database"))))

(defun neo-diredy--art-clip (file)
  "Read CanvasPreview.ImageData with Emacs' native SQLite support."
  (unless (sqlite-available-p)
    (error "This Emacs needs built-in SQLite support"))
  (let ((offset (neo-diredy--art-sqlite-offset file))
        (size (file-attribute-size (file-attributes file)))
        (temporary (make-temp-file "neo-clip-" nil ".db"))
        database
        image)
    (unwind-protect
        (progn
          (unless (<= (- size offset) (* 512 1024 1024))
            (error "CLIP database exceeds the 512 MiB preview limit"))
          ;; SQLite resolves overflow pages; scanning for a PNG signature cannot.
          (while (< offset size)
            (let ((length (min (* 1024 1024) (- size offset))))
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert (neo-diredy--art-read file offset length))
                (let ((coding-system-for-write 'no-conversion))
                  (write-region (point-min) (point-max) temporary t 'silent)))
              (setq offset (+ offset length))))
          (setq database (sqlite-open temporary t)
                image (caar (sqlite-select database "SELECT ImageData FROM CanvasPreview WHERE length(ImageData) <= 33554432 LIMIT 1")))
          (unless (and (stringp image) (<= (length image) (* 32 1024 1024))
                       (string-prefix-p (unibyte-string 137 80 78 71 13 10 26 10) image))
            (error "CLIP has no valid canvas preview PNG"))
          image)
      (unwind-protect (when database
                        (sqlite-close database))
        (delete-file temporary)))))

(neo-diredy-mode 1)
(provide 'neo-diredy)
;;; neo-diredy.el ends here
