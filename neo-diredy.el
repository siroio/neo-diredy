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
;; Preview fontification uses isolated, hook-free language modes.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'dired)
(require 'hl-line)
(require 'font-lock)
(require 'treesit)
(require 'face-remap)
(require 'image)
(require 'json)
(require 'sqlite)
(require 'tab-bar)
(require 'bookmark)
(declare-function dired-preview-mode "dired-preview" (&optional arg))
(declare-function wdired-get-filename "wdired" (&optional no-dir old))
(declare-function evil-append "evil-commands" (count &optional vcount skip-empty-lines))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))
(declare-function evil-normalize-keymaps "evil-core" (&optional force))
(defvar markdown-fontify-code-blocks-natively)
(defvar diff-font-lock-syntax)
(defvar diff-font-lock-prettify)
(defvar diff-refine)
(defvar treesit-auto-install-grammar)
(defvar sh-set-shell-hook)
(defvar sh-shell-file)
(defvar tex-mode-load-hook)
(defvar org-load-hook)
(defvar org-font-lock-hook)
(defvar org-font-lock-set-keywords-hook)
(defvar org-cycle-hook)
(defvar org-src-fontify-natively)
(defvar org-startup-folded)
(defvar org-startup-with-inline-images)
(defvar org-startup-with-latex-preview)
(defvar compilation-error-regexp-alist)
(defvar compilation-error-regexp-alist-alist)
(defvar w32-quote-process-args)
(defvar neo-diredy--clipboard nil "Session clipboard: (copy-or-cut . absolute-paths).")

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
(defcustom neo-diredy-openers nil
  "Additional opening methods as (LABEL EXTENSIONS PROGRAM ARGUMENTS).
EXTENSIONS are matched case-insensitively, without the leading dot;
an empty list matches files and directories.  PROGRAM is an executable
name or absolute path.  ARGUMENTS are literal strings, followed by the
selected absolute path as one argument.  No shell is used; Windows batch
scripts are not supported."
  :type '(repeat (list (string :tag "Label")
                       (repeat :tag "Extensions (empty means all)" string)
                       (string :tag "Executable")
                       (repeat :tag "Arguments" string)))
  :group 'neo-diredy)
(defface neo-diredy-selection
  '((t (:inherit highlight :extend t))) "Selected file row.")

(defvar neo-diredy--layout-mode nil)
(defvar neo-diredy-mode nil)
(defvar neo-diredy--center nil)
(defvar neo-diredy--left nil)
(defvar neo-diredy--right nil)
(defvar neo-diredy--modeline-window nil)
(defvar neo-diredy--modeline-parameter nil)
(defvar neo-diredy--parent nil)
(defvar neo-diredy--preview nil)
(defvar neo-diredy--configuration nil)
(defvar neo-diredy--windows nil)
(defvar neo-diredy--handoff nil
  "Selection and window contents held until navigation after a handoff.")
(defvar neo-diredy--buffers nil)
(defvar neo-diredy--existing-buffers nil)
(defvar neo-diredy--overlays nil)
(defvar neo-diredy--selected-icon nil)
(defvar neo-diredy--timer nil)
(defvar neo-diredy--key nil)
(defvar neo-diredy--choice-frame nil "Hidden menu frame retained while the layout is active.")
(defvar neo-diredy--choice-buffer nil "Empty menu buffer retained with the hidden frame.")
(defvar neo-diredy--directory nil)
(defvar neo-diredy--decorated nil)
(defvar neo-diredy--preview-key nil)
(defvar neo-diredy--details-window nil)
(defvar neo-diredy--details-buffer nil)
(defvar neo-diredy--details-key nil)
(defvar neo-diredy--details-data nil)
(defvar neo-diredy--details-cache nil)
(defvar neo-diredy--details-process nil)
(defvar neo-diredy--details-resizing nil)
(defconst neo-diredy--library (or load-file-name buffer-file-name))
(defvar neo-diredy--art-process nil)
(defvar neo-diredy--art-cache nil)
(defvar neo-diredy--metadata-jobs nil)
(defvar neo-diredy--size-cache nil)
(defvar neo-diredy--git-location nil)
(defvar neo-diredy--git-state nil)
(defvar neo-diredy--git-message nil)
(defvar neo-diredy--margins nil)
(defvar-local neo-diredy--file-sizes nil)
(defvar-local neo-diredy--modeline-face nil)
(defvar-local neo-diredy--listing-dirty t)
(defvar-local neo-diredy--preview-origin nil)
(defvar w32-get-true-file-attributes)
(defconst neo-diredy--layout-commands
  '(tab-bar-new-tab tab-bar-select-tab tab-bar-close-tab neo-git--layout)
  "Functions that must capture a clean window configuration.
Neo Git creates its layout asynchronously, so its entry command is too early.
Advising its layout function does not load or require the optional package.")
(defconst neo-diredy--variables
  '(header-line-format truncate-lines cursor-type
                       display-line-numbers hl-line-face hl-line-range-function
                       dired-hide-details-mode hl-line-mode))

(defun neo-diredy--icon (file directory)
  "Return a simple label for a file of type DIRECTORY."
  (let ((cut (and (derived-mode-p 'dired-mode) (eq (car neo-diredy--clipboard) 'cut)
                  (member file (cdr neo-diredy--clipboard)))))
    (propertize (cond (cut "↪ ") (directory "▸ ") (t "· "))
                'face (cond (cut 'warning) (directory 'dired-directory) (t 'shadow)))))

(defun neo-diredy--row-overlay ()
  "Return the visible file anchor on the current line, if any."
  (cl-find-if
   (lambda (overlay)
     (and (overlay-get overlay 'neo-diredy-file)
          (not (invisible-p (overlay-start overlay)))))
   (overlays-in (line-beginning-position)
                (min (point-max) (1+ (line-end-position))))))

(defun neo-diredy--normalize-point ()
  "Keep selection on visible file rows, without restricting WDired edits."
  (unless (or (derived-mode-p 'wdired-mode) (neo-diredy--row-overlay))
    (let ((backward (and (overlayp neo-diredy--selected-icon)
                         (eq (overlay-buffer neo-diredy--selected-icon) (current-buffer))
                         (< (point) (overlay-start neo-diredy--selected-icon))))
          before after)
      (dolist (overlay neo-diredy--overlays)
        (when (and (eq (overlay-buffer overlay) (current-buffer))
                   (overlay-get overlay 'neo-diredy-file)
                   (not (invisible-p (overlay-start overlay))))
          (let ((position (overlay-start overlay)))
            (if (< position (point))
                (when (or (null before) (> position before)) (setq before position))
              (when (or (null after) (< position after)) (setq after position))))))
      (goto-char (or (if backward (or before after) (or after before)) (point-min))))))

(defun neo-diredy--selection-range ()
  "Highlight file rows only; an empty directory has no selection."
  (when (or (derived-mode-p 'wdired-mode) (neo-diredy--row-overlay))
    (cons (line-beginning-position) (line-beginning-position 2))))

(defun neo-diredy--subdir-command (original &rest args)
  "Operate on an inserted subdirectory from its visible directory row."
  (if (not (and neo-diredy--layout-mode
                (eq (selected-window) neo-diredy--center)
                (derived-mode-p 'dired-mode)
                (not (derived-mode-p 'wdired-mode))))
      (apply original args)
    (when-let* ((file (dired-get-filename nil t))
                (entry (assoc (file-name-as-directory file) dired-subdir-alist)))
      (goto-char (cdr entry)))
    (let ((directory (dired-current-directory)))
      (prog1 (apply original args)
        ;; Headers are hidden in this view.  Keep the directory itself as
        ;; the visible target for another toggle or removal of its listing.
        (unless (equal directory default-directory)
          ;; `dired-goto-file' can land on the subdirectory's hidden "."
          ;; entry and unhide it.  Use our actual filename anchor instead.
          (when-let* ((row (cl-find-if
                           (lambda (overlay)
                             (and (eq (overlay-buffer overlay) (current-buffer))
                                  (equal (overlay-get overlay 'neo-diredy-file)
                                         (directory-file-name directory))))
                           neo-diredy--overlays)))
            (goto-char (overlay-start row))))))))

(defun neo-diredy--subdir-hidden-p (original directory)
  "Read Dired's fold state beneath our hidden-header overlays."
  (if (and neo-diredy--layout-mode (assq (current-buffer) neo-diredy--buffers))
      (save-excursion
        (dired-goto-subdir directory)
        (eq (get-text-property (point) 'invisible) 'dired))
    (funcall original directory)))

(defun neo-diredy--highlight-icon ()
  "Match the current row's icon background without repainting the listing."
  ;; Overlay before-strings do not inherit the hl-line overlay's face.
  (let ((selected (neo-diredy--row-overlay)))
    (dolist (overlay (delete-dups (list neo-diredy--selected-icon selected)))
      (when (and (overlayp overlay) (overlay-buffer overlay))
        (let* ((text (copy-sequence (overlay-get overlay 'before-string)))
               (face (cond ((and (eq (car neo-diredy--clipboard) 'cut)
                                 (member (overlay-get overlay 'neo-diredy-file)
                                         (cdr neo-diredy--clipboard))) 'warning)
                           ((overlay-get overlay 'neo-diredy-directory) 'dired-directory)
                           (t 'shadow))))
          ;; The right-margin display precedes the two-character icon.
          (put-text-property (- (length text) 2) (length text) 'face
                             (if (eq overlay selected)
                                 (list 'neo-diredy-selection face) face)
                             text)
          (overlay-put overlay 'before-string text))))
    (setq neo-diredy--selected-icon selected)))

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
  (neo-diredy--details-stop)
  (neo-diredy--metadata-stop))

(defun neo-diredy--original-file ()
  "Return the file on this row, retaining its original name during edits."
  (if (derived-mode-p 'wdired-mode)
      (ignore-errors (wdired-get-filename nil t))
    (dired-get-filename nil t)))

(defun neo-diredy--editing-p ()
  "Return non-nil while the central list is in a WDired edit session."
  (and (window-live-p neo-diredy--center)
       (with-current-buffer (window-buffer neo-diredy--center)
         (derived-mode-p 'wdired-mode))))

(defun neo-diredy--open-choices (file)
  "Return labelled opening methods for FILE."
  (let* ((choices (when (eq system-type 'windows-nt)
                    '(("Windows default app" . default) ("Show in Explorer" . explorer))))
         (labels (mapcar #'car choices))
         (directory (file-directory-p file)))
    (unless (proper-list-p neo-diredy-openers)
      (user-error "neo-diredy-openers must be a list"))
    (dolist (entry neo-diredy-openers)
      (unless (and (proper-list-p entry) (= (length entry) 4)
                   (stringp (nth 0 entry)) (not (string-empty-p (nth 0 entry)))
                   (proper-list-p (nth 1 entry)) (cl-every #'stringp (nth 1 entry))
                   (stringp (nth 2 entry)) (not (string-empty-p (nth 2 entry)))
                   (proper-list-p (nth 3 entry)) (cl-every #'stringp (nth 3 entry)))
        (user-error "Invalid neo-diredy opener: %S" entry))
      (pcase-let ((`(,label ,extensions ,program ,arguments) entry))
        (when (member label labels) (user-error "Duplicate opener label: %s" label))
        (push label labels)
        (when (or (null extensions)
                  (and (not directory)
                       (cl-some (lambda (extension)
                                  (string-suffix-p (concat "." extension) file t))
                                extensions)))
          (setq choices (append choices (list (cons label (cons program arguments))))))))
    choices))

(defun neo-diredy--check-open-file (file)
  "Reject a missing or remote FILE before opening it externally."
  (when (file-remote-p file) (user-error "Openers require a local file"))
  (unless (file-exists-p file) (user-error "File no longer exists: %s" file)))

(defun neo-diredy--open-file (file action)
  "Open FILE using ACTION, with literal process arguments."
  (neo-diredy--check-open-file file)
  (condition-case err
      (if (and (memq action '(default explorer))
               (not (and (eq system-type 'windows-nt) (fboundp 'w32-shell-execute))))
          (user-error "This opening method requires Windows")
        (let ((native (subst-char-in-string ?/ ?\\ file)))
          (cond
           ((eq action 'default) (w32-shell-execute "open" native))
           ((and (eq action 'explorer) (file-directory-p file))
            (w32-shell-execute "explore" native))
           (t
            (let* ((command (if (eq action 'explorer)
                                (list "explorer.exe" "/select," native)
                              (append action (list file))))
                   (program (executable-find (car command)))
                   (coding-system-for-write (if (eq system-type 'windows-nt)
                                                locale-coding-system coding-system-for-write))
                   (w32-quote-process-args t))
              (unless program (user-error "Opener program not found: %s" (car command)))
              (when (eq system-type 'windows-nt)
                (when (member (downcase (or (file-name-extension program) "")) '("bat" "cmd"))
                  (user-error "Use an executable opener, not a Windows batch script"))
                ;; shortcut: Windows ANSI arguments; add a Unicode launcher if broader paths are needed.
                (dolist (argument (cons program (cdr command)))
                  (unless (equal argument (decode-coding-string
                                           (encode-coding-string argument locale-coding-system)
                                           locale-coding-system))
                    (user-error "This opener cannot represent the path or arguments in %s; use Windows default app"
                                locale-coding-system))))
              (if (eq action 'explorer)
                  (w32-shell-execute "open" program (concat "/select,\"" native "\""))
                (make-process
                 :name "neo-diredy-opener" :buffer nil :noquery t :connection-type 'pipe
                 :command (cons program (cdr command))
                 :sentinel (lambda (process event)
                             (when (and (memq (process-status process) '(exit signal))
                                        (/= (process-exit-status process) 0))
                               (message "neo-diredy: %s: %s" (car command)
                                        (string-trim event)))))))))))
    (error (user-error "Could not open %s: %s" (file-name-nondirectory file)
                       (error-message-string err)))))

(defun neo-diredy--choice-clear ()
  "Release the reusable menu frame."
  (when (frame-live-p neo-diredy--choice-frame)
    (delete-frame neo-diredy--choice-frame t))
  (when (buffer-live-p neo-diredy--choice-buffer)
    (kill-buffer neo-diredy--choice-buffer))
  (setq neo-diredy--choice-frame nil neo-diredy--choice-buffer nil))

(defun neo-diredy--choice-anchor (window)
  "Return (X Y HEIGHT) for the selected row in WINDOW, in frame pixels."
  (redisplay t)
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char (window-point window))
      (when (derived-mode-p 'dired-mode) (dired-move-to-filename))
      (let* ((edges (window-inside-pixel-edges window))
             (position (pos-visible-in-window-p (point) window t))
             (glyph (posn-at-point (point) window)))
        (list (+ (car edges) (max 0 (or (car position) 0)))
              ;; Y already includes header/tab lines; X starts at the body.
              (+ (cadr (window-pixel-edges window))
                 (or (cadr position) (- (cadr edges) (cadr (window-pixel-edges window)))))
              (max (frame-char-height (window-frame window))
                   (or (cdr (posn-object-width-height glyph)) 0)))))))

(defun neo-diredy--read-choice (title choices &optional options)
  "Select one of CHOICES under TITLE in a child frame, or return nil.
OPTIONS may contain :keys (character . value), :disabled (value . reason)
and :footer.  Place the menu below the selected row, or above if needed."
  (unless (display-graphic-p) (user-error "This menu requires a graphical frame"))
  (unless choices (user-error "No choices available"))
  (unless (frame-live-p neo-diredy--choice-frame) (neo-diredy--choice-clear))
  (let* ((parent (selected-frame))
         (origin (selected-window))
         (anchor (neo-diredy--choice-anchor origin))
         (edges (window-inside-pixel-edges origin))
         (below (+ (cadr anchor) (nth 2 anchor)))
         (space-below (- (nth 3 edges) below))
         (space-above (- (cadr anchor) (cadr edges)))
         ;; Reserve the internal padding and the child frame's border.
         (max-height (max 1 (/ (- (max space-below space-above) 10)
                               (frame-char-height parent))))
         (width (max 3 (min 50 (- (frame-width parent) 4)
                            (max (1+ (string-width title))
                                 (+ 3 (apply #'max (mapcar (lambda (choice)
                                                           (string-width (car choice))) choices)))))))
         (buffer (or (and (frame-live-p neo-diredy--choice-frame)
                          (not (frame-visible-p neo-diredy--choice-frame))
                          (eq (frame-parent neo-diredy--choice-frame) parent)
                          (buffer-live-p neo-diredy--choice-buffer)
                          neo-diredy--choice-buffer)
                     (generate-new-buffer " *neo-diredy-choices*")))
         (keys (plist-get options :keys)) (disabled (plist-get options :disabled))
         (index 0) popup)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delay-mode-hooks (special-mode))
            (setq-local mode-line-format nil header-line-format nil tab-line-format nil
                        display-line-numbers nil truncate-lines nil cursor-type nil))
          (when (and (frame-live-p neo-diredy--choice-frame)
                     (not (frame-visible-p neo-diredy--choice-frame))
                     (not (eq (frame-parent neo-diredy--choice-frame) parent)))
            (neo-diredy--choice-clear))
          (setq popup
                (or (and (frame-live-p neo-diredy--choice-frame)
                         (not (frame-visible-p neo-diredy--choice-frame))
                         neo-diredy--choice-frame)
                    (make-frame
                     `((parent-frame . ,parent) (minibuffer . nil) (visibility . nil)
                       (undecorated . t) (no-accept-focus . t) (no-focus-on-map . t)
                       (no-other-frame . t) (skip-taskbar . t) (desktop-dont-save . t)
                       (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                       (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                       (left-fringe . 0) (right-fringe . 0) (internal-border-width . 4)
                       (child-frame-border-width . 1) (min-width . 1) (min-height . 1)
                       (font . ,(frame-parameter parent 'font))
                       (background-color . ,(face-background 'default parent t))
                       (foreground-color . ,(face-foreground 'default parent t))
                       (width . ,width) (height . 1)))))
          ;; A nested menu gets a temporary frame instead of overwriting the
          ;; visible menu.  Retain at most one hidden frame between commands.
          (when (and neo-diredy--layout-mode
                     (or (eq popup neo-diredy--choice-frame) (not (frame-live-p neo-diredy--choice-frame))))
            (setq neo-diredy--choice-frame popup neo-diredy--choice-buffer buffer))
          ;; Refresh appearance on reuse without invalidating unchanged faces.
          (let ((parameters `((font . ,(frame-parameter parent 'font))
                              (background-color . ,(face-background 'default parent t))
                              (foreground-color . ,(face-foreground 'default parent t))
                              (width . ,width))))
            (setq parameters (cl-remove-if (lambda (entry)
                                            (equal (frame-parameter popup (car entry)) (cdr entry)))
                                          parameters))
            (when parameters (modify-frame-parameters popup parameters)))
          (unless (equal (face-background 'child-frame-border popup t) (face-foreground 'shadow parent t))
            (set-face-attribute 'child-frame-border popup
                                :background (face-foreground 'shadow parent t)))
          (set-window-buffer (frame-root-window popup) buffer)
          (catch 'chosen
            (while (and (frame-live-p parent) (frame-live-p popup))
              (let ((footer (or (cdr (assoc (cdr (nth index choices)) disabled))
                                (plist-get options :footer))))
                (set-frame-height
                 popup (min max-height
                            (+ (length choices) (if footer 1 0)
                               (ceiling (+ 2 (string-width (car (nth index choices)))) (1- width)))))
                (set-frame-position
                 popup (max 0 (min (car anchor) (- (frame-pixel-width parent) (frame-pixel-width popup) 2)))
                 (if (<= (frame-pixel-height popup) space-below) below
                   (max (cadr edges) (- (cadr anchor) (frame-pixel-height popup) 2))))
                (with-current-buffer buffer
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert (propertize (truncate-string-to-width title (1- width) nil nil t)
                                        'face 'bold) "\n")
                    (cl-loop for choice in choices for row from 0 do
                             (insert (propertize
                                      (concat (if (= row index) "› " "  ")
                                              (if (= row index) (car choice)
                                                (truncate-string-to-width (car choice) (- width 3) nil nil t)))
                                      'face (cond ((assoc (cdr choice) disabled) 'shadow)
                                                  ((= row index) 'highlight) (t 'default))))
                             (when (< row (1- (length choices))) (insert "\n")))
                    (when footer
                      (insert "\n" (propertize (truncate-string-to-width footer (1- width) nil nil t)
                                                 'face 'shadow)))
                    (goto-char (point-min))
                    (forward-line (1+ index))
                    (set-window-point (frame-root-window popup) (point)))))
              (unless (frame-visible-p popup) (make-frame-visible popup))
              (redisplay t)
              (let* ((event (read-key nil t)) (shortcut (assq event keys)))
                (if shortcut
                    (let ((choice (rassq (cdr shortcut) choices)))
                      (unless (assoc (cdr shortcut) disabled) (throw 'chosen choice)))
                  (pcase event
                  ((or 'down ?j ?\C-n) (setq index (mod (1+ index) (length choices))))
                  ((or 'up ?k ?\C-p) (setq index (mod (1- index) (length choices))))
                  ((or 'return 'kp-enter ?\r)
                   (unless (assoc (cdr (nth index choices)) disabled)
                     (throw 'chosen (nth index choices))))
                  ((or 'escape ?\e ?\C-g) (throw 'chosen nil))
                  (_ (when (or (mouse-event-p event) (eq (car-safe event) 'switch-frame))
                       (throw 'chosen nil)))))))))
      (when (frame-live-p popup)
        (if (and neo-diredy--layout-mode (eq popup neo-diredy--choice-frame))
            (make-frame-invisible popup t)
          (delete-frame popup t)))
      (when (buffer-live-p buffer)
        (if (and neo-diredy--layout-mode (frame-live-p neo-diredy--choice-frame)
                 (eq buffer neo-diredy--choice-buffer))
            ;; Keep an empty private buffer in the hidden window.  Killing it
            ;; would make Emacs display (and retain) an unrelated Dired buffer.
            (with-current-buffer buffer (let ((inhibit-read-only t)) (erase-buffer)))
          (kill-buffer buffer))))))

;;;###autoload
(defun neo-diredy-open ()
  "Choose an opening method for the Dired item at point in a child frame."
  (interactive)
  (unless (derived-mode-p 'dired-mode) (user-error "Use this command in Dired"))
  (when (derived-mode-p 'wdired-mode) (user-error "Finish or cancel renaming first"))
  ;; shortcut: open the item at point; add marked-file dispatch when requested.
  (let ((file (dired-get-filename nil t)))
    (unless file (user-error "No file on this line"))
    (neo-diredy--check-open-file file)
    (when-let* ((choice (neo-diredy--read-choice
                        "Open"
                        (neo-diredy--open-choices file))))
      (neo-diredy--open-file file (cdr choice)))))

;;; Single and marked file actions

(defvar neo-diredy--action-results nil "Last (source status destination detail) results.")
(defvar-local neo-diredy--mark-cache nil)
(defvar neo-diredy--action-paint-key nil)
(defvar neo-diredy--action-overlays nil)

(defun neo-diredy--action-check ()
  "Require an ordinary local Dired buffer for file actions."
  (unless (and (derived-mode-p 'dired-mode) (not (derived-mode-p 'wdired-mode)))
    (user-error "Finish renaming before using file actions"))
  (when (file-remote-p default-directory) (user-error "File actions require a local directory")))

(defun neo-diredy--action-marks ()
  "Return ordinary marks, cached until the listing changes."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (equal tick (car neo-diredy--mark-cache))
      (setq neo-diredy--mark-cache
            (cons tick (let ((dired-marker-char ?*) (transient-mark-mode nil))
                         (dired-get-marked-files nil 'marked)))))
    (cdr neo-diredy--mark-cache)))

(defun neo-diredy--action-path (file)
  "Resolve FILE's parent while retaining the final symlink itself."
  (let* ((path (directory-file-name (expand-file-name file)))
         (parent (file-name-directory path)))
    (expand-file-name (file-name-nondirectory path) (file-truename parent))))

(defun neo-diredy--action-equal (a b)
  "Compare normalized paths A and B with the platform's case rules."
  (string-equal (if (eq system-type 'windows-nt) (downcase a) a)
                (if (eq system-type 'windows-nt) (downcase b) b)))

(defun neo-diredy--action-within (file directory)
  "Whether FILE is strictly inside DIRECTORY after resolving parents."
  (string-prefix-p (file-name-as-directory (neo-diredy--action-path directory))
                   (neo-diredy--action-path file) (eq system-type 'windows-nt)))

(defun neo-diredy--action-targets (&optional collapse)
  "Return normal marks or the current item; COLLAPSE covered descendants."
  (neo-diredy--action-check)
  (let* ((files (or (neo-diredy--action-marks)
                    (when-let* ((file (dired-get-filename nil t))) (list file))))
         (files (delete-dups
                 (mapcar #'directory-file-name
                         (seq-remove (lambda (f) (member (file-name-nondirectory f) '("." ".."))) files)))))
    (when collapse
      (setq files
            (seq-remove
             (lambda (file)
               (cl-some (lambda (parent)
                          (and (not (equal file parent)) (file-directory-p parent)
                               (not (file-symlink-p parent))
                               (neo-diredy--action-within file parent))) files)) files)))
    files))

(defun neo-diredy--action-exists (file)
  "Include dangling symlinks when checking FILE."
  (or (file-exists-p file) (file-symlink-p file)))

(defun neo-diredy--action-store (kind)
  "Capture current action targets as KIND without modifying files."
  (let ((files (neo-diredy--action-targets t)))
    (unless files (user-error "No files selected"))
    (setq neo-diredy--clipboard (cons kind (copy-sequence files)))
    (force-mode-line-update t)
    (message "%s %d" (if (eq kind 'cut) "Cut" "Copy") (length files))))

;;;###autoload
(defun neo-diredy-copy ()
  "Keep marked files, or the current item, ready to copy."
  (interactive) (neo-diredy--action-store 'copy))

;;;###autoload
(defun neo-diredy-cut ()
  "Keep marked files, or the current item, ready to move on paste."
  (interactive) (neo-diredy--action-store 'cut))

(defun neo-diredy-clear-clipboard ()
  "Forget pending copies or moves without changing any files."
  (interactive)
  (setq neo-diredy--clipboard nil)
  (force-mode-line-update t)
  (message "Clipboard cleared"))

(defun neo-diredy--action-conflict (source destination overwrite)
  "Choose a conflict action for SOURCE and DESTINATION.
OVERWRITE permits replacing a regular file.  Return a symbol or a new path."
  (let ((answer (read-char-choice
                 (format "%s exists: [s]kip [n]ame %s[q]uit "
                         (file-name-nondirectory destination)
                         (if overwrite "[o]verwrite [S]kip all [O]verwrite all " ""))
                 (if overwrite '(?s ?n ?o ?S ?O ?q ?\r) '(?s ?n ?q ?\r)))))
    (pcase answer
      (?n (expand-file-name (read-string "New name: " (file-name-nondirectory source))
                            (file-name-directory destination)))
      (?o 'overwrite) (?O 'overwrite-all) (?S 'skip-all) (?q (signal 'quit nil))
      (_ 'skip))))

(defun neo-diredy--action-destination-check (source destination)
  "Reject self-copies and putting SOURCE inside itself at DESTINATION."
  (when (or (neo-diredy--action-equal (neo-diredy--action-path source)
                                     (neo-diredy--action-path destination))
            (and (file-exists-p destination) (file-equal-p source destination)))
    (user-error "Source and destination are the same"))
  (when (and (file-directory-p source) (not (file-symlink-p source))
             (or (neo-diredy--action-within destination source)
                 (file-in-directory-p destination source)))
    (user-error "Cannot put a directory inside itself")))

(defun neo-diredy--action-refresh (paths)
  "Refresh existing Dired listings affected by PATHS, retaining their marks."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'dired-mode) (not (derived-mode-p 'wdired-mode))
                 (not (file-remote-p default-directory)) (file-directory-p default-directory)
                 (cl-some (lambda (path)
                            (assoc (file-name-as-directory (file-name-directory path))
                                   dired-subdir-alist)) paths))
        (condition-case err (revert-buffer nil t)
          (error (message "Could not refresh %s: %s" (buffer-name) (error-message-string err)))))))
  (force-mode-line-update t))

(defun neo-diredy--action-report (verb results)
  "Store RESULTS and summarize VERB without opening another window."
  (setq neo-diredy--action-results results)
  (let ((done (cl-count 'done results :key #'cadr))
        (failed (cl-count 'failed results :key #'cadr))
        (skipped (cl-count 'skipped results :key #'cadr))
        (pending (cl-count 'pending results :key #'cadr)))
    (message "%s: %d done%s%s%s" verb done
             (if (> skipped 0) (format ", %d skipped" skipped) "")
             (if (> failed 0) (format ", %d failed" failed) "")
             (if (> pending 0) (format ", %d pending" pending) ""))))

;;;###autoload
(defun neo-diredy-paste ()
  "Copy or move the internal clipboard into the displayed directory."
  (interactive)
  (neo-diredy--action-check)
  (unless neo-diredy--clipboard (user-error "Clipboard is empty"))
  (require 'dired-aux)
  (let* ((clipboard neo-diredy--clipboard) (kind (car clipboard))
         (directory default-directory) (remaining (copy-sequence (cdr clipboard)))
         (dired-copy-dereference nil) (dired-recursive-copies 'always)
         (dired-overwrite-confirmed nil) (overwrite-backup-query nil)
         results changed claimed all stopped)
    (dolist (source (cdr clipboard))
      (let ((destination (expand-file-name (file-name-nondirectory source) directory))
            (status 'pending) detail overwrite)
        (if stopped (setq detail "Not processed after interruption")
          (condition-case err
              (progn
                (unless (neo-diredy--action-exists source) (error "Source no longer exists"))
                (when (file-remote-p source) (user-error "Remote source"))
                (neo-diredy--action-destination-check source destination)
                (while (and (neo-diredy--action-exists destination) (not overwrite)
                            (not (eq status 'skipped)))
                  (let* ((regular (and (file-regular-p source) (not (file-symlink-p source))
                                       (file-regular-p destination) (not (file-symlink-p destination))))
                         ;; Overwrite-all never authorizes erasing output from
                         ;; another source in this same paste operation.
                         (choice (or (and regular all
                                          (or (eq all 'skip)
                                              (not (cl-member (neo-diredy--action-path destination) claimed
                                                              :test #'neo-diredy--action-equal))) all)
                                     (neo-diredy--action-conflict source destination regular))))
                    (pcase choice
                      ((or 'overwrite 'overwrite-all)
                       (unless regular (error "Only regular files may be overwritten"))
                       (setq overwrite t)
                       (when (eq choice 'overwrite-all) (setq all 'overwrite)))
                      ((or 'skip 'skip-all)
                       (setq status 'skipped detail "Destination exists")
                       (when (eq choice 'skip-all) (setq all 'skip)))
                      ((pred stringp)
                       (unless (and (not (file-remote-p choice))
                                    (neo-diredy--action-equal
                                     (file-truename (file-name-directory choice))
                                     (file-truename directory)))
                         (user-error "Choose a name in the current destination directory"))
                       (setq destination choice)
                       (neo-diredy--action-destination-check source destination))
                      (_ (signal 'quit nil)))))
                (unless (eq status 'skipped)
                  (push (neo-diredy--action-path destination) claimed)
                  (message "%s %d/%d: %s" (if (eq kind 'cut) "Move" "Copy")
                           (1+ (length results)) (length (cdr clipboard)) (file-name-nondirectory source))
                  (let ((dired-overwrite-confirmed overwrite))
                    (if (eq kind 'cut)
                        (dired-rename-file source destination overwrite)
                      (dired-copy-file source destination overwrite)))
                  (unless (and (neo-diredy--action-exists destination)
                               (or (eq kind 'copy) (not (neo-diredy--action-exists source))))
                    (error "Operation did not finish"))
                  (setq status 'done remaining (delete source remaining))))
            (quit (setq stopped t detail "Interrupted; check the destination for partial data"))
            (error (setq status 'failed detail (concat (error-message-string err)
                                                       "; check the destination for partial data")))))
        (push (list source status destination detail) results)
        (push source changed) (push destination changed)))
    (when (and (eq kind 'cut) (eq clipboard neo-diredy--clipboard))
      (setq neo-diredy--clipboard (when remaining (cons 'cut remaining))))
    (neo-diredy--action-refresh changed)
    (neo-diredy--action-report (if (eq kind 'cut) "Move" "Copy") (nreverse results))))

(defun neo-diredy--confirm-delete (files trash)
  "Show all FILES and confirm the actual deletion mode TRASH."
  (let ((buffer (generate-new-buffer " *neo-diredy-delete*")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer buffer
            (insert (if trash "Move to trash\n\n" "Permanently delete\n\n"))
            (dolist (file files) (insert file (if (file-directory-p file) "  (including contents)" "") "\n"))
            (special-mode))
          (display-buffer buffer)
          (yes-or-no-p (format "%s %d item(s)? " (if trash "Trash" "Permanently delete") (length files))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

;;;###autoload
(defun neo-diredy-delete ()
  "Confirm and delete marked files, or the current item."
  (interactive)
  (let ((files (neo-diredy--action-targets t)) (trash delete-by-moving-to-trash) results stopped)
    (unless files (user-error "No files selected"))
    (when (neo-diredy--confirm-delete files trash)
      (dolist (file files)
        (let ((status 'pending) detail)
          (if stopped (setq detail "Not processed after interruption")
            (condition-case err
                (progn
                  (unless (neo-diredy--action-exists file) (error "File no longer exists"))
                  ;; Call trash directly: never fall back to permanent deletion.
                  (if trash (move-file-to-trash file) (dired-delete-file file 'always nil))
                  (setq status 'done))
              (quit (setq stopped t detail "Interrupted"))
              (error (setq status 'failed detail (error-message-string err)))))
          (push (list file status nil detail) results)))
      (neo-diredy--action-refresh files)
      (neo-diredy--action-report (if trash "Trash" "Delete") (nreverse results)))))

(defun neo-diredy--action-status ()
  "Return only active selection/clipboard state; keep the idle view quiet."
  (string-join
   (delq nil (list (when-let* ((marks (and (derived-mode-p 'dired-mode)
                                          (not (derived-mode-p 'wdired-mode))
                                          (neo-diredy--action-marks))))
                    (format "*%d" (length marks)))
                  (when neo-diredy--clipboard
                    (format "%s %d" (if (eq (car neo-diredy--clipboard) 'cut) "Cut" "Copy")
                            (length (cdr neo-diredy--clipboard)))))) " · "))

(defun neo-diredy--action-header ()
  "Fit the current path and short state into the central header."
  (let* ((state (neo-diredy--action-status))
         (suffix (if (string-empty-p state) "" (concat "  |  " state)))
         (width (if (window-live-p neo-diredy--center) (window-total-width neo-diredy--center) (window-width)))
         (path (abbreviate-file-name default-directory))
         (room (max 1 (- width 3 (string-width suffix))))
         (skip (max 0 (- (string-width path) room))))
    (when (> skip 0)
      (setq path (concat "…" (truncate-string-to-width path (string-width path) (1+ skip)))))
    (let ((text (concat "  " path suffix)))
      (if (neo-diredy--editing-p) (propertize text 'face 'warning) text))))

(defun neo-diredy--actions-paint ()
  "Refresh pending icons and marked rows only when their state changes."
  (let ((key (list (current-buffer) (buffer-chars-modified-tick) neo-diredy--clipboard)))
    (unless (equal key neo-diredy--action-paint-key)
      (setq neo-diredy--action-paint-key key)
      (mapc #'delete-overlay neo-diredy--action-overlays)
      (setq neo-diredy--action-overlays nil)
      (let ((marks (neo-diredy--action-marks)))
        (dolist (anchor neo-diredy--overlays)
          (when-let* ((file (overlay-get anchor 'neo-diredy-file)))
            (let ((text (overlay-get anchor 'before-string)))
              (overlay-put anchor 'before-string
                           (concat (substring text 0 (max 0 (- (length text) 2)))
                                   (neo-diredy--icon file (overlay-get anchor 'neo-diredy-directory)))))
            (when (member file marks)
              (save-excursion
                (goto-char (overlay-start anchor))
                (let ((overlay (make-overlay (point) (line-end-position))))
                  (overlay-put overlay 'face 'dired-marked)
                  (overlay-put overlay 'priority -10)
                  (push overlay neo-diredy--action-overlays))))))))))

(defun neo-diredy-toggle-mark ()
  "Toggle the current item's normal mark without moving point."
  (interactive) (neo-diredy--action-check)
  (when (dired-get-filename nil t)
    (save-excursion
      (if (= (char-after (line-beginning-position)) ?*) (dired-unmark 1) (dired-mark 1)))))

(defun neo-diredy-mark-all ()
  "Mark unmarked file rows, retaining existing deletion flags."
  (interactive) (neo-diredy--action-check)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (and (= (char-after) ?\s)
                 (when-let* ((file (dired-get-filename nil t)))
                   (not (member (file-name-nondirectory file) '("." "..")))))
        (save-excursion (dired-mark 1)))
      (forward-line 1))))

(defun neo-diredy-unmark-all ()
  "Clear normal marks, retaining deletion flags."
  (interactive) (neo-diredy--action-check)
  (save-excursion (dired-unmark-all-files ?*)))

(defun neo-diredy--action-view (title lines)
  "Display scrollable LINES under TITLE, leaving files and marks untouched."
  (let ((buffer (get-buffer-create (format "*neo-diredy %s*" title)))
        (directory default-directory))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer) (insert title "\n\n" (string-join lines "\n") "\n"))
      (setq default-directory directory)
      (special-mode)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun neo-diredy-show-targets ()
  "Show the complete list of current action targets."
  (interactive)
  (let ((files (neo-diredy--action-targets)))
    (neo-diredy--action-view (format "Targets (%d)" (length files)) files)))

(defun neo-diredy-show-results ()
  "Show the last operation's per-item outcomes."
  (interactive)
  (unless neo-diredy--action-results (user-error "No operation results"))
  (neo-diredy--action-view
   "Results" (mapcar (lambda (row)
                       (format "%s  %s%s%s" (cadr row) (car row)
                               (if (nth 2 row) (concat " → " (nth 2 row)) "")
                               (if (nth 3 row) (concat "\n    " (nth 3 row)) "")))
                     neo-diredy--action-results)))

;;; Reveal uses Unicode Shell item IDs, grouping selections by parent.

(defvar neo-diredy--reveal-process nil)
(defconst neo-diredy--reveal-script
  "$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'
[Console]::InputEncoding = New-Object System.Text.UTF8Encoding($false)
[Console]::OutputEncoding = New-Object System.Text.UTF8Encoding($false)
try {
Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
public static class NeoDiredyReveal {
  [DllImport(\"ole32.dll\")] static extern int CoInitializeEx(IntPtr p, uint flags);
  [DllImport(\"ole32.dll\")] static extern void CoUninitialize();
  [DllImport(\"shell32.dll\", CharSet = CharSet.Unicode)]
  static extern int SHParseDisplayName(string name, IntPtr context, out IntPtr pidl, uint mask, out uint attributes);
  [DllImport(\"shell32.dll\")] static extern IntPtr ILFindLastID(IntPtr pidl);
  [DllImport(\"shell32.dll\")]
  static extern int SHOpenFolderAndSelectItems(IntPtr folder, uint count, IntPtr[] children, uint flags);
  public static void Show(string directory, string[] files) {
    int init = CoInitializeEx(IntPtr.Zero, 2);
    if (init < 0) Marshal.ThrowExceptionForHR(init);
    IntPtr folder = IntPtr.Zero;
    IntPtr[] full = new IntPtr[files.Length];
    IntPtr[] children = new IntPtr[files.Length];
    try {
      uint attributes;
      Marshal.ThrowExceptionForHR(SHParseDisplayName(System.IO.Path.GetFullPath(directory), IntPtr.Zero, out folder, 0, out attributes));
      for (int i = 0; i < files.Length; i++) {
        Marshal.ThrowExceptionForHR(SHParseDisplayName(System.IO.Path.GetFullPath(files[i]), IntPtr.Zero, out full[i], 0, out attributes));
        children[i] = ILFindLastID(full[i]);
      }
      Marshal.ThrowExceptionForHR(SHOpenFolderAndSelectItems(folder, (uint)children.Length, children, 0));
    } finally {
      foreach (IntPtr item in full) if (item != IntPtr.Zero) Marshal.FreeCoTaskMem(item);
      if (folder != IntPtr.Zero) Marshal.FreeCoTaskMem(folder);
      CoUninitialize();
    }
  }
}
'@
$groups = ConvertFrom-Json -InputObject ([Console]::In.ReadToEnd())
$results = @()
foreach ($group in $groups) {
  if ($null -eq $group) { continue }
  try {
    [NeoDiredyReveal]::Show([string]$group.directory, [string[]]$group.files)
    $results += @{directory=$group.directory; ok=$true}
  } catch { $results += @{directory=$group.directory; ok=$false; error=$_.Exception.Message} }
}
[Console]::WriteLine((ConvertTo-Json -InputObject @($results) -Compress -Depth 5))
} catch { [Console]::Error.WriteLine($_.Exception.Message); exit 1 }
" "Fixed helper; filenames arrive only as UTF-8 JSON on standard input.")

(defun neo-diredy--reveal-groups (files)
  "Group FILES by parent directory while retaining their order."
  (let (groups)
    (dolist (file files)
      (let* ((parent (file-name-directory (directory-file-name file)))
             (group (cl-find parent groups :key #'car :test #'neo-diredy--action-equal)))
        (if group (setcdr group (append (cdr group) (list file)))
          (setq groups (append groups (list (list parent file)))))))
    groups))

(defun neo-diredy--reveal-payload (groups)
  "Encode GROUPS as data without interpolating filenames into code."
  (json-encode (vconcat (mapcar (lambda (group)
                                `((directory . ,(car group)) (files . ,(vconcat (cdr group))))) groups))))

(defun neo-diredy--reveal-command ()
  "Return the fixed Windows PowerShell helper command."
  (let ((program (and (eq system-type 'windows-nt)
                      (expand-file-name "System32/WindowsPowerShell/v1.0/powershell.exe"
                                        (or (getenv "SystemRoot") "C:/Windows")))))
    (unless (and program (file-executable-p program))
      (user-error "Reveal requires Windows PowerShell"))
    (list program "-NoLogo" "-NoProfile" "-NonInteractive" "-STA" "-OutputFormat" "Text" "-EncodedCommand"
          (base64-encode-string (encode-coding-string neo-diredy--reveal-script 'utf-16le) t))))

(defun neo-diredy--reveal-report (groups output failure)
  "Report each item in GROUPS using helper OUTPUT or FAILURE."
  (let ((data (unless failure
                (condition-case nil
                    (json-parse-string output :object-type 'alist :array-type 'array
                                       :null-object nil :false-object nil)
                  (error nil)))) results)
    (unless (vectorp data) (setq data []))
    (dolist (group groups)
      (let ((row (seq-find (lambda (x) (and (listp x) (cl-every #'consp x)
                                          (equal (car group) (alist-get 'directory x)))) data)))
        (dolist (file (cdr group))
          (push (list file (if (and row (eq (alist-get 'ok row) t)) 'done 'failed) nil
                      (unless (and row (eq (alist-get 'ok row) t))
                        (or failure (alist-get 'error row) "Invalid or missing Reveal result"))) results))))
    (neo-diredy--action-report "Reveal" (nreverse results))))

(defun neo-diredy--reveal-stop ()
  "Cancel our helper and discard its callback and temporary buffer."
  (when-let* ((process neo-diredy--reveal-process))
    (setq neo-diredy--reveal-process nil)
    (when-let* ((timer (process-get process 'timer))) (cancel-timer timer))
    (set-process-sentinel process #'ignore)
    (when (process-live-p process) (delete-process process))
    (when-let* ((errors (process-get process 'errors))) (delete-process errors))
    (when (buffer-live-p (process-buffer process)) (kill-buffer (process-buffer process)))))

;;;###autoload
(defun neo-diredy-reveal ()
  "Reveal one item or all normal marks together in Windows Explorer."
  (interactive)
  (let* ((files (neo-diredy--action-targets)) (groups (neo-diredy--reveal-groups files))
         (command (neo-diredy--reveal-command))
         ;; Windows Emacs may launch through an ANSI current-directory path.
         ;; The helper needs no working files; pass every target via UTF-8 stdin.
         (default-directory (file-name-directory (car command))))
    (unless files (user-error "No files selected"))
    (when neo-diredy--reveal-process (user-error "Reveal is already running"))
    (let ((buffer (generate-new-buffer " *neo-diredy-reveal*"))
          ;; PowerShell diagnostics are not protocol data; discard them without
          ;; accumulating an unbounded stderr buffer.
          (errors (make-pipe-process :name "neo-diredy-reveal-stderr" :noquery t
                                     :filter #'ignore :sentinel #'ignore)))
      (condition-case err
          (let ((process
                 (make-process
                  :name "neo-diredy-reveal" :buffer buffer :stderr errors :command command
                  :coding 'utf-8-unix :connection-type 'pipe :noquery t
                  :filter (lambda (process text)
                            (when (buffer-live-p (process-buffer process))
                              (with-current-buffer (process-buffer process)
                                (if (> (+ (buffer-size) (length text)) (* 1024 1024))
                                    (progn (process-put process 'failure "Reveal output limit") (delete-process process))
                                  (goto-char (point-max)) (insert text)))))
                  :sentinel
                  (lambda (process _event)
                    (when (and (eq process neo-diredy--reveal-process)
                               (memq (process-status process) '(exit signal)))
                      (let ((output (with-current-buffer buffer (buffer-string)))
                            (failure (or (process-get process 'failure)
                                         (unless (zerop (process-exit-status process)) "Reveal helper failed"))))
                        (neo-diredy--reveal-stop)
                        (neo-diredy--reveal-report groups output failure)))))))
            (setq neo-diredy--reveal-process process)
            (process-put process 'errors errors)
            (process-put process 'timer
                         (run-at-time 30 nil
                                      (lambda ()
                                        (when (eq process neo-diredy--reveal-process)
                                          (process-put process 'failure "Reveal timed out")
                                          (delete-process process)))))
            (process-send-string process (neo-diredy--reveal-payload groups))
            (process-send-eof process)
            (message "Reveal %d" (length files)))
        (error (neo-diredy--reveal-stop)
               (when (buffer-live-p buffer) (kill-buffer buffer))
               (when (process-live-p errors) (delete-process errors))
               (neo-diredy--reveal-report groups "" (error-message-string err)))))))

;;;###autoload
(defun neo-diredy-actions ()
  "Choose the same file action for one item or all ordinary marks."
  (interactive) (neo-diredy--action-check)
  (let ((files (neo-diredy--action-targets))
        (choices `(("c  Copy" . copy) ("x  Cut" . cut)
                   (,(if neo-diredy--clipboard
                         (format "v  Paste (%s %d)" (car neo-diredy--clipboard) (length (cdr neo-diredy--clipboard)))
                       "v  Paste") . paste)
                   (,(let ((count (length (neo-diredy--reveal-groups (neo-diredy--action-targets)))))
                       (if (> count 1) (format "r  Reveal (%d folders)" count) "r  Reveal")) . reveal)
                   (,(if delete-by-moving-to-trash "d  Trash…" "d  Delete…") . delete)
                   ("?  Targets" . targets) ("m  Mark / unmark" . mark)
                   ("a  Mark all" . all) ("u  Unmark all" . unmark)
                   ("q  Clear clipboard" . clear) ("!  Results" . results)))
        disabled)
    (unless files
      (dolist (action '(copy cut reveal delete targets mark))
        (push (cons action "No target") disabled)))
    (unless (eq system-type 'windows-nt) (push '(reveal . "Windows only") disabled))
    (when neo-diredy--reveal-process (push '(reveal . "Reveal running") disabled))
    (unless neo-diredy--clipboard
      (push '(paste . "Clipboard empty") disabled) (push '(clear . "Clipboard empty") disabled))
    (unless neo-diredy--action-results (push '(results . "No results") disabled))
    (when-let* ((choice (neo-diredy--read-choice
                        (format "Actions · %d" (length files)) choices
                        (list :keys '((?c . copy) (?x . cut) (?v . paste) (?r . reveal) (?d . delete)
                                      (?? . targets) (?m . mark) (?a . all) (?u . unmark) (?q . clear) (?! . results))
                              :disabled disabled))))
      (pcase (cdr choice)
        ('copy (neo-diredy-copy)) ('cut (neo-diredy-cut)) ('paste (neo-diredy-paste))
        ('reveal (neo-diredy-reveal)) ('delete (neo-diredy-delete))
        ('targets (neo-diredy-show-targets)) ('results (neo-diredy-show-results))
        ('mark (neo-diredy-toggle-mark)) ('all (neo-diredy-mark-all))
        ('unmark (neo-diredy-unmark-all)) ('clear (neo-diredy-clear-clipboard))))))

(defun neo-diredy--bookmark-choices ()
  "Return the directory bookmarks created by neo-diredy."
  (bookmark-maybe-load-default-file)
  (cl-loop for record in bookmark-alist
           for path = (bookmark-get-filename record)
           when (and (bookmark-prop-get record 'neo-diredy) (stringp path))
           collect (cons (format "%s — %s" (car record) (abbreviate-file-name path)) (car record))))

;;;###autoload
(defun neo-diredy-bookmark-add (name)
  "Bookmark the current local Dired directory as NAME."
  (interactive (list (read-string "Bookmark directory as: "
                                  (file-name-nondirectory (directory-file-name default-directory)))))
  (unless (derived-mode-p 'dired-mode) (user-error "Use this command in Dired"))
  (when (derived-mode-p 'wdired-mode) (user-error "Finish or cancel renaming first"))
  (when (file-remote-p default-directory) (user-error "Bookmarks require a local directory"))
  (neo-diredy--check-open-file default-directory)
  (setq name (string-trim name))
  (when (string-empty-p name) (user-error "Enter a bookmark name"))
  (bookmark-maybe-load-default-file)
  (when (bookmark-get-bookmark name t) (user-error "Bookmark already exists: %s" name))
  (bookmark-store name `((filename . ,default-directory) (position . 1) (neo-diredy . t)) nil)
  (message "Bookmark added: %s" name))

;;;###autoload
(defun neo-diredy-bookmark-jump ()
  "Choose a directory bookmark in a child frame and visit it."
  (interactive)
  (when (derived-mode-p 'wdired-mode) (user-error "Finish or cancel renaming first"))
  (when-let* ((choice (neo-diredy--read-choice "Bookmarks" (neo-diredy--bookmark-choices)))
              (path (bookmark-get-filename (cdr choice))))
    (neo-diredy--check-open-file path)
    (unless (file-directory-p path) (user-error "Bookmark is no longer a directory: %s" path))
    (dired path)))

;;;###autoload
(defun neo-diredy-bookmark-delete ()
  "Choose and delete one neo-diredy bookmark in a child frame."
  (interactive)
  (when-let* ((choice (neo-diredy--read-choice "Delete bookmark" (neo-diredy--bookmark-choices))))
    (bookmark-delete (cdr choice))
    (message "Bookmark deleted: %s" (cdr choice))))

(defun neo-diredy--append-name (count)
  "Start a WDired session and append after point COUNT times with Evil."
  (interactive "p")
  (unless (derived-mode-p 'wdired-mode)
    (dired-toggle-read-only))
  (evil-append count))

;;;###autoload
(defun neo-diredy-preview-scroll-up (&optional lines)
  "Scroll the preview forward LINES lines, defaulting to five."
  (interactive "P")
  (unless (and neo-diredy--layout-mode (window-live-p neo-diredy--right))
    (user-error "No active neo-diredy preview"))
  (neo-diredy--refresh)
  (with-selected-window neo-diredy--right
    (condition-case nil (scroll-up (if lines (prefix-numeric-value lines) 5))
      (beginning-of-buffer
       (goto-char (point-min)) (set-window-start nil (point-min)))
      (end-of-buffer nil))))

;;;###autoload
(defun neo-diredy-preview-scroll-down (&optional lines)
  "Scroll the preview backward LINES lines, defaulting to five."
  (interactive "P")
  (neo-diredy-preview-scroll-up (- (if lines (prefix-numeric-value lines) 5))))

(defun neo-diredy-preview-close ()
  "Close an expanded preview and return to its directory."
  (interactive)
  (let ((origin neo-diredy--preview-origin))
    (quit-window t)
    (when (and (eq (current-buffer) origin) (derived-mode-p 'dired-mode)
               (>= (window-total-width) 72))
      (neo-diredy--layout-mode 1))))

(define-derived-mode neo-diredy-preview-mode special-mode "Neo Preview"
  "Read-only snapshot of a neo-diredy preview."
  (setq-local truncate-lines nil))
(dolist (key '("q" "<escape>" "C-c p"))
  (define-key neo-diredy-preview-mode-map (kbd key) #'neo-diredy-preview-close))

;;;###autoload
(defun neo-diredy-preview-expand ()
  "Show the current preview in the full window without visiting its file."
  (interactive)
  (unless (and neo-diredy--layout-mode (neo-diredy--layout-intact-p))
    (user-error "No active neo-diredy preview"))
  (when (neo-diredy--editing-p) (user-error "Finish or cancel renaming first"))
  (neo-diredy--refresh)
  (when (process-live-p neo-diredy--art-process) (user-error "Preview is still loading"))
  (let ((source (window-buffer neo-diredy--center))
        (preview neo-diredy--preview)
        (header (neo-diredy--preview-header))
        (buffer (generate-new-buffer " *neo-diredy-expanded-preview*")) shown)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delay-mode-hooks (neo-diredy-preview-mode))
            (let ((inhibit-read-only t)) (insert-buffer-substring preview))
            (setq-local neo-diredy--preview-origin source
                        default-directory (buffer-local-value 'default-directory source)
                        header-line-format header)
            (goto-char (point-min))
            (set-buffer-modified-p nil))
          (select-window neo-diredy--center)
          (neo-diredy--layout-mode -1)
          (pop-to-buffer buffer '(display-buffer-same-window))
          (let ((image (get-text-property (point-min) 'display)))
            (when (eq (car-safe image) 'image)
              (setq image (copy-tree image))
              (setf (image-property image :max-width) (window-body-width nil t)
                    (image-property image :max-height) (window-body-height nil t))
              (let ((inhibit-read-only t))
                (put-text-property (point-min) (1+ (point-min)) 'display image)
                (set-buffer-modified-p nil))))
          (setq shown t))
      (unless shown (kill-buffer buffer)))))

(define-minor-mode neo-diredy--edit-keys-mode
  "Use opener and editing entry keys in buffers owned by the active layout."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") #'neo-diredy-actions)
            (define-key map (kbd "<S-return>") #'neo-diredy-open)
            (define-key map (kbd "<S-kp-enter>") #'neo-diredy-open)
            (define-key map (kbd "C-c b a") #'neo-diredy-bookmark-add)
            (define-key map (kbd "C-c b j") #'neo-diredy-bookmark-jump)
            (define-key map (kbd "C-c b d") #'neo-diredy-bookmark-delete)
            (define-key map (kbd "M-<down>") #'neo-diredy-preview-scroll-up)
            (define-key map (kbd "M-<up>") #'neo-diredy-preview-scroll-down)
            (define-key map (kbd "C-c p") #'neo-diredy-preview-expand)
            map)
  (when (fboundp 'evil-normalize-keymaps)
    (evil-normalize-keymaps)))

(with-eval-after-load 'evil
  (when (boundp 'neo-diredy--edit-keys-mode-map)
    (evil-define-key* 'normal neo-diredy--edit-keys-mode-map "a" #'neo-diredy--append-name)
    (evil-define-key* '(normal visual motion) neo-diredy--edit-keys-mode-map
                      (kbd "C-c f") #'neo-diredy-actions
                      (kbd "<S-return>") #'neo-diredy-open
                      (kbd "<S-kp-enter>") #'neo-diredy-open
                      (kbd "C-c b a") #'neo-diredy-bookmark-add
                      (kbd "C-c b j") #'neo-diredy-bookmark-jump
                      (kbd "C-c b d") #'neo-diredy-bookmark-delete
                      (kbd "M-<down>") #'neo-diredy-preview-scroll-up
                      (kbd "M-<up>") #'neo-diredy-preview-scroll-down
                      (kbd "C-c p") #'neo-diredy-preview-expand)
    (evil-define-key* '(normal motion) neo-diredy-preview-mode-map
                      "q" #'neo-diredy-preview-close
                      (kbd "<escape>") #'neo-diredy-preview-close
                      (kbd "C-c p") #'neo-diredy-preview-close)))

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
  (unless neo-diredy--edit-keys-mode (neo-diredy--edit-keys-mode 1))
  (setq-local truncate-lines t
              cursor-type (when (derived-mode-p 'wdired-mode) (or cursor-type t))
              display-line-numbers nil
              hl-line-face 'neo-diredy-selection
              hl-line-range-function #'neo-diredy--selection-range
              header-line-format
              '(:eval (neo-diredy--action-header)))
  (dired-hide-details-mode 1)
  (unless neo-diredy--modeline-face
    (setq neo-diredy--modeline-face
          (face-remap-add-relative
           'mode-line-inactive
           `(:filtered (:window neo-diredy-modeline t)
                       ,(if (facep 'mode-line-active) 'mode-line-active 'mode-line)))))
  (when (window-live-p neo-diredy--center)
    (set-window-parameter neo-diredy--center 'mode-line-format 'none)
    (let ((width (if (>= (window-total-width neo-diredy--center) 32) 16 0)))
      (unless (= (or (cdr (window-margins neo-diredy--center)) 0) width)
        (set-window-margins neo-diredy--center (car neo-diredy--margins) width))))
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
    (neo-diredy--metadata-paint))
  (neo-diredy--actions-paint)
  (neo-diredy--normalize-point)
  (hl-line-mode 1)
  (hl-line-highlight)
  (when (overlayp hl-line-overlay)
    (overlay-put hl-line-overlay 'face 'neo-diredy-selection))
  (neo-diredy--highlight-icon)
  (neo-diredy--modeline-sync))

(defun neo-diredy--protect-margins (original window left &optional right)
  "Keep the owned row metadata column when other layout code sets margins."
  (when (and neo-diredy--layout-mode
             (eq (or window (selected-window)) neo-diredy--center)
             (assq (window-buffer neo-diredy--center) neo-diredy--buffers))
    (setq right (if (>= (window-total-width neo-diredy--center) 32) 16 0)))
  (funcall original window left right))

(defun neo-diredy--dwim-targets (original &rest args)
  "Exclude the owned modeline from standard Dired destination policies."
  (if (or (not (window-live-p neo-diredy--modeline-window))
          (not (eq (window-buffer neo-diredy--modeline-window)
                   (cdr (assq neo-diredy--modeline-window neo-diredy--windows))))
          (and (functionp dired-dwim-target)
               (not (memq dired-dwim-target
                          '(dired-dwim-target-next dired-dwim-target-next-visible
                            dired-dwim-target-recent)))))
      (apply original args)
    (let* ((recent (eq dired-dwim-target 'dired-dwim-target-recent))
           (frames (and (memq dired-dwim-target
                              '(dired-dwim-target-next-visible dired-dwim-target-recent))
                        'visible))
           (windows (remq neo-diredy--modeline-window
                          (remq (selected-window)
                                (window-list-1 (next-window nil 'nomini frames)
                                               'nomini frames)))))
      (when recent
        (setq windows (sort windows (lambda (a b) (> (window-use-time a) (window-use-time b))))))
      (mapcan (lambda (window)
                (with-current-buffer (window-buffer window)
                  (when (eq major-mode 'dired-mode)
                    (list (dired-current-directory)))))
              windows))))

(defun neo-diredy--modeline-select ()
  "Return keyboard commands to the list after a mode-line mouse action."
  (when (and (window-live-p neo-diredy--center)
             (window-live-p neo-diredy--modeline-window)
             (assq (window-buffer neo-diredy--center) neo-diredy--buffers)
             (eq (window-buffer neo-diredy--modeline-window)
                 (cdr (assq neo-diredy--modeline-window neo-diredy--windows)))
             (eq (selected-window) neo-diredy--modeline-window))
    (select-window neo-diredy--center)))

(defun neo-diredy--modeline-resize (frame)
  "Refit the native bar after fonts or window dimensions change on FRAME."
  ;; A temporary side window can enlarge the bar to the normal minimum
  ;; height.  Refit it even while that popup makes the full layout non-intact;
  ;; `neo-diredy--modeline-sync' still checks ownership of the bar itself.
  (when (and neo-diredy--layout-mode (window-live-p neo-diredy--center)
             (eq frame (window-frame neo-diredy--center))
             (assq (window-buffer neo-diredy--center) neo-diredy--buffers))
    (neo-diredy--modeline-sync)))

(defun neo-diredy--modeline-activity (&rest _)
  "Use the active face only while the central list has the selection."
  (when (window-live-p neo-diredy--modeline-window)
    (set-window-parameter neo-diredy--modeline-window 'neo-diredy-modeline
                          (eq (selected-window) neo-diredy--center))))

(defun neo-diredy--modeline-active-p (original)
  "Treat the owned bar as active while its Dired list is selected."
  (or (funcall original)
      (and neo-diredy--layout-mode
           (eq (selected-window) neo-diredy--modeline-window)
           (eq (old-selected-window) neo-diredy--center))))

(defun neo-diredy--modeline-sync ()
  "Show the center's native mode line across the original window's width."
  (when (and (window-live-p neo-diredy--modeline-window)
             (window-live-p neo-diredy--center))
    (let* ((bar neo-diredy--modeline-window)
           (buffer (window-buffer neo-diredy--center))
           (entry (assq bar neo-diredy--windows)))
      (when (or (not entry) (eq (window-buffer bar) (cdr entry)))
        (unless (eq (window-buffer bar) buffer)
          ;; Redisplay scrolls the one-pixel bar to its point.  When it is
          ;; the last window to leave a buffer, Emacs remembers that start
          ;; for the next visit.  Preserve the center's actual viewport.
          (when-let* ((previous (assq (window-buffer bar)
                                     (window-prev-buffers neo-diredy--center))))
            (set-window-start bar (nth 1 previous) t))
          (set-window-dedicated-p bar nil)
          (set-window-buffer bar buffer)
          (set-window-dedicated-p bar t)
          (when entry (setcdr entry buffer)))
        (set-window-point bar (window-point neo-diredy--center))
        (set-window-start bar (window-start neo-diredy--center) t)
        (let* ((height (window-mode-line-height bar))
               ;; Emacs hides a mode line if no text pixel remains.  One
               ;; pixel keeps native rendering, images, alignment and maps.
               (pixels (if (display-graphic-p (window-frame bar)) (1+ height)
                         (* 2 (frame-char-height (window-frame bar)))))
               (window-resize-pixelwise t))
          (when (and (> height 0) (/= pixels (window-pixel-height bar)))
            (ignore-errors
              (window-resize bar (- pixels (window-pixel-height bar)) nil 'safe t))))))))

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

(defun neo-diredy--git-display-data (file)
  "Return cached Git display fields for FILE in the current directory."
  (when (and neo-diredy-git-status (equal default-directory neo-diredy--git-location))
    (if (car neo-diredy--git-state)
        (let ((xy (neo-diredy--git-label file)))
          (list :git (if (equal xy "  ") "Git" (format "Git [%s]" xy))
                :branch (car neo-diredy--git-state)))
      (when neo-diredy--git-message (list :git (concat "Git " neo-diredy--git-message))))))

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
  (when (window-live-p neo-diredy--center)
    (with-current-buffer (window-buffer neo-diredy--center)
      (save-excursion
        (goto-char (window-point neo-diredy--center))
        (neo-diredy--highlight-icon))))
  (neo-diredy--details-paint)
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
  (setq neo-diredy--size-cache nil neo-diredy--git-location nil
        neo-diredy--git-state nil neo-diredy--git-message nil))

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
    (setq neo-diredy--git-location default-directory neo-diredy--git-state nil
          neo-diredy--git-message (when neo-diredy-git-status "missing"))
    (when-let* ((git (and neo-diredy-git-status (executable-find "git"))))
      (setq neo-diredy--git-message "loading")
      (let ((directory default-directory)
            (process-environment (copy-sequence process-environment)))
        (setenv "GIT_OPTIONAL_LOCKS" "0")
        (neo-diredy--metadata-start
         'git directory (list git "-C" directory "rev-parse" "--show-toplevel")
         (lambda (root)
           (if (not root)
               (progn (setq neo-diredy--git-message "unavailable") (neo-diredy--metadata-paint))
             (setq root (string-trim-right root "[\r\n]+"))
             (let ((process-environment (copy-sequence process-environment)))
               (setenv "GIT_OPTIONAL_LOCKS" "0")
               (neo-diredy--metadata-start
                'git directory
                (list git "-C" root "status" "--porcelain=v1" "-z" "--branch" "--untracked-files=normal")
                (lambda (output)
                  (setq neo-diredy--git-message (unless output "error"))
                  (when output (setq neo-diredy--git-state (neo-diredy--git-parse root output)))
                  (neo-diredy--metadata-paint)))))))))
    (neo-diredy--metadata-paint))
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

;;; Preview details (bounded reads; no visiting files or running major modes)

(defun neo-diredy--details-layout ()
  "Reserve at most a quarter of the right column for three detail lines."
  (let* ((neo-diredy--details-resizing t)
         (footer neo-diredy--details-window)
         (height (+ (window-total-height neo-diredy--right)
                    (if (window-live-p footer) (window-total-height footer) 0)))
         (column-pixels (+ (window-pixel-height neo-diredy--right)
                           (if (window-live-p footer) (window-pixel-height footer) 0)))
         (line-pixels (frame-char-height (window-frame neo-diredy--right)))
         (pixels (* 3 line-pixels))
         (window-resize-pixelwise t)
         (window-min-height 2))
    (when (and (>= height 16) (<= (* 4 pixels) column-pixels) (not (window-live-p footer)))
      (setq footer (ignore-errors (split-window neo-diredy--right -4 'below))
            neo-diredy--details-window footer)
      (when footer
        (set-window-buffer footer neo-diredy--details-buffer)
        (set-window-dedicated-p footer t)
        (set-window-parameter footer 'no-other-window t)
        (push (cons footer neo-diredy--details-buffer) neo-diredy--windows)))
    (when (window-live-p footer)
      (with-current-buffer neo-diredy--preview (setq mode-line-format nil))
      ;; The only mode line belongs to the full-width bottom window.
      (unless (= pixels (window-pixel-height footer))
        (ignore-errors (window-resize footer (- pixels (window-pixel-height footer)) nil nil t)))
      (when (or (< height 16)
                (< (window-body-height neo-diredy--right) 6)
                (> (* 4 (window-pixel-height footer))
                   (+ (window-pixel-height footer) (window-pixel-height neo-diredy--right))))
        (setq neo-diredy--windows (assq-delete-all footer neo-diredy--windows))
        (delete-window footer)
        (setq neo-diredy--details-window nil)))))

(defun neo-diredy--preview-header ()
  "Keep compact details beside the preview title when the footer cannot fit."
  (let* ((width (max 1 (- (window-body-width neo-diredy--right) 2)))
         (title (if (car neo-diredy--details-key)
                    (file-name-nondirectory (car neo-diredy--details-key)) "Preview"))
         (details (unless (window-live-p neo-diredy--details-window)
                    (neo-diredy--details-compact (neo-diredy--details-display-data)
                                                (max 1 (/ width 2)))))
         (suffix (if (or (not details) (string-empty-p details)) "" (concat " | " details))))
    (concat "  " (truncate-string-to-width title (max 0 (- width (string-width suffix))) nil nil "…")
            suffix)))

(defun neo-diredy--details-resize (frame)
  "Refit the preview and details after FRAME changes size."
  (when (and (not neo-diredy--details-resizing) neo-diredy--layout-mode
             (window-live-p neo-diredy--center)
             (eq frame (window-frame neo-diredy--center))
             (neo-diredy--layout-intact-p))
    (let ((neo-diredy--details-resizing t)) (neo-diredy--refresh))))

(defun neo-diredy--details-fit (items width)
  "Fit whole ITEMS into WIDTH columns, dropping lower priority fields."
  (let ((line ""))
    (dolist (item (delq nil (copy-sequence items)))
      (let* ((clean (replace-regexp-in-string "[[:cntrl:]]" " " item))
             (candidate (if (string-empty-p line) clean (concat line " · " clean))))
        (when (<= (string-width candidate) width) (setq line candidate))))
    line))

(defun neo-diredy--details-lines (data width)
  "Return up to three detail lines for DATA, each fitting WIDTH columns."
  (list (neo-diredy--details-fit (list (plist-get data :format) (plist-get data :size)) width)
        (let ((items (plist-get data :primary)))
          (when (and (eq (plist-get data :kind) 'binary) (car items)
                     (> (length (car items)) width) (>= width 23))
            (setq items (list (substring (car items) 0 23))))
          (neo-diredy--details-fit items width))
        (neo-diredy--details-fit (append (list (neo-diredy--details-git data
                                                  (if (plist-get data :extra) (max 12 (/ width 2)) width)))
                                        (plist-get data :extra)
                                        (list (plist-get data :modified))) width)))

(defun neo-diredy--details-display-data ()
  "Combine file details with current Git and size caches, without I/O.
Keep these volatile fields out of the file-content cache."
  (let ((data (copy-sequence neo-diredy--details-data))
        (file (car neo-diredy--details-key)))
    (when (and file (window-live-p neo-diredy--center))
      (with-current-buffer (window-buffer neo-diredy--center)
        (when (and neo-diredy--file-sizes
                   (eq (gethash file neo-diredy--file-sizes) 'directory)
                   neo-diredy-directory-sizes)
          (setq data (plist-put data :size (neo-diredy--size-label file))))
        (when (string-prefix-p default-directory file (eq system-type 'windows-nt))
          (setq data (append data (neo-diredy--git-display-data file))))))
    data))

(defun neo-diredy--details-git (data width)
  "Fit Git state and branch from DATA to WIDTH, abbreviating the branch."
  (when-let* ((git (plist-get data :git)))
    (let* ((prefix (if (> (string-width git) width) "Git" git))
           (branch (replace-regexp-in-string "[[:cntrl:]]" " " (or (plist-get data :branch) "")))
           (room (- width (string-width prefix) 1)))
      (if (and (> room 0) (not (string-empty-p branch)))
          (concat prefix " " (truncate-string-to-width branch room nil nil "…"))
        prefix))))

(defun neo-diredy--details-compact (data width)
  "Keep Git state visible in the one-line fallback, followed by file details."
  (neo-diredy--details-fit (list (plist-get data :git) (plist-get data :size)
                                (plist-get data :format) (plist-get data :branch)) width))

(defun neo-diredy--details-paint ()
  "Paint cached details without any file I/O during redisplay."
  (when (and (buffer-live-p neo-diredy--details-buffer)
             (window-live-p neo-diredy--right))
    (with-current-buffer neo-diredy--details-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-join
                 (mapcar (lambda (line) (concat " " line))
                         (neo-diredy--details-lines (neo-diredy--details-display-data)
                                                    (max 1 (- (window-body-width neo-diredy--right) 2))))
                 "\n"))
        (add-face-text-property (point-min) (point-max) 'shadow)
        (goto-char (point-min))
        (set-buffer-modified-p nil)))
    (force-mode-line-update t)))

(defun neo-diredy--details-image-header (bytes)
  "Return format, dimensions and depth from a bounded image header BYTES."
  (cond
   ((and (>= (length bytes) 26) (string-prefix-p "8BPS" bytes)
         (memq (neo-diredy--art-uint bytes 4 2) '(1 2)))
    (list (if (= (neo-diredy--art-uint bytes 4 2) 2) "PSB" "PSD")
          (neo-diredy--art-uint bytes 18 4) (neo-diredy--art-uint bytes 14 4)
          (neo-diredy--art-uint bytes 22 2)))
   ((and (>= (length bytes) 25)
         (string-prefix-p (unibyte-string 137 80 78 71 13 10 26 10) bytes)
         (equal (substring bytes 12 16) "IHDR"))
    (list "PNG" (neo-diredy--art-uint bytes 16 4) (neo-diredy--art-uint bytes 20 4)
          (aref bytes 24)))
   ((and (>= (length bytes) 10) (member (substring bytes 0 6) '("GIF87a" "GIF89a")))
    (list "GIF" (+ (aref bytes 6) (* 256 (aref bytes 7)))
          (+ (aref bytes 8) (* 256 (aref bytes 9)))))
   ((and (> (length bytes) 4) (string-prefix-p (unibyte-string 255 216) bytes))
    (let ((offset 2) found)
      (while (and (not found) (< (+ offset 4) (length bytes)) (= (aref bytes offset) 255))
        (let ((marker (aref bytes (1+ offset))))
          (cond
           ((= marker 255) (setq offset (1+ offset)))
           ((memq marker '(216 217 218)) (setq offset (length bytes)))
           (t
            (let ((size (neo-diredy--art-uint bytes (+ offset 2) 2)))
              (when (and (memq marker '(192 193 194 195 197 198 199 201 202 203 205 206 207))
                         (>= size 8) (<= (+ offset 9) (length bytes)))
                (setq found (list "JPEG" (neo-diredy--art-uint bytes (+ offset 7) 2)
                                  (neo-diredy--art-uint bytes (+ offset 5) 2)
                                  (aref bytes (+ offset 4)))))
              (setq offset (if (< size 2) (length bytes) (+ offset 2 size))))))))
      (or found '("JPEG"))))))

(defun neo-diredy--details-language (file text)
  "Guess a language from FILE and TEXT without executing mode selection."
  (or (when (equal (file-name-extension file) "C") "C++")
      (cdr (assoc (downcase (file-name-nondirectory file))
                  '(("makefile" . "Makefile") ("gnumakefile" . "Makefile")
                    ("gemfile" . "Ruby") ("rakefile" . "Ruby")
                    (".bashrc" . "Shell") (".bash_profile" . "Shell")
                    (".zshrc" . "Shell") (".zprofile" . "Shell") (".profile" . "Shell")
                    (".gitignore" . "Git ignore") (".gitconfig" . "INI"))))
      (cdr (assoc (downcase (or (file-name-extension file) ""))
                  '(("el" . "Emacs Lisp") ("py" . "Python") ("js" . "JavaScript")
                    ("pyw" . "Python") ("pyi" . "Python")
                    ("mjs" . "JavaScript") ("cjs" . "JavaScript")
                    ("jsx" . "JavaScript/JSX") ("ts" . "TypeScript") ("tsx" . "TypeScript/TSX")
                    ("rs" . "Rust") ("go" . "Go") ("c" . "C") ("h" . "C/C++ header")
                    ("cpp" . "C++") ("hpp" . "C++ header") ("cs" . "C#") ("java" . "Java")
                    ("cc" . "C++") ("cxx" . "C++") ("hh" . "C++ header") ("hxx" . "C++ header")
                    ("rb" . "Ruby") ("php" . "PHP") ("lua" . "Lua") ("sh" . "Shell")
                    ("gemspec" . "Ruby") ("bash" . "Shell") ("zsh" . "Shell") ("fish" . "Fish")
                    ("ps1" . "PowerShell") ("json" . "JSON") ("yaml" . "YAML") ("yml" . "YAML")
                    ("psm1" . "PowerShell") ("psd1" . "PowerShell")
                    ("toml" . "TOML") ("xml" . "XML") ("html" . "HTML") ("css" . "CSS")
                    ("htm" . "HTML") ("xaml" . "XML")
                    ("md" . "Markdown") ("sql" . "SQL") ("tex" . "LaTeX") ("svg" . "SVG")
                    ("markdown" . "Markdown") ("mdown" . "Markdown")
                    ("mk" . "Makefile") ("ini" . "INI") ("cfg" . "INI")
                    ("bat" . "Batch") ("cmd" . "Batch") ("pl" . "Perl") ("pm" . "Perl")
                    ("org" . "Org")
                    ("diff" . "Diff") ("patch" . "Diff") ("rej" . "Diff"))))
      (when (string-prefix-p "#!" text)
        (let ((first (car (split-string text "\n"))))
          (cond ((string-match-p "python" first) "Python")
                ((string-match-p "node" first) "JavaScript")
                ((string-match-p "\\bruby\\b" first) "Ruby")
                ((string-match-p "\\bperl\\b" first) "Perl")
                ((string-match-p "\\blua\\b" first) "Lua")
                ((string-match-p "\\b\\(?:pwsh\\|powershell\\)\\b" first) "PowerShell")
                ((string-match-p "\\bfish\\b" first) "Fish")
                ((string-match-p "\\b\\(ba\\|z\\|k\\|da\\)?sh\\b" first) "Shell"))))
      "Text"))

(defun neo-diredy--details-utf32 (bytes little-endian)
  "Decode bounded UTF-32 BYTES after its BOM, using LITTLE-ENDIAN order."
  (let ((text (string-to-multibyte (make-string (max 0 (/ (- (length bytes) 4) 4)) 0))))
    (dotimes (i (length text))
      (let* ((offset (+ 4 (* i 4)))
             (unit (substring bytes offset (+ offset 4)))
             (value (neo-diredy--art-uint (if little-endian (reverse unit) unit) 0 4)))
        (unless (and (<= value #x10ffff) (not (<= #xd800 value #xdfff)))
          (error "Invalid UTF-32 scalar"))
        (aset text i value)))
    text))

(defun neo-diredy--details-read (file attributes)
  "Read bounded descriptive data for local FILE using ATTRIBUTES."
  (let* ((size (file-attribute-size attributes))
         (data (list :size (if (numberp size) (file-size-human-readable size 'iec " ") "")
                     :modified (when attributes (format-time-string "%Y-%m-%d %H:%M"
                                                                    (file-attribute-modification-time attributes)))))
         (extension (downcase (or (file-name-extension (or file "")) ""))))
    (cond
     ((or (null file) (file-remote-p file) (null attributes)) (plist-put data :format "Unavailable"))
     ((eq (file-attribute-type attributes) t)
      (plist-put (plist-put data :size nil) :format "Directory"))
     ((not (file-regular-p file)) (plist-put data :format "Special file"))
     (t
      (let* ((bytes (neo-diredy--art-read file 0 (min size 65536)))
             (image (neo-diredy--details-image-header bytes))
             (binary (cond ((string-prefix-p "\177ELF" bytes) "ELF")
                           ((string-prefix-p "MZ" bytes) "DOS/PE")
                           ((string-prefix-p "PK\3\4" bytes) "ZIP")
                           ((string-prefix-p "%PDF-" bytes) "PDF")
                           ((string-prefix-p "SQLite format 3\0" bytes) "SQLite"))))
        (cond
         (image
          (setq data (plist-put data :kind 'image)
                data (plist-put data :format (car image)))
          (when (and (cadr image) (caddr image))
            (setq data (plist-put data :primary (list (format "%d × %d" (cadr image) (caddr image))))))
          (when (nth 3 image)
            (setq data (plist-put data :extra
                                 (list (format "%d %s" (nth 3 image)
                                               (if (equal (car image) "PNG") "bit/sample" "bit/channel")))))))
         ((member extension '("clip" "psd" "psb" "png" "jpg" "jpeg" "gif" "bmp" "webp" "tif" "tiff" "avif" "heic"))
          (setq data (plist-put data :kind 'image)
                data (plist-put data :format (concat (upcase extension) " (extension)"))))
         ((member extension '("mp4" "mkv" "webm" "mov" "avi" "wmv" "m4v" "mpg" "mpeg" "mts"))
          (setq data (plist-put data :kind 'video)
                data (plist-put data :format (concat (upcase extension) " (extension)"))))
         ((member extension '("mp3" "wav" "flac" "ogg" "opus" "m4a" "aac" "aiff" "wma"))
          (setq data (plist-put data :kind 'audio)
                data (plist-put data :format (concat (upcase extension) " (extension)"))))
         (t
          (let* ((utf32 (cond ((string-prefix-p (unibyte-string 255 254 0 0) bytes) 'little)
                              ((string-prefix-p (unibyte-string 0 0 254 255) bytes) 'big)))
                 (coding (cond ((string-prefix-p (unibyte-string 255 254) bytes) 'utf-16le-with-signature)
                               ((string-prefix-p (unibyte-string 254 255) bytes) 'utf-16be-with-signature)
                               ((string-prefix-p (unibyte-string 239 187 191) bytes) 'utf-8-with-signature)
                               (t (or (detect-coding-string bytes t) 'no-conversion))))
                 (decoded (if utf32 (neo-diredy--details-utf32 bytes (eq utf32 'little))
                            (decode-coding-string bytes (coding-system-change-eol-conversion coding 0)))))
            (when (and utf32 (<= size 65536) (not (zerop (% size 4))))
              (error "Truncated UTF-32 character"))
            (if (or binary (string-match-p "[\0-\10\16-\37]" decoded))
                (setq data (plist-put data :kind 'binary)
                      data (plist-put data :format (or binary "Binary"))
                      data (plist-put data :primary
                                      (list (mapconcat (lambda (byte) (format "%02X" byte))
                                                       (substring bytes 0 (min 16 (length bytes))) " "))))
              (let* ((crlf (string-match-p "\r\n" decoded))
                     (rest (replace-regexp-in-string "\r\n" "" decoded))
                     (lf (string-match-p "\n" rest)) (cr (string-match-p "\r" rest))
                     (eol (cond ((> (+ (if crlf 1 0) (if lf 1 0) (if cr 1 0)) 1) "Mixed EOL")
                                (crlf "CRLF") (lf "LF") (cr "CR") (t "No EOL"))))
                (setq data (plist-put data :kind 'text)
                      data (plist-put data :utf32 utf32)
                      data (plist-put data :format (neo-diredy--details-language file decoded))
                      data (plist-put data :primary
                                      (list (if utf32 (if (eq utf32 'little) "UTF-32LE" "UTF-32BE")
                                              (concat (symbol-name (coding-system-base coding))
                                                      (unless (memq coding '(utf-16le-with-signature utf-16be-with-signature utf-8-with-signature))
                                                        " (guess)")))
                                            (concat eol (if (> size (length bytes)) " (sample)" ""))))))))))
        data)))))

(defun neo-diredy--details-art (file bytes)
  "Add explicitly labelled CLIP preview dimensions from extracted BYTES."
  (when (and (equal file (car neo-diredy--details-key))
             (equal (downcase (or (file-name-extension file) "")) "clip"))
    (let ((image (neo-diredy--details-image-header bytes)))
      (when (and (cadr image) (caddr image))
        (setq neo-diredy--details-data
              (plist-put neo-diredy--details-data :primary
                         (list (format "Preview %d × %d" (cadr image) (caddr image)))))
        (neo-diredy--details-paint)))))

(defun neo-diredy--details-media (json data)
  "Merge bounded ffprobe JSON into DATA, ignoring attached cover pictures."
  (let* ((object (json-parse-string json :object-type 'alist :array-type 'list))
         (streams (alist-get 'streams object))
         (video (seq-find (lambda (s) (and (equal (alist-get 'codec_type s) "video")
                                           (not (eq (alist-get 'attached_pic (alist-get 'disposition s)) 1)))) streams))
         (audio (seq-find (lambda (s) (equal (alist-get 'codec_type s) "audio")) streams))
         (format-info (alist-get 'format object))
         (duration (alist-get 'duration format-info))
         (seconds (and (stringp duration) (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" duration)
                       (string-to-number duration)))
         (image (eq (plist-get data :kind) 'image))
         (primary nil) (extra nil))
    (unless (or video audio) (error "No media stream"))
    (when (or video audio)
      (setq data (plist-put data :kind (if video (if (eq (plist-get data :kind) 'image) 'image 'video) 'audio)))
      (if image
          (setq data (plist-put data :format
                               (upcase (or (alist-get 'codec_name video) (plist-get data :format)))))
        (when-let* ((name (alist-get 'format_name format-info)))
          (setq data (plist-put data :format
                               (cond ((string-prefix-p "mov,mp4" name) "MOV/MP4")
                                     ((equal name "matroska,webm") "Matroska/WebM")
                                     (t name))))))
      (when (and video (numberp (alist-get 'width video)) (numberp (alist-get 'height video)))
        (push (format "%d × %d" (alist-get 'width video) (alist-get 'height video)) primary))
      (when (and seconds (not image)) (push (format "%d:%02d" (/ (floor seconds) 60) (% (floor seconds) 60)) primary))
      (when audio
        (when-let* ((rate (alist-get 'sample_rate audio))) (push (format "%s Hz" rate) primary))
        (when-let* ((channels (alist-get 'channels audio))) (push (format "%s ch" channels) primary)))
      (when-let* ((codec (alist-get 'codec_name (or video audio)))) (push codec extra))
      (when-let* ((rate (and (not image) (alist-get 'avg_frame_rate video))))
        (when (string-match "\\`\\([0-9]+\\)/\\([0-9]+\\)\\'" rate)
          (let ((n (string-to-number (match-string 1 rate))) (d (string-to-number (match-string 2 rate))))
            (when (and (> n 0) (> d 0)) (push (format "%.2f fps avg" (/ (float n) d)) extra)))))
      (setq data (plist-put data :primary (nreverse primary))
            data (plist-put data :extra (nreverse extra))))
    data))

(defun neo-diredy--details-cancel ()
  "Cancel a details probe and dispose its output and deadline."
  (when-let* ((process neo-diredy--details-process))
    (setq neo-diredy--details-process nil neo-diredy--details-key nil)
    (set-process-sentinel process #'ignore)
    (when (timerp (process-get process 'timer)) (cancel-timer (process-get process 'timer)))
    (when (process-live-p process) (delete-process process))
    (when (buffer-live-p (process-buffer process)) (kill-buffer (process-buffer process)))))

(defun neo-diredy--details-stop ()
  "Release all details state on listing reload or layout exit."
  (neo-diredy--details-cancel)
  (setq neo-diredy--details-key nil neo-diredy--details-data nil neo-diredy--details-cache nil))

(defun neo-diredy--details-remember (key data)
  "Keep at most 32 compact metadata entries identified by KEY."
  (setq neo-diredy--details-cache
        (seq-take (cons (cons key data) (assoc-delete-all key neo-diredy--details-cache)) 32)))

(defun neo-diredy--details-finished (process _event)
  "Publish PROCESS's result only if its file and selection are still current."
  (when (and (eq process neo-diredy--details-process)
             (memq (process-status process) '(exit signal)))
    (let* ((key (process-get process 'key))
           (data (copy-tree (process-get process 'data)))
           (json (when (buffer-live-p (process-buffer process))
                   (with-current-buffer (process-buffer process) (buffer-string))))
           (valid (and (equal key neo-diredy--details-key)
                       (equal key (ignore-errors (neo-diredy--art-signature (car key))))
                       (neo-diredy--layout-intact-p)
                       (with-current-buffer (window-buffer neo-diredy--center)
                         (equal (car key) (neo-diredy--original-file))))))
      (if (and (zerop (process-exit-status process)) (not (process-get process 'failed)))
          (condition-case nil (setq data (neo-diredy--details-media json data))
            (error (setq data (plist-put data :extra '("Details unavailable")))))
        (setq data (plist-put data :extra '("Details unavailable"))))
      (neo-diredy--details-cancel)
      (when valid
        (setq neo-diredy--details-key key neo-diredy--details-data data)
        (neo-diredy--details-remember key data)
        (neo-diredy--details-paint)))))

(defun neo-diredy--details-probe (file key data executable)
  "Start a bounded ffprobe using EXECUTABLE for FILE, KEY and DATA."
  (let ((buffer (generate-new-buffer " *neo-diredy-probe*")))
    (condition-case nil
        (let ((process (make-process
                        :name "neo-diredy-probe" :buffer buffer :noquery t
                        :connection-type 'pipe :coding 'utf-8-unix
                        :command (list executable "-v" "quiet" "-protocol_whitelist" "file,pipe"
                                       "-probesize" "1048576" "-analyzeduration" "1000000"
                                       "-show_entries"
                                       "format=format_name,duration:stream=codec_type,codec_name,width,height,avg_frame_rate,sample_rate,channels:stream_disposition=attached_pic"
                                       "-of" "json" "-i" (expand-file-name file))
                        :filter (lambda (process chunk)
                                  (when (buffer-live-p (process-buffer process))
                                    (with-current-buffer (process-buffer process)
                                      (if (> (+ (buffer-size) (length chunk)) 65536)
                                          (progn (process-put process 'failed t) (delete-process process))
                                        (goto-char (point-max)) (insert chunk)))))
                        :sentinel #'neo-diredy--details-finished)))
          (setq neo-diredy--details-process process)
          (process-put process 'key key)
          (process-put process 'data data)
          (process-put process 'timer
                       (run-at-time 5 nil (lambda ()
                                            (when (eq process neo-diredy--details-process)
                                              (process-put process 'failed t)
                                              (delete-process process))))))
      (error (kill-buffer buffer)
             (setq neo-diredy--details-data (plist-put data :extra '("Details unavailable")))
             (neo-diredy--details-remember key neo-diredy--details-data)))))

(defun neo-diredy--details-refresh (file)
  "Read details once per FILE content change, independent of window size."
  (let ((key (ignore-errors (neo-diredy--art-signature file))))
    (unless (equal key neo-diredy--details-key)
      (neo-diredy--details-cancel)
      (setq neo-diredy--details-key key
            neo-diredy--details-data
            (or (copy-tree (cdr (assoc key neo-diredy--details-cache)))
                (condition-case nil
                    (neo-diredy--details-read file (cdr (neo-diredy--file-info file)))
                  (error '(:format "Unavailable")))))
      (unless (assoc key neo-diredy--details-cache)
        (let* ((kind (plist-get neo-diredy--details-data :kind))
               (probe (or (memq kind '(video audio))
                          (and (eq kind 'image) (not (plist-get neo-diredy--details-data :primary))
                               (not (member (downcase (or (file-name-extension file) "")) '("clip" "psd" "psb"))))))
               (executable (and probe (executable-find "ffprobe"))))
          (if executable
              (progn
                (neo-diredy--details-probe file key neo-diredy--details-data executable)
                (when neo-diredy--details-process
                  (setq neo-diredy--details-data (plist-put (copy-tree neo-diredy--details-data) :extra '("Reading details…")))))
            (when probe (setq neo-diredy--details-data (plist-put neo-diredy--details-data :extra '("ffprobe not installed"))))
            (neo-diredy--details-remember key neo-diredy--details-data)))))
    (neo-diredy--details-paint)))

(defun neo-diredy--render-preview (file)
  "Reuse FILE's preview until metadata, size limits or window size change."
  (neo-diredy--details-layout)
  (neo-diredy--details-refresh file)
  (let* ((info (ignore-errors (neo-diredy--file-info file)))
         (attributes (cdr info))
         (key (list file (car info) (file-attribute-modification-time attributes)
                    (file-attribute-status-change-time attributes)
                    (file-attribute-size attributes)
                    (window-body-width neo-diredy--right t)
                    (window-body-height neo-diredy--right t)
                    neo-diredy-preview-bytes neo-diredy-image-bytes)))
    (unless (equal key neo-diredy--preview-key)
      (let ((same-content (and (equal (seq-take key 5) (seq-take neo-diredy--preview-key 5))
                               (equal (nthcdr 7 key) (nthcdr 7 neo-diredy--preview-key))))
            (start (window-start neo-diredy--right))
            (position (window-point neo-diredy--right)))
        (neo-diredy--art-cancel)
        (setq neo-diredy--preview-key key)
        (neo-diredy--insert-preview file attributes)
        (when same-content
          (set-window-start neo-diredy--right start)
          (set-window-point neo-diredy--right position))))))

(defconst neo-diredy--preview-modes
  '(("Emacs Lisp" emacs-lisp-mode lisp-mode)
    ("Python" python-mode python)
    ("C" c-mode cc-mode) ("C/C++ header" c-mode cc-mode)
    ("C++" c++-mode cc-mode) ("C++ header" c++-mode cc-mode)
    ("C#" csharp-mode csharp-mode) ("Java" java-mode cc-mode)
    ("JavaScript" js-mode js) ("JavaScript/JSX" js-jsx-mode js)
    ("JSON" js-json-mode js) ("HTML" html-mode sgml-mode)
    ("XML" sgml-mode sgml-mode) ("SVG" sgml-mode sgml-mode)
    ("CSS" css-mode css-mode)
    ("Diff" diff-mode diff-mode)
    ("Ruby" ruby-mode ruby-mode) ("Shell" sh-mode sh-script)
    ("SQL" sql-mode sql) ("LaTeX" latex-mode tex-mode)
    ("Lua" lua-mode lua-mode) ("Makefile" makefile-mode make-mode)
    ("INI" conf-windows-mode conf-mode) ("Git ignore" conf-unix-mode conf-mode)
    ("Batch" bat-mode bat-mode) ("Perl" perl-mode perl-mode) ("Org" org-mode org)
    ("TypeScript" typescript-mode typescript-mode)
    ("TypeScript/TSX" typescript-mode typescript-mode)
    ("Rust" rust-mode rust-mode) ("PHP" php-mode php-mode)
    ("PowerShell" powershell-mode powershell) ("YAML" yaml-mode yaml-mode)
    ("TOML" conf-toml-mode conf-mode) ("Fish" fish-mode fish-mode)
    ("Markdown" markdown-mode markdown-mode) ("Go" go-mode go-mode))
  "Reviewed language modes and libraries used only for preview colors.
Optional libraries are used only when already available on `load-path'.")

(defconst neo-diredy--preview-treesit-modes
  '(("Python" python-ts-mode python python)
    ("C" c-ts-mode c-ts-mode c) ("C/C++ header" c-ts-mode c-ts-mode c)
    ("C++" c++-ts-mode c-ts-mode cpp) ("C++ header" c++-ts-mode c-ts-mode cpp)
    ("C#" csharp-ts-mode csharp-mode c-sharp) ("Java" java-ts-mode java-ts-mode java)
    ("JavaScript" js-ts-mode js javascript jsdoc) ("JavaScript/JSX" js-ts-mode js javascript jsdoc)
    ("JSON" json-ts-mode json-ts-mode json) ("CSS" css-ts-mode css-mode css)
    ("TypeScript" typescript-ts-mode typescript-ts-mode typescript tsx)
    ("TypeScript/TSX" tsx-ts-mode typescript-ts-mode typescript tsx)
    ("Rust" rust-ts-mode rust-ts-mode rust) ("Ruby" ruby-ts-mode ruby-ts-mode ruby)
    ("Go" go-ts-mode go-ts-mode go) ("Lua" lua-ts-mode lua-ts-mode lua)
    ("Shell" bash-ts-mode sh-script bash)
    ("YAML" yaml-ts-mode yaml-ts-mode yaml) ("TOML" toml-ts-mode toml-ts-mode toml)
    ("PHP" php-ts-mode php-ts-mode php phpdoc html javascript jsdoc css))
  "Preview candidates: language, mode, library, required grammars.
Check every grammar before loading: some libraries compile queries at load time.")

(defun neo-diredy--preview-faces (text mode file)
  "Return syntax face spans for TEXT using MODE, or nil on failure.
FILE is used only to recognize shell variants, never visited."
  (condition-case nil
      (let ((treesit-auto-install-grammar nil)
            (tex-mode-load-hook nil) (org-load-hook nil))
        (when (require (cadr mode) nil t)
          (let ((enable-local-variables nil) (enable-local-eval nil)
                (hack-local-variables-hook nil) (before-hack-local-variables-hook nil)
                (change-major-mode-hook nil) (font-lock-mode-hook nil)
                (kill-buffer-hook nil) (kill-buffer-query-functions nil)
                (sh-set-shell-hook nil)
                (sh-shell-file (if (string-match-p "zsh\\|zprofile" (file-name-nondirectory file))
                                   "zsh" "sh"))
                (markdown-fontify-code-blocks-natively nil)
                (org-font-lock-hook nil) (org-font-lock-set-keywords-hook nil)
                (org-cycle-hook nil) (org-src-fontify-natively nil)
                (org-startup-folded nil) (org-startup-with-inline-images nil)
                (org-startup-with-latex-preview nil)
                (diff-font-lock-syntax nil) (diff-font-lock-prettify nil) (diff-refine nil)
                (compilation-error-regexp-alist nil) (compilation-error-regexp-alist-alist nil))
            (with-temp-buffer
              (insert text)
              (delay-mode-hooks
                (funcall (car mode))
                (font-lock-ensure))
              ;; Only faces cross the boundary.  Modes must not rewrite text.
              (when (equal text (buffer-substring-no-properties (point-min) (point-max)))
                (let ((position (point-min)) spans)
                  (while (< position (point-max))
                    (let ((next (next-property-change position nil (point-max)))
                          (face (or (get-text-property position 'face)
                                    (get-text-property position 'font-lock-face))))
                      (when face (push (list position next face) spans))
                      (setq position next)))
                  spans))))))
    (error nil)))

(defface neo-diredy-diff-added
  '((((class color) (background dark)) (:background "#19352b" :extend t))
    (((class color) (background light)) (:background "#e2f3e5" :extend t))
    (t (:inherit diff-added)))
  "Quiet background for added lines." :group 'neo-diredy)
(defface neo-diredy-diff-removed
  '((((class color) (background dark)) (:background "#3c242b" :extend t))
    (((class color) (background light)) (:background "#fbe4e7" :extend t))
    (t (:inherit diff-removed)))
  "Quiet background for removed lines." :group 'neo-diredy)
(defface neo-diredy-diff-refine-added
  '((((class color) (background dark)) (:background "#286749" :weight bold))
    (((class color) (background light)) (:background "#aadbb5" :weight bold))
    (t (:inherit diff-refine-added :weight bold)))
  "Emphasize changed text within added lines." :group 'neo-diredy)
(defface neo-diredy-diff-refine-removed
  '((((class color) (background dark)) (:background "#803e4b" :weight bold))
    (((class color) (background light)) (:background "#f0acb7" :weight bold))
    (t (:inherit diff-refine-removed :weight bold)))
  "Emphasize changed text within removed lines." :group 'neo-diredy)
(defface neo-diredy-diff-file-header
  '((t (:inherit font-lock-function-name-face :weight bold :overline t :extend t)))
  "Separate file headers from diff contents." :group 'neo-diredy)
(defface neo-diredy-diff-hunk-header
  '((t (:inherit font-lock-keyword-face :weight bold :extend t)))
  "Highlight hunk locations and their function context." :group 'neo-diredy)
(defvar-local neo-diredy--diff-faces-installed nil)

(defun neo-diredy--refine-diff-lines (removed added)
  "Emphasize the changed middle of corresponding REMOVED and ADDED lines."
  ;; shortcut: unequal blocks keep line shading; add alignment if finer pairing is needed.
  (when (= (length removed) (length added))
    (cl-mapc
     (lambda (before after)
       (let* ((a (buffer-substring-no-properties (car before) (cdr before)))
              (b (buffer-substring-no-properties (car after) (cdr after)))
              (start 0) (a-end (length a)) (b-end (length b)))
         (while (and (< start a-end) (< start b-end) (= (aref a start) (aref b start)))
           (cl-incf start))
         (while (and (> a-end start) (> b-end start)
                     (= (aref a (1- a-end)) (aref b (1- b-end))))
           (cl-decf a-end) (cl-decf b-end))
         (when (< start a-end)
           (add-face-text-property (+ (car before) start) (+ (car before) a-end)
                                   'diff-refine-removed))
         (when (< start b-end)
           (add-face-text-property (+ (car after) start) (+ (car after) b-end)
                                   'diff-refine-added))))
     (nreverse removed) (nreverse added))))

(defun neo-diredy--decorate-diff ()
  "Decorate unified diff text without changing characters or buffer positions."
  (unless neo-diredy--diff-faces-installed
    (dolist (face '(diff-added diff-removed diff-refine-added diff-refine-removed
                   diff-file-header diff-hunk-header))
      (face-remap-add-relative face (intern (concat "neo-diredy-" (symbol-name face)))))
    (setq neo-diredy--diff-faces-installed t))
  (save-excursion
    (save-match-data
      (with-silent-modifications
        (remove-text-properties (point-min) (point-max) '(line-prefix nil wrap-prefix nil))
        (let ((header "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? [+]\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@")
              (width 3) (old 0) (new 0) (old-left 0) (new-left 0)
              number-format removed-lines added-lines)
          (goto-char (point-min))
          (while (re-search-forward header nil t)
            (setq width (max width
                             (length (number-to-string
                                      (+ (string-to-number (match-string 1))
                                         (string-to-number (or (match-string 2) "1")))))
                             (length (number-to-string
                                      (+ (string-to-number (match-string 3))
                                         (string-to-number (or (match-string 4) "1"))))))))
          (goto-char (point-min))
          (setq number-format (format "%%%ds %%%ds │ " width width))
          (while (not (eobp))
            (let* ((beg (point)) (end (line-end-position))
                   (next (min (point-max) (1+ end))) prefix)
              (cond
               ((looking-at header)
                (neo-diredy--refine-diff-lines removed-lines added-lines)
                (setq removed-lines nil added-lines nil)
                (setq old (string-to-number (match-string 1))
                      old-left (string-to-number (or (match-string 2) "1"))
                      new (string-to-number (match-string 3))
                      new-left (string-to-number (or (match-string 4) "1"))
                      prefix (format number-format "old" "new")))
               ((or (and (eq (char-after) ?-) (> old-left 0))
                    (and (eq (char-after) ?+) (> new-left 0))
                    (and (eq (char-after) ?\s) (> old-left 0) (> new-left 0)))
                (let ((removed (eq (char-after) ?-)) (added (eq (char-after) ?+)))
                  (setq prefix (format number-format
                                       (if added "" (number-to-string old))
                                       (if removed "" (number-to-string new))))
                  (put-text-property beg next 'face
                                     (cond (removed 'diff-removed)
                                           (added 'diff-added) (t 'diff-context)))
                  (cond (removed
                         (when added-lines
                           (neo-diredy--refine-diff-lines removed-lines added-lines)
                           (setq removed-lines nil added-lines nil))
                         (push (cons (1+ beg) end) removed-lines))
                        (added (push (cons (1+ beg) end) added-lines))
                        (t (neo-diredy--refine-diff-lines removed-lines added-lines)
                           (setq removed-lines nil added-lines nil)))
                  (unless added (cl-incf old) (cl-decf old-left))
                  (unless removed (cl-incf new) (cl-decf new-left))))
               ((looking-at "\\\\ No newline at end of file") nil)
               (t (neo-diredy--refine-diff-lines removed-lines added-lines)
                (setq removed-lines nil added-lines nil) (setq old-left 0 new-left 0)))
              (when prefix
                (put-text-property beg next 'line-prefix (propertize prefix 'face 'shadow))
                (put-text-property beg next 'wrap-prefix
                                   (propertize (concat (make-string (1+ (* 2 width)) ?\s) " │ ")
                                               'face 'shadow)))
              (forward-line 1)))
          (neo-diredy--refine-diff-lines removed-lines added-lines))))))


(defun neo-diredy--fontify-preview (file)
  "Copy syntax faces for FILE into the current bounded text preview.
Do not visit FILE, use file-local settings, or run editing hooks.
Prefer available native parsers; conventional modes skip previews containing
lines over 2000 characters.  Unknown languages and fontification errors
stay plain."
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (language (neo-diredy--details-language file text))
         (mode (cdr (assoc language neo-diredy--preview-modes)))
         (native (cdr (assoc language neo-diredy--preview-treesit-modes)))
         (short-lines (save-excursion
                        (goto-char (point-min))
                        (while (and (not (eobp))
                                    (<= (- (line-end-position) (point)) 2000))
                          (forward-line 1))
                        (eobp))))
    (when (or native (and mode short-lines))
      (let ((faces
             (or (when (and native (treesit-available-p)
                            ;; The bash parser does not describe zsh/ksh syntax.
                            (not (and (equal language "Shell")
                                      (string-match-p "zsh\\|zprofile\\|\\bksh\\b"
                                                      (concat (file-name-nondirectory file) "\n"
                                                              (car (split-string text "\n"))))))
                            (cl-every #'treesit-language-available-p (cddr native)))
                   (neo-diredy--preview-faces text native file))
                 (when (and mode short-lines) (neo-diredy--preview-faces text mode file)))))
        (dolist (span faces)
          (put-text-property (car span) (cadr span) 'face (nth 2 span)))))
    (when (equal language "Diff") (neo-diredy--decorate-diff))))

(defun neo-diredy--insert-preview (file attributes)
  "Render FILE without visiting it or evaluating file-local code."
  (with-current-buffer neo-diredy--preview
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq header-line-format '(:eval (list (neo-diredy--preview-header))))
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
                                 (progn
                                   (neo-diredy--details-art file (cdr result))
                                   (insert-image
                                  (create-image (cdr result) nil t
                                                :max-width (window-body-width window t)
                                                :max-height (window-body-height window t))))
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
                (if-let* ((utf32 (plist-get neo-diredy--details-data :utf32)))
                    (insert (replace-regexp-in-string
                             "\r\n?" "\n"
                             (neo-diredy--details-utf32
                              (neo-diredy--art-read file 0 (min size neo-diredy-preview-bytes))
                              (eq utf32 'little))))
                  (insert-file-contents file nil 0 (min size neo-diredy-preview-bytes)))
                (if (save-excursion (goto-char (point-min)) (search-forward "\0" nil t))
                    (progn (erase-buffer) (insert (format "Binary file (%s)" (file-size-human-readable size))))
                  (neo-diredy--fontify-preview file)
                  (when (> size neo-diredy-preview-bytes)
                    (goto-char (point-max))
                    (let ((start (point)))
                      (insert "\n… preview truncated")
                      (set-text-properties start (point) nil)))))))))
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
                (set-buffer-modified-p nil))))
          ;; Styling has moved the modeline to the new directory too, so
          ;; buffers left behind by navigation can now be released safely.
          (neo-diredy--cleanup-dired-buffers))
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
    (neo-diredy--modeline-select)
    (neo-diredy--modeline-activity)
    (cond
     ((not (neo-diredy--layout-intact-p))
      ;; Foreign windows and buffers are authoritative, including popups that
      ;; leave the center selected and replacements of existing windows.
      (setq neo-diredy--configuration nil)
      (neo-diredy--layout-mode -1))
     ((and (not (eq (selected-window) neo-diredy--center))
           (eq (selected-frame) (window-frame neo-diredy--center))
           (not (active-minibuffer-window))
           (not (window-dedicated-p))
           (with-current-buffer (window-buffer (selected-window))
             (and (derived-mode-p 'dired-mode 'wdired-mode)
                  (not (file-remote-p default-directory)))))
      ;; Another real Dired list now owns the selection.  Release only our
      ;; auxiliary panes; restoring the saved configuration would reselect
      ;; the old list and could discard navigation in the other window.
      (setq neo-diredy--configuration nil)
      (neo-diredy--layout-mode -1))
     ((eq (selected-window) neo-diredy--center)
      (with-current-buffer (window-buffer neo-diredy--center)
        ;; Mode/state switches can alter presentation without moving point or
        ;; changing buffer text.  Repair styling before every redisplay, while
        ;; retaining the dirty flag for the expensive listing decorations.
        (neo-diredy--style)
        (let ((key (list (current-buffer) major-mode (point) (buffer-chars-modified-tick))))
          (unless (equal key neo-diredy--key)
            (neo-diredy--art-cancel)
            (neo-diredy--details-cancel)
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
                    (run-with-idle-timer 0.12 nil #'neo-diredy--refresh))))))))))

(defun neo-diredy--before-layout-change (&rest _)
  "Close the layout before another interface saves our temporary panes."
  (when (and neo-diredy--layout-mode (window-live-p neo-diredy--center)
             (eq (window-frame neo-diredy--center) (selected-frame)))
    (neo-diredy--layout-mode -1)))

(defun neo-diredy--handoff-state ()
  "Describe the selection and displayed buffers after releasing the layout."
  (list (selected-window) (window-buffer) (window-point)
        (mapcar (lambda (window) (cons window (window-buffer window)))
                (window-list nil 'no-minibuffer))))

(defun neo-diredy--cleanup-dired-buffers ()
  "Dispose of unused directories created during the active layout."
  (dolist (entry neo-diredy--buffers)
    (let ((buffer (car entry)))
      (when (and (buffer-live-p buffer)
                 (not (memq buffer neo-diredy--existing-buffers))
                 (not (eq buffer (car neo-diredy--decorated)))
                 (not (tab-bar-get-buffer-tab buffer t))
                 (with-current-buffer buffer
                   (and (derived-mode-p 'dired-mode)
                        (not (derived-mode-p 'wdired-mode))
                        (not (buffer-modified-p))
                        (not (save-excursion
                               (goto-char (point-min))
                               (re-search-forward "^[^ \n\r]" nil t))))))
        (kill-buffer buffer))))
  (setq neo-diredy--buffers
        (seq-filter (lambda (entry) (buffer-live-p (car entry)))
                    neo-diredy--buffers)))

(defun neo-diredy--stop ()
  "Restore saved presentation and dispose of unused buffers owned by the layout."
  (mapc #'delete-overlay neo-diredy--action-overlays)
  (setq neo-diredy--action-overlays nil neo-diredy--action-paint-key nil)
  (unless (neo-diredy--layout-intact-p)
    (setq neo-diredy--configuration nil)
    (when (assq (window-buffer) neo-diredy--buffers)
      (setq neo-diredy--handoff
            (list (selected-window) (window-buffer) (window-point)))))
  (neo-diredy--art-stop)
  (neo-diredy--details-stop)
  (neo-diredy--metadata-stop)
  (neo-diredy--choice-clear)
  (advice-remove 'set-window-margins #'neo-diredy--protect-margins)
  (advice-remove 'mode-line-window-selected-p #'neo-diredy--modeline-active-p)
  (advice-remove 'dired-dwim-target-directories #'neo-diredy--dwim-targets)
  (remove-hook 'pre-command-hook #'neo-diredy--modeline-select)
  (dolist (command '(dired-hide-subdir dired-kill-subdir))
    (advice-remove command #'neo-diredy--subdir-command))
  (advice-remove 'dired-subdir-hidden-p #'neo-diredy--subdir-hidden-p)
  (remove-hook 'window-state-change-functions #'neo-diredy--modeline-resize)
  (remove-hook 'window-selection-change-functions #'neo-diredy--modeline-activity)
  (when (windowp neo-diredy--center)
    ;; Deleted windows can be resurrected by a saved configuration.  Clear
    ;; our parameter on that object too, before a later restore revives it.
    (set-window-parameter neo-diredy--center 'mode-line-format neo-diredy--modeline-parameter))
  (when (and (window-live-p neo-diredy--center)
             (assq (window-buffer neo-diredy--center) neo-diredy--buffers))
    (set-window-margins neo-diredy--center (car neo-diredy--margins) (cdr neo-diredy--margins)))
  (remove-hook 'post-command-hook #'neo-diredy--post-command)
  (remove-hook 'window-size-change-functions #'neo-diredy--details-resize)
  (dolist (command neo-diredy--layout-commands)
    (advice-remove command #'neo-diredy--before-layout-change))
  (when (timerp neo-diredy--timer) (cancel-timer neo-diredy--timer))
  (mapc #'delete-overlay neo-diredy--overlays)
  (dolist (entry neo-diredy--buffers)
    (when (buffer-live-p (car entry))
      (with-current-buffer (car entry)
        (neo-diredy--edit-keys-mode -1)
        (remove-hook 'after-change-functions #'neo-diredy--changed t)
        (remove-hook 'dired-after-readin-hook #'neo-diredy--after-readin t)
        (kill-local-variable 'neo-diredy--file-sizes)
        (when neo-diredy--modeline-face
          (face-remap-remove-relative neo-diredy--modeline-face)
          (kill-local-variable 'neo-diredy--modeline-face))
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
        (set-window-parameter (selected-window) 'mode-line-format neo-diredy--modeline-parameter)
        (when (buffer-live-p buffer)
          (set-window-buffer (selected-window) buffer t)
          (set-window-point (selected-window) position)))))
  ;; A plugin may reuse an auxiliary window after removing its dedication.
  ;; Release our navigation flag, but never delete a window it has taken over.
  (dolist (entry (list (assq neo-diredy--modeline-window neo-diredy--windows)
                       (cons neo-diredy--details-window neo-diredy--details-buffer)
                       (cons neo-diredy--left neo-diredy--parent)
                       (cons neo-diredy--right neo-diredy--preview)))
    (let ((window (car entry)) (buffer (cdr entry)))
      (when (window-live-p window)
        (set-window-parameter window 'no-other-window nil)
        (when (eq window neo-diredy--modeline-window)
          (set-window-parameter window 'neo-diredy-modeline nil)
          (when (eq (window-parameter window 'header-line-format) 'none)
            (set-window-parameter window 'header-line-format nil)))
        (when (eq (window-buffer window) buffer)
          (set-window-dedicated-p window nil)
          (unless (one-window-p t (window-frame window))
            (delete-window window))))))
  (dolist (buffer (list neo-diredy--parent neo-diredy--preview neo-diredy--details-buffer))
    (when (buffer-live-p buffer) (kill-buffer buffer)))
  (dolist (entry neo-diredy--buffers)
    (when (and (caddr entry) (buffer-live-p (car entry)))
      (with-current-buffer (car entry) (dired-preview-mode 1))))
  (neo-diredy--cleanup-dired-buffers)
  (setq neo-diredy--buffers nil neo-diredy--existing-buffers nil neo-diredy--overlays nil
        neo-diredy--selected-icon nil
        neo-diredy--windows nil neo-diredy--center nil
        neo-diredy--left nil neo-diredy--right nil neo-diredy--margins nil
        neo-diredy--details-window nil neo-diredy--details-buffer nil
        neo-diredy--modeline-window nil neo-diredy--modeline-parameter nil
        neo-diredy--configuration nil neo-diredy--key nil
        neo-diredy--directory nil neo-diredy--timer nil
        neo-diredy--decorated nil neo-diredy--preview-key nil)
  ;; Capture after our panes are gone.  Closing a popup or restoring a window
  ;; configuration must resume Dired even when its selected point is unchanged.
  (when neo-diredy--handoff
    (setq neo-diredy--handoff (neo-diredy--handoff-state))))

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
                  neo-diredy--existing-buffers (buffer-list)
                  neo-diredy--configuration (current-window-configuration)
                  neo-diredy--center (selected-window)
                  neo-diredy--modeline-parameter (window-parameter nil 'mode-line-format)
                  neo-diredy--margins (window-margins))
            (setq neo-diredy--modeline-window (split-window neo-diredy--center -2 'below))
            (set-window-buffer neo-diredy--modeline-window (current-buffer))
            (set-window-parameter neo-diredy--modeline-window 'header-line-format 'none)
            (set-window-parameter neo-diredy--modeline-window 'mode-line-format nil)
            (set-window-parameter neo-diredy--modeline-window 'no-other-window t)
            (set-window-parameter neo-diredy--modeline-window 'neo-diredy-modeline t)
            (set-window-dedicated-p neo-diredy--modeline-window t)
            (dolist (symbol '(neo-diredy--parent neo-diredy--preview neo-diredy--details-buffer))
              (set symbol (generate-new-buffer (format " *%s*" symbol)))
              (with-current-buffer (symbol-value symbol)
                (special-mode)
                (setq-local truncate-lines t cursor-type nil mode-line-format nil header-line-format nil
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
            (add-hook 'pre-command-hook #'neo-diredy--modeline-select)
            (dolist (command '(dired-hide-subdir dired-kill-subdir))
              (advice-add command :around #'neo-diredy--subdir-command))
            (advice-add 'dired-subdir-hidden-p :around #'neo-diredy--subdir-hidden-p)
            (add-hook 'window-state-change-functions #'neo-diredy--modeline-resize 90)
            (add-hook 'window-selection-change-functions #'neo-diredy--modeline-activity)
            (advice-add 'set-window-margins :around #'neo-diredy--protect-margins)
            (advice-add 'mode-line-window-selected-p :around #'neo-diredy--modeline-active-p)
            (advice-add 'dired-dwim-target-directories :around #'neo-diredy--dwim-targets)
            (add-hook 'window-size-change-functions #'neo-diredy--details-resize)
            (dolist (command neo-diredy--layout-commands)
              (advice-add command :before #'neo-diredy--before-layout-change))
            (neo-diredy--refresh)))
      (error (setq neo-diredy--layout-mode nil) (neo-diredy--stop)
             (signal (car err) (cdr err))))))

(defun neo-diredy--auto-enable (&rest entry-arguments)
  "Activate a layout for the selected, local Dired window."
  ;; A nonselecting popup leaves Dired selected.  Do not immediately recreate
  ;; its panes later in the same post-command hook, or on unrelated commands.
  ;; Moving point, closing popups, changing windows/buffers, or entering Dired
  ;; resumes normal automatic activation without special-casing keymaps.
  (when (and neo-diredy--handoff (not (active-minibuffer-window))
             (or entry-arguments
                 (not (equal neo-diredy--handoff
                             (neo-diredy--handoff-state)))))
    (setq neo-diredy--handoff nil))
  ;; set-window-configuration can restore Dired while current-buffer still
  ;; names the interface being closed throughout post-command-hook.
  (with-current-buffer (window-buffer (selected-window))
    (when (and neo-diredy-mode (not neo-diredy--layout-mode)
               (not neo-diredy--handoff)
               (not (active-minibuffer-window))
               (derived-mode-p 'dired-mode 'wdired-mode)
               (not (file-remote-p default-directory))
               (not (window-dedicated-p))
               (>= (window-total-width) 72))
      (condition-case err
          (neo-diredy--layout-mode 1)
        (error (message "neo-diredy: %s" (error-message-string err)))))))

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
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'neo-diredy-preview-mode)
          (setq neo-diredy--preview-origin nil))))
    (setq neo-diredy--handoff nil)))

(defun neo-diredy-unload-function ()
  "Remove active hooks, advice and windows before unloading the package."
  (neo-diredy-mode -1)
  (neo-diredy--reveal-stop)
  (setq neo-diredy--clipboard nil neo-diredy--action-results nil)
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
