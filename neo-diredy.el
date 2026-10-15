;;; neo-diredy.el --- A small three-column Dired view -*- lexical-binding: t; -*-

;; Version: 0.1.0
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
(require 'dired)
(require 'hl-line)
(require 'image)
(require 'tab-bar)
(declare-function dired-preview-mode "dired-preview" (&optional arg))

(defgroup neo-diredy nil "Three-column Dired view." :group 'dired)
(defcustom neo-diredy-preview-bytes (* 64 1024)
  "Maximum bytes read for a text preview."
  :type 'natnum :group 'neo-diredy)
(defcustom neo-diredy-image-bytes (* 8 1024 1024)
  "Maximum file size for inline image previews."
  :type 'natnum :group 'neo-diredy)
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
(defvar neo-diredy--buffers nil)
(defvar neo-diredy--overlays nil)
(defvar neo-diredy--timer nil)
(defvar neo-diredy--key nil)
(defvar neo-diredy--directory nil)
(defvar neo-diredy--decorated nil)
(defvar neo-diredy--preview-key nil)
(defvar-local neo-diredy--listing-dirty t)
(defvar-local neo-diredy--mark-tick nil)
(defvar-local neo-diredy--mark-count 0)
(defvar w32-get-true-file-attributes)
(defconst neo-diredy--tab-commands
  '(tab-bar-new-tab tab-bar-select-tab tab-bar-close-tab))
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
    (when (bound-and-true-p dired-preview-mode) (dired-preview-mode -1)))
  (setq-local truncate-lines t cursor-type nil display-line-numbers nil
              hl-line-face 'neo-diredy-selection
              header-line-format '(:eval (concat "  " (abbreviate-file-name default-directory)))
              mode-line-format '("  DIRED   " (:eval (neo-diredy--status)) "   M-x neo-diredy-mode: off"))
  (dired-hide-details-mode 1)
  (hl-line-mode 1)
  (hl-line-highlight)
  (when (overlayp hl-line-overlay)
    (overlay-put hl-line-overlay 'face 'neo-diredy-selection))
  (when (or neo-diredy--listing-dirty
            (not (eq (car neo-diredy--decorated) (current-buffer))))
    (setq neo-diredy--decorated
          (list (current-buffer))
          neo-diredy--listing-dirty nil)
    (mapc #'delete-overlay neo-diredy--overlays)
    (setq neo-diredy--overlays nil)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let ((file (dired-get-filename nil t)))
          (if (and file (not (member (file-name-nondirectory file) '("." ".."))))
              (when (dired-move-to-filename)
		(let ((overlay (make-overlay (point) (point))))
                  (overlay-put overlay 'before-string
                               (neo-diredy--icon file (eq (char-after (+ (line-beginning-position) 2)) ?d)))
                  (push overlay neo-diredy--overlays)))
            (let ((overlay (make-overlay (line-beginning-position)
					 (min (point-max) (1+ (line-end-position))))))
              (overlay-put overlay 'invisible t)
              (push overlay neo-diredy--overlays))))
	(forward-line 1)))))

(defun neo-diredy--status ()
  "Return the current filename and marked entry count."
  (unless (eq neo-diredy--mark-tick (buffer-chars-modified-tick))
    (setq neo-diredy--mark-count 0
          neo-diredy--mark-tick (buffer-chars-modified-tick))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*" nil t) (cl-incf neo-diredy--mark-count))))
  (format "%s  |  %d marked"
          (or (dired-get-filename 'no-dir t) "") neo-diredy--mark-count))

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
  (when (and neo-diredy--layout-mode (window-live-p neo-diredy--center)
             (eq (window-frame neo-diredy--center) (selected-frame)))
    (with-current-buffer (window-buffer neo-diredy--center)
      (when (and (derived-mode-p 'dired-mode)
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
        (neo-diredy--render-preview (dired-get-filename nil t))
        (force-mode-line-update)))))

(defun neo-diredy--post-command ()
  "Follow ordinary Dired navigation, debouncing cursor preview updates."
  (when neo-diredy--layout-mode
    (cond
     ((not (and (window-live-p neo-diredy--center)
                (window-live-p neo-diredy--left)
                (window-live-p neo-diredy--right)))
      ;; A tab change or manual layout change must not restore an old tab.
      (setq neo-diredy--configuration nil)
      (neo-diredy--layout-mode -1))
     ((eq (selected-window) neo-diredy--center)
      (if (or (not (derived-mode-p 'dired-mode))
              (file-remote-p default-directory))
          (let ((opened (current-buffer)))
            (neo-diredy--layout-mode -1)
            (switch-to-buffer opened))
        (let ((key (list (current-buffer) (point) (buffer-chars-modified-tick))))
          (unless (equal key neo-diredy--key)
            (setq neo-diredy--key key)
            (when (timerp neo-diredy--timer) (cancel-timer neo-diredy--timer))
            (if (or (not (eq (current-buffer) (car neo-diredy--decorated)))
                    (not (equal default-directory neo-diredy--directory)))
                ;; Directory styling must precede command-loop redisplay.
                (neo-diredy--refresh)
              (neo-diredy--style)
              (setq neo-diredy--timer
                    (run-with-idle-timer 0.12 nil #'neo-diredy--refresh))))))))))

(defun neo-diredy--before-tab-change (&rest _)
  "Close the layout before its windows are saved into another tab."
  (when (and neo-diredy--layout-mode (window-live-p neo-diredy--center)
             (eq (window-frame neo-diredy--center) (selected-frame)))
    (neo-diredy--layout-mode -1)))

(defun neo-diredy--stop ()
  "Restore saved presentation and dispose only of owned preview buffers."
  (remove-hook 'post-command-hook #'neo-diredy--post-command)
  (dolist (command neo-diredy--tab-commands)
    (advice-remove command #'neo-diredy--before-tab-change))
  (when (timerp neo-diredy--timer) (cancel-timer neo-diredy--timer))
  (mapc #'delete-overlay neo-diredy--overlays)
  (dolist (entry neo-diredy--buffers)
    (when (buffer-live-p (car entry))
      (with-current-buffer (car entry)
        (remove-hook 'after-change-functions #'neo-diredy--changed t)
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
          (set-window-buffer (selected-window) buffer)
          (set-window-point (selected-window) position)))))
  (dolist (buffer (list neo-diredy--parent neo-diredy--preview))
    (when (buffer-live-p buffer) (kill-buffer buffer)))
  (dolist (entry neo-diredy--buffers)
    (when (and (caddr entry) (buffer-live-p (car entry)))
      (with-current-buffer (car entry) (dired-preview-mode 1))))
  (setq neo-diredy--buffers nil neo-diredy--overlays nil
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
          (unless (derived-mode-p 'dired-mode) (user-error "Enable this mode in Dired"))
          (when (file-remote-p default-directory) (user-error "Use this layout in a local directory"))
          (when (< (window-total-width) 72) (user-error "The Dired window needs at least 72 columns"))
          (progn
            (setq neo-diredy--configuration (current-window-configuration)
                  neo-diredy--center (selected-window))
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
            (add-hook 'post-command-hook #'neo-diredy--post-command)
            (dolist (command neo-diredy--tab-commands)
              (advice-add command :before #'neo-diredy--before-tab-change))
            (neo-diredy--refresh)))
      (error (setq neo-diredy--layout-mode nil) (neo-diredy--stop)
             (signal (car err) (cdr err))))))

(defun neo-diredy--auto-enable (&rest _)
  "Activate a layout for the selected, local Dired window."
  (when (and neo-diredy-mode (not neo-diredy--layout-mode)
             (not (active-minibuffer-window))
             (eq (current-buffer) (window-buffer (selected-window)))
             (derived-mode-p 'dired-mode)
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
  (if neo-diredy-mode
      (progn
        (add-hook 'post-command-hook #'neo-diredy--auto-enable 90)
        (dolist (command '(dired dired-other-window dired-other-frame))
          (advice-add command :after #'neo-diredy--auto-enable))
        (neo-diredy--auto-enable))
    (remove-hook 'post-command-hook #'neo-diredy--auto-enable)
    (dolist (command '(dired dired-other-window dired-other-frame))
      (advice-remove command #'neo-diredy--auto-enable))
    (neo-diredy--layout-mode -1)))

(defun neo-diredy-unload-function ()
  "Remove active hooks, advice and windows before unloading the package."
  (neo-diredy-mode -1)
  nil)

(neo-diredy-mode 1)
(provide 'neo-diredy)
;;; neo-diredy.el ends here
