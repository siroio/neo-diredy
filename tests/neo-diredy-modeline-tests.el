;;; neo-diredy-modeline-tests.el --- Native status bar checks -*- lexical-binding: t; -*-
(unless (boundp 'neo-diredy-test-root)
  (load (expand-file-name "neo-diredy-tests.el" (file-name-directory load-file-name)) nil t))

(ert-deftest neo-diredy-modeline-preserves-native-format-and-left-edge ()
  (neo-diredy-test--with-directory
   (let ((format '(" USER " "%b" " L%l " mode-name)))
     (setq-local mode-line-format format)
     (neo-diredy--layout-mode 1)
     (should (equal mode-line-format format))
     (should (window-live-p neo-diredy--modeline-window))
     (should (= (car (window-edges neo-diredy--modeline-window))
                (car (window-edges neo-diredy--left))))
     (should (= (nth 2 (window-edges neo-diredy--modeline-window))
                (nth 2 (window-edges neo-diredy--right))))
     (should (eq (window-buffer neo-diredy--modeline-window) (current-buffer)))
     (should (eq (window-parameter neo-diredy--center 'mode-line-format) 'none))
     (neo-diredy--layout-mode -1)
     (should (equal mode-line-format format))
     (should-not (window-parameter (selected-window) 'mode-line-format)))))

(ert-deftest neo-diredy-modeline-margin-survives-real-center-text ()
  (neo-diredy-test--with-directory
   (let ((config (expand-file-name "../../emacs.d/init.el" neo-diredy-test-root)) definition)
     (skip-unless (file-readable-p config))
     ;; Read only this pure window-layout function; never load the user's init.
     (with-temp-buffer
       (insert-file-contents config)
       (search-forward "(defun neo-center-text ")
       (goto-char (match-beginning 0))
       (setq definition (read (current-buffer))))
     (neo-diredy--layout-mode 1)
     (funcall (eval (cons 'lambda (cddr definition)) t) (selected-frame))
     (should (= 16 (cdr (window-margins neo-diredy--center))))
     (when (display-graphic-p)
       (dired-goto-file file)
       (let ((neo-diredy--git-state (cons "main" (make-hash-table :test #'equal))) visible)
         (puthash file " M" (cdr neo-diredy--git-state))
         (neo-diredy--metadata-paint)
         (redisplay t)
         (let* ((position (posn-at-point (point) neo-diredy--center))
                (y (+ (window-header-line-height neo-diredy--center) 4 (cdr (posn-x-y position)))))
           (dotimes (x (window-pixel-width neo-diredy--center))
             (let ((pos (posn-at-x-y x y neo-diredy--center t)))
               (when (eq (posn-area pos) 'right-margin)
                 (push (car (posn-string pos)) visible)))))
         (should (cl-some (lambda (text) (and text (string-match-p " B  M" text))) visible))))
     (neo-diredy--layout-mode -1)
     (set-window-margins nil 2 3)
     (should (equal (window-margins) '(2 . 3))))))

(ert-deftest neo-diredy-modeline-details-do-not-inherit-header ()
  (let ((header-line-format '("USER HEADER")))
    (neo-diredy-test--with-directory
     (neo-diredy--layout-mode 1)
     (should-not (buffer-local-value 'header-line-format neo-diredy--details-buffer)))))

(ert-deftest neo-diredy-modeline-reused-bar-keeps-selection ()
  (neo-diredy-test--with-directory
   (neo-diredy--layout-mode 1)
   (let ((bar neo-diredy--modeline-window)
         (foreign (generate-new-buffer " *modeline-foreign*")))
     (unwind-protect
         (progn
           (set-window-dedicated-p bar nil)
           (set-window-buffer bar foreign)
           (select-window bar)
           (insert "foreign text")
           (neo-diredy--post-command)
           (should (eq (selected-window) bar))
           (should (eq (current-buffer) foreign))
           (should (= (point) 13))
           (should-not neo-diredy--layout-mode))
       (kill-buffer foreign)))))

(ert-deftest neo-diredy-modeline-native-gui-point-and-height ()
  (skip-unless (display-graphic-p))
  (neo-diredy-test--with-directory
   (setq-local mode-line-format '(" USER " "%b" " L%l " mode-name
                                 (:eval (if (mode-line-window-selected-p) " ACTIVE" " INACTIVE"))))
   (neo-diredy--layout-mode 1)
   (dired-goto-file file)
   (neo-diredy--post-command)
   (redisplay t)
   (neo-diredy--modeline-sync)
   (redisplay t)
   (should (> (window-mode-line-height neo-diredy--modeline-window) 0))
   (should (= 1 (window-body-height neo-diredy--modeline-window t)))
   (should (= (point) (window-point neo-diredy--modeline-window)))
   (should (string-prefix-p " USER " (format-mode-line mode-line-format nil
                                                     neo-diredy--modeline-window (current-buffer))))
   (should (string-suffix-p " ACTIVE" (format-mode-line mode-line-format nil
                                                      neo-diredy--modeline-window (current-buffer))))
   ;; A mouse action can select the native bar.  Commands must run in the list.
   (select-window neo-diredy--modeline-window)
   (run-hooks 'pre-command-hook)
   (should (eq (selected-window) neo-diredy--center))
   (dired-next-line 1)
   (neo-diredy--post-command)
   (should (= (point) (window-point neo-diredy--modeline-window)))))

(ert-deftest neo-diredy-modeline-user-format-with-image ()
  (skip-unless (and (display-graphic-p) (image-type-available-p 'svg)))
  (let* ((config (expand-file-name "../../emacs.d/init.el" neo-diredy-test-root))
         (load-path (cons (expand-file-name "../../emacs.d/lisp" neo-diredy-test-root) load-path))
         format)
    (skip-unless (and (file-readable-p config) (locate-library "neo-rebecca-sprite")))
    ;; Only pure formatting definitions and their image cache; no init, timers,
    ;; subprocesses or settings writes from the real configuration are run.
    (with-temp-buffer
      (insert-file-contents config)
      (dolist (name '(neo-mode-line-coding neo-rebecca-bar-steps neo-rebecca-bar-images
                     neo-rebecca-bar-frame neo-rebecca-bar-image neo-rebecca-bar))
        (goto-char (point-min))
        (re-search-forward (concat "(def[a-z]+ " (symbol-name name) "[ \n]"))
        (goto-char (match-beginning 0))
        (eval (read (current-buffer)) t))
      (goto-char (point-min))
      (search-forward "(setq-default mode-line-format")
      (goto-char (match-beginning 0))
      (setq format (eval (nth 2 (read (current-buffer))) t)))
    (neo-diredy-test--with-directory
     (setq-local mode-line-format format)
     (neo-diredy--layout-mode 1)
     (dired-goto-file file)
     (neo-diredy--post-command)
     (redisplay t)
     (neo-diredy--modeline-resize (selected-frame))
     (redisplay t)
     (should (equal mode-line-format format))
     (should (>= (window-mode-line-height neo-diredy--modeline-window) 33))
     (should (= (window-body-height neo-diredy--modeline-window t) 1))
     (let ((line (format-mode-line format nil neo-diredy--modeline-window (current-buffer))))
       (should (text-property-not-all 0 (length line) 'display nil line))
       (should (string-match-p (format "L%d:C%d" (line-number-at-pos) (current-column)) line))
       (should (string-match-p "Dired by name" line))))))

(ert-deftest neo-diredy-modeline-inactive-in-another-frame ()
  (skip-unless (display-graphic-p))
  (neo-diredy-test--with-directory
   (neo-diredy--layout-mode 1)
   (let ((frame (make-frame '((visibility . nil) (minibuffer . t)))))
     (unwind-protect
         (progn
           (with-selected-frame frame
             (neo-diredy--post-command)
             (should neo-diredy--layout-mode)
             (should-not (window-parameter neo-diredy--modeline-window 'neo-diredy-modeline)))
           (neo-diredy--post-command)
           (should (window-parameter neo-diredy--modeline-window 'neo-diredy-modeline)))
       (delete-frame frame t)))))
