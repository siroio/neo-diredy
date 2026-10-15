;;; neo-diredy-interop-tests.el --- Window handoff regressions -*- lexical-binding: t; -*-
(unless (boundp 'neo-diredy-test-root)
  (load (expand-file-name "neo-diredy-tests.el" (file-name-directory load-file-name)) nil t))

(ert-deftest neo-diredy-interop-preserves-added-window-and-margins ()
  (neo-diredy-test--with-directory
   (let ((plugin (generate-new-buffer " *diredy-plugin*")))
     (unwind-protect
         (progn
           (neo-diredy--layout-mode 1)
           (let ((extra (split-window neo-diredy--center nil 'below)))
             (set-window-buffer extra plugin)
             (switch-to-buffer plugin)
             (set-window-margins nil 2 3)
             (neo-diredy--post-command)
             (should-not neo-diredy--layout-mode)
             (should (window-live-p extra))
             (should (= 2 (length (window-list))))
             (should (eq (window-buffer extra) plugin))
             (should (equal (window-margins) '(2 . 3)))
             (should (buffer-live-p plugin))))
       (kill-buffer plugin)))))

(ert-deftest neo-diredy-interop-popup-outside-center ()
  (neo-diredy-test--with-directory
   (let ((plugin (generate-new-buffer " *diredy-popup*")))
     (unwind-protect
         (progn
           (neo-diredy--layout-mode 1)
           (let ((source (current-buffer)))
             (pop-to-buffer plugin '(display-buffer-below-selected))
             (neo-diredy--post-command)
             (should-not neo-diredy--layout-mode)
             (should (eq (current-buffer) plugin))
             (should (get-buffer-window source))
             (should (= 2 (length (window-list))))))
       (kill-buffer plugin)))))

(ert-deftest neo-diredy-interop-handoff-does-not-immediately-reactivate ()
  (neo-diredy-test--with-directory
   (let ((plugin (generate-new-buffer " *diredy-nonselected-popup*")))
     (unwind-protect
         (progn
           (neo-diredy-mode 1)
           (display-buffer plugin '(display-buffer-below-selected))
           (let ((this-command 'describe-function)) (run-hooks 'post-command-hook))
           (should-not neo-diredy--layout-mode)
           (should (get-buffer-window plugin))
           (let ((this-command 'ignore)) (run-hooks 'post-command-hook))
           (should-not neo-diredy--layout-mode)
           ;; Entering a command in the minibuffer is not Dired navigation.
           (let ((mini (minibuffer-window)))
             (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () mini))
                       ((symbol-function 'selected-window) (lambda () mini)))
               (neo-diredy--auto-enable)))
           (let ((this-command 'keyboard-quit)) (run-hooks 'post-command-hook))
           (should-not neo-diredy--layout-mode)
           ;; Explicit navigation resumes Dired without losing the popup.
           (let ((this-command 'dired-next-line))
             (dired-next-line 1)
             (run-hooks 'post-command-hook))
           (should neo-diredy--layout-mode)
           (should (get-buffer-window plugin)))
       (neo-diredy-mode -1)
       (kill-buffer plugin)))))

(ert-deftest neo-diredy-interop-reused-auxiliary-window ()
  (neo-diredy-test--with-directory
   (let ((plugin (generate-new-buffer " *diredy-reused*")))
     (unwind-protect
         (progn
           (neo-diredy--layout-mode 1)
           (let ((window neo-diredy--left))
             (set-window-dedicated-p window nil)
             (set-window-buffer window plugin)
             (with-current-buffer plugin (insert "first\nsecond\nthird\n"))
             (set-window-point window 10)
             (neo-diredy--refresh)
             (should (= (window-point window) 10))
             (select-window window)
             (delete-other-windows)
             (neo-diredy--post-command)
             (should-not neo-diredy--layout-mode)
             (should (eq (window-buffer window) plugin))
             (should-not (window-parameter window 'no-other-window))
             (should-not (window-dedicated-p window))
             (should (= 1 (length (window-list))))))
       (kill-buffer plugin)))))

(ert-deftest neo-diredy-interop-ibuffer-and-help ()
  (neo-diredy-test--with-directory
   (let ((name (generate-new-buffer-name "*diredy-test-ibuffer*"))
         (source (current-buffer))
         (old-help (get-buffer "*Help*")))
     (unwind-protect
         (progn
           (neo-diredy--layout-mode 1)
           (ibuffer nil name)
           (neo-diredy--post-command)
           (should (derived-mode-p 'ibuffer-mode))
           (should (get-buffer-window name))
           (should-not neo-diredy--layout-mode)
           (switch-to-buffer source)
           (delete-other-windows)
           (neo-diredy--layout-mode 1)
           (describe-function 'dired)
           (neo-diredy--post-command)
           (should (get-buffer-window "*Help*"))
           (should-not neo-diredy--layout-mode))
       (when (get-buffer name) (kill-buffer name))
       (when (and (not old-help) (get-buffer "*Help*")) (kill-buffer "*Help*"))))))

(ert-deftest neo-diredy-interop-terminal-display-pattern ()
  (neo-diredy-test--with-directory
   (let ((terminal (generate-new-buffer " *diredy-terminal*")))
     (unwind-protect
         (progn
           (neo-diredy--layout-mode 1)
           ;; Same display path as neo-terminal-toggle; no shell/DLL launched.
           (pop-to-buffer-same-window terminal)
           (neo-diredy--post-command)
           (should (eq (current-buffer) terminal))
           (should-not neo-diredy--layout-mode)
           (should (= 1 (length (window-list))))
           (should-not (cdr (window-margins))))
       (kill-buffer terminal)))))

(ert-deftest neo-diredy-interop-neo-task-list ()
  (let ((library (expand-file-name "../../neo-task/emacs/neo-task.el" neo-diredy-test-root)))
    (skip-unless (file-exists-p library))
    (load library nil t))
  (neo-diredy-test--with-directory
   (let ((neo-task-root root))
     (cl-letf (((symbol-function 'neo-task--call)
                (lambda (&rest args)
                  (should (equal args '("list")))
                  ;; UI integration only: never read or write a real ledger.
                  '((records . (((record . ((id . "fixture") (title . "Test task")
                                             (status . "TODO"))))))))))
       (neo-diredy--layout-mode 1)
       (neo-task-list)
       (neo-diredy--post-command)
       (should (derived-mode-p 'neo-task-list-mode))
       (should (get-buffer-window (current-buffer)))
       (should (string-match-p "Test task" (buffer-string)))
       (should-not neo-diredy--layout-mode)))))

(ert-deftest neo-diredy-interop-disable-retains-changed-existing-window ()
  (neo-diredy-test--with-directory
   (let ((plugin (generate-new-buffer " *diredy-existing*")))
     (unwind-protect
         (let ((other (split-window-below)))
           (neo-diredy--layout-mode 1)
           (set-window-buffer other plugin)
           ;; Disable before the next command hook can notice the change.
           (neo-diredy--layout-mode -1)
           (should (window-live-p other))
           (should (eq (window-buffer other) plugin))
           (should (= 2 (length (window-list)))))
       (kill-buffer plugin)))))

(ert-deftest neo-diredy-interop-handoff-advice-is-scoped ()
  (neo-diredy-test--with-directory
   (should-not (advice-member-p #'neo-diredy--before-layout-change 'neo-git--layout))
   (neo-diredy--layout-mode 1)
   (should (advice-member-p #'neo-diredy--before-layout-change 'neo-git--layout))
   (neo-diredy--layout-mode -1)
   (should-not (advice-member-p #'neo-diredy--before-layout-change 'neo-git--layout))))

(ert-deftest neo-diredy-interop-neo-git-round-trip ()
  (let ((library (expand-file-name "../../neo-git/neo-git.el" neo-diredy-test-root)))
    (skip-unless (and (file-exists-p library) (executable-find "git")))
    (load library nil t))
  (neo-diredy-test--with-directory
   (let ((source (current-buffer))
         (processes (process-list))
         (process-environment (copy-sequence process-environment))
         owner)
     (unwind-protect
         (progn
           (setenv "GIT_CONFIG_GLOBAL" null-device)
           (setenv "GIT_CONFIG_NOSYSTEM" "1")
           (should (zerop (call-process "git" nil nil nil "init" "--initial-branch=main")))
           (neo-diredy-mode 1)
           (neo-git-status)
           (let ((deadline (+ (float-time) 10)))
             (while (and (< (float-time) deadline)
                         (not (with-current-buffer (window-buffer (selected-window))
                                (derived-mode-p 'neo-git-mode))))
               (accept-process-output nil 0.05)))
           (set-buffer (window-buffer (selected-window)))
           (should (derived-mode-p 'neo-git-mode))
           (setq owner (current-buffer))
           (neo-diredy--post-command)
           (should-not neo-diredy--layout-mode)
           (should (window-live-p neo-git--history-window))
           (should-not (window-parameter nil 'no-other-window))
           (neo-git-quit)
           (set-buffer (window-buffer (selected-window)))
           (should (eq (current-buffer) source))
           (should (= 1 (length (window-list))))
           (neo-diredy--auto-enable)
           (should neo-diredy--layout-mode)
           (should (= 3 (length (window-list)))))
       (neo-diredy-mode -1)
       (when (buffer-live-p owner)
         (with-current-buffer owner
           (unless neo-git--closed (neo-git-quit))))
       ;; Only children created by this isolated test are ours to stop.
       (dolist (process (seq-difference (process-list) processes))
         (when (process-live-p process) (delete-process process)))))))
