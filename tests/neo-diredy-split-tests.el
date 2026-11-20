;;; neo-diredy-split-tests.el --- Copy between split lists -*- lexical-binding: t; -*-
(load (expand-file-name "neo-diredy-actions-tests.el" (file-name-directory load-file-name)) nil t)

(ert-deftest neo-diredy-split-standard-copy-target-skips-modeline ()
  (neo-diredy-actions--with-files
   (let ((other (split-window-below)))
     (set-window-buffer other (dired-noselect child))
     (neo-diredy--layout-mode 1)
     ;; The status bar can be more recently selected than the destination.
     (select-window neo-diredy--modeline-window)
     (select-window neo-diredy--center)
     (dolist (dired-dwim-target '(t dired-dwim-target-next
                                  dired-dwim-target-next-visible dired-dwim-target-recent))
       (should (equal (dired-dwim-target-directory) (file-name-as-directory child)))))))

(ert-deftest neo-diredy-split-standard-copy-uses-other-directory ()
  (neo-diredy-actions--with-files
   (let ((other (split-window-below)) (dired-dwim-target t))
     (set-window-buffer other (dired-noselect child))
     (neo-diredy--layout-mode 1)
     (cl-letf (((symbol-function 'read-file-name)
                (lambda (_prompt directory &rest _)
                  (should (equal directory (file-name-as-directory child)))
                  directory)))
       (dired-do-copy))
     (should (equal (neo-diredy-actions--contents
                     (expand-file-name "hello.txt" child)) "hello preview\n"))
     (should (file-exists-p file)))))

(ert-deftest neo-diredy-split-copy-target-preserves-user-policy ()
  (neo-diredy-actions--with-files
   (let ((other (split-window-below)))
     (set-window-buffer other (dired-noselect child))
     (neo-diredy--layout-mode 1)
     (let ((dired-dwim-target nil))
       (should (equal (dired-dwim-target-directory) (file-name-as-directory root))))
     (let ((dired-dwim-target (lambda () (list (file-name-as-directory root)))))
       (should (equal (dired-dwim-target-directory) (file-name-as-directory root))))
     (neo-diredy--layout-mode -1)
     (should-not (advice-member-p #'neo-diredy--dwim-targets 'dired-dwim-target-directories)))))

(ert-deftest neo-diredy-split-copy-target-with-no-other-list ()
  (neo-diredy-actions--with-files
   (let ((dired-dwim-target t))
     (neo-diredy--layout-mode 1)
     (should-not (dired-dwim-target-directories))
     (should (equal (dired-dwim-target-directory) (file-name-as-directory root))))))

(ert-deftest neo-diredy-split-copy-paste-follows-selected-list ()
  (neo-diredy-actions--with-files
   (let ((source (selected-window)) (other (split-window-below)))
     (set-window-buffer other (dired-noselect child))
     (unwind-protect
         (progn
           (neo-diredy-mode 1)
           (neo-diredy-copy)
           (select-window other)
           (run-hooks 'post-command-hook)
           (should (eq (key-binding (kbd "C-c f")) #'neo-diredy-actions))
           (should (eq (selected-window) other))
           (neo-diredy-paste)
           (should (equal (neo-diredy-actions--contents
                           (expand-file-name "hello.txt" child)) "hello preview\n"))
           (should (file-exists-p file))
           (should (window-live-p source))
           (should (equal (buffer-local-value 'default-directory (window-buffer source))
                          (file-name-as-directory root)))
           (select-window source)
           (run-hooks 'post-command-hook)
           (should (eq (key-binding (kbd "C-c f")) #'neo-diredy-actions))
           (should (eq (selected-window) source)))
       (neo-diredy-mode -1)))))

(ert-deftest neo-diredy-split-created-during-layout-retains-both-lists ()
  (neo-diredy-actions--with-files
   (unwind-protect
       (progn
         (neo-diredy-mode 1)
         (neo-diredy-copy)
         (let ((source (selected-window)) (other (split-window-below)))
           (run-hooks 'post-command-hook)
           (select-window other)
           (dired child)
           (run-hooks 'post-command-hook)
           (should (eq (key-binding (kbd "C-c f")) #'neo-diredy-actions))
           (neo-diredy-paste)
           (should (equal (neo-diredy-actions--contents
                           (expand-file-name "hello.txt" child)) "hello preview\n"))
           (should (window-live-p source))
           (should (equal (buffer-local-value 'default-directory (window-buffer source))
                          (file-name-as-directory root)))))
     (neo-diredy-mode -1))))

(ert-deftest neo-diredy-split-gui-copy-paste-menus ()
  (skip-unless (display-graphic-p))
  (neo-diredy-actions--with-files
   (let ((other (split-window-below)))
     (set-window-buffer other (dired-noselect child))
     (unwind-protect
         (progn
           (neo-diredy-mode 1)
           (let ((unread-command-events '(?c)))
             (call-interactively (key-binding (kbd "C-c f"))))
           (select-window other)
           (run-hooks 'post-command-hook)
           (let ((unread-command-events '(?v)))
             (call-interactively (key-binding (kbd "C-c f"))))
           (should (eq (selected-window) other))
           (should (equal (neo-diredy-actions--contents
                           (expand-file-name "hello.txt" child)) "hello preview\n")))
       (neo-diredy-mode -1)))))
