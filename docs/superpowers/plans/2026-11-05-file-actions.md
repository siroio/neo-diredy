# File Actions Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox syntax for tracking.

**Goal:** Give single and marked files the same copy/cut/paste/delete/reveal workflow.

**Architecture:** Keep the single-file package and native Dired listing. Store an in-memory clipboard, execute bounded per-item operations with explicit outcomes, and expose one compact action menu. Reuse the existing selector and header; Windows Reveal groups items by parent via a fixed PowerShell helper embedded in the package.

**Tech Stack:** Emacs Lisp (Emacs 29.1+), ERT, Windows Shell API for Reveal.

**Spec:** ../specs/2026-11-05-file-actions-design.md

## Global Constraints

- Only neo-diredy/; preserve the three pre-existing modified files. No stage/commit/push/worktree or installed configuration changes.
- Clipboard stays inside Emacs; preserve Dired/WDired/Evil, modeline and previews.
- Latest user instruction: minimize visible words. No extra status row; conditional short counts in the header, help in the menu only.
- Tests write only disposable data under tests/ or .bench/. External UI/trash mocked by default.
- User approved design and requested continuous implementation/validation; do not add another approval gate.

## Review Focus

- Destination symlinks/junctions and case variants must not let a directory enter itself (Task 1).
- Nested selection and duplicate basenames must not process data twice or overwrite an earlier source silently (Task 1).
- A failure or quit after partial work must retain pending cut items and report partial destinations (Task 1).
- Visiting modified files, hidden marks, and WDired must not lose edits or silently change targets (Tasks 1, 2).
- Unicode Reveal paths, mixed parents, helper errors and package-install-file must work without a loose helper file (Task 3).

### Task 1: Targets, clipboard and file operations

**Files:** neo-diredy.el; new tests/neo-diredy-actions-tests.el.

**Interfaces:** `neo-diredy--action-targets (&optional collapse)` returns absolute paths; `neo-diredy--clipboard` is `(copy|cut . paths)`; public copy/cut/paste/delete/clear-clipboard commands. `neo-diredy--action-report (verb results)` stores `(source status destination detail)` rows, where status is done/skipped/failed/pending. `neo-diredy--action-results` is the last result list.

- [x] Write ERT covering mark/point/empty/WDired selection; copy preserves source; cut defers until paste; successful cut consumes items; failed and skipped items remain; conflict decisions; self/descendant prevention; nested selection; directory/link handling; delete confirmation and trash failure; edited buffers retained.
- [x] Run new selector: `emacs -Q --batch -l tests/neo-diredy-actions-tests.el --eval '(ert-run-tests-batch-and-exit "^neo-diredy-actions-")'`. Expect undefined new commands.
- [x] Implement exact public operations and target/result helpers in neo-diredy.el using Dired/Emacs file primitives. Refresh affected Dired buffers with marks/viewport preserved; do not revert WDired or edited file buffers.
- [x] Run new selector; expect no unexpected results.

### Task 2: Compact UI and bindings

**Files:** neo-diredy.el; tests/neo-diredy-actions-tests.el; tests/neo-diredy-evil-tests.el if needed.

**Interfaces:** consumes Task 1 targets/clipboard/results. `neo-diredy-actions` opens the menu; `neo-diredy--action-status` returns short conditional state text. Public toggle-mark/mark-all/unmark-all/show-targets/show-results commands. Extend `neo-diredy--read-choice` only with optional action-menu metadata, preserving its old two-argument callers.

- [x] Write tests for quiet idle header, mark/cut distinctions, clipping and unchanged file text, menu dispatch/disabled choices and cancellation, local C-c f in Dired/Evil, cleanup and navigation retaining clipboard.
- [x] Run new tests and observe missing UI behavior.
- [x] Implement c/x/v/d/r/?/m/a/u/q menu keys, counts, scrollable target/result view, and conditional header status. Refresh mark decoration on listing/mark/clipboard changes without rescanning every cursor movement. Preserve original behavior when optional menu metadata is absent.
- [x] Run actions plus layout/modeline/navigation/workflow/opener/Evil tests; expect green with existing environment skips identified.

### Task 3: Grouped Unicode Reveal

**Files:** neo-diredy.el; new tests/neo-diredy-reveal-tests.el.

**Interfaces:** consumes targets and result reporting. `neo-diredy-reveal` launches one asynchronous Windows helper; `neo-diredy--reveal-groups (files)` returns parent/path groups. Fixed PowerShell source receives UTF-8 JSON over stdin, calls SHOpenFolderAndSelectItems with Unicode PIDLs, emits per-group JSON results.

- [x] Write grouping, Unicode payload, missing file and failure-result tests. Read helper prerequisites without opening Explorer.
- [x] Run tests and observe missing Reveal behavior.
- [x] Implement helper with shell-free argument passing, bounded output, timeout, process/buffer cleanup and cancellation on unload. No external source file dependency.
- [x] Run tests; compile helper in an isolated PowerShell process against temporary paths without opening Explorer where possible. Distinguish real Explorer verification from argument/API tests.

### Task 4: Documentation, integration and review

**Files:** README.md; plan/spec; tests as required by discovered regressions.

- [x] Document the short entry point, target semantics, pending state, conflicts, deletion behavior, Reveal dependency and synchronous transfer limitation.
- [x] Inspect full test runners for effects. Run all ERT files, package check, and isolated GUI action menu tests. Keep logs in .bench/file-actions/.
- [x] Dispatch one fresh reviewer for the scoped final diff and this spec/plan. Address substantive findings with failing regressions first; preserve existing diff.
- [x] Verify scoped diff/status and report code checks, GUI checks and remaining real-environment limits separately.

## Execution ledger

Baseline HEAD: d5723caa38605e31464bd0f1a94c279511bdb297. Existing diff: README 3 lines, core 10 lines, workflow tests 32 lines.

Ruling: native execution in the current checkout, without worktree/commits — repository instructions and user continuous authorization take precedence over generic skill setup.
Ruling: use .bench/file-actions/progress.md and native commands for task bookkeeping — Git Bash failed under this Windows sandbox in the preceding turn; no need to change Git ignore/settings for skill scripts.
Ruling: conditional header instead of a new permanent status row — user's latest instruction prioritizes fewer words and avoids changing the native modeline window.
Pre-flight: Task 2 and Task 3 consume Task 1 target/result/clipboard interfaces exactly as declared; no conflict.

## Verification — 2026-11-05

- Windows / Emacs 31.1, user init disabled; temporary data stays in this repository.
- All 206 ERT tests: batch 188 passed / 18 skipped; GUI 204 passed / 2 skipped. Zero unexpected results. GUI covers action shortcuts and disabled actions, cancellation, opener/bookmark, Evil/WDired, modeline, layout and preview interactions.
- Both remaining skips require symbolic-link creation privileges. Actual recycle-bin deletion, live user configuration/installed copy, physical keyboard and non-Windows runtime were not tested.
- Warning-as-error compilation, package metadata/load/unload and `git diff --check` pass.
- Actual Explorer verified with disposable fixtures: two selected files including Japanese/emoji/symbols, then three files across two parent folders. All three results were `done`. Sandbox Shell access denial was resolved for this check by running the same isolated Emacs fixture outside the sandbox; no configuration change.
- One fresh final reviewer found overwrite-all could replace a previous output in the same cut batch. The reproducing test failed before the fix; destinations claimed by the batch now require a fresh choice. No deferred findings or declined judgments.
- GUI verification additionally exposed Unicode current-directory process launch and Windows Shell path parsing failures; each has a failing-then-passing regression. The Windows GUI opener test now expects the resolved executable, including `.exe`.
- Logs: `.bench/file-actions/full-suite.log`, `gui-suite.log`, `package.log`, `review-red.log`, `review-green.log`, `unicode-cwd-red.log`, `native-parser-red.log`, `native-parser-green.log`, `visual-results.el`.
- Ruling consequences: current-checkout edits coexist with pre-existing changes and need a scoped commit later; native Windows verification does not exercise Bash scripts; quiet headers put discovery/help inside the action menu. These match the authorized scope and requested restrained UI.
