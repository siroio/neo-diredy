# Bookmarks, preview controls and buffer cleanup

The user requested these three additions after the child-frame opener. Keep
all edits in neo-diredy; retain the uncommitted opener and diff-preview work.
Yazi remains a functional reference. Never import its settings or data.

Use Emacs bookmarks for named local directory bookmarks. Add the current
directory, jump through a child-frame list, and delete a selected bookmark.
Only entries tagged by neo-diredy appear in its menu. Reject duplicate names
without overwriting unrelated bookmarks. Standard bookmark save settings apply.
Reuse the opener's child-frame selector with a generic title/choices interface.
Bind C-c b a/j/d locally while the layout is active, including with Evil.

Preview commands scroll the right pane without moving the central selection
(Alt+Down/Up, five lines by default), or display a read-only snapshot in the
full window (C-c p). q/Esc returns to Dired. Preserve syntax colors, resize
copied images to the larger window, and retain the existing preview byte limit.
Do not visit the actual file or run its mode hooks. The user confirmed scrolling
and the larger view; manual image zoom is outside this version.

When a layout ends, dispose of unused Dired buffers first encountered during
that layout. Preserve all pre-existing buffers, the current displayed directory,
modified/WDired/marked buffers, buffers visible in any frame or saved tab, and
non-Dired buffers. Existing preview/process cleanup remains in place. Retain
normal Dired quit behavior; do not change global Dired options.

Test using isolated bookmarks and directories under tests/. Do not write real
bookmark files or change the user's running Emacs. Verify menu input in an
isolated GUI and distinguish this from physical keyboard/live-config checks.
