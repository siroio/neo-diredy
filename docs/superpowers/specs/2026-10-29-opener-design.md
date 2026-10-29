# Child-frame opener menu

## Intent and agreed behavior

Shift+Enter in neo-diredy opens a child-frame menu of opening methods for the
item at point. The user explicitly requested a child frame after reviewing the
opener proposal. Yazi is a functional reference only: do not import, share, or
modify its configuration or data. Bookmarks remain a separate future request.

- Windows default application and Explorer are built-in choices.
- Explorer opens a directory, or selects a file in its containing directory.
- User-defined openers specify a label, extensions, executable and literal
  arguments. Append the absolute file path as one argument, without a shell.
- Match extensions without case sensitivity; an empty extension list matches
  any item. Do not preconfigure particular third-party applications.
- Show choices in a theme-aware child frame, keeping focus in the parent.
  Up/down (also j/k and C-n/C-p) select, Enter accepts, Esc or C-g cancels.
- Cancel/error must dispose of the child frame and its temporary buffer.
- Ordinary Enter and other Dired bindings retain their behavior. Shift+Enter
  applies only while neo-diredy owns the Dired buffer, including with Evil.
- Reject missing/remote targets and unfinished WDired renames. A terminal frame
  reports that this menu requires a graphical frame.

## Approach

Use native Emacs child frames and direct process arguments. Reusing the
minibuffer completion UI was superseded by the user's child-frame requirement;
adding posframe would introduce an unnecessary dependency. Reuse the existing
buffer-local key mode and its layout cleanup.

## Scope and verification

Keep changes within neo-diredy.el, README.md, relevant tests and these design
notes. Preserve the existing uncommitted diff-preview changes. Do not update
installed copies, user settings, sibling repositories or Git history.

Test matching, native launch arguments, cancellation, missing executables,
special-character paths, key restoration and popup cleanup. Exercise real child
frames in an isolated GUI Emacs in addition to batch ERT; distinguish those
checks from the user's actual configuration and external applications.

Windows launch detail: Emacs process arguments use the system ANSI code page.
Bind its coding and argument quoting explicitly; reject unrepresentable text
instead of silently changing it. The default application action still accepts
Unicode paths through Windows ShellExecute. Explorer file selection quotes the
whole path, including commas. Reject batch-script openers, which would expand
file-name characters in a command shell.
