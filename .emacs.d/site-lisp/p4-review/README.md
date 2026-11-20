# p4-review

A standalone, read-only Perforce review interface for Emacs. Review submitted
changelists or shelves while keeping the source language mode and ordinary
code navigation.

This is an independent implementation inspired by
[p4review.nvim](https://gitlab.com/ofwinterpassed/p4review.nvim), not a source
translation or port.

## Requirements and installation

- Emacs 29.1 or later.
- An authenticated `p4` CLI configured for the review's server.
- GNU `diff` on PATH (or an executable configured through `diff-command`).
- For Swarm lookup: `curl`, an HTTPS `P4.Swarm.URL` server property, and a
  matching cached P4 login ticket.
- For semantic navigation: configured lsp-mode or Eglot and its language server.

The existing `p4.el` is optional. When already loaded, its `p4-executable` and
`p4-modify-args-function` settings are reused.

```elisp
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/site-lisp/p4-review"))
(require 'p4-review)
```

Installing this package does not change your Emacs configuration automatically.
During development, validate in a separate batch Emacs rather than loading new
code into a running editor.

## Usage

Run `M-x p4-review-open`, enter a CL/review number or an HTTPS Swarm review URL,
and select your local source root (the workspace root or its `client`
subdirectory). This directory also provides the P4 command context; the P4
server and authentication must still be configured, but the current client
does not need to map the review's stream. No P4 client setting is changed.
There is no submitted/shelved question:

- Submitted CLs open their submitted revisions.
- Pending CLs with shelved files open the shelf.
- Pending numbers without shelf files are looked up as Swarm review IDs.
- Review URLs go directly through Swarm. The latest review version determines
  the submitted CL or archived shelf to open, not an earlier entry in `commits`.

A pending change can also have no shelf and no review. This produces an explicit
error, not an empty success screen. The panel title shows the review ID/version
and resolved CL when Swarm was used. Refresh resolves the original input again.

The left panel lists files and the right shows source with added-line highlights
and virtual deleted lines. Details are displayed only when explicitly requested
with `D` or `C-c C-d`; opening a review never creates a details window.
Diff overlays do not modify source text or its line numbering.
Opening a review balances its windows automatically, like `C-x +`.
`p4-review-files-width` determines the initial split; the final widths are
balanced. Focus starts in the file list, including after the first source
preview loads. Closing restores the previous layout.

Deleted and added lines have `-` and `+` markers in the review window's left
margin, keeping old/new indentation aligned. Changed words are shown with
stronger backgrounds and bold text on both sides, including the virtual deleted
lines. C/C++ identifier fragments are expanded to complete identifiers so an
argument replacement such as `shotPos` to `msg.mPos` is easy to spot.

Word refinement uses Emacs's native diff refinement on whole changed blocks,
not guessed one-to-one line pairs. It is computed when navigating to a hunk or
when a hunk becomes visible through scrolling, then cached for revisits.
`p4-review-refine-max-chars` defaults to 30000; larger hunks keep line highlighting
and markers, with a skip message. Hunks with more than eight replacement blocks
also skip automatic refinement, bounding native diff process launches.
Refinement failures are reported in the review
panels rather than hiding the ordinary line diff. Original window margins are
restored when leaving the file or closing the review.

| Action | File/details panel | Source buffer |
|---|---|---|
| Select file | `RET` | |
| Next / previous file | `n` / `p` | `C-c C-f` / `C-c C-b` |
| Next / previous hunk | `]` / `[` | `C-c C-n` / `C-c C-p` |
| Show context and versions | `D` | `C-c C-d` |
| Reload changelist | `g` | `M-x p4-review-refresh` |
| Open local source / select a file | | `C-c C-o` |
| Start configured LSP / Eglot | | `C-c C-l` |
| Definitions / references | | `M-.` / `M-?` |
| Return from xref | | `M-,` |
| Close review | `q` | `C-c C-q` |

Use `M-x p4-review-resume` after visiting another buffer. Only one review is
active at a time. Closing restores the prior window layout and source
read-only state; killing a review panel or its active source also closes it.
Binary and special files are listed but do not have a source preview.

## Source identity and LSP limitations

**WORKSPACE match:** the entire current buffer text matches the reviewed new
revision. That real file buffer keeps its major mode and existing xref/LSP
configuration. It is temporarily read-only. An unsaved buffer is never reverted,
overwritten, or saved. Narrowed buffers are not reused.

**SNAPSHOT (no LSP):** the local source file is absent, unresolved, narrowed, or
different. The package creates an unvisited language-mode buffer with mode
hooks suppressed. It does not give the snapshot a real file identity or attach
it to a workspace language server. Direct review LSP/xref commands explain
this limitation instead of navigating against a different revision.

Snapshot mode detection uses only the depot file's basename and any inline
mode hint, with directory-local settings disabled. Depot paths are never used
as filesystem paths during mode detection, avoiding Windows UNC lookups.

Local lookup first uses complete relative paths anchored at the selected
directory's name or an immediate child directory. For example, a depot path
ending in `client/gameplay/.../example.cpp` can resolve under either a
workspace root containing `client` or the `client` directory itself. It does
not recursively guess by filename. A unique local match takes priority over
`p4 where`; the P4 mapping is a fallback when no local match exists.

`C-c C-o` keeps the review visible and opens local source in another window,
following the review cursor. Multiple local matches prompt for a choice instead
of silently using the P4 client. If neither root lookup nor P4 mapping finds
an existing file, a file picker opens at the selected root. An explicitly
selected file is remembered for that review file until refresh. Custom views
or different directory layouts may require this manual selection.

Unchanged lines are mapped
through a diff, including shifts caused by local insertions/deletions, and
retain their column. Equal-sized replacement blocks with unchanged context
give an explicitly **approximate** position at the start of the corresponding
line. If no correspondence can be established, the file opens without moving
its existing cursor and a message explains that no position was found. A
narrowed workspace buffer is not widened automatically.

This is explicitly a different context: LSP results describe your local code.
Deleted lines displayed virtually in the review are anchored at the following
new-source position; they are not independently selectable source lines.
Use the real file's normal LSP/xref commands to explore it, and
`M-x p4-review-resume` to return to the review. Opening the file does not itself
start a language server beyond your normal file hooks.

The package does not itself start a language server until `C-c C-l`. Normal
user file hooks can still run when a matching real file is first opened, and
an existing LSP connection is retained. A match for one source file does **not**
establish that headers, build flags, other files, or reference indexes match
the whole changelist. Exact shelf-wide LSP needs a separate matching project
and compilation database; this MVP does not create one.

## Read-only scope

Only read-only `describe`, `print`, `where`, `info -s`, `tickets`, and the
`P4.Swarm.URL` property query are sent to P4, asynchronously. There is no
sync, unshelve, checkout, submit, workspace-file write, or Swarm publication.
Changing selection or closing cancels pending requests and invalidates their
callbacks. Transport and content errors are shown in the review panels.
Changelist description line endings are normalized for display so CRLF or
CR-only descriptions do not show `^M`; source contents are not changed.

Swarm metadata is fetched through API v11 using the existing ticket for the
workspace server/cluster and user. Run `p4 login` yourself if no matching cached
ticket exists; the package does not log in or persist credentials. Authentication
is passed to curl over stdin, not command-line arguments. Only the HTTPS server
configured by P4 receives it, redirects are not followed, and a supplied review
URL must match that configured server. HTTP/authentication failures are explicit.

Submitted file revisions are pinned by revision number. Shelves are read live
using `@=CL` and cached for this review; they are not a transactionally frozen
review version. Refresh after reshelving. A shelf changed during loading can
produce a mixed snapshot, so do not treat this MVP as version-guarded approval.

Swarm comments, approval, AI suggestions, Git providers, and isolated
revision-specific LSP workspaces are outside this initial version.

## Isolated checks

From this directory:

```powershell
emacs --batch -Q -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile p4-review.el p4-review-tests.el
emacs --batch -Q -L . -l p4-review-tests.el -f ert-run-tests-batch-and-exit
```

The tests use local fixtures and disposable child Emacs processes, not a P4
server or a language server. Actual server configuration and project-specific
LSP integration must be checked separately.

## License

GPL-3.0-or-later.
