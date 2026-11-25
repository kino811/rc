// Ctrl+J for herdr on Windows (bound in config.toml as a [[keys.command]]).
//
// WezTerm sends Ctrl+J as Ctrl+Shift+F10 (see .config/wezterm/wezterm.lua) because the
// Windows console turns Ctrl+J into a plain Enter. herdr cannot remap a key per pane, so
// this script does what the C-S-F10 binding in .tmux.conf does:
//   - focused pane runs Claude Code -> send Alt+Enter (newline in the prompt). Claude Code
//     key bindings do not match F10 keys, so binding ctrl+shift+f10 there does not work.
//   - any other pane -> send Ctrl+Shift+F10 back unchanged (Emacs maps it to C-j).
const { execFileSync } = require('child_process');

const run = (args) => execFileSync('herdr', args, { encoding: 'utf8', windowsHide: true });

const panes = JSON.parse(run(['pane', 'list'])).result.panes;
const pane = panes.find((p) => p.focused);
if (pane) {
  const key = pane.agent === 'claude' ? 'alt+enter' : 'ctrl+shift+f10';
  run(['pane', 'send-keys', pane.pane_id, key]);
}
