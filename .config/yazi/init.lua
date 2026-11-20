-- yamb.yazi: persistent bookmarks.
-- Save the current folder with "u a", jump with "u g" (see keymap.toml).
-- Bookmarks added from the UI are stored in %APPDATA%\yazi\state\bookmark, outside the
-- config folder (the config folder is a link into the public rc repo).
-- Fixed bookmarks can be listed below, e.g.
--   { tag = "D drive", path = "D:\\", key = "D" },
-- (a path ending with a separator is a folder).
local bookmarks = {}

require("yamb"):setup {
  bookmarks = bookmarks,
  jump_notify = true,
  -- Used by the "u G" / "u D" / "u R" fzf commands (fzf must be installed).
  cli = "fzf",
  keys = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
  path = os.getenv("APPDATA") .. "\\yazi\\state\\bookmark",
}
