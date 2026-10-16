local wezterm = require('wezterm')
local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- 1. [핵심] 모든 기본 단축키 비활성화 (이맥스와의 충돌 원천 차단)
config.disable_default_key_bindings = true

-- 2. [핵심] 현대적 키보드 프로토콜 활성화
config.enable_csi_u_key_encoding = true

-- 3. [핵심] 한글 입력 및 물리 키 신호 관련
config.use_ime = true
config.macos_forward_to_ime_modifier_mask = "SHIFT"

-- 4. 카라비너 설정 존중 (보조키 매핑 방지)
config.send_composed_key_when_left_alt_is_pressed = false
config.send_composed_key_when_right_alt_is_pressed = false

-- 5. 외관 설정
-- OneDark (base16), close to the spacemacs-dark look used in Emacs.
-- Its "bright black" (ANSI 90, used by PowerShell for parameters such as -nw,
-- inline predictions and dim text) is only 1.97:1 against the background, so
-- lighten it to 3.5:1 to keep that text readable.
local onedark = wezterm.color.get_builtin_schemes()['OneDark (base16)']
onedark.brights[1] = '#7a808c'
config.color_schemes = { ['OneDark (readable bright black)'] = onedark }
config.color_scheme = 'OneDark (readable bright black)'
config.font = wezterm.font('JetBrains Mono')
config.font_size = 13.0
config.window_background_opacity = 1.00

-- Keep the scrollback small (default 3500).  Claude Code repaints the whole
-- live area on every keystroke, and a smaller scrollback makes that cheaper.
config.scrollback_lines = 2000

-- 6. 꼭 필요한 단축키들 수동 등록
config.keys = {
  -- macOS 필수 시스템 단축키 (Cmd 조합)
  { key = 'c', mods = 'CMD', action = wezterm.action.CopyTo('Clipboard') },
  { key = 'v', mods = 'CMD', action = wezterm.action.PasteFrom('Clipboard') },
  { key = 'w', mods = 'CMD', action = wezterm.action.CloseCurrentPane({ confirm = true }) },
  { key = 'q', mods = 'CMD', action = wezterm.action.QuitApplication },
  { key = 'f', mods = 'CMD', action = wezterm.action.Search({ CaseInSensitiveString = '' }) },
  {key = '=', mods = 'CMD', action = wezterm.action.IncreaseFontSize },
  {key = '-', mods = 'CMD', action = wezterm.action.DecreaseFontSize },
  {key = '0', mods = 'CMD', action = wezterm.action.ResetFontSize },
}

-- Windows: font size with Ctrl+Alt+Shift + 0 / - / +
-- (Emacs already uses C-M-0, C-M--, C-M-= for its own text size, so the
-- Shift variants are used here to avoid the conflict.)
if wezterm.target_triple:find('windows') then
  local font_keys = {
    { '0', ')', wezterm.action.ResetFontSize },
    { '-', '_', wezterm.action.DecreaseFontSize },
    { '=', '+', wezterm.action.IncreaseFontSize },
  }
  for _, entry in ipairs(font_keys) do
    for _, key in ipairs({ entry[1], entry[2] }) do
      table.insert(config.keys, { key = key, mods = 'CTRL|ALT|SHIFT', action = entry[3] })
    end
  end
end

-- Windows only: key carriers for terminal Emacs.
--
-- The Windows console (ConPTY) drops or changes the modifiers of some keys before
-- they reach Emacs: Ctrl+Enter and Ctrl+J both arrive as a plain RET, Ctrl+H arrives
-- as <backspace>, Shift+Enter arrives as M-<return> (the same as Alt+Enter), and
-- Ctrl+Shift+Backspace arrives as C-\ (the same as the real Ctrl+\ key).  Ctrl+Space and
-- Ctrl+Shift+2 (C-@) lose their Ctrl and are inserted as plain characters.
-- Modified function keys do survive, so WezTerm sends
-- those instead and Emacs translates
-- them back to the original key (see the "term" section of myInit.org).
--
--   Ctrl+Enter -> Ctrl+Shift+F9   (Emacs: C-S-<f9>  -> C-<return>)
--   Ctrl+J     -> Ctrl+Shift+F10  (Emacs: C-S-<f10> -> C-j)
--   Ctrl+H     -> Ctrl+Shift+F11  (Emacs: C-S-<f11> -> C-h, the help prefix)
--   Shift+Enter-> Ctrl+Shift+F12  (Emacs: C-S-<f12> -> S-<return>; Org uses it for
--                                  org-table-copy-down)
--   Ctrl+Shift+Backspace -> Ctrl+Shift+F8
--                                 (Emacs: C-S-<f8> -> C-S-<backspace>, kill-whole-line)
--   Ctrl+Space   -> Ctrl+Shift+F7 (Emacs: C-S-<f7> -> C-SPC, set-mark-command)
--   Ctrl+Shift+2 -> Ctrl+Shift+F6 (Emacs: C-S-<f6> is bound to er/expand-region,
--                                  the command the GUI has on C-@)
--
-- Side effects: in other programs inside WezTerm, Ctrl+J no longer acts as Enter,
-- Ctrl+H no longer acts as Backspace (the Backspace key itself is unchanged), and
-- Shift+Enter no longer inserts a newline in Claude Code: use Alt+Enter there.  Ctrl+Space
-- and Ctrl+Shift+2 no longer insert a space or an @.
-- macOS and Linux are not affected by this block.
if wezterm.target_triple:find('windows') then
  local carriers = {
    -- { key, modifiers, escape sequence sent instead }
    { 'Enter', 'CTRL', '\x1b[20;6~' },  -- Ctrl+Shift+F9
    -- phys: keeps it working while the Korean IME is on
    { 'phys:J', 'CTRL', '\x1b[21;6~' },  -- Ctrl+Shift+F10
    { 'phys:H', 'CTRL', '\x1b[23;6~' },  -- Ctrl+Shift+F11
    { 'Enter', 'SHIFT', '\x1b[24;6~' },  -- Ctrl+Shift+F12
    { 'Backspace', 'CTRL|SHIFT', '\x1b[19;6~' },  -- Ctrl+Shift+F8
    { 'phys:Space', 'CTRL', '\x1b[18;6~' },  -- Ctrl+Shift+F7
    { 'phys:2', 'CTRL|SHIFT', '\x1b[17;6~' },  -- Ctrl+Shift+F6
  }
  for _, c in ipairs(carriers) do
    table.insert(config.keys, {
      key = c[1],
      mods = c[2],
      action = wezterm.action.SendString(c[3]),
    })
  end
end

-- [자동 매핑] 알파벳 a-z까지 Meta(Alt) 단축키 일괄 등록
-- 물리 키(phys:) 위치를 기준으로 매핑하여 한글/영문 입력 상태와 상관없이 
-- 이맥스에 정확한 Meta 신호를 전달합니다.
for i = string.byte('a'), string.byte('z') do
  local char = string.char(i)
  -- Alt + 물리키 (소문자 Meta)
  table.insert(config.keys, {
    key = 'phys:' .. string.upper(char),
    mods = 'ALT',
    action = wezterm.action.SendString('\x1b' .. char),
  })
  -- Alt + Shift + 물리키 (대문자 Meta)
  table.insert(config.keys, {
    key = 'phys:' .. string.upper(char),
    mods = 'ALT|SHIFT',
    action = wezterm.action.SendString('\x1b' .. string.upper(char)),
  })
end

return config
