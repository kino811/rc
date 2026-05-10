local wezterm = require 'wezterm'
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
config.color_scheme = 'Builtin Solarized Dark'
config.font = wezterm.font('JetBrains Mono')
config.font_size = 13.0
config.window_background_opacity = 0.95

-- 6. 꼭 필요한 단축키들 수동 등록
config.keys = {
  -- Ctrl-Enter: Org-mode 등에서 새로운 헤더 추가를 위해 명시적 전송
  {
    key = 'Return',
    mods = 'CTRL',
    action = wezterm.action.SendString '\x1b[13;5u',
  },

  -- macOS 필수 시스템 단축키 (Cmd 조합)
  { key = 'c', mods = 'CMD', action = wezterm.action.CopyTo 'Clipboard' },
  { key = 'v', mods = 'CMD', action = wezterm.action.PasteFrom 'Clipboard' },
  { key = 'w', mods = 'CMD', action = wezterm.action.CloseCurrentPane { confirm = true } },
  { key = 'q', mods = 'CMD', action = wezterm.action.QuitApplication },
  { key = 'f', mods = 'CMD', action = wezterm.action.Search { CaseInSensitiveString = '' } },
}

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
