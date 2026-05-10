local wezterm = require 'wezterm'
local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- 1. [핵심] 현대적 키보드 프로토콜 활성화
-- 이 옵션이 켜져 있어야 C-RET, C-, 등의 복잡한 키 조합이 이맥스(v29+)에 정확히 전달됩니다.
config.enable_csi_u_key_encoding = true

-- 2. [핵심] 한글 입력 및 물리 키 신호 관련
-- IME(한글 입력기)를 사용하면서도 단축키 충돌을 최소화합니다.
config.use_ime = true
-- 아래 옵션은 한글 입력 상태에서도 Ctrl, Alt 조합 등이 쉘이나 tmux에 더 잘 전달되게 돕습니다.
config.macos_forward_to_ime_modifier_mask = "Shift|Control|Alt|Meta"

-- 3. 카라비너 설정 존중 (보조키 매핑 방지)
-- WezTerm 자체에서 Option 키를 Alt로 바꾸는 기능을 꺼서 카라비너의 설정과 충돌하지 않게 합니다.
config.send_composed_key_when_left_alt_is_pressed = "No"
config.send_composed_key_when_right_alt_is_pressed = "No"

-- 4. 외관 및 기본 설정
config.color_scheme = 'Builtin Solarized Dark'
config.font = wezterm.font('JetBrains Mono')
config.font_size = 13.0
config.window_background_opacity = 0.95

-- 5. 이맥스 사용자들을 위한 추가 설정 (필요 시 주석 해제)
-- enable_csi_u_key_encoding 만으로 C-RET 가 작동하지 않을 때 아래 주석을 풀어 사용하세요.
-- config.keys = {
--   {
--     key = 'Return',
--     mods = 'CTRL',
--     action = wezterm.action.SendString '\x1b[13;5u', -- CSI u 형식의 C-RET 신호
--   },
-- }

return config
