local wezterm = require 'wezterm'

--color_scheme = "midnight-in-mojave",
-- color_scheme = "ayu",
--color_scheme = "Dark+",
local scheme = wezterm.get_builtin_color_schemes()['Classic Dark (base16)']
scheme.cursor_fg = "#000000"
scheme.cursor_bg = "#DDDDDD"

return {
    color_schemes = {
        ['Custom'] = scheme
    },
    color_scheme = "Custom",

    window_background_opacity = 0.98,
    font_size = 13,
    font = wezterm.font('JetBrains Mono', {weight="Light"}),
    font_rules = {
        { font = wezterm.font('JetBrains Mono', {weight="Light", italic=true}), italic=true },
        { font = wezterm.font('JetBrains Mono', {weight="ExtraBold"}), intensity='Bold' },
        { font = wezterm.font('JetBrains Mono', {weight="ExtraBold", italic=true}), intensity='Bold', italic=true }
    },

    default_prog = { '/usr/bin/bash', '-l' },
    hide_tab_bar_if_only_one_tab = true,
    harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' },
    bold_brightens_ansi_colors = false,
    window_padding = { left=0, right=0, top=0, bottom=0 },
    animation_fps = 60,
    default_cursor_style = "BlinkingUnderline",
    cursor_thickness = 2,
    cursor_blink_rate = 400,
    cursor_blink_ease_in = "Linear",
    cursor_blink_ease_out = "EaseOut",
}
