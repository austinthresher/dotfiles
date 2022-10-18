local wezterm = require 'wezterm'
return {
    --color_scheme = "midnight-in-mojave",
    --color_scheme = "Dark+",
    color_scheme = "Classic Dark (base16)",
    -- color_scheme = "ayu",

    window_background_opacity = 0.99,
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
    default_cursor_style = "BlinkingBlock",
    cursor_blink_rate = 800,
    cursor_blink_ease_in = "Linear",
    cursor_blink_ease_out = "EaseOut",
}
