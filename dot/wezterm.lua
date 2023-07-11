local wezterm = require 'wezterm'

-- Leaving this commented as a reference for how to customize colors
-- local scheme = wezterm.get_builtin_color_schemes()['Classic Dark (base16)']
-- scheme.cursor_fg = '#000000'
-- scheme.cursor_bg = '#DDDDDD'

function exists(fname)
    local f = io.open(fname, "r")
    if f ~= nil then
        io.close(f)
        return true
    end
    return false
end

wezterm.on(
    'format-tab-title',
    function(tab, tabs, _, _, _, max_width)
        local separator = " "
        if #tabs > 1 then
            separator = "▕"
        end
        local tab_title = tab.active_pane.title
        -- Remove the hostname if the tab is running locally
        local first, last = string.find(tab_title, "@" .. wezterm.hostname())
        if first ~= nil then
            tab_title = string.sub(tab_title, last+3)
        end
        local prefix = " " .. tostring(tab.tab_index+1) .. ": "
        local title =  prefix .. tab_title .. separator
        -- I think the +2 here is because the separator is more than 1 byte in UTF8
        if string.len(title) > max_width + 2 then
            title = wezterm.truncate_right(title, max_width - 2) .. "…" .. separator
        end
        return title
    end
)

local config = {
    -- Leaving this for the same reason above
    -- color_schemes = { ['Custom']=scheme },
    -- color_scheme = "Custom",
    color_scheme = "iceberg-dark",
    window_background_opacity = 1.0,
    -- font_size = 16,
    -- font = wezterm.font('Px437 Wang Pro Mono'),
    -- font_rules = {
    --     { font = wezterm.font('Px437 Rainbow100 re.80'), italic=true },
    --     { font = wezterm.font('Px437 Wang Pro Mono'), intensity='Bold' },
    --     { font = wezterm.font('Px437 Rainbow100 re.80'), intensity='Bold', italic=true }
    -- },
    --font_size = 12,
    --font = wezterm.font('JetBrains Mono', {weight="Light"}),
    --font_rules = {
    --    {font = wezterm.font('JetBrains Mono', {weight="Light", italic=true}), italic=true},
    --    {font = wezterm.font('JetBrains Mono', {weight="ExtraBold"}), intensity='Bold'},
    --    {font = wezterm.font('JetBrains Mono', {weight="ExtraBold", italic=true}), intensity='Bold', italic=true}
    --},
    font_size = 14,
    font = wezterm.font('Iosevka Nerd Font Mono'),

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

    -- Only for the retro tab bar
    tab_max_width = 128,
    colors = {
        tab_bar = {
            background = "#181820",
            active_tab = {
                bg_color = "#202038",
                fg_color = "#DDDDDD"
            },
            inactive_tab = {
                bg_color = "#101018",
                fg_color = "#777777"
            },
            inactive_tab_hover = {
                bg_color = "#282840",
                fg_color = "#999999"
            },
            new_tab = {
                bg_color = "#181820",
                fg_color = "#777777",
                intensity = "Bold"
            },
            new_tab_hover = {
                bg_color = "#181820",
                fg_color = "#FFFFFF",
                intensity = "Bold"
            }
        }
    },
    keys = {
        { key = 'l', mods='CTRL|SHIFT', action = wezterm.action.ShowLauncher },
    },
}

if string.find(wezterm.target_triple, "windows") then
    local git_bash = 'C:\\Program Files\\Git\\bin\\bash.exe'
    if exists(git_bash) then
        config.default_prog = { git_bash, '-l' }
    else
        config.default_domain = wezterm.default_wsl_domains()[1].name
    end
    config.use_fancy_tab_bar = false
else
    config.default_prog = { '/usr/bin/bash', '-l' }
end

return config
