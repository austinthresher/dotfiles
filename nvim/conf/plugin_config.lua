-- Configuration for Neovim Lua plugins

local V = _G["vim"].api.nvim_eval

-- These colors go along with my lyra colorscheme.
-- If we're using a different theme these are ignored.
local colors = {
    red      = '#8C2D2D',
    green    = '#608860',
    yellow   = '#D3944D',
    blue     = '#3B7EAA',
    magenta  = '#987CDA',
    cyan     = '#1CB37B',
    white    = '#A0A0A0',
    black    = '#121214',
    br_white = '#EEEEEE',
    dark     = '#3A3A3F',
    darker   = '#26262A',
    darkest  = '#1C1C1F',
}

local insert_color = colors.green
local normal_color = colors.blue
local replace_color = colors.yellow
local visual_color = colors.cyan

local theme = {
    normal = {
        a = { fg = colors.black, bg = normal_color, gui = 'bold' },
        b = { fg = normal_color, bg = colors.darker },
        c = { fg = colors.white, bg = colors.dark },
    },
    visual = {
        a = { fg = colors.black, bg = visual_color, gui = 'bold' },
        b = { fg = visual_color, bg = colors.darker },
    },
    inactive = {
        a = { fg = colors.black, bg = colors.dark, gui = 'bold' },
        b = { fg = colors.dark, bg = colors.darker },
        c = { fg = colors.dark, bg = colors.darkest },
    },
    replace = {
        a = { fg = colors.black, bg = replace_color, gui = 'bold' },
        b = { fg = replace_color, bg = colors.darker },
    },
    insert = {
        a = { fg = colors.black, bg = insert_color, gui = 'bold' },
        b = { fg = insert_color, bg = colors.darker },
        c = { fg = colors.white, bg = colors.dark },
    },
    terminal = {
        a = { fg = colors.black, bg = insert_color, gui = 'bold' },
        b = { fg = insert_color, bg = colors.darker },
        c = { fg = colors.white, bg = colors.dark },
    },
}

local function exclude_term()
    return V'&buftype' ~= 'terminal'
end

local function term()
    return not exclude_term()
end

local function term_status()
    if not V[[getbufvar(bufnr(), 'exited', v:false)]] then
        return 'running'
    end
    local exit_code = V'b:exit_code'
    if exit_code == 0 then
        return '%#ExitOk#finished'
    end
    return '%#ExitError#exited ' .. exit_code
end
local function deep_copy(tbl)
    local result = {}
    for k, v in pairs(tbl) do
        if type(v) == "table" then
            result[k] = deep_copy(v)
        else
            result[k] = v
        end
    end
    return result
end

local use_custom_theme = false
if not use_custom_theme then
    theme = deep_copy(require "lualine.themes.onelight")
    for _, mode in pairs(theme) do
        mode.a.fg = "#ffffff"
    end
    theme.normal.a.bg, theme.insert.a.bg = theme.insert.a.bg, theme.normal.a.bg
    theme.visual.a.bg, theme.command.a.bg = theme.command.a.bg, theme.visual.a.bg
    theme.normal.c.fg, theme.normal.c.bg = theme.normal.c.bg, theme.normal.c.fg
    theme.normal.y = {fg=theme.normal.c.fg, bg=theme.normal.c.bg}
    theme.terminal = { a = {fg = "#ffffff", bg="#26b6c2", gui = 'bold' } }
    theme.inactive.c.fg = "#7f7f7f"
    theme.inactive.c.bg = "#2c2c2c"
end

local found, lualine = pcall(require, 'lualine')
if found then
    local config = {
        options = {
            icons_enabled = false,
            theme = theme,
            component_separators = '',
            section_separators = '',
            disabled_filetypes = {},
            always_divide_middle = true,
            globalstatus = false,
        },
        sections = {
            lualine_a = {'mode'},
            lualine_b = {
                -- { 'branch', cond = exclude_term },
                -- { 'diff', cond = exclude_term },
            },
            lualine_c = {
                { "filename", path = 1, cond = exclude_term },
                { "b:term_title", cond = term }
            },
            lualine_x = {
                { 'encoding', cond = exclude_term },
                { 'fileformat', cond = exclude_term },
                { 'filetype', cond = exclude_term },
                { 'diagnostics',
                    cond = exclude_term,
                    diagnostics_color = {
                        error = { bg = "red", fg = "#303030" },
                        warn = { bg = "yellow", fg = "#303030" },
                        info = { bg = "white", fg = "#303030" },
                        hint = { bg = "grey", fg = "#303030" }
                    },
                },
                { term_status, cond = term },
            },
            lualine_y = { {'progress', cond = exclude_term } },
            lualine_z = { {'location', cond = exclude_term } }
        },
        inactive_sections = {
            lualine_a = {},
            lualine_b = {},
            lualine_c = {
                { 'filename', path = 1, cond = exclude_term },
                { 'b:term_title', cond = term },
            },
            lualine_x = {
                { 'location', cond = exclude_term },
                { term_status, cond = term },
            },
            lualine_y = {},
            lualine_z = {}
        },
        extensions = {'quickfix', 'fugitive'}
    }
    if V'&showtabline' ~= 0 then
        config.tabline = {
            lualine_a = { {
                    'buffers', buffers_color = {
                        active = 'TabLineSel',
                        inactive = 'TabLine'
            } } },
            lualine_b = { },
            lualine_c = { },
            lualine_x = { },
            lualine_y = { },
            lualine_z = {'tabs'},
        }
    end
    lualine.setup(config)
end

