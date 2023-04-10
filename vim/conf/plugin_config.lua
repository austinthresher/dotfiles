-- Configuration for Neovim Lua plugins

local V = vim.api.nvim_eval

-- These colors go along with my lyra colorscheme.
-- If we're using catppuccin these are ignored.
local colors = {
    red      = '#5F0000',
    green    = '#00DF5F',
    yellow   = '#FF8700',
    blue     = '#0087DF',
    magenta  = '#FF00AF',
    cyan     = '#00FFAF',
    white    = '#D0D0D0',
    black    = '#121212',
    br_white = '#EEEEEE',
    dark     = '#3A3A3A',
    darker   = '#262626',
    darkest  = '#1C1C1C',
}

local insert_color = colors.green
local normal_color = colors.blue
local replace_color = colors.yellow
local visual_color = colors.cyan

local lualine_theme = {
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

function exclude_term()
    return V('&buftype') ~= 'terminal'
end

function term()
    return not exclude_term()
end

function term_status()
    if not V([[getbufvar(bufnr(), 'exited', v:false)]]) then
        return 'running'
    end
    local exit_code = V('b:exit_code')
    if exit_code == 0 then
        return '%#ExitOk#finished'
    end
    return '%#ExitError#exited ' .. exit_code
end

actual_theme = lualina_theme
if vim.cmd("call has('nvim-0.8')") then
    found, catppuccin = pcall(require, 'catppuccin')
    if found then
        actual_theme = "catppuccin"
        catppuccin.setup {
            custom_highlights = function(colors)
                return {
                    Todo = {fg=colors.peach, bg=colors.none, style={}}
                }
            end
        }
    end
end

found, lualine = pcall(require, 'lualine')
if found then lualine.setup {
    options = {
        icons_enabled = false,
        theme = actual_theme,
        component_separators = '',
        section_separators = '',
        disabled_filetypes = {},
        always_divide_middle = true,
        globalstatus = false,
    },
    sections = {
        lualine_a = {'mode'},
        lualine_b = {
            { 'branch', cond = exclude_term },
            { 'diff', cond = exclude_term },
            { 'diagnostics', cond = exclude_term },
        },
        lualine_c = {
            { 'filename', path = 1, cond = exclude_term },
            { 'b:term_title', cond = term },
        },
        lualine_x = {
            { 'encoding', cond = exclude_term },
            { 'fileformat', cond = exclude_term },
            { 'filetype', cond = exclude_term },
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
    tabline = {
        lualine_a = {
            {
                'buffers',
                buffers_color = {
                    active = 'TabLineSel',
                    inactive = 'TabLine'
                }
            }
        },
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {'tabs'},
    },
    extensions = {'quickfix', 'fugitive'}
} end

