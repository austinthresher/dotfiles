" Neovim-specific plugins

Plug 'nvim-lualine/lualine.nvim'

if has('nvim-0.8')
    Plug 'catppuccin/nvim', {'as': 'catppuccin'}
endif

if executable('node')
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    call Include('nvim_coc.vim')
endif

