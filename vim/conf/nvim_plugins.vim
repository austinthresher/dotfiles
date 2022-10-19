" Neovim-specific plugins

Plug 'nvim-lualine/lualine.nvim'

if executable('node')
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    call Include('nvim_coc.vim')
endif

