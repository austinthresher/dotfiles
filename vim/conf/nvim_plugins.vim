" Neovim-specific plugins

Plug 'nvim-lualine/lualine.nvim'

" TODO: Check if node exists before installing CoC
Plug 'neoclide/coc.nvim', {'branch': 'release'}
let g:coc_global_extensions = ['coc-json', 'coc-vimlsp', 'coc-sh']

