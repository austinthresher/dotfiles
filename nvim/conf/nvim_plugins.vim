" Neovim-specific plugins

Plug 'nvim-lualine/lualine.nvim'
Plug 'CodeGradox/onehalf-lush'
Plug 'https://gitlab.com/HiPhish/repl.nvim'

if !exists('g:repl')
    let g:repl = {}
endif
" Preproc function taken from default, added newline at the end
let g:repl['python'] = {
            \ 'bin': 'python3',
            \ 'preproc':
            \ {txt -> join(filter(split(txt, "\n"), {idx, val -> !empty(val)}), "\n") .. "\n" }
            \ }


if executable('node')
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    call Include('nvim_coc.vim')
endif

