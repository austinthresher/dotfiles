" Neovim-specific plugins

Plug 'nvim-lualine/lualine.nvim'

if executable('node')
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    let g:coc_global_extensions = ['coc-json', 'coc-vimlsp', 'coc-sh', 'coc-jedi', 'coc-yaml', 'coc-lightbulb']
    if has('win32')
        let g:coc_global_extensions += ['coc-omnisharp']
    endif
    augroup Shellcheck
        au!
        au FileType sh if !executable('shellcheck') | echomsg "shellcheck not found" | endif
    augroup END
endif

