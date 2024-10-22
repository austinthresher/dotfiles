" Neovim-specific plugins

Plug 'nvim-lualine/lualine.nvim'
Plug 'CodeGradox/onehalf-lush'
Plug 'https://gitlab.com/HiPhish/repl.nvim'

if !exists('g:repl')
    let g:repl = {}
endif

function! PreprocPythonREPL(txt) abort
    let lines = filter(split(a:txt, "\n"), {idx, val -> !empty(val)})
    let minspace = min(mapnew(lines, {_, s -> match(s, "[^ \t]") }))
    let lines = map(lines, {_, x -> x[minspace:]})
    if match(lines[-1], "[ \t]") == 0
        return join(lines, "\n") .. "\n\n"
    else
        return join(lines, "\n") .. "\n"
    endif
endfunc

let g:repl['python'] = {
            \ 'bin': 'python3',
            \ 'preproc': function('PreprocPythonREPL'),
            \ }


if executable('node')
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    call Include('nvim_coc.vim')
endif

