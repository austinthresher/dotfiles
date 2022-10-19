" CoC related config

let g:coc_global_extensions = [
            \ 'coc-json',
            \ 'coc-vimlsp',
            \ 'coc-sh',
            \ 'coc-jedi',
            \ 'coc-yaml',
            \ 'coc-lightbulb'
            \ ]

if has('win32')
    let g:coc_global_extensions += ['coc-omnisharp']
endif

" Warn for missing shellcheck so that we notice linting isn't happening
augroup Shellcheck
    au!
    au FileType sh if !executable('shellcheck') | echomsg "shellcheck not found" | endif
augroup END

function! FormatAction(idx, action) abort
    return string(a:idx+1) .. ') ' .. toupper(a:action.kind[0]) .. a:action.kind[1:] .. ': ' .. a:action.title
endfunc

function! ShowCodeActions() abort
    " Need a better way to check if it's ok to call CocAction
    if &readonly || &buftype == 'help' || &buftype == 'nofile' || (exists('b:coc_enabled') && !b:coc_enabled)
        return
    endif
    let l:actions = CocAction('codeActions', 'cursor')
    if len(l:actions) > 0
        " TODO: Show 
        call v:lua.vim.lsp.util.open_floating_preview(map(l:actions, function('FormatAction')))
    endif
endfunc

" TODO: Show code actions on right-click instead of hover
"augroup AutoShowCodeActions
"    au!
"    au CursorHold * call ShowCodeActions()
"augroup END
