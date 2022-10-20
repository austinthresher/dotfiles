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

" Scroll floating window when visible, taken from coc help
if has('nvim-0.4.0') || has('patch-8.2.0750')
    nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
    nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
    inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
    inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
    vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
    vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

set tagfunc=CocTagFunc

" use <tab> for trigger completion and navigate to the next complete item
function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

function! QuietCocAsync(...) abort
    if CocAction('ensureDocument')
        call call(function("CocActionAsync"), a:000)
    endif
endfunc

inoremap <silent><expr> <Tab>
      \ coc#pum#visible() ?
      \ coc#pum#next(1) :
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()
inoremap <silent><expr> <S-Tab> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"
nnoremap <silent> <C-h> :call QuietCocAsync('doHover')<cr>
inoremap <silent><expr> <C-Space> coc#refresh()
nnoremap <silent> <leader>ca :call QuietCocAsync("codeAction", "cursor")<CR>

" This isn't great, figure out a better way to do this
function! CoCMenu() abort
    let l:menu = [
        \ "&References",
        \ "Super&types",
        \ "&Subtypes",
        \ "Re&name"
        \ ]
    let l:choice = confirm("Shortcut:", join(l:menu, "\n"))
    if l:choice == 1
        call QuietCocAsync("jumpReferences", v:false)
    endif
    if l:choice == 2
        call QuietCocAsync("showSuperTypes")
    endif
    if l:choice == 3
        call QuietCocAsync("showSubTypes")
    endif
    if l:choice == 4
        call QuietCocAsync("rename")
    endif
endfunc

nnoremap + <Cmd>call CoCMenu()<CR>

" Sort imports / includes
command! -nargs=0 Imports :call QuietCocAsync('runCommand', 'editor.action.organizeImport')

" Navigate diagnostics with [g and ]g
nnoremap <silent> [g <Plug>(coc-diagnostic-prev)
nnoremap <silent> ]g <Plug>(coc-diagnostic-next)
