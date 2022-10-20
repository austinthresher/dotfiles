" Alt + key mapping is wonky in terminal vim, this hack fixes it
if !has('nvim')
    for i in range(97, 122)
        let c = nr2char(i)
        exec "map \e" .. c .. " <M-" .. c .. ">"
        exec "map! \e" .. c .. " <M-" .. c .. ">"
    endfor
endif

" Use tab and shift-tab to indent lines
nnoremap <tab> >>
nnoremap <s-tab> <<
xnoremap <tab> >
xnoremap <s-tab> <

" Delete buffer while keeping window open
nnoremap <silent> <leader>bd :set nobuflisted\|bp\|bd #<cr>

" Clear search with <C-l>
nnoremap <silent> <c-l> :noh<cr><c-l>

" * Sets word under cursor to search term but doesn't go to the next match
nnoremap * *N

" Navigate out of terminal mode more easily
tnoremap <esc><esc> <c-\><c-n>

" Navigate across windows with Alt + hjkl in any mode
inoremap <M-h> <C-\><C-N><C-w>h
inoremap <M-j> <C-\><C-N><C-w>j
inoremap <M-k> <C-\><C-N><C-w>k
inoremap <M-l> <C-\><C-N><C-w>l
nnoremap <M-h> <C-w>h
nnoremap <M-j> <C-w>j
nnoremap <M-k> <C-w>k
nnoremap <M-l> <C-w>l

" These don't work in terminal vim, but work in nvim and gvim
tmap <M-h> <C-\><C-N><C-w>h
tmap <M-j> <C-\><C-N><C-w>j
tmap <M-k> <C-\><C-N><C-w>k
tmap <M-l> <C-\><C-N><C-w>l

" Switch tab pages with Alt+N and Alt+P
inoremap <M-n> <C-\><C-N>gt
inoremap <M-p> <C-\><C-N>gT
nnoremap <M-n> gt
nnoremap <M-p> gT

" Fast re-run
nnoremap <leader>! :!!<cr>

" Fast find-in-files
command! -nargs=1 F :execute 'vimgrep /<args>/j **' | copen | wincmd J

" Navigate buffers with Ctrl+J/K
function! NoTermExec(cmd)
    if &buftype !=# 'terminal' | sil exec a:cmd | endif
endfunc
nmap <silent> <C-j> :call NoTermExec('bn')<CR>
nmap <silent> <C-k> :call NoTermExec('bp')<CR>

" View highlight stack for debugging
function! SynStack()
    if !exists("*synstack")
        return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
nnoremap <leader>H :call SynStack()<CR>

" Change working directory to current file location
command CD cd %:p:h
