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
nmap <leader>bd :set nobuflisted\|bp\|bd #<cr>

" Clear search with <C-l>
nnoremap <c-l> :noh<cr><c-l>

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

" Fast re-run
nnoremap <leader>! :!!<cr>

" Fast find-in-files
command! -nargs=1 F :execute 'vimgrep /<args>/j **' | copen | wincmd J

" Show and jump to quickfix, close quickfix when in the list
function! ToggleQuickfix()
    if &buftype ==# 'quickfix'
        wincmd p
        exec 'cclose'
    else
        exec 'copen 5'
        set nowrap nobuflisted
        stopinsert
        " NOTE: Not sure if I want this, jumps to the first line with 'error'
        "norm gg
        "silent! exec '/\<error\>'
    endif
endfunction
nnoremap <silent> Q :call ToggleQuickfix()<cr>
