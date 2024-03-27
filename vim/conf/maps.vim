" Alt + key mapping is wonky in terminal vim, this hack fixes it
if !has('nvim')
    for i in range(97, 122)
        let c = nr2char(i)
        "exec "map \e" .. c .. " <M-" .. c .. ">"
        "exec "map! \e" .. c .. " <M-" .. c .. ">"
        " Alternate version from stack overflow, seems to work better
        " but I haven't taken a close look why.
          exec "set <A-".c.">=\e".c
          exec "imap \e".c." <A-".c.">"
          exec "tmap \e".c." <A-".c.">"
    endfor
endif
" Came along with the stack overflow addition
set ttimeout ttimeoutlen=50

" Double-tap = to format the current block instead of just the current line
" This would be far less verbose if we didn't preserve the cursor location
function! FormatInPlace()
    let l:pos = winsaveview()
    let l:oldlen = col([l:pos.lnum, "$"])
    normal! =ip
    let l:diff = l:oldlen - col([l:pos.lnum, "$"])
    let l:pos.col = l:pos.col - l:diff
    let l:pos.curswant = l:pos.curswant - l:diff
    call winrestview(l:pos)
endfunc

nnoremap == <Cmd>call FormatInPlace()<CR>

" Use tab and shift-tab to indent lines
nnoremap <tab> >>
nnoremap <s-tab> <<
xnoremap <tab> >
xnoremap <s-tab> <

" Delete buffer while keeping window open
nnoremap <silent> <leader>bd :set nobuflisted\|bp\|bd #<cr>

" Clear search with <C-l>
nnoremap <silent> <c-l> <cmd>noh<cr><c-l>
inoremap <silent> <c-l> <cmd>noh<cr>
" Also exit visual mode if used there
xnoremap <silent> <c-l> <cmd>noh<cr><esc>

" * Sets word under cursor to search term but doesn't go to the next match
nnoremap * *N

function! TermNormalMode()
    " See terminal.vim
    let b:term_mode = v:false
    call feedkeys("\<c-\>\<c-n>")
endfunc

" Navigate out of terminal mode more easily
tnoremap <esc><esc> <cmd>call TermNormalMode()<cr>

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
tmap <M-n> <C-\><C-N>gt
tmap <M-p> <C-\><C-N>gT
tmap <S-space> <space>
tmap <S-backspace> <backspace>

" Fast re-run
nnoremap <leader>! :!!<cr>

" Fast find-in-files, preferring ripgrep
if executable('rg')
    command! -nargs=1 F :cgetexpr system('rg --vimgrep <args>') | copen | wincmd J
else
    command! -nargs=1 F :execute 'vimgrep /<args>/j **' | copen | wincmd J
endif

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

" Vim 'sentences' aren't very useful for code. Move to next or prev parens.
function! NextParen()
    if sneak#is_sneaking() && sneak#state().input ==# ')'
        exec "normal \<Plug>Sneak_f"
    else
        call sneak#cancel()
        exec "normal \<Plug>Sneak_f)"
    endif
endfunc

function! PrevParen()
    if sneak#is_sneaking() && sneak#state().input ==# '('
        exec "normal \<Plug>Sneak_F"
    else
        call sneak#cancel()
        exec "normal \<Plug>Sneak_F("
    endif
endfunc

nnoremap <silent> ( <Cmd>call PrevParen()<cr>
onoremap <silent> ( <Cmd>call PrevParen()<cr>
xnoremap <silent> ( <Cmd>call PrevParen()<cr>
nnoremap <silent> ) <Cmd>call NextParen()<cr>
onoremap <silent> ) <Cmd>call NextParen()<cr>
xnoremap <silent> ) <Cmd>call NextParen()<cr>


" Convert a non-zero number to and from hex
function! DecToHex(num)
    let l:val = (a:num+1)-1
    if l:val ==# 0
        return
    endif
    let l:hex = printf("0x%X", l:val)
    exec "norm! ciw" . l:hex
endfunc

function! HexToDec(num)
    let l:val = (a:num+1)-1
    if l:val ==# 0
        return
    endif
    let l:dec = printf("%d", l:val)
    exec "norm! ciw" . l:dec
endfunc

nnoremap <leader>x <Cmd>call DecToHex(expand("<cexpr>"))<cr>
nnoremap <leader>X <Cmd>call HexToDec(expand("<cexpr>"))<cr>

" Swap _ and - so I don't keep accidentally opening dirvish
nnoremap _ <Plug>(dirvish_up)
nnoremap - _

" switch between related files (.c -> .h, etc.)
" nnoremap <leader>A <Plug>(altr-forward)

" save and return to normal mode if not already
nnoremap <C-s> <Cmd>update<cr>
inoremap <C-s> <Cmd>update<cr><esc>
xnoremap <C-s> <Cmd>update<cr><esc>

nmap <leader>a <Plug>(EasyAlign)
xmap <leader>a <Plug>(EasyAlign)
