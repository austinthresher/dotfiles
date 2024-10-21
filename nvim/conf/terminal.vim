" Load terminal colorscheme values
runtime conf/terminal_colors.vim

function! s:InitializeTerm()
    silent setlocal nobuflisted winhl=Normal:Terminal nonumber winfixheight
    let b:term_title = substitute(b:term_title,
                \ 'term://\(.*\)//[0-9]*:\(.*\)', '\1$ \2', '')
    redraw!
endfunc

let g:terminal_color_0  = g:term_black
let g:terminal_color_1  = g:term_red
let g:terminal_color_2  = g:term_green
let g:terminal_color_3  = g:term_yellow
let g:terminal_color_4  = g:term_blue
let g:terminal_color_5  = g:term_purple
let g:terminal_color_6  = g:term_cyan
let g:terminal_color_7  = g:term_white
let g:terminal_color_8  = g:term_br_black
let g:terminal_color_9  = g:term_br_red
let g:terminal_color_10 = g:term_br_green
let g:terminal_color_11 = g:term_br_yellow
let g:terminal_color_12 = g:term_br_blue
let g:terminal_color_13 = g:term_br_purple
let g:terminal_color_14 = g:term_br_cyan
let g:terminal_color_15 = g:term_br_white

" Let Ctrl+W swap windows / etc
tnoremap <C-W> <C-\><C-N><C-W>

" Most of the above functions need to be called on certain autocommands
augroup TerminalAG
    autocmd!
    autocmd ColorScheme *
                \ exec 'hi Terminal guifg=' .. g:term_fg .. ' guibg=' .. g:term_bg |
                \ exec 'hi ExitOK guifg=#22CC22 guibg=#262626' |
                \ exec 'hi ExitError guifg=#CC2222 guibg=#262626'
    autocmd TermOpen * call s:InitializeTerm()
augroup END
