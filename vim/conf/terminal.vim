set termwinkey=<ins>
let g:terminal_ansi_colors = [
        \ g:term_black,
        \ g:term_red,
        \ g:term_green,
        \ g:term_yellow,
        \ g:term_blue,
        \ g:term_purple,
        \ g:term_cyan,
        \ g:term_white,
        \ g:term_br_black,
        \ g:term_br_red,
        \ g:term_br_green,
        \ g:term_br_yellow,
        \ g:term_br_blue,
        \ g:term_br_purple,
        \ g:term_br_cyan,
        \ g:term_br_white
    \ ]

augroup TerminalAG
    autocmd!
    " Terminal FG / BG are set separately from colors 0-15
    autocmd ColorScheme *
                \ exec 'hi Terminal guifg='.g:term_fg.' guibg='.g:term_bg
    " Automatically enter insert mode when selecting terminal window
    " The redraw fixes a cursor placemenet bug with vim-flip
    autocmd BufEnter *
                \ if &buftype ==# 'terminal' |
                \ exec 'norm i' | exec 'redraw!' |
                \ endif
    " Unlist terminals so we don't hit them with bprev / bnext
    autocmd TerminalOpen * set nobuflisted
augroup END
