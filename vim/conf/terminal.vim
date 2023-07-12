" Load terminal colorscheme values
runtime conf/terminal_colors.vim

function! InitializeTerm()
    " Unlist terminals so we don't hit them with bprev / bnext
    silent set nobuflisted
    " Automatically enter terminal mode when focusing the terminal window
    autocmd BufEnter <buffer> sil! exec 'norm i | redraw!'
endfunc

" If we're in Neovim, redirect to Neovim-specific setup.
if has('nvim')
    runtime conf/nvim_terminal.vim
    finish
endif

command! -nargs=* -complete=file Terminal terminal <args>
command! -nargs=* -complete=file TTerminal terminal <args>

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
    autocmd TerminalOpen * call InitializeTerm()
augroup END
