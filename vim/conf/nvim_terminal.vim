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

" This is a bunch of ugly hacks to make Neovim's terminal
" behave in a way that I find easier to work with

function! SaneTerm(args)
    " Start terminals in a new split instead of taking over the window
    exec 'new | startinsert | terminal '.a:args
    " Set a flag to auto-close interactive terminals but not one-off commands
    let b:autoclose = (len(trim(a:args)) == 0)
endfunc

command! -nargs=* -complete=file Terminal call SaneTerm('<args>')

" This is so dumb but it effectively replaces the built-in command with ours
function! RemapTerminal()
    if !exists("*CmdAlias") | return | endif
    for i in range(1, len('terminal'))
        call CmdAlias('terminal'[:i], 'Terminal')
    endfor
endfunc

" These functions are used to make terminal windows unclosable
" unless forced or the job has exited
function! OnTermWinClosed()
    let l:bn = winbufnr(expand('<afile>'))
    " As soon as a terminal window is hidden, we record the buffer number.
    " This won't work if multiple terminals are hidden at once, but for most
    " uses it's fine.
    if getbufvar(l:bn, '&buftype') ==# 'terminal'
        exec "sb"..l:bn
        if exists('g:hidden_term')
            unlet g:hidden_term
        endif
    endif
endfunc

func! ReopenHiddenTerm()
    " If a terminal was just hidden, we re-open it. This won't work if
    " multiple terminals are hidden at once, but for most uses it's fine
    if exists('g:hidden_term') | try
       exec "sb"..g:hidden_term
       wincmd p
       unlet g:hidden_term
    catch | endtry | endif
endfunc

" Most of the above functions need to be called on certain autocommands
augroup NeovimTerminalAG
    autocmd!
    " CmdAlias won't exist until VimEnter fires, so we delay the remapping
    autocmd VimEnter * call RemapTerminal()
    " Terminal FG / BG are set separately from colors 0-15
    autocmd ColorScheme *
                \ exec 'hi Terminal guifg='.g:term_fg.' guibg='.g:term_bg
    " Automatically enter insert mode when selecting terminal window
    " The redraw fixes a cursor placemenet bug with vim-flip
    autocmd BufEnter *
                \ if &buftype ==# 'terminal' |
                \ exec 'norm i' | exec 'redraw!' |
                \ endif
    " These three autocommands are what keep active terminals visible
    autocmd BufHidden term://* let g:hidden_term=expand('<abuf>')
    autocmd WinClosed * call OnTermWinClosed()
    autocmd BufEnter * call ReopenHiddenTerm()
    " Terminal-specific settings
    autocmd TermOpen *
                \ setlocal 
                    \ winhl=Normal:Terminal
                    \ statusline=%{b:term_title}
                    \ nobuflisted 
    " Autoclose a terminal buffer on process exit if its autoclose flag is set
    autocmd TermClose * 
                \ try | if b:autoclose |
                    \ sil exec 'bdelete! '.expand('<abuf>') |
                \ endif | catch | endtry
augroup END
