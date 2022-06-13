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

" Keep a list of hidden terminals that were still running
let s:hidden = get(s:, 'hidden', [])

function! OnTermHidden()
    let l:bn = expand('<abuf>')
    if getbufvar(l:bn, 'exit_code') == -1
        let s:hidden += [l:bn]
    endif
endfunc

function! NInitializeTerm()
    call InitializeTerm()
    setlocal statusline=%{b:term_title} winhl=Normal:Terminal
    " -1 indicates the job has not yet exited
    let b:exit_code = -1
    autocmd BufHidden <buffer> call OnTermHidden()
endfunc

function! SaneTerm(args)
    " Start terminals in a new split instead of taking over the window
    exec 'new | startinsert | terminal '.a:args
    " Set a flag to auto-close interactive terminals but not one-off commands
    let b:autoclose = (len(trim(a:args)) == 0)
endfunc

command! -nargs=* -complete=file Terminal call SaneTerm('<args>')

" This is so dumb but it effectively replaces the built-in command
function! RemapTerminal()
    if !exists("*CmdAlias") | return | endif
    for i in range(1, len('terminal'))
        call CmdAlias('terminal'[:i], 'Terminal')
    endfor
endfunc

" Called when the job in a terminal finishes, despite misleading the event name
function! OnTermClose(status)
    let b:exit_code = a:status
    if exists('b:autoclose') && b:autoclose
        sil exec 'bd! ' .. expand('<abuf>')
    elseif b:exit_code == 0
        " Scroll up to hide the [Process exited 0] message that wastes space
        " For some reason bufwinid always returns -1 but win_findbuf works
        if win_gotoid(win_findbuf(expand('<abuf>'))[0])
            " win_execute doesn't play nicely with :norm in terminal windows
            call feedkeys("\<C-y>", "x")
            wincmd p
        endif
    endif
endfunc

" These functions are used to make terminal windows unclosable
" unless forced or the job has exited
function! OnTermWinClosed()
    let l:bn = winbufnr(expand('<afile>'))
    " As soon as a terminal window is hidden, we record the buffer number.
    " This won't work if multiple terminals are hidden at once, but for most
    " uses it's fine.
    if getbufvar(l:bn, '&buftype') ==# 'terminal' && getbufvar(l:bn, 'exit_code') == -1
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
    " These three autocommands are what keep active terminals visible
    " FIXME: Make this play nicely with the build commands
    "autocmd WinClosed * call OnTermWinClosed()
    "autocmd BufEnter * call ReopenHiddenTerm()
    " Terminal-specific settings
    autocmd TermOpen * call NInitializeTerm()
    " Autoclose a terminal buffer on process exit if its autoclose flag is set
    autocmd TermClose * call OnTermClose(v:event.status)
augroup END
