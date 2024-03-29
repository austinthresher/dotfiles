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

" Let Ctrl+W swap windows / etc
tnoremap <C-W> <C-\><C-N><C-W>

" Keep a list of hidden terminals that were still running
let s:hidden = get(s:, 'hidden', [])

function! OnTermHidden()
    let l:bn = expand('<abuf>')
    let s:hidden += [l:bn]
endfunc

function! TermExitCodeStatusline()
    if !getbufvar(bufnr('%'), 'exited', v:false)
        return ''
    endif
    let exit_code = getbufvar(bufnr('%'), 'exit_code', -1)
    if exit_code == 0
        return '%#ExitOk#finished%* '
    endif
    return '%#ExitError#exited ' .. exit_code .. '%* '
endfunc

function! s:NInitializeTerm()
    call InitializeTerm()
    silent setlocal winhl=Normal:Terminal nonumber winfixheight
    let b:term_title = substitute(b:term_title,
                \ 'term://\(.*\)//[0-9]*:\(.*\)', '\1$ \2', '')
    autocmd BufHidden <buffer> call OnTermHidden()
    redraw!
    silent resize 20
endfunc

function! SaneTerm(args)
    " Start terminals in a new split instead of taking over the window
    silent exec 'new | startinsert | terminal '.a:args
    " Set a flag to auto-close interactive terminals but not one-off commands
    let b:autoclose = (len(trim(a:args)) == 0)
endfunc

function! AutoCloseSaneTerm(args)
    call SaneTerm(args)
    let b:autoclose = v:true
endfunc

command! -nargs=* -complete=file Terminal call SaneTerm('<args>')
command! -nargs=* -complete=file TTerminal call SaneTerm('<args>')

" This is so dumb but it effectively replaces the built-in command
function! s:RemapTerminal()
    if !exists("*CmdAlias") | return | endif
    for i in range(1, len('terminal'))
        call CmdAlias('terminal'[:i], 'Terminal')
    endfor
endfunc

" Called when the job in a terminal finishes, despite misleading the event name
function! s:OnTermClose(status)
    let b:exit_code = a:status
    let b:exited = v:true
    if exists('b:autoclose') && b:autoclose
        sil exec 'bd! ' .. expand('<abuf>')
    elseif a:status == 0
        " Scroll up to hide the [Process exited 0] message that wastes space
        " For some reason bufwinid always returns -1 but win_findbuf works
        if win_gotoid(win_findbuf(expand('<abuf>'))[0])
            " win_execute doesn't play nicely with :norm in terminal windows
            call feedkeys("\<C-y>", "x")
            wincmd p
        endif
    endif
    redraw!
endfunc

" These functions are used to make terminal windows unclosable
" unless forced or the job has exited
function! s:ReopenTerms()
    try
        for bn in s:hidden
            if !getbufvar(bn, 'exited', v:false)
                exec 'sb ' .. bn
            endif
        endfor
        let s:hidden = []
    catch | endtry
endfunc


" Most of the above functions need to be called on certain autocommands
augroup NeovimTerminalAG
    autocmd!
    " CmdAlias won't exist until VimEnter fires, so we delay the remapping
    autocmd VimEnter * call s:RemapTerminal()
    " Terminal FG / BG are set separately from colors 0-15
    autocmd ColorScheme *
                \ exec 'hi Terminal guifg=' .. g:term_fg .. ' guibg=' .. g:term_bg |
                \ exec 'hi ExitOK guifg=#22CC22 guibg=#262626' |
                \ exec 'hi ExitError guifg=#CC2222 guibg=#262626'
    autocmd CursorHold * call s:ReopenTerms()
    " Terminal-specific settings
    autocmd TermOpen * call s:NInitializeTerm()
    " Autoclose a terminal buffer on process exit if its autoclose flag is set
    autocmd TermClose * call s:OnTermClose(v:event.status)
augroup END
