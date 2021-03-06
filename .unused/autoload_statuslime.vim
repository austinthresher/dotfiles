" Copyright (c) 2020 Austin Thresher
"
" MIT License
"
" Permission is hereby granted, free of charge, to any person obtaining
" a copy of this software and associated documentation files (the
" "Software"), to deal in the Software without restriction, including
" without limitation the rights to use, copy, modify, merge, publish,
" distribute, sublicense, and/or sell copies of the Software, and to
" permit persons to whom the Software is furnished to do so, subject to
" the following conditions:
"
" The above copyright notice and this permission notice shall be
" included in all copies or substantial portions of the Software.
"
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
" EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
" MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
" NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
" LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
" OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
" WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

if exists('g:autoloaded_statuslime') | finish | endif
let g:autoloaded_statuslime = v:true

" Highlights
" FIXME: Prune this list to what's actually used
for m in ['Normal', 'Visual', 'Insert', 'Replace', 'Terminal', 'Command',
            \ 'Shell', 'Preview', 'Help', 'Other', 'File', 'Error',
            \ 'Right', 'Left', 'InactiveBar', 'InactiveMode', 'Row', 'Col' ]
    if hlexists('Lime'.m) == 0
        exe 'hi link Lime'.m.' StatusLine'
    endif
endfor

func! statuslime#filename() abort
    let filename = expand('%') !=# '' ? expand('%') : '[No Name]'
    let modified = &modified ? '+' : ''
    let ro = &readonly ? ' [RO]' : ''
    return '  '.filename.modified.ro.' '
endfunc

function! statuslime#row() abort
    let lchars = strlen(line('$'))
    return Pad(printf('Line %d', line('.')))
endfunction

function! statuslime#col() abort
    return Pad(printf('Col %d', virtcol('.')))
endfunction

func! Pad(str)
    return '  '.a:str.' '
endfunc

func! s:add_state(condition, hlname, text)
    exe 'setlocal statusline+=%#'.a:hlname.'#'
                \.'%{('.a:condition.')?Pad('''.a:text.'''):''''}'
endfunc

func! statuslime#left() abort
    if exists("b:statuslime_left")
        return Pad(b:statuslime_left)
    endif
    return ''
endfunc

func! statuslime#right() abort
    if exists("b:statuslime_right")
        return Pad(b:statuslime_right)
    endif
    return ''
endfunc

function! statuslime#percent() abort
    return Pad(string(round(100.0 * line('.') / line('$')))[:-3].'%')
endfunc

function! statuslime#qflen() abort
    if !empty(getloclist(winnr()))
        return Pad(len(getloclist(winnr())).' entries')
    else
        return Pad(len(getqflist()).' entries')
    endif 
endfunction

" User should define a function called SetStatusLime()
" that updates the values of g:statuslime_left and
" g:statuslime_right

func! statuslime#focused() abort
    try
        if type(SetStatusLime()) == type('')
            setlocal statusline=%{SetStatusLime()}
        else
            setlocal statusline=
        endif
    catch
        setlocal statusline=
    endtry
    if &previewwindow
        call s:add_state('v:true', 'LimePreview', 'PREVIEW')
        setlocal statusline+=%<
        setlocal statusline+=%#StatusLine#
    elseif &filetype is# 'qf'
        call s:add_state('!empty(getloclist(winnr()))', 'LimePreview', 'LOCATION')
        call s:add_state('empty(getloclist(winnr()))', 'LimePreview', 'QUICKFIX')
        setlocal statusline+=%#LimeFile#
        setlocal statusline+=%{statuslime#qflen()}
        setlocal statusline+=%<
        setlocal statusline+=%#StatusLine#
    elseif &filetype is# 'help'
        call s:add_state('v:true', 'LimeHelp', 'HELP')
        setlocal statusline+=%<
        setlocal statusline+=%#StatusLine#
    elseif &filetype is# 'nerdtree'
        call s:add_state('v:true', 'LimeUtil', 'NERDTREE')
        setlocal statusline+=%<
        setlocal statusline+=%#StatusLine#
    elseif &filetype is# 'minibufexpl'
        call s:add_state('v:true', 'LimeUtil', 'BUFFERS')
        setlocal statusline+=%<
        setlocal statusline+=%#StatusLine#
    else
        call s:add_state('mode()[0]==#''n''', 'LimeNormal', 'NORMAL')
        call s:add_state('mode()[0]==#''i''', 'LimeInsert', 'INSERT')
        call s:add_state('mode()[0]==#''R''', 'LimeReplace', 'REPLACE')
        call s:add_state('mode()[0]==#''t''', 'LimeTerminal', 'TERMINAL')
        call s:add_state('mode()[0]==#''v''', 'LimeVisual', 'VISUAL')
        call s:add_state('mode()[0]==#''V''', 'LimeVisual', 'VISUAL-LINE')
        call s:add_state('char2nr(mode()[0])==0x16', 'LimeVisual', 'VISUAL-BLOCK')
        call s:add_state('mode()[0]==#''c''', 'LimeCommand', 'COMMAND')
        call s:add_state('mode()[0]==#''r''', 'LimeOther', 'CONTINUE')
        call s:add_state('mode()[0]==#''!''', 'LimeOther', 'SHELL')
        call s:add_state('mode()==#''no''', 'LimeNormal', 'PENDING')
        setlocal statusline+=%#LimeFile#%{statuslime#filename()}
        setlocal statusline+=%#StatusLine#
        setlocal statusline+=%<
        setlocal statusline+=%#LimeLeft#%{statuslime#left()}
        setlocal statusline+=%#StatusLine#
        setlocal statusline+=%=
        setlocal statusline+=%#LimeRight#%{statuslime#right()}
        setlocal statusline+=%#LimeCol#%{statuslime#col()}
        setlocal statusline+=%#LimeRow#%{statuslime#row()}
        setlocal statusline+=%#LimeCol#%{statuslime#percent()}
    endif
endfunc

func! statuslime#unfocused()
    try
        if type(SetStatusLime()) == type('')
            setlocal statusline=%{SetStatusLime()}
        else
            setlocal statusline=
        endif
    catch
        setlocal statusline=
    endtry
    if &previewwindow
        call s:add_state('v:true', 'LimeInactiveFT', 'PREVIEW')
        setlocal statusline+=%<
        setlocal statusline+=%*
    elseif &filetype is# 'qf'
        call s:add_state('!empty(getloclist(winnr()))', 'LimeInactiveFT', 'LOCATION')
        call s:add_state('empty(getloclist(winnr()))', 'LimeInactiveFT', 'QUICKFIX')
        setlocal statusline+=%#LimeInactiveBar#
        setlocal statusline+=%{statuslime#qflen()}
        setlocal statusline+=%<
        setlocal statusline+=%#StatusLine#
    elseif &filetype is# 'help'
        call s:add_state('v:true', 'LimeInactiveFT', 'HELP')
        setlocal statusline+=%<
        setlocal statusline+=%#StatusLine#
    elseif &filetype is# 'minibufexpl'
        call s:add_state('v:true', 'LimeInactiveUtil', 'BUFFERS')
        setlocal statusline+=%<
        setlocal statusline+=%#StatusLine#
    elseif &filetype is# 'nerdtree'
        call s:add_state('v:true', 'LimeInactiveUtil', 'NERDTREE')
        setlocal statusline+=%<
        setlocal statusline+=%#StatusLine#
    else
        call s:add_state('&buftype!=#''terminal''', 'LimeInactiveMode', 'NORMAL')
        call s:add_state('&buftype==#''terminal''', 'LimeInactiveMode', 'TERMINAL')
        setlocal statusline+=%#LimeInactiveBar#%{statuslime#filename()}
        setlocal statusline+=%<
        setlocal statusline+=%#StatusLine#
        setlocal statusline+=%#LimeInactiveBar#
        setlocal statusline+=%=
        setlocal statusline+=%#LimeInactiveBar#%{statuslime#col()}
        setlocal statusline+=%#LimeInactiveBar#%{statuslime#row()}
        setlocal statusline+=%#LimeCol#%{statuslime#percent()}
    endif
endfunc
