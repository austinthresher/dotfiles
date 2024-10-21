let s:building       = get(s:, 'building', v:false)
let s:build_cmd      = get(s:, 'build_cmd', '')
let s:post_build_cmd = get(s:, 'post_build_cmd', '')
let s:tempfile       = get(s:, 'tempfile', '')
let s:build_buf      = get(s:, 'build_buf', 0)
let s:post_build_buf = get(s:, 'post_build_buf', 0)
if !exists('g:build_window_height')
    let g:build_window_height = 13
endif

" Returns a nested dictionary where the outer keys are the x coordinate of
" each vsplit and the inner keys are the y coordinate of each window inside
" that column. No guarantee is made about the order of entries.
function! s:GetWindowLayout() abort
    let l:wincount = winnr('$')
    let l:layout = {}
    for i in range(1, l:wincount)
        let l:pos = win_screenpos(i)
        let l:row = l:pos[0]
        let l:col = l:pos[1]
        let l:in_col = get(l:layout, l:col, {})
        let l:in_col[l:row] = i
        let l:layout[l:col] = l:in_col
    endfor
    return l:layout
endfunc

" Selects bottom right window, making splits if one doesn't
" already exist.
function! s:BottomRightWindow() abort
    let l:layout = s:GetWindowLayout()
    let l:xs = sort(map(keys(l:layout), {_, x -> str2nr(x)}))
    let l:curwin = winnr()
    " Add a vsplit if the display is wide enough
    if len(l:xs) < 2 && &columns >= 140
        wincmd v
        wincmd l
        wincmd s
        redraw!
        exec 'resize ' .. g:build_window_height
        setlocal winfixheight
        " maintain previous window
        exec l:curwin .. "wincmd w"
        wincmd p
        return
    endif
    let l:rightmost = l:layout[l:xs[-1]]
    let l:ysplits = len(keys(l:rightmost))
    exec len(l:xs) .. "wincmd l"
    " Add the hsplit if there isn't one already
    if l:ysplits < 2
        wincmd s
        redraw!
        exec 'resize ' .. g:build_window_height
        setlocal winfixheight
        exec l:curwin .. "wincmd w"
        wincmd p
        return
    endif
    exec l:ysplits .. "wincmd j"
    exec l:curwin .. "wincmd w"
    wincmd p
endfunc

" https://vi.stackexchange.com/questions/21401/load-quickfixlist-into-a-specific-window
function! s:ReplaceWindowWithQuickfix() abort
    cclose
    copen
    let l:bufn = bufnr('%')
    let l:winn = winnr()
    wincmd p
    exec 'b' .. l:bufn
    exec l:winn .. 'close'
endfunc

function! s:NewBuildWindow() abort
    if s:build_buf != 0
        let l:existing = bufwinnr(s:build_buf)
        if l:existing != -1
            exec l:existing .. 'wincmd w'
            setlocal nomodified
            return
        endif
    endif
    call s:BottomRightWindow()
    if s:build_buf == 0
        enew
        let s:build_buf = bufnr('%')
    else
        exec 'b' .. s:build_buf
    endif
    setlocal nomodified
endfunc

function! s:NewPostBuildWindow() abort
    if s:post_build_buf != 0
        let l:existing = bufwinnr(s:post_build_buf)
        if l:existing != -1
            exec l:existing .. 'wincmd w'
            setlocal nomodified
            return
        endif
    endif
    call s:BottomRightWindow()
    if s:post_build_buf == 0
        enew
        let s:post_build_buf = bufnr('%')
    else
        exec 'b' .. s:post_build_buf
    endif
    setlocal nomodified
endfunc

function! s:ShowQuickfix() abort
    call s:BottomRightWindow()
    call s:ReplaceWindowWithQuickfix()
endfunc

function! s:PostPostBuild(job_id, data, event) dict
    call setbufvar(s:post_build_buf, 'exited', v:true)
    call setbufvar(s:post_build_buf, 'exit_code', a:data)
endfunc

function! s:PostBuild(job_id, data, event) dict
    let s:building = v:false
    call setbufvar(s:build_buf, 'exited', v:true)
    call setbufvar(s:build_buf, 'exit_code', a:data)
    " Populate the quickfix list with the output, stripped of color codes
    let l:remove_ansi = 'sed -r ''s/\x1b[(]?\[?[0-9;]*[A-Za-z]//g'''
    cgetexpr system('cat ' .. s:tempfile .. ' | ' .. l:remove_ansi)
    call setqflist([], 'a', {'title': s:build_cmd})
    call setbufvar(s:build_buf, 'term_title', getbufvar(s:build_buf, 'term_title', ''))
    " If the build failed, open the quickfix window automatically
    " Otherwise, run the post-build command
    if a:data != 0
        if len(getqflist()) > 0
            call s:ShowQuickfix()
            silent exec "norm z\<cr>"
            wincmd p
        endif
    elseif s:post_build_cmd != ''
        call s:NewPostBuildWindow()
        call setbufvar(s:post_build_buf, 'exited', v:false)
        call termopen(s:post_build_cmd, {'on_exit': function('s:PostPostBuild')})
        call setbufvar(s:post_build_buf, 'term_title', 'post build: ' .. s:post_build_cmd)
        wincmd p
    endif
    call delete(s:tempfile)
    let s:tempfile = ''
endfunc

" Reuses the existing build command if it exists
function! Build(cmd='', force_prompt=v:false) abort
    if s:building
        echom 'Cannot start build before previous build is finished'
        return
    endif
    if a:force_prompt || (s:build_cmd == '' && a:cmd == '')
        let s:build_cmd = input('Build command:', s:build_cmd, 'shellcmd')
        let s:post_build_cmd = input('Post-build command:', s:post_build_cmd, 'shellcmd')
        redraw!
        echo ''
    else
        if a:cmd != ''
            let s:build_cmd = a:cmd
        endif
    endif
    let l:actual_cmd = s:build_cmd
    if l:actual_cmd == ''
        let l:actual_cmd = 'true'
    endif
    call s:NewBuildWindow()
    call setbufvar(s:build_buf, 'exited', v:false)
    let s:tempfile = tempname()
    " Run the command and write output to tempfile for quickfix parsing later
    let l:cmd = '{ ' .. l:actual_cmd .. ' ; } |& tee '
                \ .. s:tempfile .. ' && exit ${PIPESTATUS[0]}'
    call termopen(l:cmd, { 'on_exit': function('s:PostBuild') })
    call setbufvar(s:build_buf, 'term_title', 'build: ' .. s:build_cmd)
    norm G
    wincmd p
    let s:building = v:true
endfunc



command! -nargs=* -complete=file Build call Build('<args>', v:false)
command! -nargs=* -complete=file PostBuild let s:post_build_cmd='<args>'

nnoremap <F5> <Cmd>call Build()<CR>
nnoremap <F6> <Cmd>call Build('', v:true)<CR>
