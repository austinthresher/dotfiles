let s:building       = get(s:, 'building', v:false)
let s:build_cmd      = get(s:, 'build_cmd', 'make')
let s:post_build_cmd = get(s:, 'post_build_cmd', '')
let s:tempfile       = get(s:, 'tempfile', '')
let s:build_buf      = get(s:, 'build_buf', 0)

function! s:NewBuildWindow(wintype='new')
    cclose
    if s:build_buf > 0 && bufwinnr(s:build_buf) != -1
        exec 'bd' .. s:build_buf
    endif
    exec 'bot' .. g:quickfix_window_height .. ' ' .. a:wintype
    set winfixheight noswapfile nobuflisted
    let s:build_buf = bufnr('%')
endfunc

function! s:PostBuild(job_id, data, event) dict
    let s:building = v:false
    " Populate the quickfix list with the output, stripped of color codes
    let l:remove_ansi = 'sed -r ''s/\x1b[(]?\[?[0-9;]*[A-Za-z]//g'''
    cgetexpr system('cat ' .. s:tempfile .. ' | ' .. l:remove_ansi)
    call setqflist([], 'a', {'title': s:build_cmd})
    " If the build failed, open the quickfix window automatically
    " Otherwise, run the post-build command
    if a:data != 0
        call s:NewBuildWindow('copen')
        exec "norm z\<cr>"
        wincmd p
    elseif s:post_build_cmd != ''
        call s:NewBuildWindow()
        call termopen(s:post_build_cmd)
        wincmd p
    endif
    call delete(s:tempfile)
    let s:tempfile = ''
endfunc

function! Build(cmd='') abort
    if s:building
        echom 'Cannot start build before previous build is finished'
        return
    endif
    call s:NewBuildWindow()
    if a:cmd != '' 
        let s:build_cmd = a:cmd
    endif
    let s:tempfile = tempname()
    " Run the command, displaying both stdout and stderr in the terminal
    " but writing stderr to file for quickfix parsing later
    let l:cmd = '{ ' .. s:build_cmd .. ' ; } |& tee ' .. s:tempfile .. ' && exit ${PIPESTATUS[0]}'
    call termopen(l:cmd, { 'on_exit': function('s:PostBuild') })
    norm G
    wincmd p
    let s:building = v:true
endfunc

command! -nargs=* -complete=file Build call Build('<args>')
command! -nargs=* -complete=file PostBuild let s:post_build_cmd='<args>'

nnoremap <F5> <Cmd>call Build()<CR>
