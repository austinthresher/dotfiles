" Snap view to left if line length is <= win width
function! ViewSnap()
    let l:view = winsaveview()
    let l:left = l:view['leftcol']
    let l:col = l:view['col']
    let l:linelen = strwidth(getline('.'))
    let l:winlen = winwidth(0)
    if l:left > 0 && l:linelen < l:winlen
        let l:view['leftcol'] = 0
        call winrestview(l:view)
    endif
endfunction

augroup Snap
    autocmd!
    autocmd CursorMoved,CursorMovedI * call ViewSnap()
augroup END
