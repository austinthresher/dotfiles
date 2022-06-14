" Show and jump to quickfix, close quickfix when in the list
function! ToggleQuickfix()
    if &buftype ==# 'quickfix'
        wincmd p
        exec 'cclose'
    else
        exec 'copen ' .. g:quickfix_window_height
        set nowrap nobuflisted
        stopinsert
        " NOTE: Not sure if I want this, jumps to the first line with 'error'
        "norm gg
        "silent! exec '/\<error\>'
    endif
endfunction

nnoremap <silent> Q :call ToggleQuickfix()<cr>

let s:type = {'e': 'error', 'w': 'warning', 'i': 'info', 'n': 'note'}
function! QuickFixTextFunc(info) abort
    if a:info.quickfix
        let src = getqflist(#{id: a:info.id, items: 0}).items
    else
        let src = getloclist(a:info.winid, #{id: a:info.id, items: 0}).items
    endif
    let dst = []
    for i in range(a:info.start_idx-1, a:info.end_idx-1)
        let e = src[i]
        if !e.valid
            call add(dst, e.text)
        else
            let loc = '<' .. fnamemodify(bufname(e.bufnr), ':~:.')
            if e.lnum != 0 | let loc .= ':' .. e.lnum | endif
            if e.col != 0 | let loc .= ':' .. e.col | endif
            let loc .= '>'
            let line = loc .. ' ' .. get(s:type, tolower(e.type), 'other') .. ': ' .. e.text
            call add(dst, line)
        endif
    endfor
    return dst
endfunc

set quickfixtextfunc=QuickFixTextFunc
