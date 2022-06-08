" Find and highlight trailing whitespace, based on:
" https://vim.fandom.com/wiki/Remove_unwanted_spaces
function! ShowSpaces(...)
    let @/='\v(\s+$)|( +\ze\t)'
    let oldhlsearch=&hlsearch
    if !a:0
        let &hlsearch=!&hlsearch
    else
        let &hlsearch=a:1
    endif
    return oldhlsearch
endfunction

function! TrimSpaces() range
    let oldhlsearch = ShowSpaces(1)
    execute a:firstline.",".a:lastline."substitute ///gec"
    let &hlsearch = oldhlsearch
endfunction

command! -bar -nargs=? ShowSpaces call ShowSpaces(<args>)
command! -bar -nargs=0 -range=% TrimSpaces <line1>,<line2>call TrimSpaces()
nnoremap <leader>s :ShowSpaces 1<cr>
nnoremap <leader>S m`:TrimSpaces<cr>``
vnoremap <leader>S :TrimSpaces<CR>

