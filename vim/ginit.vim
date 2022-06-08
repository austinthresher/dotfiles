let $MYGVIMRC = expand('<sfile>')

" Automatically source $MYGVIMRC after write.
augroup AutoReloadRC
    autocmd!
    autocmd BufWritePost $MYGVIMRC ++nested 
                \ source <afile> | redraw | 
                \ echo ':source '.expand('<afile>')
augroup END

if exists("*GuiShowContextMenu")
    " Right Click Context Menu (Copy-Cut-Paste)
    nnoremap <silent><RightMouse> :call GuiShowContextMenu()<CR>
    inoremap <silent><RightMouse> <Esc>:call GuiShowContextMenu()<CR>
    xnoremap <silent><RightMouse> :call GuiShowContextMenu()<CR>gv
    snoremap <silent><RightMouse> <C-G>:call GuiShowContextMenu()<CR>gv
endif
