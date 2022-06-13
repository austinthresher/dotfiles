let $MYGVIMRC = expand('<sfile>:p')

let g:font_name = "CamingoCode"

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

"nvim-qt
if exists(':GuiAdaptiveColor')
    GuiAdaptiveColor 1
endif
if exists(':GuiAdaptiveFont')
    GuiAdaptiveFont 1
endif
if exists(':GuiAdaptiveStyle')
    GuiAdaptiveStyle Fusion
endif
if exists(':GuiRenderLigatures')
    GuiRenderLigatures 0
endif

if exists('g:nvui')
    NvuiCursorHideWhileTyping v:true
    NvuiFrameless v:true
    NvuiAnimationsEnabled v:true
    NvuiCursorAnimationDuration 0.05
    NvuiTitlebarColors #eeeeee #262626

    function! ChangeTransparency()
        let alpha = max([20, min([100, input("Transparency (1-100): ")])]) / 100.0
        NvuiOpacity alpha
    endfunc
    noremap <leader>t <Cmd>call ChangeTransparency()<CR>
    nnoremap <A-CR> <Cmd>NvuiToggleFullscreen<CR>

    let g:default_font_size = 11
else
    let g:default_font_size = 12
endif

" Set the font and allow Ctrl +/- and Ctrl Scrollwheel resizing
let g:font_size = g:default_font_size
function! SetFontSize(size)
    let g:font_size = a:size
    if exists(':GuiFont')
        GuiFont! {g:font_name}:h{g:font_size}
    else
        if has('gui_gtk') | let l:sep = ' ' | else | let l:sep = ':h' | endif
        let &guifont=g:font_name .. l:sep .. g:font_size
    endif
    redraw!
endfunc
noremap <C-ScrollWheelUp> :call SetFontSize(g:font_size+1)<CR>
noremap <C-ScrollWheelDown> :call SetFontSize(g:font_size-1)<CR>
noremap <C-=> :call SetFontSize(g:font_size+1)<CR>
noremap <C--> :call SetFontSize(g:font_size-1)<CR>
noremap <C-0> :call SetFontSize(g:default_font_size)<CR>
" GVim doesn't like Ctrl+ any of the above
noremap <leader>fi :call SetFontSize(g:font_size+1)<CR>
noremap <leader>fo :call SetFontSize(g:font_size-1)<CR>
noremap <leader>f0 :call SetFontSize(g:default_font_size)<CR>

call SetFontSize(g:default_font_size)

" Set the window title to the filename
set title
function! SetWindowTitle(bufcount_offset)
    let l:tabs = gettabinfo()
    " This glitched out with multiple tabs, which I don't really use, so
    " just pause title updates while another tab is open
    if len(l:tabs) == 1
        let l:nbuf = len(getbufinfo({'buflisted':1,'bufloaded':1})) + a:bufcount_offset
        if l:nbuf > 1
            let &titlestring = '[' . l:nbuf . '] ' . expand("%:t")
        else
            let &titlestring = expand("%:t")
        endif
    endif
endfunc
autocmd! BufDelete * call SetWindowTitle(-1)
autocmd! BufEnter * call SetWindowTitle(0)
