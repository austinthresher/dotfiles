let $MYGVIMRC = expand('<sfile>:p')

let g:font_name = "Iosevka"

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

let g:default_font_size = 18

" Set the font and allow Ctrl +/- and Ctrl Scrollwheel resizing
let g:font_size = g:default_font_size
function! SetFontSize(size)
    let g:font_size = a:size
    if has('gui_gtk') | let l:sep = ' ' | else | let l:sep = ':h' | endif
    let &guifont=g:font_name .. l:sep .. g:font_size
    redraw!
endfunc
noremap <C-ScrollWheelUp> :call SetFontSize(g:font_size+1)<CR>
noremap <C-ScrollWheelDown> :call SetFontSize(g:font_size-1)<CR>
noremap <C-=> :call SetFontSize(g:font_size+1)<CR>
noremap <C--> :call SetFontSize(g:font_size-1)<CR>
noremap <C-0> :call SetFontSize(g:default_font_size)<CR>
" GVim doesn't like Ctrl+ any of the above
" noremap <leader>fi :call SetFontSize(g:font_size+1)<CR>
" noremap <leader>fo :call SetFontSize(g:font_size-1)<CR>
" noremap <leader>f0 :call SetFontSize(g:default_font_size)<CR>

call SetFontSize(g:default_font_size)

set title

if exists("g:neovide")
    let g:neovide_padding_left = 1
    let g:neovide_padding_right = 1
    let g:neovide_padding_top = 1
    let g:neovide_padding_bottom = 0
    let g:neovide_scroll_animation_length = 0.05
    let g:neovide_position_animation_length = 0.05
    let g:neovide_cursor_animation_length = 0.025
    let g:neovide_cursor_trail_size = 0.5
    let g:neovide_theme = 'light'
    let g:experimental_layer_grouping = v:true
    let g:neovide_cursor_antialiasing = v:true
    let g:neovide_cursor_smooth_blink = v:true
    set guicursor=n-v-c:block,i-ci-ve:ver25,r-cr:hor20,o:hor50
      \,i-ci-ve-r-cr:blinkwait10-blinkoff400-blinkon250
      \,a:Cursor/lCursor
      \,sm:block-blinkwait10-blinkoff150-blinkon175
    let g:neovide_text_gamma = 0.8
    let g:neovide_text_contrast = 0.1
    let g:neovide_floating_z_height = 5
    let g:neovide_light_radius = 2
    let g:neovide_scale_factor = 1.0
endif
