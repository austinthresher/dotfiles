let $MYGVIMRC = expand('<sfile>:p')
if exists('g:started_by_firenvim')
    finish
endif

let g:font_name = "CamingoCode"

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

"FVim
if exists('g:fvim_loaded')
    function! ChangeTransparency()
        let alpha = max([0, min([100, input("Transparency (1-100): ")])]) / 100.0
        if alpha > 0
            if alpha == 100
                FVimBackgroundOpacity 1.0
                FVimBackgroundAltOpacity 1.0
                FVimBackgroundComposition 'none'
            else
                FVimBackgroundOpacity alpha
                FVimBackgroundAltOpacity alpha
                FVimBackgroundComposition 'transparent'
            endif
        endif
    endfunc
    noremap <leader>t <Cmd>call ChangeTransparency()<CR>
    nnoremap <A-CR> <Cmd>FVimToggleFullScreen<CR>

    FVimCustomTitleBar v:true
    FVimFontAutoSnap v:false
    FVimFontAntialias v:false
    FVimFontAutohint v:false
    FVimFontLigature v:false
    FVimFontSubpixel v:false
    FVimFontLineHeight '+1.0'
    " This doesn't work...
    FVimCursorSmoothBlink v:true
    "FVimFontNormalWeight 400

    let g:default_font_size = 14
elseif exists('g:nvui')
    NvuiCursorHideWhileTyping v:true
    NvuiFrameless v:true
    NvuiAnimationsEnabled v:true
    "NvuiScrollAnimationDuration 0
    "NvuiMoveAnimationDuration 0
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
        let &guifont=g:font_name . ':h' . g:font_size
    endif
endfunc
noremap <C-ScrollWheelUp> <Cmd>call SetFontSize(g:font_size+1)<CR>
noremap <C-ScrollWheelDown> <Cmd>call SetFontSize(g:font_size-1)<CR>
noremap <C-=> <Cmd>call SetFontSize(g:font_size+1)<CR>
noremap <C--> <Cmd>call SetFontSize(g:font_size-1)<CR>
noremap <C-0> <Cmd>call SetFontSize(g:default_font_size)<CR>

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
