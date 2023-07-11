" Plugins and related configuration

" Auto-install vim-plug if it's missing
let autoload = has('nvim') ? stdpath('config') .. '/autoload/' : '~/.vim/autoload/'
if empty(glob(autoload .. 'plug.vim'))
    exec '!wget -nc -q https://github.com/junegunn/vim-plug/raw/master/plug.vim -P ' .. autoload
    autocmd VimEnter * PlugInstall --sync | wincmd q | source $MYVIMRC | echo "Plugins installed!"
endif

" Disable syntax highlighting for built-ins,
" this is then customized in after/python.vim
let g:python_no_builtin_highlight = v:true

" Disable netrw, dirvish is way nicer
let g:loaded_netrw = v:true
let g:loaded_netrwPlugin = v:true

let g:loaded_python_provider = v:false
let g:loaded_ruby_provider = v:false
let g:loaded_node_provider = v:false

let g:lyra_use_system_colors = v:false
let g:lyra_transparent = v:false
let g:lyra_no_highlighting = v:false
let g:lyra_dim_inactive = v:false

let g:sneak#s_next = v:true
let g:sneak#absolute_dir = v:true

augroup PostHighlight
    autocmd!
    autocmd ColorScheme * hi Sneak ctermfg=0 ctermbg=13 cterm=bold guifg='#EEEEEE' guibg='#803080' gui=bold
augroup END


call plug#begin()
    Plug 'austinthresher/vim-lyra'
    Plug 'google/vim-searchindex'
    "Plug 'tpope/vim-unimpaired'
    Plug 'tpope/vim-eunuch'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-repeat'
    Plug 'jonhiggs/vim-readline'
    Plug 'ekalinin/dockerfile.vim'
    Plug 'lbrayner/vim-rzip'
    Plug 'justinmk/vim-dirvish'
    Plug 'justinmk/vim-syntax-extra'
    Plug 'justinmk/vim-sneak'
    Plug 'vim-scripts/cmdalias.vim'
    Plug 'junegunn/rainbow_parentheses.vim'
    if has('nvim')
        runtime conf/nvim_plugins.vim
    endif
call plug#end()


function! LispRainbows()
    let g:rainbow#pairs = [['(', ')'], ['[', ']']]
    let g:rainbow#colors = {
                \ 'dark': [
                \[ 255, "#EEEEEE" ],
                \[ 172, "#E67029" ],
                \[  39, "#24D830" ],
                \[   1, "#2266FF" ],
                \[   2, "#F0D824" ],
                \[   3, "#F058D8" ],
                \[   4, "#26FFF4" ],
                \[   5, "#F01818" ],
                \[   6, "#0AD484" ],
                \[   9, "#DFA3D3" ],
                \[  10, "#D0F92B" ],
                \[  11, "#7890F0" ],
                \[  12, "#BAA67F" ],
                \[  13, "#8B22B3" ],
                \[  14, "#0099CC" ],
                \[  15, "#9EF6A4" ],
                \]}
    RainbowParentheses
endfunction

function! PyRainbows()
    let g:rainbow#pairs = [['{', '}'], ['[', ']']]
    let g:rainbow#colors = {
                \ 'dark': [
                \[ 255, "#EEEEEE" ],
                \[ 193, "#BAFFAA" ],
                \[ 223, "#ECE1C8" ],
                \[ 189, "#9FD3E5" ],
                \[ 225, "#DEB3DF" ],
                \[ 183, "#B8BCD6" ],
                \[ 152, "#85B2AA" ],
                \[ 138, "#E67E80" ],
                \]}
    RainbowParentheses
endfunction

function! CRainbows()
    let g:rainbow#pairs = [['{', '}']]
    let g:rainbow#colors = {
                \ 'dark': [
                \[ 255, "#EEEEEE" ],
                \[ 193, "#BAFFAA" ],
                \[ 223, "#ECE1C8" ],
                \[ 189, "#9FD3E5" ],
                \[ 225, "#DEB3DF" ],
                \[ 183, "#B8BCD6" ],
                \[ 152, "#85B2AA" ],
                \[ 138, "#E67E80" ],
                \]}
    RainbowParentheses
endfunction

augroup rainbow
    autocmd!
    autocmd FileType lisp,clojure,scheme call LispRainbows()
    autocmd FileType c,cpp call CRainbows()
    autocmd FileType python call PyRainbows()
augroup END
