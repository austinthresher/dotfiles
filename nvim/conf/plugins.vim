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

" Put folders at the top in dirvish
let g:dirvish_mode = ':sort ,^\v(.*[\/])|\ze,'

let g:loaded_python_provider = v:false
let g:loaded_ruby_provider = v:false
let g:loaded_node_provider = v:false

let g:lyra_use_system_colors = v:false
let g:lyra_transparent = v:false
let g:lyra_no_highlighting = v:false
let g:lyra_dim_inactive = v:false

let g:sneak#s_next = v:true
let g:sneak#absolute_dir = v:true

call plug#begin()
    Plug 'austinthresher/vim-lyra'
    Plug 'google/vim-searchindex'
    Plug 'tpope/vim-unimpaired'
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
    Plug 'luochen1990/rainbow'
    Plug 'calebsmith/vim-lambdify'
    "Plug 'jpalardy/vim-slime'
    Plug 'junegunn/vim-easy-align'
    Plug 'sheerun/vim-polyglot'
    Plug 'wellle/visual-split.vim'
    if has('nvim')
        runtime conf/nvim_plugins.vim
    endif
call plug#end()

if has("nvim")
    let g:slime_target = "neovim"
else
    let g:slime_target = "vimterminal"
endif

" Rainbow parens

let s:cterm_dark = [255, 3, 4, 5, 6, 9, 10, 11, 12, 13, 14, 15, 172, 39, 1, 2]
let s:gui_dark = [
            \ '#EEEEEE', '#CA7DBE', '#5BC8C3', '#BA4F4F', '#3EA37A', '#D0B3CB',
            \ '#B0C55E', '#97A3D3', '#baa67f', '#7B4790', '#338099', '#B3DFB6',
            \ '#B67C58', '#45B54C', '#5B7BC8', '#BDB156']
let s:cterm_light = [0, 3, 4, 5, 6]
let s:gui_light = [
            \ '#458588', '#b16286', '#cc241d', '#d65d0e', '#458588', '#b16286',
            \ '#cc241d', '#d65d0e', '#458588', '#b16286', '#cc241d', '#d65d0e',
            \ '#458588', '#b16286', '#cc241d', '#d65d0e' ]
let s:cterm_light_default = s:cterm_light
let s:gui_light_default = s:gui_light
let s:cterm_dark_default = [ 255, 193, 223, 189, 225, 183, 152, 138 ]
let s:gui_dark_default = [ '#EEEEEE', '#BAFFAA', '#ECE1C8', '#9FD3E5',
                         \ '#DEB3DF', '#B8BCD6', '#85B2AA', '#E67E80' ]
let s:c = s:cterm_dark "s:cterm_light 
let s:g = s:gui_dark "s:gui_light 
let s:cterm_default = s:cterm_dark_default "s:cterm_light_default
let s:gui_default = s:gui_dark_default "s:gui_light_default
let s:p = ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold']
let g:rainbow_conf = {
            \   'ctermfgs': s:cterm_default,
            \   'guifgs': s:gui_default,
            \   'parentheses': ['start=/{/ end=/}/ fold'],
            \   'separately': {
            \       'ls': { 'guifgs': s:g, 'ctermfgs': s:c, 'parentheses': s:p },
            \       'lisp': { 'guifgs': s:g, 'ctermfgs': s:c, 'parentheses': s:p },
            \       'scheme': { 'guifgs': s:g, 'ctermfgs': s:c, 'parentheses': s:p },
            \       'racket': { 'guifgs': s:g, 'ctermfgs': s:c, 'parentheses': s:p },
            \       'clojure': { 'guifgs': s:g, 'ctermfgs': s:c, 'parentheses': s:p },
            \       'python': 0
            \   }
            \}

let g:rainbow_active = 1

" Easy align line continuations
let g:easy_align_delimiters = {
            \ '\': { 'pattern': '\\$' },
            \ }

