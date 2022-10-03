" Plugins and related configuration

" Auto-install vim-plug if it's missing
let autoload = has('nvim') ? stdpath('config') .. '/autoload/' : '~/.vim/autoload/'
if empty(glob(autoload .. 'plug.vim'))
    exec '!wget -nc -q https://github.com/junegunn/vim-plug/raw/master/plug.vim -P ' .. autoload
    autocmd VimEnter * PlugInstall --sync | wincmd q | source $MYVIMRC | echo "Plugins installed!"
endif

" Disable netrw, dirvish is way nicer
let g:loaded_netrw = v:true
let g:loaded_netrwPlugin = v:true

let g:loaded_python_provider = v:false
let g:loaded_ruby_provider = v:false
let g:loaded_node_provider = v:false

let g:lyra_use_system_colors = v:false
let g:lyra_transparent = v:true
let g:lyra_no_highlighting = v:false
let g:lyra_dim_inactive = v:false

let g:gutentags_ctags_tagfile = '.tags'

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
    Plug 'vim-scripts/cmdalias.vim'
    Plug 'ludovicchabant/vim-gutentags'
    if has('nvim') | runtime conf/nvim_plugins.vim | endif
call plug#end()

