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
    Plug 'luochen1990/rainbow'
    Plug 'kana/vim-altr'
    if has('nvim')
        runtime conf/nvim_plugins.vim
    endif
call plug#end()

" Rainbow parens

let s:c = [255, 3, 4, 5, 6, 9, 10, 11, 12, 13, 14, 15, 172, 39, 1, 2]
let s:g = [
            \ '#EEEEEE', '#CA7DBE', '#5BC8C3', '#BA4F4F', '#3EA37A', '#D0B3CB',
            \ '#B0C55E', '#97A3D3', '#baa67f', '#7B4790', '#338099', '#B3DFB6',
            \ '#B67C58', '#45B54C', '#5B7BC8', '#BDB156']
let s:p = ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold']

let g:rainbow_conf = {
\   'ctermfgs': [ 255, 193, 223, 189, 225, 183, 152, 138 ],
\   'guifgs': [ '#EEEEEE', '#BAFFAA', '#ECE1C8', '#9FD3E5',
\               '#DEB3DF', '#B8BCD6', '#85B2AA', '#E67E80' ],
\   'parentheses': ['start=/{/ end=/}/ fold'],
\   'separately': {
\       'ls': { 'guifgs': s:g, 'ctermfgs': s:c, 'parentheses': s:p },
\       'lisp': { 'guifgs': s:g, 'ctermfgs': s:c, 'parentheses': s:p },
\       'scheme': { 'guifgs': s:g, 'ctermfgs': s:c, 'parentheses': s:p },
\       'racket': { 'guifgs': s:g, 'ctermfgs': s:c, 'parentheses': s:p },
\       'clojure': { 'guifgs': s:g, 'ctermfgs': s:c, 'parentheses': s:p }
\   }
\}

let g:rainbow_active = 1

" altr additional patterns to find related files
call altr#define('%/%.c', '%/include/%.h')
call altr#define('%/%.cpp', '%/include/%.h')

