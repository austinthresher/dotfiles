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

let g:gutentags_ctags_tagfile = '.tags'

let g:sneak#s_next = v:true
let g:sneak#absolute_dir = v:true

augroup PostHighlight
    autocmd!
    autocmd ColorScheme * hi Sneak ctermfg=0 ctermbg=13 cterm=bold guifg='#EEEEEE' guibg='#803080' gui=bold
augroup END


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
    Plug 'kien/rainbow_parentheses.vim'
    if has('nvim')
        runtime conf/nvim_plugins.vim
    else
        " Use CoC on nvim, ctags for vim
        Plug 'ludovicchabant/vim-gutentags'
    endif
call plug#end()

let g:rbpt_colorpairs = [
    \ ['magenta', '#8C2D2D'],
    \ ['red',     '#D3944D'],
    \ ['blue',    '#3B7EAA'],
    \ ['green',   '#608860'],
    \ ['white',   '#987CDA'],
    \ ['red',     '#1CB38B'],
    \ ['blue',    '#D37474'],
    \ ['green',   '#82CBFE'],
    \ ['white',   '#EAC871'],
    \ ['red',     '#76DE94'],
    \ ['blue',    '#DE95DE'],
    \ ['green',   '#5BEBEB'],
    \ ['white',   '#A0A0A0'],
    \ ['magenta', '#8C2D2D'],
    \ ['red',     '#D3944D'],
    \ ['blue',    '#3B7EAA'],
    \ ['green',   '#608860'],
    \ ['white',   '#987CDA'],
    \ ['green',   '#1CB38B'],
    \ ['red',     '#D37474'],
    \ ['blue',    '#82CBFE'],
    \ ['white',   '#5BEBEB'],
    \ ['green',   '#EAC871'],
    \ ['red',     '#76DE94'],
    \ ['blue',    '#DE95DE'],
    \ ['white',   '#A0A0A0']]

augroup rainbow
    autocmd!
    autocmd VimEnter * RainbowParenthesesActivate
    autocmd BufEnter * RainbowParenthesesLoadRound
augroup END
