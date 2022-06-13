set encoding=utf-8 | scriptencoding utf-8
set termguicolors
set autoindent
set backspace=indent,eol,start
set eadirection=both
set expandtab
set fillchars=vert:│,fold:\ ,diff:-
set hidden
set hlsearch
set ignorecase
set incsearch
set lazyredraw
set listchars=eol:$,tab:>\ ,extends:>,precedes:<,nbsp:+,trail:_
set matchtime=1
set noerrorbells
set path+=**
set ruler
set scrolloff=0
set shiftwidth=4
set showbreak==>\
set showcmd
set showmatch
set sidescroll=1
set sidescrolloff=1
set smartcase
set softtabstop=4
set splitbelow
set wildmenu
set winminheight=1
set winminwidth=1
set updatetime=100
set mouse=a
set foldmethod=marker
set tags+=.tags
set equalalways

if has('nvim')
    set inccommand=nosplit
else
    "set timeout ttimeout ttimeoutlen=100 timeoutlen=100
endif

" Ignore files when searching
set wildignore+=*.pyc,*.egg-info/,*__pycache__/,tags,.tags
