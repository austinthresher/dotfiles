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
set listchars=tab:▹▹▷,extends:»,precedes:«,nbsp:∙,trail:❚
set list
set matchtime=1
set noerrorbells
set path+=**
set ruler
set scrolloff=0
set shiftwidth=4
set showbreak=═▶
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
set mousemodel=extend
set foldmethod=marker
set tags+=.tags
set equalalways
set noshowmode
set number
set cursorline
set cursorlineopt=number

" Set this in an AutoCmd so that filetype-specific configs can't overwrite it
augroup NoNewlineComments
    au!
    au FileType * setlocal formatoptions-=cro
augroup END

if has('nvim')
    set inccommand=nosplit
else
    "set timeout ttimeout ttimeoutlen=100 timeoutlen=100
endif

" Ignore files when searching
set wildignore+=*.pyc,*.egg-info/,*__pycache__/,tags,.tags,*.o,*.a,*.lib,*.dll,*.exe

" Highlight jsonc comments in any json file
augroup JsonToJsonc
    au!
    au FileType json set filetype=jsonc
augroup END

" Make it easier to write errorformat so we don't have to escape spaces
set efm=
function! EFM(pattern)
    let &efm .= ',' .. a:pattern
endfunc

" Match a sequence of spaces
let s:wh = ' %#'
" Equivalent to .*
let s:any = '%.%#'

" errorformat for GCC
call EFM('%-GIn file included from' .. s:any)
call EFM('%-G' .. s:wh .. 'from' .. s:any)
call EFM('%-G%f: In ' .. s:any .. 'function' .. s:any)
call EFM('%-G%f: In ' .. s:any .. 'ctor' .. s:any)
call EFM('%-G%f: At global scope:')
call EFM('%W%f:%l:%c: warning: %m')
call EFM('%W%f:%l: warning: %m')
call EFM('%N%f:%l:%c: note: %m')
call EFM('%E%f:%l:%c: error: %m')
call EFM('%E%f:%l: error: %m')
call EFM('%C' .. s:wh .. '%l |' .. s:any)
call EFM('%Z' .. s:wh .. '|' .. s:any .. '^' .. s:any)
call EFM('%Z' .. s:wh .. '+++ |' .. s:any)
call EFM('%-G' .. s:wh .. '%l |' .. s:any)
call EFM('%-G' .. s:wh .. '|' .. s:any)
call EFM('%-G......')

call EFM('%f:%l:%c:%m') " ripgrep with --vimgrep flag

" Used in some of my other configs to open a terminal or quickfix on the
" bottom of the screen
let g:quickfix_window_height = 7
