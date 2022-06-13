if exists('g:started_by_firenvim')
    set laststatus=0
    set guifont=JetBrains\ Mono\ NL:h10
    nnoremap <esc> :wq<cr>
    nnoremap <s-cr> :wq<cr>
    nnoremap <c-cr> :wq<cr>
    inoremap <s-cr> <esc>:wq<cr>
    inoremap <c-cr> <esc>:wq<cr>

    au! TextChanged * ++nested write
    au! TextChangedI * ++nested write
    let g:firenvim_config = {
            \ 'globalSettings': {
                \ 'cmdlineTimeout': 3000,
            \ }
        \ }
endif


" Don't change shell if we launched from cmd.exe
if $TERM !=# 'vtpcon'
    let &shell='"C:/Program Files/Git/usr/bin/bash.exe"'
    set shellcmdflag=-c
    set shellxquote="
    set shellslash
endif

set autoindent
set backspace=indent,eol,start
set eadirection=both
set equalalways
set expandtab
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
set foldlevel=1
set noignorecase
set termguicolors
set wildignore+=*.pyc,*.egg-info/,*__pycache__/,*.mypy_cache/,*.idea/,*.git/
set tags=./.tags,./tags,tags
set grepprg=grep
if &equalalways
    set noequalalways
endif

let g:gutentags_ctags_tagfile = '.tags'
let g:gutentags_ctags_exclude = ['*.mypy_cache*', '*.idea*', '*.git*']

let g:loaded_ruby_provider = v:false
let g:loaded_node_provider = v:false
let g:loaded_perl_provider = v:false

let g:flip_left_char = '<'
let g:flip_right_char = '>'


call plug#begin('C:/Users/athre/AppData/Local/nvim/plugged')
    Plug 'austinthresher/vim-flip'
    Plug 'austinthresher/vim-lyra'
    Plug 'google/vim-searchindex'
    Plug 'jonhiggs/vim-readline'
    Plug 'justinmk/vim-dirvish'
    Plug 'ludovicchabant/vim-gutentags'
    Plug 'tpope/vim-eunuch'
    Plug 'tpope/vim-unimpaired'
    Plug 'tpope/vim-fugitive'
    Plug 'vim-scripts/cmdalias.vim'
    Plug 'nvim-lualine/lualine.nvim'
    Plug 'farmergreg/vim-lastplace'
    Plug 'AndrewRadev/switch.vim'
call plug#end()

"set completeopt=menu,menuone,noselect

lua <<EOF
  require('lualine').setup {
    options = {
      icons_enabled = false,
      theme = 'codedark',
      component_separators = '',
      section_separators = '',
      disabled_filetypes = {},
      always_divide_middle = true,
      globalstatus = false,
    },
    sections = {
      lualine_a = {'mode'},
      lualine_b = {'branch', 'diff', 'diagnostics'},
      lualine_c = { { 'filename', path = 1 } },
      lualine_x = {'encoding', 'fileformat', 'filetype'},
      lualine_y = {'progress'},
      lualine_z = {'location'}
    },
    inactive_sections = {
      lualine_a = {},
      lualine_b = {},
      lualine_c = { { 'filename', path = 1 } },
      lualine_x = {'location'},
      lualine_y = {},
      lualine_z = {}
    },
    tabline = {},
    extensions = {}
  }
EOF

" Use tab and shift-tab to indent lines
nnoremap <tab> >>
nnoremap <s-tab> <<
xnoremap <tab> >
xnoremap <s-tab> <

" Sets word under cursor to search term but doesn't go to next match
nnoremap * *N

" Quick reload vimrc / init.vim
nnoremap <leader>R :source $MYVIMRC<cr> :source $LOCALAPPDATA/nvim/ginit.vim<cr>

" Delete buffer while keeping window open
nnoremap <leader>bd <Cmd>set nobuflisted\|bp\|bd #<cr>

" Snap view to left if line length <= window width
function! ViewSnap()
    let l:view = winsaveview()
    let l:left = l:view['leftcol']
    let l:col = l:view['col']
    let l:linelen = strwidth(getline('.'))
    let l:winlen = winwidth(0)
    if l:left > 0 && l:linelen < l:winlen
        let l:view['leftcol'] = 0
        call winrestview(l:view)
    endif
endfunction
augroup Snap
    autocmd!
    autocmd CursorMoved,CursorMovedI * call ViewSnap()
augroup END

" Navigate out of terminal mode more easily
tnoremap <esc><esc> <c-\><c-n>

" Alt navigation out of any mode
tnoremap <A-h> <C-\><C-N><C-w>h
tnoremap <A-j> <C-\><C-N><C-w>j
tnoremap <A-k> <C-\><C-N><C-w>k
tnoremap <A-l> <C-\><C-N><C-w>l
inoremap <A-h> <C-\><C-N><C-w>h
inoremap <A-j> <C-\><C-N><C-w>j
inoremap <A-k> <C-\><C-N><C-w>k
inoremap <A-l> <C-\><C-N><C-w>l
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l

" Because EOF isn't the same on a lot of Windows programs,
" this sends both Ctrl+D and Ctrl+Z Return
tnoremap <C-d> <C-d><BS><C-z><CR>

" Terminal colorscheme (Gruvbox Dark)
let s:term_fg        = '#ebdbb2'
let s:term_bg        = '#1d2021'
let s:term_black     = '#282828'
let s:term_red       = '#cc241d'
let s:term_green     = '#98971a'
let s:term_yellow    = '#d79921'
let s:term_blue      = '#458588'
let s:term_purple    = '#b16286'
let s:term_cyan      = '#689d6a'
let s:term_white     = '#a89984'
let s:term_br_black  = '#928374'
let s:term_br_red    = '#fb4934'
let s:term_br_green  = '#b8bb26'
let s:term_br_yellow = '#fabd2f'
let s:term_br_blue   = '#83a598'
let s:term_br_purple = '#d3869b'
let s:term_br_cyan   = '#8ec07c'
let s:term_br_white  = '#fbf1c7'

let g:terminal_color_0 = s:term_black
let g:terminal_color_1 = s:term_red
let g:terminal_color_2 = s:term_green
let g:terminal_color_3 = s:term_yellow
let g:terminal_color_4 = s:term_blue
let g:terminal_color_5 = s:term_purple
let g:terminal_color_6 = s:term_cyan
let g:terminal_color_7 = s:term_white
let g:terminal_color_8 = s:term_br_black
let g:terminal_color_9 = s:term_br_red
let g:terminal_color_10 = s:term_br_green
let g:terminal_color_11 = s:term_br_yellow
let g:terminal_color_12 = s:term_br_blue
let g:terminal_color_13 = s:term_br_purple
let g:terminal_color_14 = s:term_br_cyan
let g:terminal_color_15 = s:term_br_white


function! SaneTerm(args, prefix='')
    " Start terminals in a new split instead of taking over the window
    let l:cmd = a:prefix .. ' new | startinsert | terminal ' .. a:args
    echomsg l:cmd
    exec l:cmd
    " Automatically close interactive terminals but not one-off commands
    let b:autoclose = (len(trim(a:args)) == 0)
    let b:quickfix = v:false
endfunc
command! -nargs=* -complete=file Terminal call SaneTerm('<args>')
function! RemapTerminal()
    " This is so dumb but it effectively replaces the built-in command
    if exists("*CmdAlias")
        for i in range(1, len('terminal'))
            call CmdAlias('terminal'[:i], 'Terminal')
        endfor
    endif
endfunc
augroup RemapTermOnStartup
    autocmd!
    autocmd VimEnter * call RemapTerminal()
augroup END


" Automatically enter insert mode when selecting terminal window
augroup TerminalAuto
    autocmd!
    " Terminal FG / BG are set separately from colors 0-15
    autocmd ColorScheme *
                \ exec 'hi Terminal guifg='.s:term_fg.' guibg='.s:term_bg
    " Automatically enter insert mode when selecting terminal window
    autocmd BufEnter *
                \ if &buftype ==# 'terminal' |
                \ exec 'norm i' | exec 'redraw!' |
                \ endif
    autocmd TermOpen *
                \ setlocal 
                \ winhl=Normal:Terminal
                \ statusline=%{b:term_title}
                \ nobuflisted 
    " Autoclose buffer if we set the flag above
    autocmd TermClose * 
                \ try | if b:autoclose |
                \ sil exec 'bdelete! '.expand('<abuf>') |
                \ endif | catch | endtry
augroup END

" Show and jump to quickfix, close quickfix when in the list
function! ToggleQuickfix()
    if &buftype ==# 'quickfix'
        wincmd p
        exec 'cclose'
    else
        exec 'bot copen 5'
        silent! exec '/\<error\>'
    endif
endfunction
nnoremap <silent> Q :call ToggleQuickfix()<cr>

function! WriteAndRun()
    exec 'w'
    let l:cmd = ''
    if exists('b:run_on_save')
        let l:cmd = b:run_on_save
    elseif exists('t:run_on_save')
        let l:cmd = t:run_on_save
    endif
    if l:cmd != ''
        exec '5sp term://echo ' . l:cmd ' && ' . l:cmd
        exec 'wincmd p'
        exec 'stopinsert'
    endif
endfunc

command! -bar -nargs=0 W call WriteAndRun()
nnoremap <leader>! :!!<cr>

" Async C++ builds {{{
let s:building = v:false
let s:build_buf = 0

function! NewBuildWindow(wintype='new')
    cclose
    if s:build_buf > 0 && bufwinnr(s:build_buf) != -1
        exec 'bd' .. s:build_buf
    endif
    exec 'bot5 ' .. a:wintype
    set winfixheight noswapfile nobuflisted
    let s:build_buf = bufnr('%')
endfunc

function! CppPostBuild(job_id, data, event) dict
    let s:building = v:false
    if a:data != 0
        let l:remove_ansi = 'sed -r ''s/\x1b[(]?\[?[0-9;]*[A-Za-z]//g'''
        cgetexpr system('cat ' .. s:build_tempfile .. ' | ' .. l:remove_ansi)
        call NewBuildWindow('copen')
        exec "norm z\<cr>"
        wincmd p
    elseif s:build_run_cmd != ''
        call NewBuildWindow()
        call termopen(s:build_run_cmd)
        wincmd p
    endif
endfunc

function! CppBuildAndQuickfix(run_cmd='') abort
    if s:building
        echom 'Cannot start build before previous build is finished'
        return
    endif
    try
        call NewBuildWindow()
        let l:build = './build.sh'
        let s:build_tempfile = tempname()
        let s:build_run_cmd = a:run_cmd
        let l:cmd = 'set -o pipefail; ' .. l:build .. ' | tee ' .. s:build_tempfile
        call termopen(l:cmd, {
                    \ 'on_exit': 'CppPostBuild',
                    \})
        norm G
        wincmd p
        let s:building = v:true
    catch
        bd
        redraw
        echom 'Error starting build process: ' .. v:exception
    endtry
endfunc
" }}}

augroup FileTypeMappings
    autocmd!
    autocmd FileType python nnoremap <leader>F <Cmd>!black %<cr>
    " Very project specific, but for my needs it works
    autocmd FileType cpp nnoremap <F5> <Cmd>:call CppBuildAndQuickfix('./*.exe')<cr>
    autocmd FileType cpp nnoremap <F8> <Cmd>:call CppBuildAndQuickfix()<cr>
augroup END

try
    let g:lyra_use_system_colors = v:false
    let g:lyra_transparent = v:false
    let g:lyra_no_highlighting = v:false
    let g:lyra_dim_inactive = v:false
    colorscheme lyra
    syntax on
catch
    colorscheme darkblue
endtry

if exists('g:nvui')
    source $LOCALAPPDATA/nvim/ginit.vim
endif
