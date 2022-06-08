" Automatically source $MYVIMRC after a write to it or any files in conf
augroup AutoReloadRC
    autocmd!
    autocmd BufWritePost $MYVIMRC,*/conf/*.vim ++nested 
                \ source <afile> | redraw | 
                \ echo ':source '.expand('<afile>')
augroup END

runtime conf/settings.vim
runtime conf/plugins.vim
runtime conf/maps.vim

runtime conf/terminal_colors.vim
if has('nvim')
    runtime conf/nvim_terminal.vim
else
    runtime conf/terminal.vim
endif

" Mini plugins
runtime conf/viewsnap.vim
runtime conf/spaces.vim

try | colorscheme lyra | catch | colorscheme darkblue | endtry
