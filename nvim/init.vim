" Automatically source $MYVIMRC after a write to it or any files in conf
augroup AutoReloadRC
    autocmd!
    autocmd BufWritePost $MYVIMRC,*/conf/*.vim ++nested
                \ source <afile> | redraw |
                \ echo ':source '.expand('<afile>')
augroup END

" We could use 'runtime' instead, but this makes conditionals cleaner
let s:config_dir = split(&rtp, ',')[0] .. '/conf/'
function! Include(name, condition=v:true)
    if a:condition
        let l:file = glob(s:config_dir .. a:name)
        if l:file != ''
            exec 'source ' .. s:config_dir .. a:name
        else
            echom 'Could not find ' .. a:name
        endif
    endif
endfunc

" Dummy autocommand to prevent an error if .vimrc.local doesn't add one
augroup Config
    autocmd!
    autocmd User PostConfig :
augroup END

" Source .vimrc.local if it exists for host-specific configuration
if filereadable(expand('~/.vimrc.local'))
    exec 'source ' .. expand('~/.vimrc.local')
endif

" Basic config and settings
call Include('settings.vim')
call Include('maps.vim')
call Include('windows.vim', has('win32'))

" Terminal buffer setup
call Include('terminal.vim')

" Managed plugins
call Include('plugins.vim')

" Mini plugins
call Include('viewsnap.vim')
call Include('spaces.vim')
call Include('quickfix.vim')

" Neovim-specific mini plugins and plugin config
call Include('nvim_build.vim', has('nvim'))
call Include('plugin_config.lua', has('nvim'))

"colorscheme lyra
colorscheme wildcharm
"set background=light " wildcharm supports a light version too
" Get rid of the default bold and inverse
hi StatusLine gui=NONE cterm=NONE
hi StatusLineNC gui=NONE cterm=NONE

" This autocommand is a hook for .vimrc.local to override any previous config
doautocmd <nomodeline> User PostConfig
