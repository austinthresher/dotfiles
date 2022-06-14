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

" Set colorscheme with a default fallback
try | colorscheme lyra | catch | colorscheme darkblue | endtry

" nvui ignores ginit.vim for some reason
if exists('g:nvui')
    runtime ginit.vim
endif
