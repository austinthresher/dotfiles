" Windows-specific configuration

" Use Git Bash as the shell, but only if we didn't launch from cmd.exe
let s:git_bash = 'C:/Program Files/Git/usr/bin/bash.exe'
if glob(s:git_bash) != '' && $TERM !=# 'vtpcon'
    let &shell = '"' .. s:git_bash .. '"'
    set shellcmdflag=-c
    set shellxquote="
    set shellslash
endif
