" vim-syntax-extra incorrectly highlights definitions that have a space
" followed by parentheses as function-style macros.
" This redefines cDefine from the built-in C syntax to exclude cUserFunction
" and adds a new highlight group specifically for function-style macros.

syn clear cDefine
syn region cDefine start="^\s*\zs\%(%:\|#\)\s*\%(define\|undef\)\>" skip="\\$" end="$" keepend contains=ALLBUT,@cPreProcGroup,@Spell,cUserFunction
syn match cUserFunctionDefine "\<\h\w*\>("me=e-1 containedin=cDefine contained
hi def link cUserFunctionDefine cFunction
