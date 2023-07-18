if exists('b:current_syntax')
    finish
endif
runtime syntax/scheme.vim

syn sync fromstart
" This is easier than pattern matching for case
syn keyword schemeFunction eq? str sym str? sym? func? num? strcat split halt
syn keyword schemeFunction EQ? STR SYM STR? SYM? FUNC? NUM? STRCAT SPLIT HALT
syn keyword schemeFunction print readc rb wb openr openw intern
syn keyword schemeFunction PRINT READC RB WB OPENR OPENW INTERN
syn keyword schemeBoolean true false TRUE FALSE
syn keyword schemeConstant nil NIL

syn match lsEarmuffs "\*\w[a-z_0-9-]*\*"
hi link lsEarmuffs Type
