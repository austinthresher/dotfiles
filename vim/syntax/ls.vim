if exists('b:current_syntax')
    finish
endif
runtime syntax/scheme.vim

syn sync fromstart
" This is easier than pattern matching for case
syn keyword schemeBoolean true false TRUE FALSE
syn keyword schemeConstant nil NIL

syn keyword schemeFunction eq? string->symbol symbol->string string? symbol?
syn keyword schemeFunction function? number? list->string string->list halt
syn keyword schemeFunction char->number number->char string-concat
syn keyword schemeFunction print readc rb wb openr openw intern
syn keyword schemeFunction match nil? list* let let* letrec not-eq? not-nil?
syn keyword schemeFunction file-open file-close file-write
syn keyword schemeFunction file-read-string file-read-bytes

syn keyword schemeFunction EQ? STRING->SYMBOL SYMBOL->STRING STRING? SYMBOL?
syn keyword schemeFunction FUNCTION? NUMBER? LIST->STRING STRING->LIST HALT
syn keyword schemeFunction CHAR->NUMBER NUMBER->CHAR STRING-CONCAT
syn keyword schemeFunction PRINT READC RB WB OPENR OPENW INTERN
syn keyword schemeFunction FILE-OPEN FILE-CLOSE FILE-WRITE
syn keyword schemeFunction FILE-READ-STRING FILE-READ-BYTES
syn keyword schemeFunction MATCH NIL? LIST* LET LET* LETREC NOT-EQ? NOT-NIL?

syn keyword schemeFunction syntax-transform SYNTAX-TRANSFORM

syn match lsEarmuffs "\*\w[a-z_0-9-]*\*"
hi link lsEarmuffs Type
