if exists('b:current_syntax')
    finish
endif
"runtime syntax/scheme.vim

syn sync fromstart
" This is easier than pattern matching for case
syn keyword schemeBoolean true false
syn keyword schemeConstant nil

" Special forms
syn keyword schemeSyntax match let let* letrec define define-object begin and
            \ or if cond when unless

" Primops, copied from the source table
syn keyword schemeFunction ++ -- bool bool-not bytes->string car make-bytes
            \ nil? number? string? symbol? function? list? file? bytes? vector?
            \ number->char string->list string-concat vector-length
            \ bytes-length cdr char->number file-close file-get-pos intern
            \ list->string make-vector number->string print string->bytes
            \ string->number string->symbol string-length symbol->string
            \ string-hash symbol-hash bytes-hash object?
            \ set! cons + - * < > <= >= / mod eq? not-eq?
            \ file-open file-read-string file-read-bytes file-write
            \ file-set-pos string-ref bytes-ref vector-ref apply
            \ bytes-set! vector-set!

" Function-style transforms and library functions
syn keyword schemeFunction not boolean? not-nil? first second third fourth
            \ fifth sixth seventh eighth nineth tenth equal? not-equal?
            \ for-each error type fold-left fold-right reduce filter remove
            \ map reverse list-copy vector-map member assoc alist-cons
            \ alist-delete unique length concat append string-append
            \ char->upper char->lower string->upper string->lower
            \ string-replace bytes->list list->bytes bytes vector->list
            \ list->vector vector vector-resize vector-copy take drop list-ref
            \ list-index string-head string-tail substring assert list*


" syn keyword schemefunction syntax-transform

syn match lsEarmuffs "\*\w[a-z_0-9-]*\*"
hi link lsEarmuffs Type

syntax keyword schemeSyntax lambda conceal cchar=λ containedin=ALLBUT,schemeComment
" This highlights correctly but the conceal doesn't work
syntax match shortLambda "/\\" conceal cchar=λ containedin=ALLBUT,schemeComment
hi link shortLambda Statement
" This would be better somewhere else but it works for now
highlight! link Conceal Statement

"exec 'syntax keyword schemeSyntax "/\\" conceal cchar=λ'
