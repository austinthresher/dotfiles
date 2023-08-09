if exists('b:did_ftplugin')
    finish
endif
runtime ftplugin/scheme.vim

setl lisp
setl shiftwidth=2
let &lispwords='define,set!,lambda,if,case,let,let*,letrec,do,labels,'
            \. 'unless,when,match,for,while'
