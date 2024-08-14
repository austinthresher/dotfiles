if exists('b:did_ftplugin')
    finish
endif
runtime ftplugin/scheme.vim

setlocal iskeyword=33,35-39,42-43,45,47-58,60-90,95,97-122,126,92

setl lisp
setl shiftwidth=2
let &lispwords='define,set!,lambda,let,let*,letrec,do,labels,'
            \. 'unless,when,match,for,while,for-each,define-object'
