" Instead of highlighting builtins, highlight the places builtins are used
" so that user-defined types and functions don't clash as much.
" Note that this will not work with non-ascii identifiers

" Builtins that aren't going to fit the categories below
syn keyword pythonBuiltin NotImplemented Ellipsis __debug__

" Type annotations, including nested stuff like tuple[list[str], int]
syn match pythonType /\h\w*/ contained display
syn region pythonTypeSub start=/\[/ end=/\]/ contains=pythonTypeSub,pythonType contained
syn region pythonTypeAnno start=/[^)]\zs\:\|->/rs=e+1,hs=e+1 end=/=\|(\|:\|,\|\n/re=e-1,he=e-1 contains=pythonTypeSub,pythonType display
" Also match right after dtype= since numpy is so commonly used, only works inside function calls
syn match pythonType /\<dtype\>\s*=\s*\zs\<\h\w*\(\.\h\w*\)\?\>\ze/ contained containedin=pythonFunctionCall,pythonFunction display
" Ensure that dictionaries don't confuse the type annotation highlighting
syn region pythonDict start=/{/ end=/}/ transparent contains=TOP,pythonTypeAnno
" Prevent false matches after one-line conditionals / loops
syn region pythonNoTypes start=/./ end=/$/ transparent contains=TOP,pythonTypeAnno contained
syn keyword pythonRepeat for while nextgroup=pythonNoTypes skipwhite
syn keyword pythonConditional elif else if nextgroup=pythonNoTypes skipwhite
syn keyword pythonStatement with nextgroup=pythonNoTypes skipwhite

" Function calls
syn region pythonFunctionCall matchgroup=pythonFunction start=/\<\h\w*\>(/rs=e-1 end=/)/re=s+1 transparent matchgroup=pythonFunction contains=TOP

" Imports / includes
syn clear pythonInclude
syn match pythonImported /\h\w*\(\.\h\w*\)\?/ contained
syn keyword pythonImport import nextgroup=pythonImported skipwhite
syn region pythonFromImport matchgroup=pythonImport start="\<from\>" end="\<import\>" display

" Make docstrings comments
" Looks for a triple-quote that is the first non-whitespace on the line
" Make sure the first quote of the ending triple-quote was not escaped
syn region pythonDoc start=/^\s*"""/ end=/\\\@<!"""/

" None / True / False
syn keyword pythonConstant None
syn keyword pythonBoolean True False

" Links
hi link pythonDoc Comment
hi link pythonImport Include
hi link pythonFromImport cIncluded
hi link pythonImported cIncluded
hi link pythonType Type
hi link pythonConstant Constant
hi link pythonBoolean Boolean
