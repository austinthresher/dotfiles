if exists("b:current_syntax")
    finish
endif

syn region QuickfixFilename start="^<" end=">"
            \ contains=QuickfixNumber
            \ nextgroup=
                \QuickfixError,
                \QuickfixWarning,
                \QuickfixNote,
                \QuickfixInfo,
                \QuickfixOther
                \ skipwhite

syn match QuickfixNumber ":[0-9]*" contained
syn keyword QuickfixError error contained nextgroup=QuickfixMessage
syn keyword QuickfixWarning warning contained nextgroup=QuickfixMessage
syn keyword QuickfixNote note contained nextgroup=QuickfixMessage
syn keyword QuickfixInfo info contained nextgroup=QuickfixMessage
syn keyword QuickfixOther other contained nextgroup=QuickfixMessage
syn region QuickfixMessage start="." end="$" contained contains=QuickfixGCCFlag
syn match QuickfixGCCFlag "\[-W[a-zA-Z0-9_=-]*\]"

hi def link QuickfixError Error
hi def link QuickfixWarning Warning
hi def link QuickfixNote Todo
hi def link QuickfixInfo Debug
hi def link QuickfixOther Identifier
hi def link QuickfixFileName cIncluded
hi def link QuickfixNumber Normal
hi def link QuickfixMessage String
hi def link QuickfixEntry Comment
hi def link QuickfixGCCFlag NonText

hi QuickFixLine ctermfg=NONE ctermbg=237 guifg=NONE guibg=#333333

let b:current_syntax = "qf"
