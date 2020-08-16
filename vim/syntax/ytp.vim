if exists("b:current_syntax")
	finish
endif

syn keyword ytpKeywords if else let while fn return extern const val as
syn keyword ytpTypes int float string void bool byte
syn keyword ytpBooleanConstant true false 


syn match ytpIdentifiers '[_A-Za-z][0-9a-zA-Z_]*'

" Numbers
" 12345 and 12345f
syn match ytpNumbers '-\?\d\+f\?'
syn match ytpNumbers '-\?\d\+\.\d\+f\?'


syn match ytpComment "//.*$"

syn region syntaxElementRegion start='{' end='}' transparent fold


let b:current_syntax = "ytp"

hi def link ytpKeywords Statement
hi def link ytpComment Comment
hi def link ytpNumbers Number
hi def link ytpTypes Type
hi def link ytpIdentifiers Identifier
hi def link ytpBooleanConstant Constant
