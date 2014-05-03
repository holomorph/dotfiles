" Vim syntax file
" Language:	systemd units
" a stripped down version of the Vim desktop syntax file

if exists("b:current_syntax")
    finish
endif

syn case match

syn match sdGroup	/^\[.*\]/
syn match sdComment	/^\s*#.*$/
syn match sdDelim	/=/ contained
syn match sdKey		/^\u\w\+=/ contains=sdDelim
syn match sdSpecifier	/%\([nNpPiIfcrRtuUhsmbHv%]\)/

hi def link sdGroup	Special
hi def link sdComment	Comment
hi def link sdDelim	Operator
hi def link sdKey	Identifier
hi def link sdSpecifier	SpecialChar

let b:current_syntax = "systemd"

" vim: ts=8
