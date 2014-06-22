" Vim syntax file
" Language:     Pentadactyl configuration file
" Contributor:  Doug Kearns <dougkearns@gmail.com>
" Last Change:  2010 Oct 1

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

runtime! syntax/vim.vim
unlet b:current_syntax

syn include @javascriptTop syntax/javascript.vim
unlet b:current_syntax

" Javascript:
syn region pentadactylJavaScript	matchgroup=pentadactylJSHereDoc
    \ start="\%(js\s\+\)\@<=<<\s*\z(\h\w*\)"hs=s+2 end="^\z1$" contains=@javascriptTop fold

" NOTE: match vim.vim highlighting group names
hi def link pentadactylJSHereDoc	Operator

let b:current_syntax = "pentadactyl"

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: ts=18 sts=0 tw=130
