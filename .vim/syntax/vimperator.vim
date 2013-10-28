" Vim syntax file
" Language:         VIMperator configuration file
" Contributor:      Doug Kearns <dougkearns@gmail.com>
" Last Change:      2009 Nov 14

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn include @javascriptTop syntax/javascript.vim
unlet b:current_syntax

" Comments:
syn keyword vimperatorTodo	contained FIXME NOTE TODO XXX
syn match   vimperatorComment	+".*$+ contains=vimperatorTodo,@Spell
syn match   vimperatorLineComment	+^\s*".*$+ contains=vimperatorTodo,@Spell

" Commands:
syn keyword vimperatorCommand	contained se[t]
syn keyword vimperatorCommand	hi map nmap noremap nnoremap
syn match   vimperatorCommand	contained "\<z[-+^.=]\="
syn match   vimperatorIsCommand	"\<\h\w*\>" contains=vimperatorCommand

" Options:
syn keyword vimperatorOption	contained complete focuscontent incsearch noscrollbars showstatuslinks smartcase
syn region  vimperatorSet	matchgroup=vimperatorCommand start="\<\%(setl\%[ocal]\|setg\%[lobal]\|se\%[t]\)\>" skip="\%(\\\\\)*\\." end="$" end="<[cC][rR]>" keepend oneline contains=vimperatorOption,vimperatorComment

" Javascript:
syn region vimperatorJavaScript matchgroup=vimperatorJSHereDoc
    \ start="\%(js\s\+\)\@<=<<\s*\z(\h\w*\)"hs=s+2 end="^\z1$" contains=@javascriptTop fold

" NOTE: match vim.vim highlighting group names
hi def link vimperatorCommand	Statement
hi def link vimperatorComment	Comment
hi def link vimperatorJSHereDoc	Operator
hi def link vimperatorLineComment	vimperatorComment
hi def link vimperatorOption	PreProc
hi def link vimperatorTodo	Todo

let b:current_syntax = "vimperator"

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: ts=18 sts=0 tw=130
