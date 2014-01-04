" Vim syntax file
" Language:     Pentadactyl configuration file
" Constributor: Doug Kearns <dougkearns@gmail.com>
" Last Change:  2010 Oct 1

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn include @javascriptTop syntax/javascript.vim
unlet b:current_syntax

" Comments:
syn keyword pentadactylTodo	contained FIXME NOTE TODO XXX
syn match   pentadactylComment	excludenl +\s"[^\-:.%#=*].*$+lc=1	contains=pentadactylTodo
syn match   pentadactylComment	+\<endif\s\+".*$+lc=5	contains=pentadactylTodo
syn match   pentadactylComment	+\<else\s\+".*$+lc=4	contains=pentadactylTodo
syn match   pentadactylLineComment	+^[ \t:]*".*$+ contains=pentadactylTodo

" Commands:
syn keyword pentadactylCommand	contained se[t]
syn keyword pentadactylCommand	hi map nmap runtime source
syn match   pentadactylCommand	contained "\<z[-+^.=]\="
syn match   pentadactylIsCommand	"\<\h\w*\>" contains=pentadactylCommand

" Options:
syn keyword pentadactylOption	contained guioptions passkeys runtimepath showtabline
syn region  pentadactylSet	matchgroup=pentadactylCommand start="\<\%(setl\%[ocal]\|setg\%[lobal]\|se\%[t]\)\>" skip="\%(\\\\\)*\\." end="$" end="<[cC][rR]>" keepend oneline contains=pentadactylOption,pentadactylComment,pentadactylString

" Javascript:
syn region pentadactylJavaScript	matchgroup=pentadactylJSHereDoc
    \ start="\%(js\s\+\)\@<=<<\s*\z(\h\w*\)"hs=s+2 end="^\z1$" contains=@javascriptTop fold

" NOTE: match vim.vim highlighting group names
hi def link pentadactylCommand	Statement
hi def link pentadactylComment	Comment
hi def link pentadactylJSHereDoc	Operator
hi def link pentadactylLineComment	pentadactylComment
hi def link pentadactylOption	PreProc
hi def link pentadactylTodo	Todo

let b:current_syntax = "pentadactyl"

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: ts=18 sts=0 tw=130
