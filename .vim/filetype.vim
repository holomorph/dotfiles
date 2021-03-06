augroup filetypedetect
    au BufNewFile,BufRead *.tmux.conf,.tmux.conf*,tmux.conf* setf tmux
    au BufNewFile,BufRead */elinks/*.conf setf elinks
    au BufNewFile,BufRead llpp.conf setf xml
    au BufNewFile,BufRead *.md setf markdown
    au BufNewFile,BufRead *.ml,*.ml[ilyp] setl cms=(*%s*)
    au BufNewFile,BufRead *.muttrc setf muttrc
    au BufNewFile,BufRead neomutt-*[0-9]\\\{19\} setf mail
    au BufNewFile,BufRead *.rl setf ragel
    au BufNewFile,BufRead *.rst set tw=70 fo+=t
    au BufNewFile,BufRead *.sage,*.spyx,*.pyx setf python
    au BufNewFile,BufRead *.rules setf udevrules

    au BufReadPre *.nfo setl ro fencs=cp437,utf-8
augroup END
