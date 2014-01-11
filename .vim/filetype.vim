augroup filetypedetect
    au BufNewFile,BufRead *.automount,*.mount,*.service,*.socket,*.target,*.link setf desktop
    au BufNewFile,BufRead *.sage,*.spyx,*.pyx setf python
    au BufNewFile,BufRead *.tmux.conf,.tmux.conf*,tmux.conf* setf tmux
    au BufNewFile,BufRead PKGBUILD* set syn=sh ts=2 sw=2 et
augroup END
