augroup filetypedetect
    au BufNewFile,BufRead *.automount,*.busname,*.mount,*.service,*.socket,*.target,*.link,*.netdev,*.network set ft=systemd cms=#%s
    au BufNewFile,BufRead /etc/systemd/* set ft=systemd cms=#%s
    au BufNewFile,BufRead *.sage,*.spyx,*.pyx setf python
    au BufNewFile,BufRead *.tmux.conf,.tmux.conf*,tmux.conf* setf tmux
    au BufNewFile,BufRead PKGBUILD* set syn=sh ts=2 sw=2 et
augroup END
