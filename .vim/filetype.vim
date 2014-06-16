augroup filetypedetect
    au BufNewFile,BufRead *.automount,*.busname,*.mount,*.service,*.socket,*.target,*.timer,*.link,*.netdev,*.network set ft=systemd cms=#%s
    au BufNewFile,BufRead /etc/systemd/* set ft=systemd cms=#%s
    au BufNewFile,BufRead *.tmux.conf,.tmux.conf*,tmux.conf* setf tmux

    au BufNewFile,BufRead PKGBUILD* set syn=sh ts=2 sw=2 et
    au BufNewFile,BufRead */elinks/*.conf setf elinks
    au BufNewFile,BufRead *.md setf markdown
    au BufNewFile,BufRead *.ml,*.ml[ilyp] setl cms=(*%s*)
    au BufNewFile,BufRead *.muttrc setf muttrc
    au BufNewFile,BufRead ~/.rtorrent.rc,*/rtorrent/config set ft=cfg cms=#%s
    au BufNewFile,BufRead *.sage,*.spyx,*.pyx setf python
    au BufNewFile,BufRead *.pentadactylrc,*.penta set ft=pentadactyl cms=\"%s
    au BufNewFile,BufRead *.vimperatorrc,*.vimp set ft=vimperator cms=\"%s
augroup END
