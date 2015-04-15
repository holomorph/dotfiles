augroup filetypedetect
    au BufNewFile,BufRead *.automount,*.busname,*.mount,*.service,*.slice,*.socket,*.target,*.timer,*.link,*.netdev,*.network setf systemd
    au BufNewFile,BufRead *.override.conf* setf systemd
    au BufNewFile,BufRead /etc/systemd/* setf systemd

    au BufNewFile,BufRead *.tmux.conf,.tmux.conf*,tmux.conf* setf tmux

    au BufNewFile,BufRead PKGBUILD*,*.install set ft=sh ts=2 sw=2 et
    au BufNewFile,BufRead */elinks/*.conf setf elinks
    au BufNewFile,BufRead llpp.conf setf xml
    au BufNewFile,BufRead *.md setf markdown
    au BufNewFile,BufRead *.ml,*.ml[ilyp] setl cms=(*%s*)
    au BufNewFile,BufRead *.muttrc setf muttrc
    au BufNewFile,BufRead *.sage,*.spyx,*.pyx setf python

    au BufNewFile,BufRead /tmp/pentadactyl.wiki.archlinux.org.* setf mediawiki
    au BufNewFile,BufRead /tmp/pentadactyl.github.com.txt setf markdown
augroup END
