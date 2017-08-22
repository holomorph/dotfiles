au BufNewFile,BufRead *.automount,*.busname,*.mount,*.service,*.slice,*.socket,*.target,*.timer,*.link,*.netdev,*.network setf systemd
au BufNewFile,BufRead .#override.conf\x\\\{16\} setf systemd
au BufNewFile,BufRead */systemd/*.d/[^/]\+.conf setf systemd
au BufNewFile,BufRead /etc/systemd/* setf systemd
