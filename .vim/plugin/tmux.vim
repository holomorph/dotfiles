" tmux.vim - Set xterm input codes passed by tmux
" Version:        0.1
" Documentation:  help:xterm-modifier-keys man:tmux(1)

if exists("g:loaded_tmux") || &cp
  finish
endif
let g:loaded_tmux = 1

function! s:SetXtermCapabilities()
  set ttymouse=sgr

  execute "set <xUp>=\e[1;*A"
  execute "set <xDown>=\e[1;*B"
  execute "set <xRight>=\e[1;*C"
  execute "set <xLeft>=\e[1;*D"

  execute "set <xHome>=\e[1;*H"
  execute "set <xEnd>=\e[1;*F"

  execute "set <Insert>=\e[2;*~"
  execute "set <Delete>=\e[3;*~"
  execute "set <PageUp>=\e[5;*~"
  execute "set <PageDown>=\e[6;*~"

  execute "set <xF1>=\e[1;*P"
  execute "set <xF2>=\e[1;*Q"
  execute "set <xF3>=\e[1;*R"
  execute "set <xF4>=\e[1;*S"

  execute "set <F5>=\e[15;*~"
  execute "set <F6>=\e[17;*~"
  execute "set <F7>=\e[18;*~"
  execute "set <F8>=\e[19;*~"
  execute "set <F9>=\e[20;*~"
  execute "set <F10>=\e[21;*~"
  execute "set <F11>=\e[23;*~"
  execute "set <F12>=\e[24;*~"
endfunction

if exists('$TMUX_PANE')
  " tmux gives each pane an identifier unique for the life of the server and
  " is passed to the child process of the pane in the TMUX_PANE environment
  " variable
  "
  " For tmux to work correctly, the default-terminal setting for tmux must
  " always be set to ‘screen’ or a derivative of it.

  call s:SetXtermCapabilities()
endif
