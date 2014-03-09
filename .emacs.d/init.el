;; ~/.emacs.d/init.el

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(load-theme 'zenburn t)

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(blink-cursor-mode 0)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(show-paren-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(set-frame-font "monospace-13")

(setq
 dired-listing-switches "-al -hF --group-directories-first"
 echo-keystrokes 0.1
 gnutls-min-prime-bits 2048
 inhibit-startup-screen t
 auto-save-default nil
 search-highlight t
 query-replace-highlight t
 save-interprogram-paste-before-kill t
 shift-select-mode nil
 scroll-margin 3
 scroll-conservatively 40
 scroll-preserve-screen-position t
 x-gtk-use-system-tooltips nil
 x-select-enable-clipboard t
 x-select-enable-primary t)

(setq-default
 apropos-do-all t
 bookmark-default-file "~/.local/share/emacs/bookmarks"
 compilation-read-command nil
 gnus-home-directory "~/.local/share/emacs/gnus/"
 gnus-init-file (concat user-emacs-directory "gnus")
 ido-enable-flex-matching t
 ido-save-directory-list-file "~/.local/share/emacs/ido"
 indent-tabs-mode nil
 mouse-sel-retain-highlight t
 save-place t
 save-place-file "~/.local/share/emacs/places"
 tramp-persistency-file-name "~/.local/share/emacs/tramp"
 truncate-lines t
 uniquify-buffer-name-style 'forward)

(require 'saveplace)
(require 'uniquify)

;; backup
(setq
 make-backup-files t
 backup-directory-alist `(("." . "~/.cache/emacs/backup"))
 backup-by-copying-when-linked t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "M-i") 'back-to-indentation)
(global-set-key (kbd "M-o") 'other-window)

(windmove-default-keybindings)

(defadvice terminal-init-screen
  (before tmux activate)
  "Apply xterm keymap, allowing use of keys passed through tmux."
  (let ((map (copy-keymap xterm-function-map)))
    (set-keymap-parent map (keymap-parent input-decode-map))
    (set-keymap-parent input-decode-map map)))

;; mail
(setq-default
 message-send-mail-function 'smtpmail-send-it
 smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587)

;; extensions
(autoload 'magit-status "magit" nil t)
(autoload 'ff++-mode "ff++-mode" "Major mode for FreeFem++ code" t)
(add-to-list 'auto-mode-alist '("\\.[ei]dp\\'" . ff++-mode))
