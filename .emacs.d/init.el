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
(winner-mode)

(fset 'yes-or-no-p 'y-or-n-p)

(setq
 default-frame-alist '((font . "monospace-13"))
 dired-listing-switches "-al -hF --group-directories-first"
 echo-keystrokes 0.1
 gnutls-min-prime-bits 2048
 inhibit-startup-screen t
 mode-line-end-spaces '(:eval "-%-")
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
 c-basic-offset 4
 compilation-read-command nil
 gnus-home-directory "~/.local/share/emacs/gnus/"
 gnus-init-file (concat user-emacs-directory "gnus")
 ido-enable-flex-matching t
 ido-save-directory-list-file "~/.cache/emacs/ido"
 indent-tabs-mode nil
 mouse-sel-retain-highlight t
 save-place t
 save-place-file "~/.cache/emacs/places"
 tramp-persistency-file-name "~/.local/share/emacs/tramp"
 truncate-lines t
 uniquify-buffer-name-style 'forward
 windmove-wrap-around t)

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

;; tls
(when (fboundp 'gnutls-available-p) (fmakunbound 'gnutls-available-p))

(setq-default
 tls-program
 '("gnutls-cli --strict-tofu --x509cafile /etc/ssl/certs/ca-certificates.crt --x509certfile ~/.config/ssl/certs/holomorph.pem --x509keyfile ~/.config/ssl/private/holomorph.key -p %p %h"
   "gnutls-cli --strict-tofu --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h"))

;; mail
(setq-default
 message-send-mail-function 'smtpmail-send-it
 smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587)

;; irc
(setq-default
 erc-default-port 6697
 erc-disable-ctcp-replies t
 erc-hide-list '("329" "353" "366")
 erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
 erc-prompt-for-password nil
 erc-nick "holomorph"
 erc-user-full-name "holomorph"
 erc-user-mode "+R")

(defun irc ()
  "ERC with SSL/TLS."
  (interactive)
  (erc-tls :server "chat.freenode.net" :port 7000))

;; org
(setq-default
 org-directory "~/doc/notes"
 org-return-follows-link t
 org-use-speed-commands t)

;; filetype
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))


;; extensions
(setq-default
 flycheck-check-syntax-automatically '(save)
 flycheck-disabled-checkers '(emacs-lisp-checkdoc)
 ledger-highlight-xact-under-point nil)

;; autoloads
(autoload 'clang-format-buffer "clang-format" "Tool to format C/C++/Obj-C code" t)
(autoload 'legalese "legalese" "Add legalese to your program files" t)
(autoload 'newpaste "newpaste" "Paste to http://paste.lisp.org" t)
(autoload 'notmuch "notmuch" "The mail indexer" t)
(autoload 'magit-status "magit" nil t)
(autoload 'ff++-mode "ff++-mode" "Major mode for FreeFem++ code" t)
(autoload 'tuareg-mode "tuareg" "Major mode for OCaml code" t)
(autoload 'ledger-mode "ledger-mode" "Major mode for editing ledger data" t)
(autoload 'flycheck-mode "flycheck" "Minor mode for on-the-fly syntax checking" t)

;; filetype
(add-to-list 'auto-mode-alist '("\\.[ei]dp\\'" . ff++-mode))
(add-to-list 'auto-mode-alist '("\\.ml[ilyp]?\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.ldg\\'" . ledger-mode))

(defun common-prog-modes ()
  "Default modes for `prog-mode-hook'."
  (ignore-errors (flycheck-mode 1)))

(add-hook 'prog-mode-hook 'common-prog-modes)
