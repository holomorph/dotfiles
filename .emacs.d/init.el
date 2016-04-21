;; ~/.emacs.d/init.el

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (make-directory default-directory t)
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq default-frame-alist '((font . "monospace-13")))
(with-demoted-errors (load-theme 'zenburn t))

(if (fboundp 'electric-indent-mode) (electric-indent-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(blink-cursor-mode 0)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(winner-mode)

(if (fboundp 'calc) (defalias 'bc 'calc))
(defalias 'cal 'calendar)
(fset 'display-startup-echo-area-message 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

(setq
 dired-listing-switches "-alhF"
 disabled-command-function nil
 echo-keystrokes 0.1
 enable-dir-local-variables nil
 enable-recursive-minibuffers t
 gnutls-min-prime-bits 2048
 history-length 1000
 inhibit-startup-screen t
 mode-line-end-spaces '(:eval "-%-")
 auto-save-default nil
 next-error-highlight t
 search-highlight t
 lazy-highlight-initial-delay 0
 query-replace-highlight t
 require-final-newline t
 save-interprogram-paste-before-kill t
 shift-select-mode nil
 scroll-margin 3
 scroll-conservatively 40
 scroll-preserve-screen-position t
 vc-follow-symlinks t
 x-gtk-use-system-tooltips nil
 x-select-enable-clipboard t
 x-select-enable-primary t)

(setq-default
 Man-width fill-column
 apropos-do-all t
 c-basic-offset 4
 compilation-read-command nil
 display-time-24hr-format t
 eldoc-idle-delay 0.08
 eshell-list-files-after-cd t
 gnus-init-file (concat user-emacs-directory "gnus.el")
 gnus-home-directory (concat user-emacs-directory "gnus/")
 ido-enable-flex-matching t
 indent-tabs-mode nil
 mouse-sel-retain-highlight t
 octave-blink-matching-block nil
 reftex-idle-time 0.1
 show-paren-delay 0.02
 shr-width fill-column
 truncate-lines t
 uniquify-buffer-name-style 'forward
 windmove-wrap-around t)

(make-directory "~/.cache/emacs" t)
(make-directory "~/.local/share/emacs" t)

(setq-default
 bookmark-default-file "~/.local/share/emacs/bookmarks"
 eww-bookmarks-directory "~/.local/share/emacs"
 ido-save-directory-list-file "~/.cache/emacs/ido-cache"
 kkc-init-file-name "~/.local/share/emacs/kkc-data"
 nsm-settings-file "~/.cache/emacs/nsm-settings"
 recentf-save-file "~/.local/share/emacs/recentf"
 remember-data-file "~/.local/share/emacs/notes"
 save-place-file "~/.local/share/emacs/places"
 savehist-file "~/.local/share/emacs/history"
 tramp-persistency-file-name "~/.cache/emacs/tramp")

(setq custom-file "~/.local/share/emacs/custom.el")
(load custom-file t)

(display-time-mode)
(if (fboundp 'savehist-mode) (savehist-mode 1))
(show-paren-mode)
(if (fboundp 'save-place-mode) (save-place-mode))
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
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c i") 'imenu)

(windmove-default-keybindings)

;; mail
(setq-default
 send-mail-function 'sendmail-send-it
 smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587
 smtpmail-stream-type 'starttls)

;; irc
(setq-default
 erc-disable-ctcp-replies t
 erc-hide-list '("329" "353" "366")
 erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
 erc-port 7070
 erc-prompt-for-password nil
 erc-nick "holomorph"
 erc-server "chat.freenode.net"
 erc-user-full-name "holomorph"
 erc-user-mode "+R"
 rcirc-default-nick "holomorph"
 rcirc-server-alist '(("chat.freenode.net" :port 7070 :encryption tls)))

;; org
(setq-default
 org-agenda-files '("~/doc/notes")
 org-default-notes-file "notes.org"
 org-directory "~/doc/notes"
 org-export-backends '(ascii html latex man texinfo)
 org-log-done 'time
 org-use-speed-commands t)

;; filetype
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(add-hook 'latex-mode-hook 'reftex-access-scan-info)
(add-hook 'latex-mode-hook #'reftex-mode)
(add-hook 'latex-mode-hook #'auto-fill-mode)


;; extensions
(setq-default
 company-idle-delay 0.1
 company-lighter nil
 company-minimum-prefix-length 2
 company-selection-wrap-around t
 company-show-numbers t
 flycheck-check-syntax-automatically '(save)
 flycheck-disabled-checkers '(emacs-lisp-checkdoc make)
 flycheck-display-errors-delay 0.1
 ledger-highlight-xact-under-point nil
 magit-auto-revert-mode nil
 magit-diff-highlight-hunk-body nil
 magit-diff-refine-hunk 'all
 merlin-default-flags '("-w" "+a-4" "-safe-string")
 merlin-show-instance-in-lighter nil
 notmuch-search-oldest-first nil
 paredit-lighter nil)

(global-set-key (kbd "C-x gb") 'magit-blame)
(global-set-key (kbd "C-x gs") 'magit-status)

;; autoloads
(autoload 'legalese "legalese" "Add legalese to your program files" t)
(autoload 'notmuch "notmuch" "The mail indexer" t)
(autoload 'dictionary "dictionary" "Client for RFC2249 dictionary servers" t)
(autoload 'magit-status "magit" "Control Git from Emacs" t)
(autoload 'magit-blame "magit-blame" "Blame support for Magit" t)
(autoload 'tuareg-mode "tuareg" "Major mode for OCaml code" t)
(autoload 'ledger-mode "ledger-mode" "Major mode for editing ledger data" t)
(autoload 'flycheck-mode "flycheck" "Minor mode for on-the-fly syntax checking" t)
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code" t)
(autoload 'company-mode "company" "Modular in-buffer completion framework" t)
(autoload 'merlin-mode "merlin" "Minor mode for interacting with a merlin process" t)

;; filetype
(add-to-list 'auto-mode-alist '("\\.ml[ilyp]?\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.ldg\\'" . ledger-mode))

(defun default-lisp-modes ()
  (ignore-errors (paredit-mode)))

(defun default-prog-modes ()
  (ignore-errors (company-mode))
  (ignore-errors (flycheck-mode))
  (ignore-errors (rainbow-delimiters-mode)))

(add-hook 'lisp-interaction-mode-hook #'default-lisp-modes)
(add-hook 'emacs-lisp-mode-hook #'default-lisp-modes)
(add-hook 'scheme-mode-hook #'default-lisp-modes)
(add-hook 'prog-mode-hook #'default-prog-modes)
(add-hook 'tex-mode-hook #'default-prog-modes)
(remove-hook 'magit-region-highlight-hook 'magit-diff-update-hunk-region)
(remove-hook 'magit-section-highlight-hook 'magit-section-highlight)
(add-hook 'tuareg-mode-hook 'merlin-mode)
