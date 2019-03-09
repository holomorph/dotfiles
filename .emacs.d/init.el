;; ~/.emacs.d/init.el

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (make-directory default-directory t)
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq default-frame-alist '((font . "monospace-13")))
(with-demoted-errors (load-theme 'zenburn t))

(if (fboundp 'minibuffer-depth-indicate-mode) (minibuffer-depth-indicate-mode))
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
 debugger-stack-frame-as-list t
 gnutls-min-prime-bits 2048
 history-delete-duplicates t
 history-length 10000
 inhibit-startup-screen t
 mode-line-end-spaces '(:eval "-%-")
 auto-save-default nil
 next-error-highlight t
 search-highlight t
 lazy-highlight-initial-delay 0
 query-replace-highlight t
 recenter-positions '(top middle bottom)
 require-final-newline t
 save-interprogram-paste-before-kill t
 shift-select-mode nil
 scroll-margin 3
 scroll-conservatively 40
 scroll-preserve-screen-position t
 switch-to-buffer-preserve-window-point t
 vc-follow-symlinks t
 x-gtk-use-system-tooltips nil
 x-select-enable-clipboard t
 x-select-enable-primary t)

(setq-default
 Man-width fill-column
 apropos-do-all t
 comint-prompt-read-only t
 compilation-read-command nil
 dired-dwim-target t
 display-time-24hr-format t
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally
 eldoc-idle-delay 0.08
 eldoc-minor-mode-string nil
 eshell-list-files-after-cd t
 gnus-init-file (concat user-emacs-directory "gnus.el")
 gnus-home-directory (concat user-emacs-directory "gnus/")
 ido-enable-flex-matching t
 indent-tabs-mode nil
 mouse-sel-retain-highlight t
 octave-blink-matching-block nil
 octave-comment-char ?%
 package-check-signature t
 python-indent-guess-indent-offset nil
 pulse-flag nil
 reftex-idle-time 0.1
 show-paren-delay 0.02
 shr-width fill-column
 truncate-lines t
 uniquify-buffer-name-style 'forward
 windmove-wrap-around t)

(make-directory "~/.cache/emacs" t)
(set-file-modes "~/.cache/emacs" #o700)
(make-directory "~/.local/share/emacs" t)
(set-file-modes "~/.local/share/emacs" #o700)

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
(define-key help-map "a" 'apropos)

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
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(add-hook 'latex-mode-hook 'reftex-access-scan-info)
(add-hook 'latex-mode-hook #'reftex-mode)
(add-hook 'latex-mode-hook #'auto-fill-mode)
(remove-hook 'sh-mode-hook 'sh-electric-here-document-mode)


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
 notmuch-search-oldest-first nil
 paredit-lighter nil)

(global-set-key (kbd "C-x gb") 'magit-blame)
(global-set-key (kbd "C-x gs") 'magit-status)

;; autoloads
(autoload 'legalese "legalese" "Add legalese to your program files" t)
(autoload 'notmuch "notmuch" "The mail indexer" t)
(autoload 'magit-status "magit" "Control Git from Emacs" t)
(autoload 'magit-blame "magit-blame" "Blame support for Magit" t)
(autoload 'ledger-mode "ledger-mode" "Major mode for editing ledger data" t)
(autoload 'flycheck-mode "flycheck" "Minor mode for on-the-fly syntax checking" t)
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code" t)
(autoload 'company-mode "company" "Modular in-buffer completion framework" t)

;; hooks
(add-hook 'my-lisp-mode-hook 'paredit-mode)
(add-hook 'my-prog-mode-hook 'company-mode)
(add-hook 'my-prog-mode-hook 'flycheck-mode)
(add-hook 'my-prog-mode-hook 'rainbow-delimiters-mode)

(defun my-hook-wrapper (fun &rest args)
  (ignore-errors (apply fun args))
  nil)

(defun my-lisp-modes ()
  (run-hook-wrapped 'my-lisp-mode-hook #'my-hook-wrapper))

(defun my-prog-modes ()
  (run-hook-wrapped 'my-prog-mode-hook #'my-hook-wrapper))

(add-hook 'lisp-interaction-mode-hook #'my-lisp-modes)
(add-hook 'emacs-lisp-mode-hook #'my-lisp-modes)
(add-hook 'scheme-mode-hook #'my-lisp-modes)
(add-hook 'prog-mode-hook #'my-prog-modes)
(add-hook 'tex-mode-hook #'my-prog-modes)
(remove-hook 'magit-region-highlight-hook 'magit-diff-update-hunk-region)
(remove-hook 'magit-section-highlight-hook 'magit-section-highlight)
