;; ~/.emacs.d/init.el

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'saveplace)
(require 'uniquify)
(require 'zenburn-theme)

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(blink-cursor-mode 0)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(show-paren-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(set-face-attribute 'default nil :family "monospace" :height 130)

(setq
 apropos-do-all t
 dired-listing-switches "-al -hF --group-directories-first"
 gnus-home-directory "~/.local/share/emacs/gnus/"
 gnus-init-file (concat user-emacs-directory "gnus")
 gnutls-min-prime-bits 2048
 inhibit-startup-screen t
 compilation-read-command nil
 auto-save-default nil
 auto-save-list-file-prefix "~/.cache/emacs/auto-save-list/.saves-"
 echo-keystrokes 0.1
 search-highlight t
 query-replace-highlight t
 eshell-directory-name "~/.local/share/emacs/eshell"
 ido-enable-flex-matching t
 ido-save-directory-list-file "~/.local/share/emacs/ido"
 mouse-sel-retain-highlight t
 save-interprogram-paste-before-kill t
 scroll-margin 3
 scroll-conservatively 40
 scroll-preserve-screen-position t
 shift-select-mode nil
 tramp-persistency-file-name "~/.local/share/emacs/tramp"
 x-gtk-use-system-tooltips nil
 x-select-enable-clipboard t
 x-select-enable-primary t)

(setq-default
 indent-tabs-mode nil
 save-place t
 save-place-file "~/.local/share/emacs/places"
 truncate-lines t
 uniquify-buffer-name-style 'forward)

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
(defun goto-next-window nil
  (interactive)
  (select-window (next-window)))
(defun goto-prev-window nil
  (interactive)
  (select-window (previous-window)))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "M-i") 'back-to-indentation)
(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "M-p") 'previous-buffer)
(global-set-key (kbd "C-M-n") 'goto-next-window)
(global-set-key (kbd "C-M-p") 'goto-prev-window)

(add-hook 'term-mode-hook (lambda ()
  (define-key term-raw-map (kbd "M-n") 'next-buffer)
  (define-key term-raw-map (kbd "M-p") 'previous-buffer)
  (define-key term-raw-map (kbd "C-M-n") 'goto-next-window)
  (define-key term-raw-map (kbd "C-M-p") 'goto-prev-window)))

;; extensions
(autoload 'magit-status "magit" nil t)
(autoload 'freefem++-mode "freefem++-mode" "Major mode for FreeFem++ code" t)
(add-to-list 'auto-mode-alist '("\\.[ei]dp\\'" . freefem++-mode))
