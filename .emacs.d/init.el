;; ~/.emacs.d/init.el

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'zenburn-theme)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(show-paren-mode 1)

(setq
 inhibit-startup-screen t
 compilation-read-command nil
 auto-save-default nil
 search-highlight t
 query-replace-highlight t
 mouse-sel-retain-highlight t
 scroll-margin 3
 scroll-conservatively 40
 scroll-preserve-screen-position t)

(setq-default
 truncate-lines t
 indent-tabs-mode nil)

;; backup
(setq make-backup-files t
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
(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "M-p") 'previous-buffer)
(global-set-key (kbd "C-M-n") 'goto-next-window)
(global-set-key (kbd "C-M-p") 'goto-prev-window)

(add-hook 'term-mode-hook (lambda ()
  (define-key term-raw-map (kbd "M-n") 'next-buffer)
  (define-key term-raw-map (kbd "M-p") 'previous-buffer)
  (define-key term-raw-map (kbd "C-M-n") 'goto-next-window)
  (define-key term-raw-map (kbd "C-M-p") 'goto-prev-window)))
