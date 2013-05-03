;; Copyright (C) 1996-2010  Dirk-Jan C. Binnema.
;; URL: http://www.djcbsoftware.nl/dot-emacs.html
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq make-backup-files t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying-when-linked t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq inhibit-startup-message    t)  ; Don't want any startup message
(setq search-highlight           t)  ; Highlight search object
(setq query-replace-highlight    t)  ; Highlight query object
(setq mouse-sel-retain-highlight t)  ; Keep mouse highlighting

(scroll-bar-mode -1)
(tool-bar-mode   -1)
(menu-bar-mode   -1)

;; modeline
(line-number-mode     t)
(column-number-mode   t)
(size-indication-mode t)

; (require 'linum)
; (global-linum-mode)
; (setq linum-disabled-modes-list '(term-mode wl-summary-mode compilation-mode))
; (defun linum-on ()
;   (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
;     (linum-mode 1)))

; (defun kill-temp-buffers nil
;   "Kill temporary buffers."
;   (interactive)
;   (save-excursion
;     (setq windows (window-list nil t)) ; Get all windows in frame (visible)
;     (setq buffers (buffer-list)) ; Get all buffers
;     (dolist (window windows) ; For each window
;       (setq buffers (remove (window-buffer window) buffers))) ; Skip buffers in windows
;     (dolist (buffer buffers) ; For each buffer
;       (set-buffer buffer)
;       (kill-temp-buffer))))

;; Use keybinding instead
; (global-set-key (kbd "C-M-k") 'kill-temp-buffers)

(fset 'yes-or-no-p 'y-or-n-p)         ;; enable y/n answers to yes/no

;; color theme
; (require 'zenburn)
; (zenburn)
; (require 'rainbow-delimiters)
; (global-rainbow-delimiters-mode)

;; scrolling
;(setq
;  scroll-margin 0                     ;; do smooth scrolling, ...
;  scroll-conservatively 100000        ;; ... the defaults ...
;  scroll-up-aggressively 0            ;; ... are very ...
;  scroll-down-aggressively 0          ;; ... annoying
;  scroll-preserve-screen-position t)  ;; preserve screen pos with C-v/M-v

; (setq completion-ignore-case t        ;; ignore case when completing...
;       read-file-name-completion-ignore-case t) ;; ...filenames too

; (setq initial-scratch-message
;       ";; scratch buffer created -- happy hacking\n")

; (put 'erase-buffer 'disabled nil)     ;; ... useful things
; (file-name-shadow-mode t)             ;; be smart about filenames in mbuf

; (setq inhibit-startup-message t            ;; don't show ...
;       inhibit-startup-echo-area-message t) ;; ... startup messages
; (setq-default cursor-type 'box)            ;; cursor type

;(setq-default truncate-lines t)
; (setq-default indent-tabs-mode nil)

;; prevent or disable autoload vc
; (remove-hook 'find-file-hooks 'vc-find-file-hook)
; (setq vc-handled-backends nil)        ;; disable version control


;; minibuffer
; (setq
;   enable-recursive-minibuffers nil    ;; allow mb commands in the mb
;   max-mini-window-height .25          ;; maximum 2 lines
;   minibuffer-scroll-window nil
;   resize-mini-windows nil)

;(icomplete-mode t)                    ;; completion in minibuffer

; (setq
;   icomplete-prospects-height 1        ;; don't spam my minibuffer
;   icomplete-compute-delay 0)          ;; don't wait
; (require 'icomplete+ nil 'noerror)    ;; drew adams' extras

;; global keybindings
; (global-set-key (kbd "M-g") 'goto-line)

(if window-system
  (set-face-attribute 'default nil :font "monospace" :height 100))

;; BINDINGS

;; Window cycle helpers
(defun goto-next-window nil
  (interactive)
  (select-window (next-window)))

(defun goto-prev-window nil
  (interactive)
  (select-window (previous-window)))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-.") 'repeat)
(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "M-p") 'previous-buffer)
(global-set-key (kbd "C-M-n") 'goto-next-window)
(global-set-key (kbd "C-M-p") 'goto-prev-window)

(add-hook 'term-mode-hook
					(lambda ()
						(define-key term-raw-map (kbd "M-n") 'next-buffer)
						(define-key term-raw-map (kbd "M-p") 'previous-buffer)
						(define-key term-raw-map (kbd "C-M-n") 'goto-next-window)
						(define-key term-raw-map (kbd "C-M-p") 'goto-prev-window)))

;; safe locals
;; we mark these as 'safe', so emacs22+ won't give us annoying warnings
; (setq safe-local-variable-values
;       (quote ((auto-recompile . t)
;               (folding-mode . t)
;               (outline-minor-mode . t)
;               auto-recompile outline-minor-mode)))

