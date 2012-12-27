;; -*-mode: Emacs-Lisp; folding-mode:t-*-
;; Copyright (C) 1996-2010  Dirk-Jan C. Binnema.
;; URL: http://www.djcbsoftware.nl/dot-emacs.html
;; This file is free software licensed under the terms of the
;; GNU General Public License, version 3 or later.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set the load path  

;; add everything under ~/.emacs.d to it
(let* ((my-lisp-dir "/home/mvo/.emacs.d/")
       (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))
;;(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")
;;(add-to-list 'load-path "~/path/to/matlab-emacs")
;;(load-library "matlab-load")
;;(matlab-cedet-setup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; general settings
;; put these three setting into Xresources, instead!
(menu-bar-mode -1)                      ;; show the menu...
(tool-bar-mode -1)                      ;; turn-off toolbar
(scroll-bar-mode -1)                    ;; turn off scroll bar

(setq ;; scrolling
  scroll-margin 0                       ;; do smooth scrolling, ...
  scroll-conservatively 100000          ;; ... the defaults ...
  scroll-up-aggressively 0              ;; ... are very ...
  scroll-down-aggressively 0            ;; ... annoying
  scroll-preserve-screen-position t)    ;; preserve screen pos with C-v/M-v

(setq search-highlight t                ;; highlight when searching...
      query-replace-highlight t)        ;; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)           ;; enable y/n answers to yes/no

(setq completion-ignore-case t          ;; ignore case when completing...
      read-file-name-completion-ignore-case t) ;; ...filenames too

(setq initial-scratch-message
      ";; scratch buffer created -- happy hacking\n")

(put 'narrow-to-region 'disabled nil)   ;; enable...
(put 'erase-buffer 'disabled nil)       ;; ... useful things
(file-name-shadow-mode t)               ;; be smart about filenames in mbuf

(setq inhibit-startup-message t            ;; don't show ...
      inhibit-startup-echo-area-message t) ;; ... startup messages
(setq-default cursor-type 'box)            ;; cursor type

(setq-default
  frame-title-format
  '(:eval
     (format "%s@%s:%s"
             (or (file-remote-p default-directory 'user) user-login-name)
             (or (file-remote-p default-directory 'host) system-name)
             (file-name-nondirectory (or (buffer-file-name) default-directory)))))

;; color theme
(require 'org-install)
(require 'color-theme)
(require 'zenburn)
(zenburn)
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;;(set-face-attribute 'default nil :font "cousine" :height 90)
;;(set-face-attribute 'default nil :font "dejavu sans mono")
(if window-system
  (set-face-attribute 'default nil :font "envy code r" :height 90))

(remove-hook 'find-file-hooks 'vc-find-file-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the modeline
;; 
(line-number-mode t)                     ;; show line numbers
(column-number-mode t)                   ;; show column numbers
(size-indication-mode t)                 ;; show file size (emacs 22+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the minibuffer
;;
(setq
  enable-recursive-minibuffers nil       ;; allow mb commands in the mb
  max-mini-window-height .25             ;; maximum 2 lines
  minibuffer-scroll-window nil
  resize-mini-windows nil)

(icomplete-mode t)                       ;; completion in minibuffer

(setq 
  icomplete-prospects-height 1         ;; don't spam my minibuffer
  icomplete-compute-delay 0)         ;; don't wait
(require 'icomplete+ nil 'noerror)       ;; drew adams' extras
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; global keybindings
;;
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-g") 'goto-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; safe locals
;; we mark these as 'safe', so emacs22+ won't give us annoying warnings
(setq safe-local-variable-values
      (quote ((auto-recompile . t)
              (folding-mode . t)
              (outline-minor-mode . t)
              auto-recompile outline-minor-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
