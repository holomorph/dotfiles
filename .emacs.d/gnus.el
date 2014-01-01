;;; ~/.emacs.d/gnus.el

(setq
 gnus-mode-line-image-cache nil
 gnus-startup-file (concat gnus-home-directory "newsrc")
 gnus-summary-line-format "%U%R%z%(%[%&user-date; %-15,15f%]%) %B%s\n"
 gnus-user-date-format-alist '((t . "%m/%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "└► "
 gnus-sum-thread-tree-vertical "│")

;; methods
(setq
 gnus-select-method '(nntp "news.gwene.org" (nntp-port-number 119))
 gnus-secondary-select-methods
 '((nnmaildir
    "gmail"
    (directory "~/.mutt/mail")
    (directory-files nnheader-directory-files-safe)
    (get-new-mail nil))))
