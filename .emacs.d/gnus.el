;;; ~/.emacs.d/gnus.el

(setq-default
 gnus-message-archive-group "sent"
 gnus-mode-line-image-cache nil
 gnus-startup-file (concat gnus-home-directory "newsrc")
 gnus-summary-line-format "%U%R%z%(%[%&user-date; %-15,15f%]%) %B%s\n"
 gnus-user-date-format-alist '((t . "%m/%d"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "└► "
 gnus-sum-thread-tree-vertical "│")

;; methods
(setq-default
 gnus-select-method '(nntp "news.gmane.org"
                           (nntp-open-connection-function nntp-open-tls-stream)
                           (nntp-port-number 563)
                           (nntp-address "news.gmane.org")))
