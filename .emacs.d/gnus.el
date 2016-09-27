;;; ~/.emacs.d/gnus.el

(setq-default
 gnus-message-archive-group "sent"
 gnus-mode-line-image-cache nil
 gnus-read-newsrc-file nil
 gnus-startup-file (concat gnus-home-directory "newsrc")
 gnus-summary-line-format "%U%R%z%(%&user-date; %-17,17f%) %B%s\n"
 gnus-thread-indent-level 2
 gnus-user-date-format-alist '((t . "%m/%d"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-root "")

;; methods
(setq-default
 gnus-select-method '(nntp "news.gmane.org"
                           (nntp-open-connection-function nntp-open-tls-stream)
                           (nntp-port-number 563)
                           (nntp-address "news.gmane.org")))
