;;; ~/.emacs.d/gnus.el

(setq-default
 gnus-check-new-newsgroups nil
 gnus-check-bogus-newsgroups nil
 gnus-read-active-file nil
 gnus-read-newsrc-file nil
 gnus-always-read-dribble-file t
 gnus-save-newsrc-file nil)

(setq-default
 gnus-break-pages nil
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

(setq-default
 gnus-thread-sort-functions '(gnus-thread-sort-by-number
                              gnus-thread-sort-by-most-recent-date
                              gnus-thread-sort-by-total-score)
 ;; gnus-use-adaptive-scoring '(word line)
 gnus-score-default-duration 'p
 gnus-score-expiry-days 30
 gnus-decay-scores "\\.ADAPT\\'")

;; methods
(setq-default
 gnus-select-method '(nntp "news.gmane.org" (nntp-address "news.gmane.org")))
