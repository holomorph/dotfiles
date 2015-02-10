;;; twitch.el --- Query teams and streamers from http://twitch.tv -*- lexical-binding: t -*-

;; Copyright (C) 2015  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Version: 0.4
;; Package-Requires: ((emacs "24.4") (let-alist "1.0.3") (seq "1.1"))
;; Keywords: convenience, multimedia

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'let-alist)
(require 'seq)
(require 'url)

(defgroup twitch nil
  "Query teams and streamers from http://twitch.tv"
  :group 'external)

(defcustom twitch-livestreamer-program "livestreamer"
  "The name by which to invoke livestreamer."
  :group 'twitch
  :type 'string)

(defcustom twitch-streamers nil
  "List of streamer user names on Twitch."
  :type '(repeat (string :tag "Name"))
  :group 'twitch)

(defcustom twitch-teams nil
  "List of team names on Twitch."
  :type '(repeat (string :tag "Name"))
  :group 'twitch)

(defconst twitch-api-plist
  '(:name (display_name . display_name)
    :title (title . status)
    :viewers (current_viewers . viewers) ; somewhere else in v3
    :views (total_views . views)
    :followers (followers_count . followers)
    :url (link . url)
    :game (meta_game . game)
    :bio (description . bio)            ; somewhere else in v3
    :user (name . name))
  "Plist containing keys for corresponding values of Twitch APIs.
The car of each value corresponds to v2, the cdr to v3. See
https://github.com/justintv/twitch-api for more information.")

(defconst twitch-api-streamer-limit 100
  "Maximum number of streamers allowed in a single query.")

(defun twitch-request (url)
  (with-current-buffer (url-retrieve-synchronously url t)
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n" nil t)
    (json-read)))

(defun twitch--batch-list ()
  (let ((copy (delete-dups (delq nil (seq-copy twitch-streamers)))))
    (mapcar (lambda (seq) (mapconcat #'identity seq ","))
            (seq-partition copy twitch-api-streamer-limit))))

(defun twitch--munge-v3 (response)
  "Munge v3 API response RESPONSE in `twitch-get-streamers' so it is
more compatible with v2."
  (cl-map 'vector (lambda (a)
                    (list (append (assq 'channel a) (list (assq 'viewers a)))))
          response))

(defun twitch--encode-string (s)
  (decode-coding-string (encode-coding-string s 'latin-1) 'utf-8))

(defun twitch-hash (channel &optional v3)
  "Return a hash table of stream information in CHANNEL.  The
hash table keys are the properties in `twitch-api-plist', values
of which are used to find the key-values in the CHANNEL alist.
Do API v3-specific things if V3 is non-nil."
  (let ((table (make-hash-table)))
    (mapc (lambda (elt)
            (let* ((key (funcall (if v3 'cdr 'car) (cadr elt)))
                   (val (cdr (assq key channel))))
              (when (stringp val)
                (let ((newval (if v3 val (twitch--encode-string val))))
                  (setq val (replace-regexp-in-string "\r?\n" " " newval t t))))
              (puthash (car elt) val table)))
          (seq-partition twitch-api-plist 2))
    table))

(defun twitch-hash-vector (response &optional v3)
  (cl-map 'vector (lambda (elt) (twitch-hash (cdar elt) v3)) response))

(defun twitch-get-streamers ()
  "Get stream information using the v3 API with `twitch-streamers'."
  (let ((vector []))
    (dolist (batch (twitch--batch-list))
      (let* ((fmt "https://api.twitch.tv/kraken/streams?channel=%s&limit=%d")
             (url (format fmt batch twitch-api-streamer-limit)))
        (let-alist (twitch-request url)
          (setq vector (vconcat vector (twitch--munge-v3 .streams))))))
    (twitch-hash-vector vector t)))

(defun twitch-get-teams ()
  "Get stream information using the v2 API with `twitch-teams'."
  (let ((vector []))
    (dolist (team twitch-teams)
      (let* ((fmt "http://api.twitch.tv/api/team/%s/live_channels.json")
             (url (format fmt team)))
        (let-alist (twitch-request url)
          (setq vector (vconcat vector .channels)))))
    (twitch-hash-vector vector)))

(defun twitch-query ()
  "Return a vector containing hashtables for each streamer from
the users in `twitch-streamers' and teams in `twitch-teams'.  The
vector is sorted lexically by twitch user name; duplicates are
removed."
  (let ((vector (vconcat (twitch-get-teams)
                         (twitch-get-streamers)))
        (result []))
    ;; Altered `seq-uniq'
    (seq-doseq (elt vector)
      (unless (seq-contains-p result elt
                              (lambda (a b)
                                (string= (gethash :user a) (gethash :user b))))
        (setq result (vconcat result (vector elt)))))
    (seq-sort (lambda (a b) (string< (gethash :user a) (gethash :user b))) result)))

(defun twitch-insert-entry (list url)
  (let* ((entry (mapconcat #'identity (reverse list) ""))
         (start (point))
         (end (+ start (length entry))))
    (insert entry)
    (add-text-properties start end 'url)
    (put-text-property start end 'url url)
    (let* ((beg (save-excursion
                  (goto-char start)
                  (forward-line)
                  (point)))
           (overlay (make-overlay beg end)))
      (overlay-put overlay 'twitch-info t)
      (overlay-put overlay 'invisible t))))

(defun twitch-format-info (key val &optional face)
  (concat (propertize (format "  %s " (concat key ":"))
                      'face 'font-lock-comment-face)
          (propertize (format "%s" (if (numberp val)
                                       (number-to-string val)
                                     val))
                      'face (if face face 'font-lock-comment-face))
          "\n"))

(defun twitch-refresh ()
  "Erase the buffer and draw a new one."
  (interactive)
  (let ((vector (twitch-query))
        (index 0))
    (setq buffer-read-only nil)
    (erase-buffer)
    (while (< index (length vector))
      (let* ((ht (elt vector index))
             (name (gethash :name ht))
             (title (gethash :title ht))
             (url (or (gethash :url ht) (format "http://twitch.tv/%s" name)))
             list)
        (push (format "%-22.18s" (propertize name 'face 'font-lock-type-face)) list)
        (push (format "%s\n" (propertize (or title "") 'face 'font-lock-variable-name-face)) list)
        (push (twitch-format-info "Game" (gethash :game ht)) list)
        (push (twitch-format-info "Viewers" (gethash :viewers ht)) list)
        (push (twitch-format-info "Followers" (gethash :followers ht)) list)
        (push (twitch-format-info "Total views" (gethash :views ht)) list)
        (push (twitch-format-info "Bio" (gethash :bio ht)) list)
        (twitch-insert-entry list url)
        (setq index (1+ index)))))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (goto-char (point-min)))

(defun twitch-info-overlay-at (position)
  (seq-some-p (lambda (ov) (overlay-get ov 'twitch-info))
              (overlays-at position)))

(defun twitch-toggle-overlay (overlay)
  (if (overlay-get overlay 'invisible)
      (progn (overlay-put overlay 'invisible nil)
             (or (pos-visible-in-window-p
                  (save-excursion
                    (goto-char (overlay-end overlay))
                    (forward-line -1)
                    (point)))
                 (recenter -8)))
    (overlay-put overlay 'invisible t)))

(defun twitch-info ()
  (interactive)
  (let ((overlay (twitch-info-overlay-at (point))))
    (if overlay
        (while (twitch-info-overlay-at (point))
          (forward-line -1))
      ;; Find next info overlay
      (save-excursion
        (while (and (not (eobp)) (not overlay))
          (let ((next (next-overlay-change (point))))
            (setq overlay (twitch-info-overlay-at next))
            (goto-char next)))))
    (and overlay (twitch-toggle-overlay overlay))))

(defun twitch-open ()
  (interactive)
  (let ((url (get-text-property (point) 'url)))
    (if url
        (progn (message "Playing %s" url)
               (start-process "twitch" nil twitch-livestreamer-program url))
      (message "No stream selected"))))

(defun twitch-copy-url ()
  "Copy the URL of the stream under point to the kill ring."
  ;; Simpler version of `shr-copy-url'
  (interactive)
  (let ((url (get-text-property (point) 'url)))
    (if url
        (with-temp-buffer
          (insert url)
          (copy-region-as-kill (point-min) (point-max))
          (message "Copied %s" (buffer-string)))
      (message "No stream under point"))))

(defvar twitch-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "C-c C-o") 'twitch-open)
    (define-key map (kbd "RET") 'twitch-info)
    (define-key map (kbd "SPC") 'twitch-info)
    (define-key map "g" 'twitch-refresh)
    (define-key map "i" 'twitch-info)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'quit-window)
    (define-key map "u" 'twitch-copy-url)
    (define-key map "w" 'twitch-copy-url)
    map)
  "Keymap used in `twitch-mode' buffers.")

(define-derived-mode twitch-mode nil "Twitch"
  "Major mode for launching streams from <http://www.twitch.tv>.
The hook `twitch-mode-hook' is run at mode initialization.

Key bindings:
\\{twitch-mode-map}"
  :group 'twitch
  (setq buffer-read-only t)
  (run-mode-hooks 'twitch-mode-hook))

;;;###autoload
(defun twitch ()
  "Open a `twitch-mode' buffer if `twitch-streamers' or
`twitch-teams' is populated."
  (interactive)
  (let* ((name "*twitch*")
         (buffer (or (get-buffer name)
                     (generate-new-buffer name))))
    (if (or twitch-streamers twitch-teams)
        (progn (switch-to-buffer-other-window buffer)
               (unless (equal major-mode 'twitch-mode)
                 (twitch-mode)
                 (twitch-refresh)))
      (message "Nothing to show"))))

(provide 'twitch)

;;; twitch.el ends here
