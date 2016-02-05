;;; twitch.el --- Query teams and streamers from http://twitch.tv -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Version: 0.7
;; Package-Requires: ((emacs "24.4") (seq "1.5"))
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

(eval-when-compile (require 'cl-lib))
(require 'json)
(require 'seq)
(require 'url)

(defgroup twitch nil
  "Query teams and streamers from http://twitch.tv"
  :group 'external)

(defcustom twitch-livestreamer-program "livestreamer"
  "The name by which to invoke livestreamer."
  :group 'twitch
  :type 'string)

(defcustom twitch-livestreamer-options '()
  "Extra arguments to pass to `twitch-livestreamer-program'."
  :group 'twitch
  :type '(repeat string))

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
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n" nil t)
    (json-read)))

(defun twitch--batch-list ()
  (let ((copy (delete-dups (remq nil twitch-streamers))))
    (mapcar (lambda (seq) (mapconcat #'identity seq ","))
            (seq-partition copy twitch-api-streamer-limit))))

(defun twitch--munge-v3 (response)
  "Munge streams in v3 API response RESPONSE to be compatible with v2 API."
  (vconcat (mapcar (lambda (a)
                     (list (append (assq 'channel a) (list (assq 'viewers a)))))
                   response)))

(defun twitch-hash (channel &optional v3)
  "Return a hash table of stream information in alist CHANNEL.
The hash table keys are the properties in `twitch-api-plist',
values of which are used to find the key-values in the CHANNEL
alist.  Do API v3-specific things if V3 is non-nil."
  (let ((table (make-hash-table)))
    (cl-loop for (p v) on twitch-api-plist by #'cddr
             with getter = (if v3 'cdr 'car) do
             (let ((val (cdr (assq (funcall getter v) channel))))
               (when (stringp val)
                 (setq val (replace-regexp-in-string "\r?\n" " " val t t)))
               (puthash p val table)))
    table))

(defun twitch-hash-vector (response &optional v3)
  (vconcat (mapcar (lambda (elt) (twitch-hash (cdar elt) v3)) response)))

(defun twitch-get-streamers ()
  "Get stream information using the v3 API with `twitch-streamers'."
  (let ((vector []))
    (dolist (batch (twitch--batch-list))
      (let* ((fmt "https://api.twitch.tv/kraken/streams?channel=%s&limit=%d")
             (url (format fmt batch twitch-api-streamer-limit))
             (streams (cdr-safe (assq 'streams (twitch-request url)))))
        (setq vector (vconcat vector (twitch--munge-v3 streams)))))
    (twitch-hash-vector vector t)))

(defun twitch-get-teams ()
  "Get stream information using the v2 API with `twitch-teams'."
  (let ((vector []))
    (dolist (team twitch-teams)
      (let* ((fmt "http://api.twitch.tv/api/team/%s/live_channels.json")
             (url (format fmt team))
             (channels (cdr-safe (assq 'channels (twitch-request url)))))
        (setq vector (vconcat vector channels))))
    (twitch-hash-vector vector)))

(defun twitch-query ()
  "Return a vector containing hashtables for each streamer from
the users in `twitch-streamers' and teams in `twitch-teams'.  The
vector is sorted lexically by twitch user name; duplicates are
removed."
  (let ((list (seq-uniq (vconcat (twitch-get-teams) (twitch-get-streamers))
                        (lambda (a b) (string= (gethash :user a) (gethash :user b))))))
    (seq-sort (lambda (a b) (string< (gethash :user a) (gethash :user b)))
              (vconcat list))))

(defun twitch-insert-entry (vec url)
  (let* ((entry (mapconcat #'identity vec ""))
         (start (point))
         (end (+ start (length entry))))
    (insert entry)
    (add-text-properties start end (list 'url url))
    (let* ((beg (save-excursion
                  (goto-char start)
                  (forward-line)
                  (point)))
           (overlay (make-overlay beg end)))
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'invisible t))))

(defun twitch-format-info (key val &optional face)
  (concat (propertize (format "  %s " (concat key ":"))
                      'font-lock-face 'font-lock-comment-face)
          (propertize (format "%s" val)
                      'font-lock-face (or face 'font-lock-comment-face))
          "\n"))

(defun twitch-refresh (&optional _arg _noconfirm)
  "Erase the buffer and draw a new one."
  (let ((vector (twitch-query)))
    (setq buffer-read-only nil)
    (erase-buffer)
    (seq-doseq (ht vector)
      (let* ((name (gethash :name ht))
             (title (gethash :title ht))
             (url (or (gethash :url ht) (format "http://twitch.tv/%s" name))))
        (twitch-insert-entry
         (vector (format "%-20.18s" (propertize name 'font-lock-face 'font-lock-type-face))
                 (format "%s\n" (propertize (or title "") 'font-lock-face 'font-lock-variable-name-face))
                 (twitch-format-info "Game" (gethash :game ht))
                 (twitch-format-info "Viewers" (gethash :viewers ht))
                 (twitch-format-info "Followers" (gethash :followers ht))
                 (twitch-format-info "Total views" (gethash :views ht))
                 (twitch-format-info "Bio" (gethash :bio ht)))
         url))))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (goto-char (point-min)))

(defun twitch-overlay-at (position)
  (car (overlays-at position)))

(defun twitch-toggle-overlay (overlay)
  (if (not (overlay-get overlay 'invisible))
      (overlay-put overlay 'invisible t)
    (overlay-put overlay 'invisible nil)
    (or (pos-visible-in-window-p
         (save-excursion
           (goto-char (overlay-end overlay))
           (forward-line -1)
           (point)))
        (recenter -8))))

(defun twitch-info ()
  (interactive)
  (let ((overlay (twitch-overlay-at (point))))
    (if overlay
        (while (twitch-overlay-at (point))
          (forward-line -1))
      ;; Find next info overlay
      (save-excursion
        (while (and (not (eobp)) (not overlay))
          (let ((next (next-overlay-change (point))))
            (setq overlay (twitch-overlay-at next))
            (goto-char next)))))
    (if overlay (twitch-toggle-overlay overlay))))

(defun twitch-open ()
  (interactive)
  (let ((url (get-text-property (point) 'url)))
    (if (not url) (user-error "No stream selected")
      (message "Playing %s" url)
      (apply #'start-process "twitch" nil twitch-livestreamer-program
             (cons url twitch-livestreamer-options)))))

(defun twitch-copy-url ()
  "Copy the URL of the stream under point to the kill ring."
  (interactive)
  (let ((url (get-text-property (point) 'url)))
    (if url (progn (kill-new url) (message "Copied %s" url))
      (message "No stream under point"))))

(defvar twitch-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (define-key map [double-mouse-1] 'twitch-open)
    (define-key map (kbd "C-c C-o") 'twitch-open)
    (define-key map (kbd "RET") 'twitch-info)
    (define-key map (kbd "SPC") 'twitch-info)
    (define-key map "i" 'twitch-info)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "u" 'twitch-copy-url)
    (define-key map "w" 'twitch-copy-url)
    map)
  "Keymap used in `twitch-mode' buffers.")

(easy-menu-define twitch-mode-menu twitch-mode-map
  "Menu used in `twitch-mode' buffers."
  '("Twitch"
    ["Open Stream" twitch-open]
    ["Toggle Info" twitch-info]
    ["Copy Stream URL" twitch-copy-url]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" quit-window]))

(define-derived-mode twitch-mode special-mode "Twitch"
  "Major mode for launching streams from <http://www.twitch.tv>.
The hook `twitch-mode-hook' is run at mode initialization.

Key bindings:
\\{twitch-mode-map}"
  :group 'twitch
  (buffer-disable-undo)
  (setq-local revert-buffer-function #'twitch-refresh))

;;;###autoload
(defun twitch ()
  "Open a `twitch-mode' buffer if `twitch-streamers' or
`twitch-teams' is populated."
  (interactive)
  (let* ((name "*twitch*")
         (buffer (or (get-buffer name) (generate-new-buffer name))))
    (if (not (or twitch-streamers twitch-teams))
        (message "Nothing to show")
      (unless (eq buffer (current-buffer))
        (with-current-buffer buffer
          (unless (eq major-mode 'twitch-mode)
            (twitch-mode)
            (twitch-refresh))
          (switch-to-buffer-other-window buffer))))))

(provide 'twitch)

;;; twitch.el ends here
