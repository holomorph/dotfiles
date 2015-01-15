;;; twitch.el --- Query teams and streamers from http://twitch.tv -*- lexical-binding: t -*-

;; Copyright (C) 2015  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
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

(defun twitch--element (alist key)
  "Return the cdr of of the KEY in ALIST."
  (cdr (assq key alist)))

(defun twitch--batch-list ()
  "Return a list of strings, each element being comma-separated
values of `twitch-streamers' sliced into lengths of
`twitch-api-streamer-limit'.  The custom value `twitch-streamers'
is filtered for nil values and duplicates.  The return value is
meant for consumption by `twitch-get-streamers'."
  (let ((copy (delete-dups (delq nil (copy-sequence twitch-streamers)))))
    (if (<= (length copy) twitch-api-streamer-limit)
        (when copy
          (list (mapconcat 'identity copy ",")))
      (let ((result '()))
        (while copy
          (push (mapconcat 'identity (seq-take copy twitch-api-streamer-limit) ",")
                result)
          (setq copy (seq-drop copy twitch-api-streamer-limit)))
        result))))

(defun twitch--munge-v3 (response)
  "Munge v3 API response RESPONSE in `twitch-get-streamers' so it is
more compatible with v2."
  (let ((index 0)
        (vector []))
    (while (< index (length response))
      (let* ((stream (elt response index))
             (channel (assq 'channel stream))
             (viewers (assq 'viewers stream)))
        (setq vector (vconcat vector
                              (list (list (append channel (list viewers))))))
        (setq index (1+ index))))
    vector))

(defun twitch--encode-string (s)
  (decode-coding-string (encode-coding-string s 'latin-1) 'utf-8))

(defun twitch-hash (channel &optional v3)
  "Return a hash table of stream information.  The hash table
keys are the properties in `twitch-api-plist', the values of
which are used to find the key-values in the CHANNEL alist.

Do API v3-specific things if V3 is non-nil."
  (let ((plist twitch-api-plist)
        (props (/ (length twitch-api-plist) 2))
        (hashtable (make-hash-table)))
    (dotimes (_ props)
      (let* ((prop (pop plist))
             (key (funcall (if v3 'cdr 'car) (pop plist)))
             (val (twitch--element channel key)))
        ;; Remove newlines and "fix" encoding
        (when (and (or (eq prop :title) (eq prop :game) (eq prop :bio)) val)
          (let ((newval (if v3 val (twitch--encode-string val))))
            (setq val (replace-regexp-in-string "\r?\n" " " newval t t))))
        (puthash prop (or val "") hashtable)))
    hashtable))

(defun twitch-hash-vector (response &optional v3)
  (let ((index 0)
        (vector []))
    (while (< index (length response))
      (let ((channel (cdar (elt response index))))
        (setq vector (vconcat vector (vector (twitch-hash channel v3)))))
      (setq index (1+ index)))
    vector))

(defun twitch-get-streamers ()
  "Get stream information using the v3 API with `twitch-streamers'."
  (let ((list (twitch--batch-list))
        vector)
    (dolist (batch list)
      (let* ((fmt "https://api.twitch.tv/kraken/streams?channel=%s&limit=%d")
             (url (format fmt batch twitch-api-streamer-limit))
             (response (twitch--element (twitch-request url) 'streams)))
        (setq vector (vconcat vector (twitch--munge-v3 response)))))
    (twitch-hash-vector vector t)))

(defun twitch-get-teams ()
  "Get stream information using the v2 API with `twitch-teams'."
  (let ((vector))
    (dolist (team twitch-teams)
      (let* ((fmt "http://api.twitch.tv/api/team/%s/live_channels.json")
             (url (format fmt team))
             (response (twitch--element (twitch-request url) 'channels)))
        (setq vector (vconcat vector response))))
    (twitch-hash-vector vector)))

(defun twitch-query ()
  "Return a vector containing hashtables for each streamer from
the list of users in `twitch-streamers' and streamers in the
teams in `twitch-teams'.  The vector is sorted lexically by
twitch user name, and duplicates are removed."
  (let ((vector (vconcat (twitch-get-teams)
                         (twitch-get-streamers)))
        (result []))
    ;; Altered `seq-uniq'
    (seq-doseq (elt vector)
      (unless (seq-contains-p result elt
                              (lambda (a b)
                                (string= (gethash :user a) (gethash :user b))))
        (setq result (vconcat result (vector elt)))))
    (sort result
          (lambda (a b)
            (when (string< (gethash :user a) (gethash :user b)) t)))))

(defun twitch-insert-entry (list url)
  (let* ((entry (mapconcat 'identity (reverse list) ""))
         (start (point))
         (end (+ start (length entry))))
    (insert entry)
    (add-text-properties start end '(url help-echo))
    (put-text-property start end 'url url)
    (put-text-property start end 'help-echo url)
    (let* ((beg (save-excursion
                  (goto-char start)
                  (forward-line)
                  (point)))
           (overlay (make-overlay beg end)))
      (overlay-put overlay 'twitch-info t)
      (overlay-put overlay 'intangible t)
      (overlay-put overlay 'invisible t))))

(defun twitch-format-info (key val &optional face)
  (concat (propertize (format "  %s " (concat key ":"))
                      'face 'font-lock-comment-face)
          (propertize (format "%s" (if (numberp val)
                                       (number-to-string val)
                                     val))
                      'face (if face face 'font-lock-comment-face))
          "\n"))

(defun twitch-draw ()
  "Erase the buffer and draw a new one."
  (erase-buffer)
  (let ((vector (twitch-query))
        (index 0))
    (while (< index (length vector))
      (let* ((ht (elt vector index))
             (name (gethash :name ht))
             (title (gethash :title ht))
             (url (gethash :url ht))
             list)
        (push (format "%-22.18s" (propertize name 'face 'font-lock-type-face)) list)
        (push (format "%s\n" (propertize title 'face 'font-lock-variable-name-face)) list)
        (push (twitch-format-info "Game" (gethash :game ht)) list)
        (push (twitch-format-info "Viewers" (gethash :viewers ht)) list)
        (push (twitch-format-info "Followers" (gethash :followers ht)) list)
        (push (twitch-format-info "Total views" (gethash :views ht)) list)
        (push (twitch-format-info "Bio" (gethash :bio ht)) list)
        (twitch-insert-entry list url)
        (setq index (1+ index))))))

(defun twitch-info-overlay-at (position)
  "Return the overlay at POSITION with the 'twitch-info property,
else nil."
  (let* ((overlays (overlays-at position)))
    (when overlays
      (let ((overlay (pop overlays)))
        (while (and overlay (not (overlay-get overlay 'twitch-info)))
          (setq overlay (pop overlays)))
        overlay))))

(defun twitch-toggle-overlay (overlay)
  (if (overlay-get overlay 'twitch-info)
      (if (overlay-get overlay 'invisible)
          (progn (overlay-put overlay 'invisible nil)
                 (overlay-put overlay 'intangible nil))
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'intangible t))
    (warn "bad input")))

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

(defun twitch-refresh ()
  "Erase the buffer and draw a new one."
  (interactive)
  (setq buffer-read-only nil)
  (twitch-draw)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

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
