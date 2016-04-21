;;; twitch.el --- Query streamers from http://twitch.tv -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Version: 0.8
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
(require 'format-spec)
(require 'json)
(require 'seq)
(require 'url)

(defgroup twitch nil
  "Query streamers from http://twitch.tv"
  :group 'external)

(defcustom twitch-player "mpv"
  "The name by which to invoke stream player."
  :type 'string)

(defcustom twitch-player-options '("--title=" "%t (%n)")
  "Extra arguments to pass to `twitch-player'.
Format specifiers %n and %t will expand to streamer name and
stream title, respectively."
  :type '(repeat string))

(defcustom twitch-streamers nil
  "List of streamer user names on Twitch."
  :type '(repeat (string :tag "Name")))

(defconst twitch-api-plist
  '(:name display_name
    :title status
    :viewers viewers
    :views views
    :followers followers
    :url url
    :game game
    :user name)
  "Plist containing keywords for corresponding Twitch API keys.
See https://github.com/justintv/twitch-api for more information.")

(defconst twitch-api-streamer-limit 100
  "Maximum number of streamers allowed in a single query.")

(defun twitch-handle-response ()
  (set-buffer-multibyte t)
  (goto-char (point-min))
  (re-search-forward "\r?\n\r?\n" nil t)
  (json-read))

(defun twitch--batch-list ()
  (let ((copy (delete-dups (remq nil twitch-streamers))))
    (mapcar (lambda (seq) (mapconcat #'identity seq ","))
            (seq-partition copy twitch-api-streamer-limit))))

(defun twitch--munge-v3 (response)
  "Munge streams in v3 API response RESPONSE to be compatible with v2 API."
  (mapcar (lambda (a)
            (list (append (assq 'channel a) (list (assq 'viewers a)))))
          response))

(defun twitch-hash (channel)
  "Return a hash table of stream information in alist CHANNEL.
The hash table keys are the properties in `twitch-api-plist',
values of which are used to find the key-values in the CHANNEL
alist."
  (let ((table (make-hash-table)))
    (cl-loop for (p v) on twitch-api-plist by #'cddr do
             (let ((val (cdr (assq v channel))))
               (when (stringp val)
                 (setq val (replace-regexp-in-string "\r?\n" " " val t t)))
               (puthash p val table)))
    table))

(defun twitch-streamer-urls ()
  "Return list of API URLs generated from `twitch-streamers'."
  (mapcar (lambda (batch)
            (format "https://api.twitch.tv/kraken/streams?channel=%s&limit=%d"
                    batch twitch-api-streamer-limit))
          (twitch--batch-list)))

(defun twitch-sort (list)
  "Sort hash tables in LIST according to user name, removing duplicates."
  (sort (seq-uniq list
                  (lambda (a b) (string= (gethash :user a) (gethash :user b))))
        (lambda (a b) (string< (gethash :user a) (gethash :user b)))))

(defun twitch-format-data (ht)
  (let* ((name (gethash :name ht))
         (title (gethash :title ht))
         (user (gethash :user ht))
         (bio (gethash :bio ht)))
    (propertize
     (concat
      (format "%-20.18s" (propertize name 'font-lock-face 'font-lock-type-face))
      (format "%s\n" (propertize (or title "") 'font-lock-face 'font-lock-variable-name-face))
      (twitch-format-info "Game" (gethash :game ht))
      (twitch-format-info "Viewers" (gethash :viewers ht))
      (twitch-format-info "Followers" (gethash :followers ht))
      (twitch-format-info "Total views" (gethash :views ht))
      (if bio (twitch-format-info "Bio" bio)))
     'url (format "http://twitch.tv/%s" user) 'name name 'title title)))

(defun twitch-format-spec (str)
  "Interpolate format specifiers in STR."
  (let ((title (or (get-text-property (point) 'title) ""))
        (name (or (get-text-property (point) 'name) "")))
    (format-spec str `((?t . ,title) (?n . ,name)))))

(defun twitch-insert-entry (ht)
  (let* ((entry (twitch-format-data ht))
         (start (point))
         (end (+ start (length entry))))
    (insert entry)
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

(defun twitch-redraw (list)
  "Erase the buffer and draw a new one from the hash tables in LIST."
  (let ((first (zerop (length (buffer-string))))
        (link (get-text-property (point) 'url)))
    (with-silent-modifications
      (erase-buffer)
      (mapc #'twitch-insert-entry (twitch-sort list)))
    (goto-char
     (if first (point-min)
       (save-excursion
         (goto-char (point-min))
         (while (string< (get-text-property (point) 'url) link)
           (forward-line))
         (point))))))

(defun twitch-refresh (&optional _arg _noconfirm)
  "Erase the buffer and draw a new one."
  (let* ((buffer (current-buffer))
         (urls (twitch-streamer-urls))
         (count (length urls))
         (tables nil))
    (seq-doseq (url urls)
      (url-retrieve
       url
       (lambda (_status)
         (cl-decf count)
         (let ((json (twitch-handle-response)))
           (seq-doseq (elt (twitch--munge-v3 (cdr (assq 'streams json))))
             (push (twitch-hash (cdar elt)) tables)))
         (when (zerop count)
           (with-current-buffer buffer
             (twitch-redraw tables))))
       nil t t))))

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
      (apply #'start-process "twitch" nil twitch-player
             url (mapcar #'twitch-format-spec twitch-player-options)))))

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
  "Open a `twitch-mode' buffer if `twitch-streamers' or is populated."
  (interactive)
  (let* ((name "*twitch*")
         (buffer (or (get-buffer name) (generate-new-buffer name))))
    (if (not twitch-streamers)
        (message "Nothing to show")
      (unless (eq buffer (current-buffer))
        (with-current-buffer buffer
          (unless (eq major-mode 'twitch-mode)
            (twitch-mode)
            (twitch-refresh))
          (switch-to-buffer-other-window buffer))))))

(provide 'twitch)

;;; twitch.el ends here
