;;; twitch.el --- Query streamers from https://twitch.tv -*- lexical-binding: t -*-

;; Copyright (C) 2015-2020  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Version: 0.10
;; Package-Requires: ((emacs "24.3") (let-alist "1.0.5") (seq "1.5"))
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

(require 'calc-ext)
(require 'format-spec)
(require 'json)
(require 'seq)

(eval-when-compile
  (require 'cl-lib)
  (require 'let-alist))

(defgroup twitch nil
  "Query streamers from Twitch"
  :link '(url-link "https://twitch.tv")
  :group 'external)

(defcustom twitch-client-id "kimne78kx3ncx6brgo4mv6wki5h1ko"
  "Client ID for accessing Twitch API."
  :link '(url-link "https://dev.twitch.tv/docs/v5/#getting-a-client-id")
  :type 'string)

(defcustom twitch-player "mpv"
  "The name by which to invoke stream player."
  :type 'string)

(defcustom twitch-player-options '("--title=%t (%n)")
  "Extra arguments to pass to `twitch-player'.
Format specifiers %n, %t, and %i will expand to streamer name,
stream title, and client ID, respectively."
  :type '(repeat string))

(defcustom twitch-streamers nil
  "List of streamer user names on Twitch."
  :type '(repeat (string :tag "Name")))

(defcustom twitch-curl-program "curl"
  "Name by which to invoke the curl program."
  :type 'string)

(defconst twitch-curl-config "header=\"Client-ID: %s\"
url=https://api.twitch.tv/gql
data=%s\n")

(defconst twitch-api-streamer-limit 100
  "Maximum number of streamers allowed in a single query.")

(defconst twitch-graphql-query "query{users(logins:[\"%s\"]){id,login,displayName,profileViewCount,followers{totalCount}stream{title,viewersCount,game{name}}}}")


(defun twitch-build-queries (list count)
  "Return a list of GraphQL queries from user logins in LIST in groups of COUNT."
  (cl-loop for seq in (seq-partition (delete-dups (remq nil list)) count)
           collect (format twitch-graphql-query (mapconcat #'identity seq "\",\""))))

(defun twitch-sort-pred (alist1 alist2)
  "Predicate for sorting alists according to user name."
  (string< (cdr (assq 'login alist1)) (cdr (assq 'login alist2))))

(defun twitch-group-digits (number)
  "Return a formatted string of integer N with digits grouped."
  (let ((str (number-to-string number)))
    (if (< number 10000) str (math-group-float str))))

(defun twitch-format-info (k v)
  (let ((str (format "  %s: %s" k (if (numberp v) (twitch-group-digits v) v))))
    (concat (propertize str 'font-lock-face 'font-lock-comment-face) "\n")))

(defun twitch-format-stream (object)
  (let-alist object
    (propertize
     (concat
      (truncate-string-to-width
       (propertize .displayName 'font-lock-face 'font-lock-type-face) 15 nil nil "…")
      (propertize " " 'display '(space :align-to 16))
      (propertize (replace-regexp-in-string "\r?\n" " " (or .stream.title "") t t)
                  'font-lock-face 'font-lock-variable-name-face)
      "\n"
      (twitch-format-info "Game" .stream.game.name)
      (twitch-format-info "Viewers" .stream.viewersCount)
      (twitch-format-info "Followers" .followers.totalCount)
      (twitch-format-info "Total views" .profileViewCount))
     'url (format "https://twitch.tv/%s" .login) 'name .displayName 'title .stream.title)))

(defun twitch-format-spec (str)
  "Interpolate format specifiers in STR."
  (let ((title (or (get-text-property (point) 'title) ""))
        (name (or (get-text-property (point) 'name) ""))
        deactivate-mark)
    (format-spec str `((?t . ,title) (?n . ,name) (?i . ,twitch-client-id)))))

(defun twitch-insert-entry (object)
  (let* ((entry (twitch-format-stream object))
         (start (point))
         (end (+ start (length entry))))
    (insert entry)
    (let* ((beg (save-excursion
                  (goto-char start)
                  (forward-line)
                  (point)))
           (overlay (make-overlay beg end)))
      (overlay-put overlay 'twitch t)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'isearch-open-invisible #'twitch-toggle-overlay)
      (overlay-put overlay 'invisible t))))

(defun twitch-redraw (list)
  "Erase the buffer and draw a new one from the alists in LIST."
  (let ((first (zerop (length (buffer-string))))
        (link (get-text-property (point) 'url)))
    (with-silent-modifications
      (erase-buffer)
      (mapc #'twitch-insert-entry (sort list #'twitch-sort-pred)))
    (goto-char
     (if first (point-min)
       (save-excursion
         (goto-char (point-min))
         (while (string< (get-text-property (point) 'url) link)
           (forward-line))
         (point))))))

(defmacro twitch-request (data &rest body)
  (declare (indent 1) (debug t))
  (let ((p (make-symbol "process")))
    `(let ((,p (start-process "twitch" (generate-new-buffer " *twitch")
                              twitch-curl-program "-sSqK" "-")))
       (setf (process-sentinel ,p)
             (lambda (process _status)
               (unwind-protect
                   (when (buffer-live-p (process-buffer process))
                     (with-current-buffer (process-buffer process)
                       ,@body))
                 (kill-buffer (process-buffer process)))))
       (process-send-string ,p (format twitch-curl-config twitch-client-id ,data))
       (process-send-eof ,p))))

(defun twitch-refresh (&optional _arg _noconfirm)
  "Erase the buffer and draw a new one."
  (let ((buffer (current-buffer))
        (queries (twitch-build-queries twitch-streamers twitch-api-streamer-limit))
        count list)
    (setq count (length queries))
    (dolist (query queries)
      (twitch-request (json-encode `[(:query ,query)])
        (cl-decf count)
        (goto-char (point-min))
        (search-forward "\"users\":" nil t)
        (cl-loop for object across (json-read-array)
                 when (cdr (assq 'stream object)) do (push object list))
        (when (zerop count)
          (with-current-buffer buffer
            (twitch-redraw list)))))))

(defun twitch-overlay-at (pos)
  (cl-loop for ov in (overlays-at pos) when (overlay-get ov 'twitch) return ov))

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
      (user-error "No stream under point"))))

(defun twitch-browse-url ()
  "Browse the URL of the stream under point."
  (interactive)
  (let ((url (get-text-property (point) 'url)))
    (if url (browse-url url) (user-error "No stream under point"))))

(defvar twitch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [double-mouse-1] 'twitch-open)
    (define-key map (kbd "C-c C-o") 'twitch-open)
    (define-key map "X" 'twitch-open)
    (define-key map (kbd "RET") 'twitch-info)
    (define-key map "i" 'twitch-info)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "W" 'twitch-browse-url)
    (define-key map "w" 'twitch-copy-url)
    map)
  "Keymap used in `twitch-mode' buffers.")

(easy-menu-define twitch-mode-menu twitch-mode-map
  "Menu used in `twitch-mode' buffers."
  '("Twitch"
    ["Open Stream" twitch-open]
    ["Toggle Info" twitch-info]
    ["Browse Stream URL" twitch-browse-url]
    ["Copy Stream URL" twitch-copy-url]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" quit-window]))

(define-derived-mode twitch-mode special-mode "Twitch"
  "Major mode for launching streams from <https://www.twitch.tv>."
  :group 'twitch
  (buffer-disable-undo)
  (setq-local revert-buffer-function #'twitch-refresh))

;;;###autoload
(defun twitch ()
  "Open a `twitch-mode' buffer if `twitch-streamers' is populated."
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
