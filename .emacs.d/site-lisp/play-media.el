;;; play-media.el --- Launch videos and stuff from Emacs

;; Copyright (C) 2014  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Keywords: convenience

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

;; Play media from Emacs using “mpv” or “livestreamer”.  For mpv, at
;; least v0.7.0 is required for its “youtube-dl” hook. For
;; livestreamer, at least v1.9.0 is needed for the “--default-stream”
;; config option.

;;; Code:

(require 'eww)

(defgroup play-media nil
  "Open media with an external player."
  :group 'external)

(defcustom play-media-livestreamer-program "livestreamer"
  "The name by which to invoke livestreamer."
  :group 'play-media
  :type 'string)

(defcustom play-media-mpv-program "mpv"
  "The name by which to invoke mpv."
  :group 'play-media
  :type 'string)

(defun play-media-start-process (program &rest args)
  "Thin wrapper for `start-process'."
  (apply 'start-process "play-media" nil program args))

;;;###autoload
(defun play-media (url)
  "Play media from URL.  Uses mpv (with its youtube-dl hook) or
livestreamer, depending on the input."
  (interactive "sURL: ")
  (if (string-match "\\(hitbox\\|twitch\\)\.tv" url)
      (play-media-start-process play-media-livestreamer-program url)
    (play-media-start-process play-media-mpv-program "--ytdl" (concat "ytdl://" url))))

;;;###autoload
(defun play-media-at-point ()
  "Try to play media at point. See `play-media'."
  (interactive)
  (let ((link (or (thing-at-point 'url)
                  (car-safe (eww-links-at-point))
                  ;; The symbol check facilitates opening media from
                  ;; incomplete URLs or written youtube video hashes.
                  (thing-at-point 'symbol))))
    (if link (play-media link)
      (message "No link found"))))

(provide 'play-media)

;;; play-media.el ends here
